# Build the `shards` map fake_io() expects (pattern -> file path) from a prior
# run's out_dir, as if every asset it wrote had been published to the release.
release_shards <- function(dir) {
  files <- list.files(dir, pattern = "\\.(db|json)$")
  stats::setNames(file.path(dir, files), files)
}

test_that("cold bootstrap builds year shards, recent, summary, and manifest", {
  shard_2017 <- paste0(SHARD_PREFIX, "-2017.db")
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = daily,
                cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  res <- run_update(io, out, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  expect_true(file.exists(file.path(out, shard_2017)))
  expect_true(file.exists(file.path(out, shard_2026)))
  expect_true(file.exists(file.path(out, shard_recent)))
  expect_true(file.exists(file.path(out, shard_summary)))
  man <- jsonlite::fromJSON(file.path(out, "manifest.json"))
  expect_true(shard_2026 %in% man$changed_shards)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, shard_summary)); on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='r-mass'", SUMMARY_TABLE))
  expect_equal(s$origin, "cran"); expect_equal(s$canonical_name, "MASS")
})

test_that("cold bootstrap aborts rather than publish an empty release when the fetch returns no rows", {
  out <- withr::local_tempdir()
  empty_daily <- data.frame(date = character(0), package = character(0), count = integer(0),
                             stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = empty_daily,
                cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  expect_error(run_update(io, out, force_full = FALSE, live_floor = 1L, bioc_floor = 0L), "cold build fetched no data")
  expect_false(file.exists(file.path(out, "manifest.json")))
})

test_that("cold bootstrap resolves both bioc and cran origins end-to-end through run_update", {
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2026-06-29", "2026-06-30"),
    package = c("bioconductor-deseq2", "r-mass"),
    count = c(10L, 5L), stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = daily,
                cran = c("MASS"), bioc = c("DESeq2"), now = "2026-07-01 05:00:00")
  run_update(io, out, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, shard_summary))
  on.exit(DBI::dbDisconnect(con))
  # If the bioc map were empty, "deseq2" would fail to resolve at all, leaving
  # bioconductor-deseq2 classified as out-of-scope ("other") and dropped from
  # the summary entirely, rather than falling back to a stripped-name
  # canonical value. So this assertion only passes when the bioc map is
  # actually built and applied.
  s_bioc <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='bioconductor-deseq2'", SUMMARY_TABLE))
  expect_equal(s_bioc$origin, "bioc")
  expect_equal(s_bioc$canonical_name, "DESeq2")

  s_cran <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='r-mass'", SUMMARY_TABLE))
  expect_equal(s_cran$origin, "cran")
  expect_equal(s_cran$canonical_name, "MASS")
})

test_that("incremental run adds a new day and touches only that year, recent, and summary", {
  shard_2017 <- paste0(SHARD_PREFIX, "-2017.db")
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  daily2 <- rbind(daily1, data.frame(
    date = "2026-07-01", package = "r-mass", count = 7L, stringsAsFactors = FALSE))
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1))
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  expect_setequal(res2$changed_shards, c(shard_2026, shard_recent, shard_summary))
  expect_false(shard_2017 %in% res2$changed_shards)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_2026))
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con,
    sprintf("SELECT count FROM %s WHERE package='r-mass' AND date='2026-07-01'", DAILY_TABLE))
  expect_equal(d$count, 7L)
})

test_that("an incremental run whose re-fetch is unchanged yields no changed shards but still refreshes the manifest", {
  out1 <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  man1 <- jsonlite::fromJSON(file.path(out1, "manifest.json"))

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily,   # identical source data, nothing new
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 15:00:00",
                 shards = release_shards(out1))
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  expect_length(res2$changed_shards, 0L)
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$last_changed, man1$last_changed)   # carried forward, not bumped
  expect_true(man2$last_checked > man1$last_checked)   # but the check itself is recorded
})

test_that("incremental run aborts rather than publish a truncated shard when a touched-year shard listed in the prior manifest fails to download", {
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  man1 <- jsonlite::fromJSON(file.path(out1, "manifest.json"))
  expect_true(shard_2026 %in% names(man1$shards))  # sanity: prior manifest lists it

  out2 <- withr::local_tempdir()
  daily2 <- rbind(daily1, data.frame(
    date = "2026-07-01", package = "r-mass", count = 7L, stringsAsFactors = FALSE))
  broken_shards <- as.list(release_shards(out1))
  broken_shards[[shard_2026]] <- NULL  # published per manifest, but unfetchable this run
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = broken_shards)

  expect_error(run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L), "protect")
  # The prior manifest was downloaded (needed to determine the revision window)
  # but never rewritten, and the touched-year shard was never (re-)exported.
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$tag, man1$tag)
  expect_false(file.exists(file.path(out2, shard_2026)))
})

test_that("incremental run aborts rather than treat as cold start when the recent shard cannot be downloaded", {
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  broken_shards <- as.list(release_shards(out1))
  broken_shards[[shard_recent]] <- NULL  # published per manifest, but unfetchable this run
  io2 <- fake_io(release_present = TRUE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = broken_shards)

  expect_error(run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L), "protect accumulated history")
  expect_false(file.exists(file.path(out2, shard_recent)))
  expect_false(file.exists(file.path(out2, shard_2026)))
  expect_false(file.exists(file.path(out2, shard_summary)))
})

test_that("incremental run heartbeats rather than errors when the daily source is unreachable", {
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  man1 <- jsonlite::fromJSON(file.path(out1, "manifest.json"))

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1), fail_fetch = TRUE)
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  expect_length(res2$changed_shards, 0L)
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$source_kind, "frozen")
  expect_length(man2$changed_shards, 0L)
  expect_equal(man2$last_changed, man1$last_changed)   # carried forward, not bumped
  expect_false(file.exists(file.path(out2, shard_2026)))

  notes <- readLines(file.path(out2, "release_notes.md"))
  expect_true(any(grepl("source unreachable this run", notes)))
})

test_that("incremental run falls back to the cached packages table when the identity assets are unreachable", {
  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  daily2 <- rbind(daily1, data.frame(
    date = "2026-07-01", package = "r-mass", count = 7L, stringsAsFactors = FALSE))
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = character(0), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1), fail_identity = TRUE)
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_summary))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='r-mass'", SUMMARY_TABLE))
  expect_equal(s$origin, "cran")
  expect_equal(s$canonical_name, "MASS")
  s2 <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='r-ggplot2'", SUMMARY_TABLE))
  expect_equal(s2$origin, "cran")
  expect_equal(s2$canonical_name, "ggplot2")
})

test_that("incremental run falls back to the cached packages table when the identity size gate fails", {
  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  daily2 <- rbind(daily1, data.frame(
    date = "2026-07-01", package = "r-mass", count = 7L, stringsAsFactors = FALSE))
  # Identity fixtures ARE present and reachable this run, but live_floor is set
  # above the fixture's cran-name count, so check_size() fails the gate and the
  # tryCatch inside run_update routes to the same cache fallback as an
  # unreachable-asset (fail_identity) failure.
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1))
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 999999L, bioc_floor = 0L)

  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")
  expect_true(file.exists(file.path(out2, "manifest.json")))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_summary))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='r-mass'", SUMMARY_TABLE))
  expect_equal(s$origin, "cran")
  expect_equal(s$canonical_name, "MASS")
  s2 <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE package='r-ggplot2'", SUMMARY_TABLE))
  expect_equal(s2$origin, "cran")
  expect_equal(s2$canonical_name, "ggplot2")
})

test_that("cold run aborts rather than publish when the identity size gate fails", {
  out <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = daily,
                cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  # Cold builds have no cache to fall back to, so a failed gate must abort.
  expect_error(run_update(io, out, force_full = FALSE, live_floor = 999999L, bioc_floor = 0L),
               "identity size gate failed")
  expect_false(file.exists(file.path(out, "manifest.json")))
})

test_that("cold run drops a conda-only out-of-scope package but publishes an in-scope one", {
  out <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-yr"),
    count = c(10L, 5L), stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = daily,
                cran = c("MASS"), now = "2026-07-01 05:00:00")
  run_update(io, out, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, shard_summary))
  on.exit(DBI::dbDisconnect(con))
  pkgs <- DBI::dbGetQuery(con, sprintf("SELECT package FROM %s", SUMMARY_TABLE))$package
  expect_true("r-mass" %in% pkgs)
  expect_false("r-yr" %in% pkgs)

  # Dropped from the summary does not mean dropped from history: out-of-scope
  # packages are classified but not promoted, while their raw daily counts are
  # still retained in the year shard.
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")
  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, shard_2026))
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  raw_yr <- DBI::dbGetQuery(con2,
    sprintf("SELECT count FROM %s WHERE package='r-yr'", DAILY_TABLE))
  expect_true(nrow(raw_yr) >= 1L)   # raw rows retained even though out-of-scope

  raw_mass <- DBI::dbGetQuery(con2,
    sprintf("SELECT count FROM %s WHERE package='r-mass'", DAILY_TABLE))
  expect_true(nrow(raw_mass) >= 1L)   # sanity: in-scope package's raw rows also present
})

test_that("incremental run carries the prior summary forward so first_date does not regress and an inactive package survives in the roster", {
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-oldpkg", "r-mass", "r-ggplot2"),
    count = c(1L, 1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2", "oldpkg"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  con1 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out1, shard_summary))
  s1 <- DBI::dbGetQuery(con1, sprintf("SELECT * FROM %s WHERE package='r-mass'", SUMMARY_TABLE))
  DBI::dbDisconnect(con1)
  expect_equal(s1$first_date, "2017-04-05")   # sanity: the cold build got this right

  out2 <- withr::local_tempdir()
  daily2 <- rbind(daily1, data.frame(
    date = "2026-07-01", package = "r-mass", count = 7L, stringsAsFactors = FALSE))
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = c("MASS", "ggplot2", "oldpkg"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1))
  run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_summary))
  on.exit(DBI::dbDisconnect(con2))
  s2_mass <- DBI::dbGetQuery(con2, sprintf("SELECT * FROM %s WHERE package='r-mass'", SUMMARY_TABLE))
  expect_equal(s2_mass$first_date, "2017-04-05")   # must not regress to the recent-window start

  s2_old <- DBI::dbGetQuery(con2, sprintf("SELECT * FROM %s WHERE package='r-oldpkg'", SUMMARY_TABLE))
  expect_equal(nrow(s2_old), 1L)                   # must still be present, not vanished
  expect_equal(s2_old$total_365d, 0L)
  expect_equal(s2_old$first_date, "2017-04-05")
})

test_that("force_full re-exports every year shard from the prior manifest, not just the touched-window year", {
  shard_2017 <- paste0(SHARD_PREFIX, "-2017.db")
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  # sanity: the prior manifest lists both years, contrast case below
  man1 <- jsonlite::fromJSON(file.path(out1, "manifest.json"))
  expect_true(shard_2017 %in% names(man1$shards))
  expect_true(shard_2026 %in% names(man1$shards))

  # Same underlying source data (fake_io's fetch_daily filters by requested
  # months, so a fetch spanning full history returns 2017 and 2026 rows alike);
  # only new fresh data is the 2026-07-01 row, same as the plain-incremental test.
  out2 <- withr::local_tempdir()
  daily2 <- rbind(daily1, data.frame(
    date = "2026-07-01", package = "r-mass", count = 7L, stringsAsFactors = FALSE))
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1))
  res2 <- run_update(io2, out2, force_full = TRUE, live_floor = 1L, bioc_floor = 0L)

  expect_true(shard_2017 %in% res2$changed_shards)
  expect_true(shard_2026 %in% res2$changed_shards)
  expect_true(shard_recent %in% res2$changed_shards)
  expect_true(shard_summary %in% res2$changed_shards)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_2017))
  on.exit(DBI::dbDisconnect(con))
  d <- DBI::dbGetQuery(con,
    sprintf("SELECT count FROM %s WHERE package='r-mass' AND date='2017-04-05'", DAILY_TABLE))
  expect_equal(nrow(d), 1L)
  expect_equal(d$count, 1L)
})

test_that("reclassify-only republishes the in-scope summary with zero fetch and touches no year shard", {
  shard_2017 <- paste0(SHARD_PREFIX, "-2017.db")
  shard_2026 <- paste0(SHARD_PREFIX, "-2026.db")
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-yr"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  # fail_fetch makes fetch_daily() stop if called; reclassify-only must never
  # call it, so a passing run here proves zero daily-data fetch.
  io2 <- fake_io(release_present = TRUE, daily = daily1,
                 cran = c("MASS"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1), fail_fetch = TRUE)
  res2 <- run_update(io2, out2, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L)

  expect_length(res2$changed_shards, 2L)
  expect_setequal(res2$changed_shards, c(shard_recent, shard_summary))
  expect_false(shard_2026 %in% res2$changed_shards)
  expect_false(shard_2017 %in% res2$changed_shards)
  expect_false(file.exists(file.path(out2, shard_2026)))
  expect_false(file.exists(file.path(out2, shard_2017)))

  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$source_kind, "reclassify")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_summary))
  on.exit(DBI::dbDisconnect(con))
  pkgs <- DBI::dbGetQuery(con, sprintf("SELECT package, identity_state FROM %s", SUMMARY_TABLE))
  expect_true("r-mass" %in% pkgs$package)
  expect_false("r-yr" %in% pkgs$package)   # origin='other' rows absent from the republished summary
  expect_false(is.na(pkgs$identity_state[pkgs$package == "r-mass"]))
})

test_that("reclassify-only aborts rather than treat a cold start as reclassifiable when there is no existing release", {
  out <- withr::local_tempdir()
  empty_daily <- data.frame(date = character(0), package = character(0), count = integer(0),
                             stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = empty_daily,
                cran = c("MASS"), now = "2026-07-01 05:00:00")
  expect_error(run_update(io, out, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L),
               "existing release")
  expect_false(file.exists(file.path(out, "manifest.json")))
})

test_that("reclassify-only aborts rather than degrade when the identity ledger is unreachable", {
  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-ggplot2"),
    count = c(10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1), fail_identity = TRUE)
  expect_error(run_update(io2, out2, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L),
               "ledger")
  # The prior manifest was downloaded (protect-history, needed before the
  # identity block runs) but never rewritten by this aborted run.
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  man1 <- jsonlite::fromJSON(file.path(out1, "manifest.json"))
  expect_equal(man2$tag, man1$tag)
})

test_that("a same-day re-run replaces rather than duplicates a revised (package, date) row", {
  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  daily2 <- daily1
  daily2$count[daily2$date == "2026-06-29" & daily2$package == "r-mass"] <- 22L
  io2 <- fake_io(release_present = TRUE, daily = daily2,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 15:00:00",
                 shards = release_shards(out1))
  run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_recent))
  on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con,
    sprintf("SELECT count FROM %s WHERE package='r-mass' AND date='2026-06-29'", DAILY_TABLE))
  expect_equal(nrow(rows), 1L)     # replaced, not duplicated
  expect_equal(rows$count, 22L)
})

test_that("a ledger change with no new counts republishes the summary with the recent shard, and the manifest describes that summary", {
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily,
                 cran = c("MASS"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily,   # same counts, the ledger now knows ggplot2
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1))
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  expect_setequal(res2$changed_shards, c(shard_recent, shard_summary))
  s <- read_summary_table(file.path(out2, shard_summary))
  expect_setequal(s$package, c("r-mass", "r-ggplot2"))
  expect_setequal(read_summary_table(file.path(out2, shard_recent))$package, c("r-mass", "r-ggplot2"))
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$db_sha256, file_sha256(file.path(out2, shard_summary)))
})

test_that("with nothing new, the manifest describes the summary already on the release", {
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  # The release's copy holds the same rows in different bytes, as one written by an older SQLite would.
  published <- file.path(withr::local_tempdir(), shard_summary)
  file.copy(file.path(out1, shard_summary), published)
  pcon <- DBI::dbConnect(RSQLite::SQLite(), published)
  DBI::dbExecute(pcon, "PRAGMA user_version = 7")
  DBI::dbDisconnect(pcon)
  shards <- as.list(release_shards(out1))
  shards[[shard_summary]] <- published

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00", shards = shards)
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  expect_length(res2$changed_shards, 0L)
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$db_sha256, file_sha256(published))
  expect_equal(man2$db_bytes, file.size(published))
})

test_that("an identity fallback with no new counts leaves the release's summary and its checksum in place", {
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")

  out1 <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily,
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00",
                 shards = release_shards(out1), fail_identity = TRUE)
  res2 <- run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  # The cached identity has no identity_state, so its summary must not replace the release's.
  expect_length(res2$changed_shards, 0L)
  man2 <- jsonlite::fromJSON(file.path(out2, "manifest.json"))
  expect_equal(man2$db_sha256, file_sha256(file.path(out1, shard_summary)))
})

test_that("force_full aborts rather than publish a year shard with fewer rows than the release lists", {
  shard_2017 <- paste0(SHARD_PREFIX, "-2017.db")

  out1 <- withr::local_tempdir()
  daily1 <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io1 <- fake_io(release_present = FALSE, daily = daily1,
                 cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  # The 2017 shard comes down short and the source no longer has the row either.
  short <- file.path(withr::local_tempdir(), shard_2017)
  file.copy(file.path(out1, shard_2017), short)
  scon <- DBI::dbConnect(RSQLite::SQLite(), short)
  DBI::dbExecute(scon, sprintf("DELETE FROM %s", DAILY_TABLE))
  DBI::dbDisconnect(scon)
  shards <- as.list(release_shards(out1))
  shards[[shard_2017]] <- short

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = daily1[daily1$date != "2017-04-05", ],
                 cran = c("MASS", "ggplot2"), now = "2026-07-02 05:00:00", shards = shards)
  expect_error(run_update(io2, out2, force_full = TRUE, live_floor = 1L, bioc_floor = 0L),
               "protect accumulated history")
  expect_false(file.exists(file.path(out2, "release_notes.md")))
})

test_that("assert_summary_keeps stops when a package on the release would be dropped", {
  prior <- data.frame(package = c("r-mass", "r-ggplot2"), stringsAsFactors = FALSE)
  expect_silent(assert_summary_keeps(prior, data.frame(package = c("r-mass", "r-ggplot2", "r-new"))))
  expect_silent(assert_summary_keeps(NULL, data.frame(package = "r-mass")))
  expect_error(assert_summary_keeps(prior, data.frame(package = "r-mass")),
               "r-ggplot2.*protect accumulated history")
})
