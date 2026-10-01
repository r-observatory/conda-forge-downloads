# End-to-end runs over a prior release whose summary holds rows the current
# scope rules no longer allow.

# Map every asset a prior run wrote to its path, as if published to the release.
.published <- function(dir) {
  files <- list.files(dir, pattern = "\\.(db|json)$")
  stats::setNames(file.path(dir, files), files)
}

# Rewrite a prior run's published summary and packages cache as the code before
# the metapackage fix left them: r-essentials counted as CRAN's archived essentials.
.plant_essentials_as_cran <- function(dir) {
  row <- data.frame(
    package = "r-essentials", package_lower = "r-essentials", origin = "cran",
    canonical_name = "essentials", total_30d = 500L, total_90d = 500L, total_365d = 500L,
    rank_30d = 1L, rank_90d = 1L, rank_365d = 1L, avg_daily_30d = 16.67, trend = NA_real_,
    first_date = "2026-06-29", last_date = "2026-06-30", identity_state = "archived",
    stringsAsFactors = FALSE)
  for (f in paste0(SHARD_PREFIX, c("-recent.db", "-summary.db"))) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, f))
    DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE package = 'r-essentials'", SUMMARY_TABLE))
    DBI::dbWriteTable(con, SUMMARY_TABLE, row, append = TRUE)
    if (PACKAGES_TABLE %in% DBI::dbListTables(con))
      DBI::dbExecute(con, sprintf(
        "UPDATE %s SET origin = 'cran', canonical_name = 'essentials' WHERE package = 'r-essentials'",
        PACKAGES_TABLE))
    DBI::dbDisconnect(con)
  }
}

.essentials_daily <- function() data.frame(
  date = c("2017-04-05", "2026-06-29", "2026-06-30", "2026-06-30"),
  package = c("r-mass", "r-mass", "r-essentials", "r-ggplot2"),
  count = c(1L, 10L, 500L, 5L), stringsAsFactors = FALSE)

test_that("reclassify-only removes a metapackage that the prior summary counted as a CRAN package", {
  shard_recent <- paste0(SHARD_PREFIX, "-recent.db")
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")
  out1 <- withr::local_tempdir()
  io1 <- fake_io(release_present = FALSE, daily = .essentials_daily(),
                 cran = c("MASS", "ggplot2", "essentials"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  .plant_essentials_as_cran(out1)

  out2 <- withr::local_tempdir()
  io2 <- fake_io(release_present = TRUE, daily = .essentials_daily(),
                 cran = c("MASS", "ggplot2", "essentials"), now = "2026-07-02 05:00:00",
                 shards = .published(out1), fail_fetch = TRUE)
  res2 <- run_update(io2, out2, reclassify_only = TRUE, live_floor = 1L, bioc_floor = 0L)
  expect_setequal(res2$changed_shards, c(shard_recent, shard_summary))

  for (f in c(shard_summary, shard_recent)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, f))
    s <- DBI::dbGetQuery(con, sprintf("SELECT package, rank_30d FROM %s", SUMMARY_TABLE))
    DBI::dbDisconnect(con)
    expect_false("r-essentials" %in% s$package)
    expect_setequal(s$package, c("r-mass", "r-ggplot2"))
    expect_equal(sort(s$rank_30d), c(1L, 2L))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_recent))
  on.exit(DBI::dbDisconnect(con))
  p <- DBI::dbGetQuery(con, sprintf("SELECT origin FROM %s WHERE package = 'r-essentials'", PACKAGES_TABLE))
  expect_equal(p$origin, "other")
  d <- DBI::dbGetQuery(con, sprintf("SELECT SUM(count) AS n FROM %s WHERE package = 'r-essentials'", DAILY_TABLE))
  expect_equal(d$n, 500L)   # the daily counts stay
})

test_that("a run that falls back to a cache still mapping r-essentials to CRAN keeps it out of the summary", {
  shard_summary <- paste0(SHARD_PREFIX, "-summary.db")
  out1 <- withr::local_tempdir()
  io1 <- fake_io(release_present = FALSE, daily = .essentials_daily(),
                 cran = c("MASS", "ggplot2", "essentials"), now = "2026-07-01 05:00:00")
  run_update(io1, out1, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)
  .plant_essentials_as_cran(out1)

  out2 <- withr::local_tempdir()
  daily2 <- rbind(.essentials_daily(), data.frame(
    date = "2026-07-01", package = "r-essentials", count = 40L, stringsAsFactors = FALSE))
  io2 <- fake_io(release_present = TRUE, daily = daily2, now = "2026-07-02 05:00:00",
                 shards = .published(out1), fail_identity = TRUE)
  run_update(io2, out2, force_full = FALSE, live_floor = 1L, bioc_floor = 0L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, shard_summary))
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, sprintf("SELECT package FROM %s", SUMMARY_TABLE))
  expect_false("r-essentials" %in% s$package)
  expect_true("r-mass" %in% s$package)
})
