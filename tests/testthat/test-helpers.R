test_that("export_shard round-trips the daily table with DELETE journalling", {
  df <- data.frame(package = c("r-mass", "r-mass", "r-ggplot2"),
                   date = c("2026-06-01", "2026-06-02", "2026-06-01"),
                   count = c(10L, 12L, 5L), stringsAsFactors = FALSE)
  path <- withr::local_tempfile(fileext = ".db")
  export_shard(path, df)
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s ORDER BY package, date", DAILY_TABLE))
  expect_equal(nrow(got), 3L)
  expect_equal(DBI::dbGetQuery(con, "PRAGMA journal_mode")$journal_mode, "delete")
})

test_that("extract_recent and extract_year filter correctly", {
  con <- new_daily_con(data.frame(
    package = "r-mass",
    date = c("2025-01-01", "2026-06-01", "2026-06-30"),
    count = c(1L, 2L, 3L), stringsAsFactors = FALSE))
  on.exit(DBI::dbDisconnect(con))
  rec <- extract_recent(con, "2026-01-01", DAILY_TABLE)
  expect_equal(nrow(rec), 2L)
  yr <- extract_year(con, 2026L, DAILY_TABLE)
  expect_equal(sum(yr$count), 5L)
})

test_that("build_summary computes windows, ranks, trend, and joins identity", {
  # r-mass: 30 days of 10/day ending 2026-06-30; prior 30 days 5/day -> trend +100%
  d1 <- data.frame(package = "r-mass",
                   date = as.character(seq(as.Date("2026-05-02"), as.Date("2026-06-30"), by = "day")),
                   stringsAsFactors = FALSE)
  d1$count <- ifelse(d1$date >= "2026-06-01", 10L, 5L)
  d2 <- data.frame(package = "r-ggplot2", date = "2026-06-30", count = 3L, stringsAsFactors = FALSE)
  con <- new_daily_con(rbind(d1, d2)); on.exit(DBI::dbDisconnect(con))
  cran_names <- data.frame(name_lower = c("mass", "ggplot2"), canonical_name = c("MASS", "ggplot2"),
                           identity_state = c("live", "live"), stringsAsFactors = FALSE)
  maps <- robservatory::resolve_identity_set(cran_names, data.frame(name_lower = character(0), canonical_name = character(0), identity_state = character(0)))
  ident <- resolve_identities(c("r-mass", "r-ggplot2"), maps)
  s <- build_summary(con, ident, DAILY_TABLE)
  mass <- s[s$package == "r-mass", ]
  expect_equal(mass$origin, "cran")
  expect_equal(mass$canonical_name, "MASS")
  expect_equal(mass$total_30d, 300L)     # 30 * 10
  expect_equal(mass$avg_daily_30d, 10)
  expect_equal(mass$trend, 100)          # (300-150)/150 * 100
  expect_equal(mass$rank_30d, 1L)        # ranked above r-ggplot2
  expect_equal(s$package_lower[s$package == "r-mass"], "r-mass")
})

test_that("build_summary returns an empty frame with the right columns when no data", {
  con <- new_daily_con(data.frame(package=character(), date=character(), count=integer())); on.exit(DBI::dbDisconnect(con))
  empty_cran <- data.frame(name_lower = character(0), canonical_name = character(0), identity_state = character(0))
  maps <- robservatory::resolve_identity_set(empty_cran, empty_cran)
  s <- build_summary(con, resolve_identities(character(0), maps), DAILY_TABLE)
  expect_equal(names(s), SUMMARY_COLS)
  expect_equal(nrow(s), 0L)
})

test_that("summary_table_ddl and packages_table_ddl declare the expected columns", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, summary_table_ddl("s"))
  expect_equal(DBI::dbListFields(con, "s"), SUMMARY_COLS)
  DBI::dbExecute(con, packages_table_ddl("p"))
  expect_equal(DBI::dbListFields(con, "p"), c("package", "origin", "canonical_name"))
})

.sample_summary_row <- function() data.frame(
  package = "r-mass", package_lower = "r-mass", origin = "cran", canonical_name = "MASS",
  total_30d = 300L, total_90d = 900L, total_365d = 3650L,
  rank_30d = 1L, rank_90d = 1L, rank_365d = 1L,
  avg_daily_30d = 10, trend = 100,
  first_date = "2026-01-01", last_date = "2026-06-30", stringsAsFactors = FALSE)

test_that("export_summary_shard round-trips SUMMARY_COLS with DELETE journalling", {
  s <- .sample_summary_row()
  path <- withr::local_tempfile(fileext = ".db")
  export_summary_shard(path, s)
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", SUMMARY_TABLE))
  expect_equal(names(got), SUMMARY_COLS)
  expect_equal(got$package, "r-mass")
  expect_equal(DBI::dbGetQuery(con, "PRAGMA journal_mode")$journal_mode, "delete")
})

test_that("embed_aux writes SUMMARY_TABLE and PACKAGES_TABLE into a recent shard that already holds the daily table", {
  daily <- data.frame(package = "r-mass", date = "2026-06-30", count = 10L, stringsAsFactors = FALSE)
  path <- withr::local_tempfile(fileext = ".db")
  export_shard(path, daily)   # recent shard already holds the daily table before embed_aux runs

  summary_df  <- .sample_summary_row()
  packages_df <- data.frame(package = "r-mass", origin = "cran", canonical_name = "MASS",
                            stringsAsFactors = FALSE)
  embed_aux(path, summary_df, packages_df)

  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  expect_true(all(c(DAILY_TABLE, SUMMARY_TABLE, PACKAGES_TABLE) %in% DBI::dbListTables(con)))
  expect_equal(DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", DAILY_TABLE))$n, 1L)
  s <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", SUMMARY_TABLE))
  expect_equal(names(s), SUMMARY_COLS)
  expect_equal(s$package, "r-mass")
  p <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", PACKAGES_TABLE))
  expect_equal(names(p), c("package", "origin", "canonical_name"))
  expect_equal(p$origin, "cran")
})

test_that("write_manifest writes pretty JSON with changed_shards as an array even when empty", {
  path <- withr::local_tempfile(fileext = ".json")
  write_manifest(path, list(tag = "v1", changed_shards = list()))
  txt <- paste(readLines(path), collapse = "\n")
  expect_match(txt, '"changed_shards": \\[\\]')
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(parsed$tag, "v1")
  expect_equal(length(parsed$changed_shards), 0L)

  path2 <- withr::local_tempfile(fileext = ".json")
  write_manifest(path2, list(tag = "v2", changed_shards = as.list(c("a.db", "b.db"))))
  parsed2 <- jsonlite::fromJSON(path2, simplifyVector = FALSE)
  expect_equal(unlist(parsed2$changed_shards), c("a.db", "b.db"))
})

test_that("coverage summarizes row count and date span, NA on empty", {
  rows <- data.frame(package = c("r-mass", "r-mass"), date = c("2026-06-01", "2026-06-30"),
                     count = c(1L, 2L), stringsAsFactors = FALSE)
  cv <- coverage(rows)
  expect_equal(cv$rows, 2L)
  expect_equal(cv$date_min, "2026-06-01")
  expect_equal(cv$date_max, "2026-06-30")

  empty <- coverage(rows[0, ])
  expect_equal(empty$rows, 0L)
  expect_true(is.na(empty$date_min))
  expect_true(is.na(empty$date_max))
})

test_that("merge_shard_coverage overlays updates onto prior coverage, keeping untouched shards", {
  prev <- list(`conda-forge-downloads-2025.db` = list(rows = 10L, date_min = "2025-01-01", date_max = "2025-12-31"))
  updates <- list(`conda-forge-downloads-2026.db` = list(rows = 5L, date_min = "2026-01-01", date_max = "2026-06-30"))
  merged <- merge_shard_coverage(prev, updates)
  expect_equal(names(merged), c("conda-forge-downloads-2025.db", "conda-forge-downloads-2026.db"))
  expect_equal(merged[["conda-forge-downloads-2025.db"]]$rows, 10L)

  expect_equal(merge_shard_coverage(NULL, updates), updates)
})

test_that("write_release_notes renders the manifest summary, caveat, and shard coverage table", {
  manifest <- list(
    last_checked = "2026-06-30T05:00:00Z",
    source_kind = "s3",
    changed_shards = as.list("conda-forge-downloads-recent.db"),
    shards = list(`conda-forge-downloads-recent.db` =
                    list(rows = 400L, date_min = "2025-06-01", date_max = "2026-06-30")),
    summary = list(packages = 3500L, latest_date = "2026-06-30"))
  path <- withr::local_tempfile(fileext = ".md")
  write_release_notes(path, manifest,
    "Counts are conda-forge CDN downloads, not directly comparable across sources.")
  txt <- paste(readLines(path), collapse = "\n")
  expect_match(txt, "2026-06-30 05:00:00 UTC", fixed = TRUE)
  expect_match(txt, "3,500", fixed = TRUE)
  expect_match(txt, "conda-forge-downloads-recent.db", fixed = TRUE)
  expect_match(txt, "not directly comparable across sources", fixed = TRUE)
})

test_that("write_release_notes reports 'no new data' (not 'unreachable') for an empty changed_shards on a live source", {
  manifest <- list(
    last_checked = "2026-06-30T05:00:00Z",
    source_kind = "hourly",
    changed_shards = list(),
    shards = list(),
    summary = list(packages = 3500L, latest_date = "2026-06-30"))
  path <- withr::local_tempfile(fileext = ".md")
  write_release_notes(path, manifest,
    "Counts are conda-forge CDN downloads, not directly comparable across sources.")
  txt <- paste(readLines(path), collapse = "\n")
  expect_match(txt, "none (no new data this run)", fixed = TRUE)
  expect_false(grepl("unreachable", txt, fixed = TRUE))
})

test_that("write_release_notes still reports 'source unreachable' for an empty changed_shards on a frozen (heartbeat) source", {
  manifest <- list(
    last_checked = "2026-06-30T05:00:00Z",
    source_kind = "frozen",
    changed_shards = list(),
    shards = list(),
    summary = list(packages = 3500L, latest_date = "2026-06-30"))
  path <- withr::local_tempfile(fileext = ".md")
  write_release_notes(path, manifest,
    "Counts are conda-forge CDN downloads, not directly comparable across sources.")
  txt <- paste(readLines(path), collapse = "\n")
  expect_match(txt, "none (source unreachable this run)", fixed = TRUE)
})

test_that("build_summary keeps only in-scope rows and ranks them densely", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE d (package TEXT, date TEXT, count INTEGER)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('r-dplyr','2026-07-01',100),('r-base','2026-07-01',999),('r-maptools','2026-07-01',50)")
  ident <- data.frame(
    package = c("r-dplyr", "r-base", "r-maptools"),
    origin = c("cran", "other", "cran"),
    canonical_name = c("dplyr", NA, "maptools"),
    identity_state = c("live", NA, "archived"), stringsAsFactors = FALSE)
  s <- build_summary(con, ident, "d", anchor_date = "2026-07-01")
  expect_setequal(s$package, c("r-dplyr", "r-maptools"))  # r-base (other) excluded
  expect_equal(s$rank_30d[s$package == "r-dplyr"], 1L)    # dense ranks over in-scope only
  expect_equal(s$rank_30d[s$package == "r-maptools"], 2L)
  expect_equal(s$identity_state[s$package == "r-maptools"], "archived")
})

test_that("build_summary never carries forward an out-of-scope prior row, but does carry an in-scope only_prior row", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE d (package TEXT, date TEXT, count INTEGER)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('r-dplyr','2026-07-01',100)")
  ident <- data.frame(package = "r-dplyr", origin = "cran", canonical_name = "dplyr",
                     identity_state = "live", stringsAsFactors = FALSE)

  # Prior published summary: an out-of-scope row (r-base, origin='other') that
  # must never reappear, plus an in-scope row (r-oldpkg) that is inactive in
  # the current window and so must be carried forward as only_prior.
  prior_summary <- data.frame(
    package = c("r-base", "r-oldpkg"),
    package_lower = c("r-base", "r-oldpkg"),
    origin = c("other", "cran"),
    canonical_name = c(NA, "oldpkg"),
    total_30d = c(500L, 200L), total_90d = c(1500L, 600L), total_365d = c(6000L, 2400L),
    rank_30d = c(1L, 2L), rank_90d = c(1L, 2L), rank_365d = c(1L, 2L),
    avg_daily_30d = c(16.67, 6.67), trend = c(0, 0),
    first_date = c("2020-01-01", "2020-01-01"), last_date = c("2026-01-01", "2026-01-01"),
    identity_state = c(NA, "live"), stringsAsFactors = FALSE)

  s <- build_summary(con, ident, "d", anchor_date = "2026-07-01", prior_summary = prior_summary)
  expect_false("r-base" %in% s$package)   # out-of-scope prior row dropped, not carried forward
  expect_true("r-oldpkg" %in% s$package)  # in-scope only_prior row is carried forward
  expect_true("r-dplyr" %in% s$package)
})

test_that("build_summary backfills a missing identity_state column from an old-schema prior summary without erroring", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE d (package TEXT, date TEXT, count INTEGER)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('r-dplyr','2026-07-01',100)")
  ident <- data.frame(package = "r-dplyr", origin = "cran", canonical_name = "dplyr",
                     identity_state = "live", stringsAsFactors = FALSE)

  # Old-schema prior summary published before identity_state existed: no such
  # column at all. r-oldpkg2 is inactive in the current window (only_prior).
  prior_summary <- data.frame(
    package = "r-oldpkg2", package_lower = "r-oldpkg2", origin = "cran", canonical_name = "oldpkg2",
    total_30d = 50L, total_90d = 150L, total_365d = 600L,
    rank_30d = 1L, rank_90d = 1L, rank_365d = 1L,
    avg_daily_30d = 1.67, trend = 0,
    first_date = "2020-01-01", last_date = "2026-01-01", stringsAsFactors = FALSE)
  expect_false("identity_state" %in% names(prior_summary))

  expect_no_error(s <- build_summary(con, ident, "d", anchor_date = "2026-07-01", prior_summary = prior_summary))
  expect_true("identity_state" %in% names(s))
  expect_true(is.na(s$identity_state[s$package == "r-oldpkg2"]))
})
