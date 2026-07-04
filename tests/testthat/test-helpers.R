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
