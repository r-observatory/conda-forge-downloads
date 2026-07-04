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
  ident <- resolve_identities(c("r-mass", "r-ggplot2"),
                              build_cran_map(c("MASS", "ggplot2")), NULL)
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
  s <- build_summary(con, resolve_identities(character(0), character(0)), DAILY_TABLE)
  expect_equal(names(s), SUMMARY_COLS)
  expect_equal(nrow(s), 0L)
})
