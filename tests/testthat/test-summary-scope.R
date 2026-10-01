# Which rows the summary may hold: metapackages, carried rows and the ledger.

test_that("summary_in_scope accepts cran and bioc rows and refuses other, NA and metapackages", {
  expect_identical(
    summary_in_scope(c("r-dplyr", "bioconductor-limma", "r-yr", "r-na", "r-essentials"),
                     c("cran", "bioc", "other", NA, "cran")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

.metapackage_prior <- function() data.frame(
  package = c("r-essentials", "r-oldpkg"), package_lower = c("r-essentials", "r-oldpkg"),
  origin = c("cran", "cran"), canonical_name = c("essentials", "oldpkg"),
  total_30d = c(10351L, 20L), total_90d = c(30737L, 60L), total_365d = c(132587L, 240L),
  rank_30d = c(1L, 2L), rank_90d = c(1L, 2L), rank_365d = c(1L, 2L),
  avg_daily_30d = c(345.03, 0.67), trend = c(5.63, 0),
  first_date = c("2025-05-30", "2020-01-01"), last_date = c("2026-08-31", "2026-01-01"),
  identity_state = c("archived", "live"), stringsAsFactors = FALSE)

test_that("build_summary drops a metapackage even when a cached identity frame calls it cran", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE d (package TEXT, date TEXT, count INTEGER)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('r-essentials','2026-07-01',500),('r-dplyr','2026-07-01',100)")
  # The packages cache written before the fix maps r-essentials to CRAN's essentials.
  ident <- data.frame(package = c("r-essentials", "r-dplyr"), origin = c("cran", "cran"),
                      canonical_name = c("essentials", "dplyr"), identity_state = NA_character_,
                      stringsAsFactors = FALSE)
  s <- build_summary(con, ident, "d", anchor_date = "2026-07-01")
  expect_equal(s$package, "r-dplyr")
  expect_equal(s$rank_30d, 1L)
})

test_that("merge_prior_summary never carries a metapackage row forward, whatever its stored origin", {
  m <- empty_summary()
  out <- merge_prior_summary(m, .metapackage_prior())
  expect_false("r-essentials" %in% out$package)
  expect_equal(out$package, "r-oldpkg")
  expect_equal(out$total_365d, 0L)
  expect_equal(out$rank_30d, 1L)
})
