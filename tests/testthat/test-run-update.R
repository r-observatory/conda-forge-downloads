test_that("cold bootstrap builds year shards, recent, summary, and manifest", {
  out <- withr::local_tempdir()
  daily <- data.frame(
    date = c("2017-04-05", "2026-06-29", "2026-06-30"),
    package = c("r-mass", "r-mass", "r-ggplot2"),
    count = c(1L, 10L, 5L), stringsAsFactors = FALSE)
  io <- fake_io(release_present = FALSE, daily = daily,
                cran = c("MASS", "ggplot2"), now = "2026-07-01 05:00:00")
  res <- run_update(io, out, force_full = FALSE)
  expect_true(file.exists(file.path(out, "conda-forge-downloads-2017.db")))
  expect_true(file.exists(file.path(out, "conda-forge-downloads-2026.db")))
  expect_true(file.exists(file.path(out, "conda-forge-downloads-recent.db")))
  expect_true(file.exists(file.path(out, "conda-forge-downloads-summary.db")))
  man <- jsonlite::fromJSON(file.path(out, "manifest.json"))
  expect_true("conda-forge-downloads-2026.db" %in% man$changed_shards)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "conda-forge-downloads-summary.db")); on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, "SELECT * FROM conda_forge_downloads_summary WHERE package='r-mass'")
  expect_equal(s$origin, "cran"); expect_equal(s$canonical_name, "MASS")
})
