# Live smoke test for default_io()$fetch_daily: exercises the real DuckDB/S3
# path against the public anaconda-package-data bucket. Skips cleanly (rather
# than failing the suite) whenever the network, the S3 bucket, or the httpfs
# extension install is unavailable, so CI/offline runs are unaffected.
test_that("fetch_daily returns r- rows for a recent month", {
  skip_on_cran()
  skip_if_offline()

  io <- default_io()
  df <- tryCatch(io$fetch_daily("2026-06"), error = function(e) {
    skip(paste("live S3 fetch unavailable:", conditionMessage(e)))
  })

  expect_true(all(startsWith(df$package, "r-")))
  expect_true(nrow(df) > 1000L)
  expect_true(all(nchar(df$date) == 10L))
})
