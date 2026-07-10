test_that("config exposes the conda-forge constants", {
  expect_equal(PUBLISH_REPO, "r-observatory/conda-forge-downloads")
  expect_equal(DATA_SOURCE, "conda-forge")
  expect_equal(DAILY_TABLE, "conda_forge_downloads_daily")
  expect_equal(SUMMARY_TABLE, "conda_forge_downloads_summary")
  expect_identical(NAME_PREFIXES, "r-")
  expect_equal(RECENT_WINDOW, 400L)
  expect_equal(REVISION_WINDOW, 10L)
  expect_true(all(c("origin", "canonical_name") %in% SUMMARY_COLS))
})

test_that("config exposes identity-asset settings and identity_state column", {
  expect_true("identity_state" %in% SUMMARY_COLS)
  expect_true(exists("CRAN_ARCHIVE_REPO") && CRAN_ARCHIVE_REPO == "r-observatory/cran-archive")
  expect_true(exists("BIOC_META_REPO") && BIOC_META_REPO == "r-observatory/bioconductor-metadata")
  expect_true(exists("CRAN_NAMES_FLOOR") && CRAN_NAMES_FLOOR == 15000L)
  expect_true(exists("BIOC_NAMES_FLOOR") && BIOC_NAMES_FLOOR == 1500L)
  expect_false(exists("LOAD_BIOC_MAP"))  # live-source config removed
})
