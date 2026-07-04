test_that("parse_views_packages extracts Package fields from a VIEWS blob", {
  txt <- paste(c("Package: DESeq2", "Version: 1.44.0", "biocViews: RNASeq", "",
                 "Package: Biobase", "Version: 2.64.0"), collapse = "\n")
  expect_equal(parse_views_packages(txt), c("DESeq2", "Biobase"))
})
