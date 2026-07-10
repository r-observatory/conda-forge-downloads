.mk_maps <- function() {
  cran <- data.frame(name_lower = c("dplyr", "maptools"),
                     canonical_name = c("dplyr", "maptools"),
                     identity_state = c("live", "archived"), stringsAsFactors = FALSE)
  bioc <- data.frame(name_lower = "complexheatmap", canonical_name = "ComplexHeatmap",
                     identity_state = "live", stringsAsFactors = FALSE)
  robservatory::resolve_identity_set(cran, bioc)
}

test_that("resolve_identities classifies conda names against the identity maps", {
  maps <- .mk_maps()
  out <- resolve_identities(c("r-dplyr", "r-maptools", "r-complexheatmap", "r-yr", "bioconductor-limma"), maps)
  row <- function(p) out[out$package == p, ]
  expect_equal(row("r-dplyr")$origin, "cran")          # live CRAN
  expect_equal(row("r-maptools")$origin, "cran")       # archived CRAN, recovered
  expect_equal(row("r-maptools")$identity_state, "archived")
  expect_equal(row("r-complexheatmap")$origin, "bioc") # r- prefixed Bioc, recovered
  expect_equal(row("r-yr")$origin, "other")            # conda-only, out of scope
  expect_true(is.na(row("r-yr")$canonical_name))
  expect_equal(row("bioconductor-limma")$origin, "other")  # limma absent from this fixture bioc set
})
