test_that("resolve_identities maps r- names to CRAN and flags tooling as other", {
  cran <- setNames(c("MASS", "ggplot2"), c("mass", "ggplot2"))  # names are lowercase keys
  out <- resolve_identities(c("r-mass", "r-ggplot2", "r-base"), cran, NULL)
  expect_equal(out$origin, c("cran", "cran", "other"))
  expect_equal(out$canonical_name, c("MASS", "ggplot2", NA_character_))
})

test_that("resolve_identities maps bioconductor- names to bioc identity", {
  bioc <- setNames(c("DESeq2", "Biobase"), c("deseq2", "biobase"))
  out <- resolve_identities(c("bioconductor-deseq2", "bioconductor-newpkg"),
                            cran_map = character(0), bioc_map = bioc)
  expect_equal(out$origin, c("bioc", "bioc"))
  # unmapped bioconductor- keeps the stripped lowercase name (still useful)
  expect_equal(out$canonical_name, c("DESeq2", "newpkg"))
})

test_that("resolve_identities never emits bioc when bioc_map is NULL", {
  out <- resolve_identities("bioconductor-deseq2", cran_map = character(0), bioc_map = NULL)
  # with no bioc map the prefix still fixes origin=bioc, canonical=stripped
  expect_equal(out$origin, "bioc")
  expect_equal(out$canonical_name, "deseq2")
})

test_that("build_cran_map keys canonical names by lowercase", {
  m <- build_cran_map(c("MASS", "ggplot2", "data.table", "MASS"))  # dup tolerated
  expect_equal(unname(m[["mass"]]), "MASS")
  expect_equal(unname(m[["data.table"]]), "data.table")
  expect_false(any(duplicated(names(m))))
})

test_that("build_bioc_map behaves the same and drops blanks/NA", {
  m <- build_bioc_map(c("DESeq2", "", NA, "Biobase"))
  expect_equal(sort(unname(m)), c("Biobase", "DESeq2"))
  expect_equal(unname(m[["deseq2"]]), "DESeq2")
})
