.root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("scripts/config.R", "scripts/helpers.R", "scripts/update.R")) {
  p <- file.path(.root, f)
  if (file.exists(p)) sys.source(p, envir = globalenv())
}
