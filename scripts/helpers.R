`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a

# packages : character vector of conda names (lowercase, e.g. "r-mass")
# cran_map : named character vector, names = lowercase CRAN name, values = canonical case
# bioc_map : named character vector, names = lowercase Bioc name, values = canonical case (or NULL)
resolve_identities <- function(packages, cran_map, bioc_map = NULL) {
  n <- length(packages)
  origin    <- rep("other", n)
  canonical <- rep(NA_character_, n)

  is_bioc <- startsWith(packages, "bioconductor-")
  is_r    <- startsWith(packages, "r-") & !is_bioc

  if (any(is_bioc)) {
    stripped <- substring(packages[is_bioc], nchar("bioconductor-") + 1L)
    origin[is_bioc] <- "bioc"
    mapped <- if (!is.null(bioc_map)) unname(bioc_map[stripped]) else rep(NA_character_, length(stripped))
    canonical[is_bioc] <- ifelse(is.na(mapped), stripped, mapped)
  }

  if (any(is_r)) {
    stripped <- substring(packages[is_r], nchar("r-") + 1L)
    mapped <- unname(cran_map[stripped])          # NA where not a known CRAN package
    origin[is_r]    <- ifelse(is.na(mapped), "other", "cran")
    canonical[is_r] <- mapped                      # stays NA (-> other) when unmapped
  }

  data.frame(package = packages, origin = origin,
             canonical_name = canonical, stringsAsFactors = FALSE)
}

.build_name_map <- function(names) {
  names <- names[!is.na(names) & nzchar(names)]
  names <- names[!duplicated(tolower(names))]   # first canonical wins on case collision
  stats::setNames(names, tolower(names))
}

build_cran_map <- function(cran_names) .build_name_map(cran_names)
build_bioc_map <- function(bioc_names) .build_name_map(bioc_names)
