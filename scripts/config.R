# conda-forge-downloads configuration.
# This file is the ONLY thing that differs between conda-forge-downloads and
# bioconda-downloads. helpers.R and update.R read these constants and are identical
# across both repos.

PUBLISH_REPO   <- "r-observatory/conda-forge-downloads"
SHARD_PREFIX   <- "conda-forge-downloads"   # release-asset filename stem
TABLE_PREFIX   <- "conda_forge"             # SQLite table-name stem
FORCE_REBUILD_ENV <- "CONDA_FORGE_FORCE_REBUILD"
RECLASSIFY_ONLY_ENV <- "CONDA_FORGE_RECLASSIFY_ONLY"

DAILY_TABLE    <- paste0(TABLE_PREFIX, "_downloads_daily")
SUMMARY_TABLE  <- paste0(TABLE_PREFIX, "_downloads_summary")
PACKAGES_TABLE <- paste0(TABLE_PREFIX, "_packages")

DATA_SOURCE    <- "conda-forge"             # anaconda-package-data `data_source` value
NAME_FILTER    <- "pkg_name LIKE 'r-%'"     # SQL predicate applied in the DuckDB fetch
NAME_PREFIXES  <- c("r-")                   # prefixes this repo ingests

S3_HOURLY_BASE <- "s3://anaconda-package-data/conda/hourly"
S3_REGION      <- "us-east-1"
HISTORY_START  <- "2017-04"                 # first month with conda-forge data (YYYY-MM)

RECENT_WINDOW   <- 400L                      # days retained in the recent shard
REVISION_WINDOW <- 10L                       # trailing days re-fetched each incremental run

# Identity is resolved by the robservatory package against two downloaded assets:
# cran-archive.db's cran_names_all and bioconductor-metadata's bioc_names_all.
CRAN_ARCHIVE_REPO <- "r-observatory/cran-archive"
CRAN_ARCHIVE_DB   <- "cran-archive.db"
BIOC_META_REPO    <- "r-observatory/bioconductor-metadata"
BIOC_META_DB      <- "bioconductor-metadata.db"
CRAN_NAMES_FLOOR  <- 15000L   # below this the identity fetch is treated as partial
BIOC_NAMES_FLOOR  <- 1500L

SUMMARY_COLS <- c(
  "package", "package_lower", "origin", "canonical_name",
  "total_30d", "total_90d", "total_365d",
  "rank_30d", "rank_90d", "rank_365d",
  "avg_daily_30d", "trend", "first_date", "last_date", "identity_state"
)

RELEASE_CAVEAT <- paste(
  "Counts are conda-forge CDN downloads (served through Anaconda's infrastructure,",
  "best-effort deduped by Anaconda) and are not directly comparable across sources.")
