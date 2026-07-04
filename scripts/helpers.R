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

# Shared DDL for the daily-series table: (package, date, count) plus a date index.
daily_table_ddl <- function(table) sprintf(
  "CREATE TABLE IF NOT EXISTS %s (
     package TEXT NOT NULL, date TEXT NOT NULL, count INTEGER NOT NULL,
     PRIMARY KEY (package, date));
   CREATE INDEX IF NOT EXISTS idx_%s_date ON %s(date);",
  table, table, table)

# Write the daily-series table for one shard. daily_df has (package, date, count).
export_shard <- function(path, daily_df) {
  unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  for (stmt in strsplit(daily_table_ddl(DAILY_TABLE), ";\\s*")[[1]]) if (nzchar(trimws(stmt))) DBI::dbExecute(con, stmt)
  DBI::dbWriteTable(con, DAILY_TABLE, daily_df, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
}

# Trailing-window rows of the daily series, for the recent shard.
extract_recent <- function(con, cutoff_date, table)
  DBI::dbGetQuery(con, sprintf("SELECT package, date, count FROM %s WHERE date >= ? ORDER BY package, date", table),
                  params = list(cutoff_date))

# All daily rows for a calendar year, for the per-year archive shards.
extract_year <- function(con, year, table)
  DBI::dbGetQuery(con, sprintf("SELECT package, date, count FROM %s WHERE substr(date,1,4)=? ORDER BY package, date", table),
                  params = list(as.character(year)))
