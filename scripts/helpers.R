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

# Descending rank (1 = largest x), ties get the minimum rank.
rank_desc <- function(x) as.integer(rank(-x, ties.method = "min"))

# Zero-row data.frame with exactly the SUMMARY_COLS columns, correctly typed.
empty_summary <- function() {
  df <- as.data.frame(setNames(rep(list(character(0)), length(SUMMARY_COLS)), SUMMARY_COLS),
                      stringsAsFactors = FALSE)
  for (c in c("total_30d","total_90d","total_365d","rank_30d","rank_90d","rank_365d")) df[[c]] <- integer(0)
  for (c in c("avg_daily_30d","trend")) df[[c]] <- numeric(0)
  df
}

# Per-package 30/90/365-day rolling summary, ranked and joined to identity_df.
build_summary <- function(daily_con, identity_df, daily_table, anchor_date = NULL) {
  if (is.null(anchor_date)) {
    anchor_date <- DBI::dbGetQuery(daily_con, sprintf("SELECT MAX(date) AS d FROM %s", daily_table))$d
  }
  if (length(anchor_date) == 0L || is.na(anchor_date)) return(empty_summary())
  a <- as.Date(anchor_date)
  start <- function(days) as.character(a - (days - 1L))
  prev_lo <- as.character(a - 59L); prev_hi <- start(30L)  # prior-30d window [a-59, a-30)
  sql <- sprintf(
    "SELECT package,
       SUM(CASE WHEN date >= '%s' THEN count ELSE 0 END) AS total_30d,
       SUM(CASE WHEN date >= '%s' THEN count ELSE 0 END) AS total_90d,
       SUM(CASE WHEN date >= '%s' THEN count ELSE 0 END) AS total_365d,
       SUM(CASE WHEN date >= '%s' AND date < '%s' THEN count ELSE 0 END) AS prev_30d,
       MIN(date) AS first_date, MAX(date) AS last_date
     FROM %s WHERE date <= '%s' GROUP BY package",
    start(30L), start(90L), start(365L), prev_lo, prev_hi, daily_table, anchor_date)
  agg <- DBI::dbGetQuery(daily_con, sql)
  if (nrow(agg) == 0L) return(empty_summary())

  m <- merge(agg, identity_df, by = "package", all.x = TRUE)
  m$origin <- ifelse(is.na(m$origin), "other", m$origin)
  m$package_lower <- tolower(m$package)
  m$avg_daily_30d <- round(m$total_30d / 30, 2)
  m$trend <- ifelse(m$prev_30d > 0, round((m$total_30d - m$prev_30d) / m$prev_30d * 100, 2), NA_real_)
  m$rank_30d  <- rank_desc(m$total_30d)
  m$rank_90d  <- rank_desc(m$total_90d)
  m$rank_365d <- rank_desc(m$total_365d)
  m <- m[order(m$rank_30d), ]
  m[, SUMMARY_COLS]
}
