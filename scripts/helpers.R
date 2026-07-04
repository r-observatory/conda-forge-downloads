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

# Canonical Package: names from a Bioconductor VIEWS DCF blob (one category's
# worth of package records concatenated as plain text).
parse_views_packages <- function(views_text) {
  lines <- unlist(strsplit(views_text, "\n", fixed = TRUE))
  hits <- grep("^Package:\\s*", lines, value = TRUE)
  trimws(sub("^Package:\\s*", "", hits))
}

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

# Shared DDL for the summary table: the Task 5 SUMMARY_COLS, used both in the
# standalone summary shard and embedded into the recent shard.
summary_table_ddl <- function(table) sprintf(
  "CREATE TABLE IF NOT EXISTS %s (
     package TEXT PRIMARY KEY, package_lower TEXT, origin TEXT, canonical_name TEXT,
     total_30d INTEGER, total_90d INTEGER, total_365d INTEGER,
     rank_30d INTEGER, rank_90d INTEGER, rank_365d INTEGER,
     avg_daily_30d REAL, trend REAL, first_date TEXT, last_date TEXT);", table)

# Shared DDL for the name-identity cache: one row per package known so far, so a
# transient CRAN/Bioc name-map fetch failure can fall back to the prior mapping.
packages_table_ddl <- function(table) sprintf(
  "CREATE TABLE IF NOT EXISTS %s (
     package TEXT PRIMARY KEY, origin TEXT, canonical_name TEXT);", table)

# Write a minimal SQLite file containing only the summary table (for the merger).
export_summary_shard <- function(path, summary_df) {
  unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, summary_table_ddl(SUMMARY_TABLE))
  DBI::dbWriteTable(con, SUMMARY_TABLE, summary_df, append = TRUE)
  DBI::dbExecute(con, "VACUUM")
}

# Embed the summary table and the name-identity cache into the recent shard
# (which already holds the daily table), so a single download answers most
# queries and the next run can fall back to the cache if the name maps fail.
embed_aux <- function(recent_path, summary_df, packages_df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), recent_path); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", SUMMARY_TABLE))
  DBI::dbExecute(con, summary_table_ddl(SUMMARY_TABLE))
  DBI::dbWriteTable(con, SUMMARY_TABLE, summary_df, append = TRUE)
  DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", PACKAGES_TABLE))
  DBI::dbExecute(con, packages_table_ddl(PACKAGES_TABLE))
  DBI::dbWriteTable(con, PACKAGES_TABLE, packages_df[c("package", "origin", "canonical_name")], append = TRUE)
}

# Row count and date span of a shard's daily rows, for the manifest's per-shard
# coverage table. NA span when the shard is empty (e.g. a not-yet-seen year).
coverage <- function(rows) {
  if (nrow(rows) == 0L) return(list(rows = 0L, date_min = NA, date_max = NA))
  list(rows = nrow(rows), date_min = min(rows$date), date_max = max(rows$date))
}

# Overlay this run's shard-coverage updates onto the prior manifest's `shards`
# map, leaving untouched shards (not re-exported this run) as they were.
merge_shard_coverage <- function(prev, updates) {
  out <- prev %||% list()
  for (k in names(updates)) out[[k]] <- updates[[k]]
  out
}

# Write the manifest as pretty JSON. `changed_shards` must be passed as a list
# (e.g. as.list(character(0)) or as.list(chr_vec)) so it serializes as a JSON
# array even when empty, never as `{}` or `null`.
write_manifest <- function(path, obj) {
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"), path)
}

# Render the GitHub release body (markdown) from a manifest object. `caveat` is
# the one project-specific line (channel/counting caveats differ between
# conda-forge-downloads and bioconda-downloads even though this file is shared
# verbatim); everything else is driven off the SHARD_PREFIX / PUBLISH_REPO
# constants from config.R so the two repos render structurally identical notes.
write_release_notes <- function(path, manifest, caveat) {
  ts  <- function(s) if (is.null(s) || is.na(s)) "n/a" else sub("Z$", " UTC", sub("T", " ", s))
  big <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "0" else
    formatC(as.numeric(x), format = "d", big.mark = ",")
  cs <- manifest$changed_shards
  changed <- if (length(cs) == 0) {
    if (identical(manifest$source_kind, "frozen")) "none (source unreachable this run)"
    else "none (no new data this run)"
  } else paste(unlist(cs), collapse = ", ")

  lines <- c(
    sprintf("Daily per-package download statistics published to [%s](https://github.com/%s).",
            PUBLISH_REPO, PUBLISH_REPO),
    "",
    sprintf(paste0("This is a single rolling release. Assets are SQLite shards: per-year archives ",
                   "(`%1$s-YYYY.db`), a rolling window (`%1$s-recent.db`), and a summary-only file ",
                   "(`%1$s-summary.db`), alongside `manifest.json`. Each run replaces only the shards ",
                   "that changed."), SHARD_PREFIX),
    "",
    "| | |",
    "|---|---|",
    sprintf("| **Last checked** | %s |", ts(manifest$last_checked)),
    sprintf("| **Source this run** | %s |", manifest$source_kind %||% "n/a"),
    sprintf("| **Latest day** | %s |", manifest$summary$latest_date %||% "n/a"),
    sprintf("| **Packages tracked** | %s |", big(manifest$summary$packages)),
    sprintf("| **Changed this run** | %s |", changed),
    "",
    sprintf("> %s", caveat),
    "",
    "## Shard coverage",
    "",
    "| Shard | Rows | From | To |",
    "|---|---:|---|---|")
  shards <- manifest$shards %||% list()
  for (nm in sort(names(shards))) {
    s <- shards[[nm]]
    lines <- c(lines, sprintf("| `%s` | %s | %s | %s |",
      nm, big(s$rows), s$date_min %||% "n/a", s$date_max %||% "n/a"))
  }
  lines <- c(lines, "",
    "_Fetch the rolling window:_",
    "```bash",
    sprintf("gh release download current --repo %s --pattern %s-recent.db", PUBLISH_REPO, SHARD_PREFIX),
    "```")
  writeLines(lines, path)
  invisible(NULL)
}
