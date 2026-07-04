#!/usr/bin/env Rscript
# scripts/update.R: conda-forge-downloads producer.
#
# Tracks daily per-package conda-forge download counts for R packages (the
# `r-*` slice of Anaconda's public anaconda-package-data dataset) and publishes
# year-sharded SQLite to a rolling `current` GitHub release. run_update(io,
# out_dir) takes an injectable io for offline testing.
#
# Only the cold-bootstrap path (no prior release, source reachable) is
# implemented so far: full backfill from HISTORY_START to the current month.
# The incremental accrual, heartbeat, and protect-history abort paths land in
# a later change.

options(timeout = 600)

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(jsonlite)
})

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f) && file.exists(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("build_summary", mode = "function")) {
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "helpers.R"))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Inclusive sequence of "YYYY-MM" month strings from start to end.
months_between <- function(start, end) {
  s <- as.Date(paste0(start, "-01")); e <- as.Date(paste0(end, "-01"))
  format(seq(s, e, by = "month"), "%Y-%m")
}

run_update <- function(io, out_dir, force_full = FALSE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(out_dir, "manifest.json")
  recent_path   <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))

  if (io$release_exists()) {
    # A prior release exists. The incremental accrual, heartbeat, and
    # protect-history abort paths are not implemented yet; only the
    # cold-bootstrap path (below) is available so far.
    stop("run_update: a prior release exists but the incremental update path ",
         "is not implemented yet; only the cold-bootstrap path is available")
  }

  # Cold bootstrap: no prior release, full backfill from HISTORY_START.
  now    <- io$now()
  months <- months_between(HISTORY_START, format(now, "%Y-%m"))
  daily  <- io$fetch_daily(months)
  daily  <- daily[c("package", "date", "count")]

  work_path <- tempfile(fileext = ".sqlite")
  work_con  <- DBI::dbConnect(RSQLite::SQLite(), work_path)
  on.exit({ DBI::dbDisconnect(work_con); unlink(work_path) }, add = TRUE)
  DBI::dbExecute(work_con, "PRAGMA journal_mode=WAL")
  for (stmt in strsplit(daily_table_ddl(DAILY_TABLE), ";\\s*")[[1]])
    if (nzchar(trimws(stmt))) DBI::dbExecute(work_con, stmt)
  if (nrow(daily) > 0) DBI::dbWriteTable(work_con, DAILY_TABLE, daily, append = TRUE)

  cran_map <- build_cran_map(io$cran_names())
  bioc_map <- if (isTRUE(LOAD_BIOC_MAP)) build_bioc_map(io$bioc_names()) else NULL

  all_packages <- sort(unique(daily$package))
  ident        <- resolve_identities(all_packages, cran_map, bioc_map)
  summary_df   <- build_summary(work_con, ident, DAILY_TABLE)

  years <- if (nrow(daily) > 0) sort(unique(substr(daily$date, 1, 4))) else character(0)
  changed_shards <- character(0); shard_updates <- list()
  for (yr in years) {
    shard <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
    dy    <- extract_year(work_con, yr, DAILY_TABLE)
    export_shard(file.path(out_dir, shard), dy)
    changed_shards <- c(changed_shards, shard)
    shard_updates[[shard]] <- coverage(dy)
  }

  now_date <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
  cutoff   <- format(now_date - RECENT_WINDOW, "%Y-%m-%d")
  r_rows   <- extract_recent(work_con, cutoff, DAILY_TABLE)
  export_shard(recent_path, r_rows)
  embed_aux(recent_path, summary_df, ident)
  export_summary_shard(file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX)), summary_df)
  recent_shard  <- sprintf("%s-recent.db", SHARD_PREFIX)
  summary_shard <- sprintf("%s-summary.db", SHARD_PREFIX)
  changed_shards <- c(changed_shards, recent_shard, summary_shard)
  shard_updates[[recent_shard]] <- coverage(r_rows)

  out <- list(
    tag            = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at   = iso(now),
    last_checked   = iso(now),
    last_changed   = iso(now),
    source_kind    = "hourly",
    changed_shards = as.list(changed_shards),
    shards         = merge_shard_coverage(list(), shard_updates),
    summary        = list(
      packages    = nrow(summary_df),
      latest_date = if (nrow(daily) > 0) max(daily$date) else NA_character_))
  write_manifest(manifest_path, out)
  write_release_notes(file.path(out_dir, "release_notes.md"), out, RELEASE_CAVEAT)
  list(changed_shards = changed_shards, manifest = out)
}

default_io <- function() {
  list(
    release_exists = function() {
      st <- suppressWarnings(system2("gh",
        c("release", "view", "current", "--repo", PUBLISH_REPO),
        stdout = FALSE, stderr = FALSE))
      identical(as.integer(st), 0L)
    },
    release_download = function(pattern, dir) {
      for (i in seq_len(3L)) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", PUBLISH_REPO,
            "--pattern", pattern, "--dir", dir, "--clobber"),
          stdout = TRUE, stderr = TRUE))
        code <- as.integer(attr(st, "status") %||% 0L)
        if (identical(code, 0L)) return(0L)
        if (i < 3L) Sys.sleep(3 * i)
      }
      code
    },
    # The real anonymous S3/DuckDB fetch (reading anaconda-package-data hourly
    # Parquet) lands in a later change; inject a custom io$fetch_daily until then.
    fetch_daily = function(months) {
      stop("default_io()$fetch_daily is not implemented yet; inject a custom io$fetch_daily")
    },
    cran_names = function() rownames(utils::available.packages(repos = CRAN_REPO)),
    bioc_names = function() character(0),
    now = function() Sys.time())
}

if (sys.nframe() == 0L) {
  args       <- commandArgs(trailingOnly = TRUE)
  out_dir    <- if (length(args) >= 1) args[1] else "out"
  force_full <- tolower(Sys.getenv("CONDA_FORGE_FORCE_REBUILD", "")) %in% c("true", "1", "yes")
  res <- run_update(default_io(), out_dir, force_full = force_full)
  cat("Changed shards:", if (length(res$changed_shards))
        paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}
