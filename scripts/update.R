#!/usr/bin/env Rscript
# scripts/update.R: shared conda-channel download-stats producer (see config.R for the channel).
#
# Tracks daily per-package conda download counts for R packages (the
# `r-*` slice of Anaconda's public anaconda-package-data dataset) and publishes
# year-sharded SQLite to a rolling `current` GitHub release. run_update(io,
# out_dir) takes an injectable io for offline testing.
#
# Cold-bootstraps full history from HISTORY_START on first run, then accrues
# incrementally, heartbeating (and aborting rather than truncating history)
# when the daily source or a prior release asset is unreachable.

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

# The 4-digit year encoded in a year-shard filename ("<SHARD_PREFIX>-YYYY.db"),
# or NA for a non-year shard (e.g. "-recent" or "-summary"). Used by force_full
# to recover every year the prior manifest ever published, so a full rebuild
# re-pulls all of them rather than just the touched-window year.
shard_year <- function(name) {
  pat <- sprintf("^%s-([0-9]{4})\\.db$", SHARD_PREFIX)
  if (grepl(pat, name)) sub(pat, "\\1", name) else NA_character_
}

# Merge a downloaded shard's daily rows into the working DB via ATTACH +
# INSERT OR REPLACE, so rows already present (e.g. loaded from the recent
# shard) are updated in place rather than raising a primary-key conflict.
# Returns TRUE if the shard file existed and had a daily table to merge.
load_daily_shard <- function(con, path) {
  if (!file.exists(path)) return(invisible(FALSE))
  DBI::dbExecute(con, sprintf("ATTACH DATABASE '%s' AS src", normalizePath(path, mustWork = TRUE)))
  on.exit(DBI::dbExecute(con, "DETACH DATABASE src"), add = TRUE)
  has_tbl <- nrow(DBI::dbGetQuery(con,
    "SELECT name FROM src.sqlite_master WHERE name = ?", params = list(DAILY_TABLE))) > 0
  if (has_tbl) {
    DBI::dbExecute(con, sprintf(
      "INSERT OR REPLACE INTO %s (package, date, count) SELECT package, date, count FROM src.%s",
      DAILY_TABLE, DAILY_TABLE))
  }
  invisible(has_tbl)
}

# TRUE if two row-sets differ in content over `cols` (order-independent).
# Used to change-gate a shard: only list it in changed_shards when what we
# are about to publish actually differs from what was already downloaded.
content_changed <- function(old, new, cols) {
  if (nrow(old) != nrow(new)) return(TRUE)
  if (nrow(old) == 0L) return(FALSE)
  o <- old[cols]; n <- new[cols]
  o <- o[do.call(order, o), , drop = FALSE]; rownames(o) <- NULL
  n <- n[do.call(order, n), , drop = FALSE]; rownames(n) <- NULL
  !isTRUE(all.equal(o, n, check.attributes = FALSE))
}

run_update <- function(io, out_dir, force_full = FALSE,
                        live_floor = CRAN_NAMES_FLOOR, bioc_floor = BIOC_NAMES_FLOOR) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(out_dir, "manifest.json")
  recent_path   <- file.path(out_dir, sprintf("%s-recent.db", SHARD_PREFIX))
  summary_path  <- file.path(out_dir, sprintf("%s-summary.db", SHARD_PREFIX))

  if (io$release_exists()) {
    # A prior release exists: pull its manifest + recent shard (the trailing
    # history needed to determine the revision window), re-fetch the trailing
    # REVISION_WINDOW days, merge idempotently, and change-gate what actually
    # differs from what was downloaded before republishing.
    mcode <- io$release_download("manifest.json", out_dir)
    rcode <- io$release_download(basename(recent_path), out_dir)
    if (!identical(as.integer(mcode), 0L) || !file.exists(manifest_path) ||
        !identical(as.integer(rcode), 0L) || !file.exists(recent_path)) {
      stop("release 'current' exists but its manifest/recent shard could not be ",
           "downloaded; aborting to protect accumulated history")
    }

    prev        <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
    prev_shards <- prev$shards %||% list()
    now         <- io$now()

    work_path <- tempfile(fileext = ".sqlite")
    work_con  <- DBI::dbConnect(RSQLite::SQLite(), work_path)
    on.exit({ DBI::dbDisconnect(work_con); unlink(work_path) }, add = TRUE)
    DBI::dbExecute(work_con, "PRAGMA journal_mode=WAL")
    for (stmt in strsplit(daily_table_ddl(DAILY_TABLE), ";\\s*")[[1]])
      if (nzchar(trimws(stmt))) DBI::dbExecute(work_con, stmt)

    load_daily_shard(work_con, recent_path)
    prior_summary <- read_summary_table(recent_path)
    last_known <- DBI::dbGetQuery(work_con, sprintf("SELECT MAX(date) AS d FROM %s", DAILY_TABLE))$d
    if (is.na(last_known))
      stop("run_update: the downloaded recent shard has no daily rows; cannot ",
           "determine the incremental revision window")

    # force_full pulls the whole history window (so revised old months are
    # re-fetched too), mirroring the cold-bootstrap path's month range instead
    # of the normal trailing revision window.
    months <- if (force_full)
      months_between(HISTORY_START, format(now, "%Y-%m"))
    else
      months_between(format(as.Date(last_known) - REVISION_WINDOW, "%Y-%m"),
                      format(now, "%Y-%m"))
    fresh <- tryCatch(io$fetch_daily(months), error = function(e) e)
    if (inherits(fresh, "error")) {
      # The daily-data source is unreachable this run. A prior release exists,
      # so rather than fail (and rather than silently republish untouched
      # shards as if nothing changed), record a heartbeat: refresh only the
      # manifest's last_checked/source_kind, publish no shard changes, and
      # leave the accumulated history exactly as downloaded.
      out <- prev
      out$last_checked   <- iso(now)
      out$source_kind    <- "frozen"
      out$changed_shards <- list()
      write_manifest(manifest_path, out)
      write_release_notes(file.path(out_dir, "release_notes.md"), out, RELEASE_CAVEAT)
      return(list(changed_shards = character(0), manifest = out))
    }
    fresh <- fresh[c("package", "date", "count")]
    if (!force_full)
      fresh <- fresh[fresh$date >= as.character(as.Date(last_known) - REVISION_WINDOW), , drop = FALSE]

    # Pull the touched-year shards (years present in `fresh`) so each year's
    # full history is in the working DB before the fresh rows are merged in.
    # force_full widens this to every year the prior manifest ever published
    # (union'd with any years present in `fresh`), so the whole published
    # history gets pulled into the working DB and re-exported below.
    touched_years <- if (nrow(fresh) > 0) sort(unique(substr(fresh$date, 1, 4))) else character(0)
    if (force_full) {
      prior_years <- Filter(Negate(is.na), vapply(names(prev_shards), shard_year, character(1)))
      touched_years <- sort(unique(c(prior_years, touched_years)))
    }
    for (yr in touched_years) {
      shard <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
      st <- io$release_download(shard, out_dir)
      sp <- file.path(out_dir, shard)
      if (!file.exists(sp) && !is.null(prev_shards[[shard]])) {
        stop("year shard ", shard, " is expected on the release (status ", st,
             ") but could not be downloaded; aborting to protect accumulated history")
      }
      load_daily_shard(work_con, sp)
    }

    # Snapshot pre-merge content (what was actually downloaded) for change-gating.
    pre_year <- list()
    for (yr in touched_years)
      pre_year[[sprintf("%s-%s.db", SHARD_PREFIX, yr)]] <- extract_year(work_con, yr, DAILY_TABLE)

    now_date <- as.Date(format(now, "%Y-%m-%d", tz = "UTC"))
    cutoff   <- format(now_date - RECENT_WINDOW, "%Y-%m-%d")
    pre_recent <- extract_recent(work_con, cutoff, DAILY_TABLE)

    all_packages <- sort(unique(c(
      DBI::dbGetQuery(work_con, sprintf("SELECT DISTINCT package FROM %s", DAILY_TABLE))$package,
      fresh$package)))
    ident <- tryCatch({
      dbs  <- io$identity_dbs()
      maps <- robservatory::load_identity(dbs$cran, dbs$bioc)
      if (!robservatory::check_size(maps$n_cran, floor = live_floor) ||
          !robservatory::check_size(maps$n_bioc, floor = bioc_floor)) {
        stop("identity size gate failed (cran=", maps$n_cran, ", bioc=", maps$n_bioc, ")")
      }
      resolve_identities(all_packages, maps)
    }, error = function(e) {
      # The identity assets (CRAN archive / Bioc metadata DBs) are unreachable
      # or failed the size gate this run; fall back to the packages cache
      # embedded in the recent shard just downloaded, so origins/canonical
      # names still populate for every already-known package (a genuinely new
      # package falls through as "other" until the assets are reachable
      # again). The cache never stores identity_state (packages_table_ddl has
      # no such column), so it is backfilled as NA (honest unknown, never a
      # silent value) here.
      message("identity unavailable (", conditionMessage(e),
               "); falling back to the cached packages table")
      cache_con <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
      on.exit(DBI::dbDisconnect(cache_con), add = TRUE)
      has_cache  <- PACKAGES_TABLE %in% DBI::dbListTables(cache_con)
      cache_cols <- if (has_cache) DBI::dbListFields(cache_con, PACKAGES_TABLE) else character(0)
      select_cols <- intersect(c("package", "origin", "canonical_name", "identity_state"), cache_cols)
      cached <- if (has_cache)
        DBI::dbGetQuery(cache_con,
          sprintf("SELECT %s FROM %s", paste(select_cols, collapse = ", "), PACKAGES_TABLE))
      else
        data.frame(package = character(0), origin = character(0),
                   canonical_name = character(0), stringsAsFactors = FALSE)
      merged <- merge(data.frame(package = all_packages, stringsAsFactors = FALSE),
                       cached, by = "package", all.x = TRUE)
      merged$origin <- ifelse(is.na(merged$origin), "other", merged$origin)
      if (!"identity_state" %in% names(merged)) merged$identity_state <- NA_character_
      merged[c("package", "origin", "canonical_name", "identity_state")]
    })
    pre_summary <- build_summary(work_con, ident, DAILY_TABLE, prior_summary = prior_summary)

    if (nrow(fresh) > 0)
      DBI::dbExecute(work_con,
        sprintf("INSERT OR REPLACE INTO %s (package, date, count) VALUES (?, ?, ?)", DAILY_TABLE),
        params = list(fresh$package, fresh$date, fresh$count))

    post_summary <- build_summary(work_con, ident, DAILY_TABLE, prior_summary = prior_summary)

    changed_shards <- character(0); shard_updates <- list()
    for (yr in touched_years) {
      shard <- sprintf("%s-%s.db", SHARD_PREFIX, yr)
      dy    <- extract_year(work_con, yr, DAILY_TABLE)
      export_shard(file.path(out_dir, shard), dy)
      shard_updates[[shard]] <- coverage(dy)
      # force_full re-exports and republishes every year shard unconditionally
      # (that is the point of a full rebuild), rather than only those whose
      # content actually changed.
      if (force_full || content_changed(pre_year[[shard]], dy, c("package", "date", "count")))
        changed_shards <- c(changed_shards, shard)
    }

    r_rows <- extract_recent(work_con, cutoff, DAILY_TABLE)
    export_shard(recent_path, r_rows)
    embed_aux(recent_path, post_summary, ident)
    export_summary_shard(summary_path, post_summary)
    shard_updates[[basename(recent_path)]] <- coverage(r_rows)
    if (force_full || content_changed(pre_recent, r_rows, c("package", "date", "count")))
      changed_shards <- c(changed_shards, basename(recent_path))
    if (force_full || content_changed(pre_summary, post_summary, SUMMARY_COLS))
      changed_shards <- c(changed_shards, basename(summary_path))

    out <- list(
      tag            = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
      generated_at   = iso(now),
      last_checked   = iso(now),
      last_changed   = if (length(changed_shards) > 0) iso(now) else (prev$last_changed %||% iso(now)),
      source_kind    = "hourly",
      changed_shards = as.list(changed_shards),
      shards         = merge_shard_coverage(prev_shards, shard_updates),
      summary        = list(
        packages    = nrow(post_summary),
        latest_date = DBI::dbGetQuery(work_con, sprintf("SELECT MAX(date) AS d FROM %s", DAILY_TABLE))$d))
    write_manifest(manifest_path, out)
    write_release_notes(file.path(out_dir, "release_notes.md"), out, RELEASE_CAVEAT)
    return(list(changed_shards = changed_shards, manifest = out))
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
  fetched_rows <- DBI::dbGetQuery(work_con, sprintf("SELECT count(*) AS n FROM %s", DAILY_TABLE))$n
  if (fetched_rows == 0)
    stop("cold build fetched no data; aborting rather than publish an empty release")

  # Cold builds have no prior release and thus no cache to fall back to: a
  # download error or a failed size gate here must abort the build rather
  # than publish with everything misclassified as "other".
  dbs  <- io$identity_dbs()
  maps <- robservatory::load_identity(dbs$cran, dbs$bioc)
  if (!robservatory::check_size(maps$n_cran, floor = live_floor) ||
      !robservatory::check_size(maps$n_bioc, floor = bioc_floor)) {
    stop("identity size gate failed (cran=", maps$n_cran, ", bioc=", maps$n_bioc, ")")
  }

  all_packages <- sort(unique(daily$package))
  ident        <- resolve_identities(all_packages, maps)
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
  export_summary_shard(summary_path, summary_df)
  changed_shards <- c(changed_shards, basename(recent_path), basename(summary_path))
  shard_updates[[basename(recent_path)]] <- coverage(r_rows)

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
    # Anonymous DuckDB-over-S3 fetch of anaconda-package-data hourly Parquet,
    # aggregated to (date, package, count) for this channel's DATA_SOURCE /
    # NAME_FILTER (config.R). `months` is a character vector of "YYYY-MM".
    # DuckDB's read_parquet() throws if a glob matches zero files, so a month
    # with no daily files published yet (e.g. the current month early on, or a
    # not-yet-arrived future month in the revision window) would otherwise
    # abort the whole fetch. glob() itself returns zero rows rather than
    # throwing for an absent/empty month, so each requested month is
    # glob-counted first and only months with >=1 file are read; a genuine
    # network/S3 outage still throws out of the glob query and must keep
    # propagating so the existing heartbeat/cold-abort handling still applies.
    fetch_daily = function(months) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
      DBI::dbExecute(con, sprintf("SET s3_region='%s';", S3_REGION))
      month_glob <- function(m)
        sprintf("%s/%s/%s/*.parquet", S3_HOURLY_BASE, substr(m, 1, 4), substr(m, 6, 7))
      present <- months[vapply(months, function(m)
        DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM glob('%s')", month_glob(m)))$n > 0,
        logical(1))]
      if (length(present) == 0L)
        return(data.frame(date = character(0), package = character(0), count = integer(0),
                           stringsAsFactors = FALSE))
      glob_sql <- paste0("['", paste(vapply(present, month_glob, character(1)), collapse = "','"), "']")
      sql <- sprintf(
        "SELECT regexp_extract(filename, '(\\d{4}-\\d{2}-\\d{2})\\.parquet$', 1) AS date,
                pkg_name AS package, CAST(SUM(counts) AS BIGINT) AS count
         FROM read_parquet(%s, filename = true)
         WHERE data_source = '%s' AND %s
         GROUP BY 1, 2",
        glob_sql, DATA_SOURCE, NAME_FILTER)
      DBI::dbGetQuery(con, sql)
    },
    # Downloads the shared identity assets (the CRAN archive's cran_names_all
    # and Bioconductor metadata's bioc_names_all) from each source repo's
    # `current` release into a temp dir, for robservatory::load_identity.
    identity_dbs = function() {
      tmp <- tempfile(); dir.create(tmp, showWarnings = FALSE)
      dl <- function(repo, db) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", repo,
            "--pattern", db, "--dir", tmp, "--clobber"), stdout = FALSE, stderr = FALSE))
        p <- file.path(tmp, db)
        if (!identical(as.integer(st), 0L) || !file.exists(p)) stop("identity asset unreachable: ", repo, "/", db)
        p
      }
      list(cran = dl(CRAN_ARCHIVE_REPO, CRAN_ARCHIVE_DB),
           bioc = dl(BIOC_META_REPO, BIOC_META_DB))
    },
    now = function() Sys.time())
}

if (sys.nframe() == 0L) {
  args       <- commandArgs(trailingOnly = TRUE)
  out_dir    <- if (length(args) >= 1) args[1] else "out"
  force_full <- tolower(Sys.getenv(FORCE_REBUILD_ENV, "")) %in% c("true", "1", "yes")
  res <- run_update(default_io(), out_dir, force_full = force_full)
  cat("Changed shards:", if (length(res$changed_shards))
        paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}
