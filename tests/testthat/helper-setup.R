.root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("scripts/config.R", "scripts/helpers.R", "scripts/update.R")) {
  p <- file.path(.root, f)
  if (file.exists(p)) sys.source(p, envir = globalenv())
}

# Build an in-memory DAILY_TABLE connection preloaded with the given rows
# (package, date, count), for tests of extract_recent()/extract_year().
new_daily_con <- function(df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  for (stmt in strsplit(daily_table_ddl(DAILY_TABLE), ";\\s*")[[1]]) {
    if (nzchar(trimws(stmt))) DBI::dbExecute(con, stmt)
  }
  DBI::dbWriteTable(con, DAILY_TABLE, df, append = TRUE)
  con
}

# Write a cran_names_all/bioc_names_all fixture DB at `path` (schema matching
# the real identity assets: name_lower, canonical_name, identity_state,
# first_seen, last_seen), populated from `names` (a character vector of
# canonical package names). Always creates the table, even for an empty
# vector, since robservatory::load_identity requires both tables to exist.
.write_names_db <- function(path, table, names) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (name_lower TEXT PRIMARY KEY, canonical_name TEXT,
       identity_state TEXT, first_seen TEXT, last_seen TEXT)", table))
  if (length(names) > 0L) {
    DBI::dbWriteTable(con, table, data.frame(
      name_lower = tolower(names), canonical_name = names,
      identity_state = "live", first_seen = "x", last_seen = "y",
      stringsAsFactors = FALSE), append = TRUE)
  }
}

# An injectable io for run_update() tests. `daily` is a data.frame(date, package,
# count) standing in for the full fetchable history; fetch_daily(months) filters
# it to the requested "YYYY-MM" months, the same shape the real DuckDB/S3 fetch
# returns. `shards` optionally pre-seeds release_download with a named list of
# pattern -> file path (as if already published to the `current` release), so
# release_download can copy a requested asset into `dir` and return 0L, or
# return a nonzero code when release_present is TRUE but the requested pattern
# was not pre-seeded (the protect-history abort case). `cran`/`bioc` populate
# fixture cran_names_all/bioc_names_all SQLite DBs (in a tempdir that outlives
# this call, since it must survive until run_update's identity_dbs() call
# reads it) surfaced via identity_dbs().
fake_io <- function(release_present, daily, cran = character(0), bioc = character(0),
                     now, shards = list(), fail_fetch = FALSE, fail_identity = FALSE) {
  ident_dir <- tempfile("identity-dbs-"); dir.create(ident_dir)
  withr::defer(unlink(ident_dir, recursive = TRUE), envir = parent.frame())
  cran_db <- file.path(ident_dir, "cran-archive.db")
  bioc_db <- file.path(ident_dir, "bioc-meta.db")
  .write_names_db(cran_db, "cran_names_all", cran)
  .write_names_db(bioc_db, "bioc_names_all", bioc)

  list(
    release_exists = function() release_present,
    release_download = function(pattern, dir) {
      if (!isTRUE(release_present)) return(1L)
      src <- shards[[pattern]]
      if (is.null(src) || !file.exists(src)) return(1L)
      file.copy(src, file.path(dir, pattern), overwrite = TRUE)
      0L
    },
    # fail_fetch simulates the daily-data source being unreachable (for
    # heartbeat tests); fail_identity simulates the identity assets (CRAN
    # archive / Bioc metadata DBs) being unreachable (for cache-fallback tests).
    fetch_daily = function(months) {
      if (isTRUE(fail_fetch)) stop("daily data source unreachable")
      daily[substr(daily$date, 1, 7) %in% months, , drop = FALSE]
    },
    identity_dbs = function() {
      if (isTRUE(fail_identity)) stop("identity assets unreachable")
      list(cran = cran_db, bioc = bioc_db)
    },
    now = function() as.POSIXct(now, tz = "UTC"))
}
