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

# An injectable io for run_update() tests. `daily` is a data.frame(date, package,
# count) standing in for the full fetchable history; fetch_daily(months) filters
# it to the requested "YYYY-MM" months, the same shape the real DuckDB/S3 fetch
# returns. `shards` optionally pre-seeds release_download with a named list of
# pattern -> file path (as if already published to the `current` release), so
# release_download can copy a requested asset into `dir` and return 0L, or
# return a nonzero code when release_present is TRUE but the requested pattern
# was not pre-seeded (the protect-history abort case).
fake_io <- function(release_present, daily, cran = character(0), bioc = character(0),
                     now, shards = list()) {
  list(
    release_exists = function() release_present,
    release_download = function(pattern, dir) {
      if (!isTRUE(release_present)) return(1L)
      src <- shards[[pattern]]
      if (is.null(src) || !file.exists(src)) return(1L)
      file.copy(src, file.path(dir, pattern), overwrite = TRUE)
      0L
    },
    fetch_daily = function(months) daily[substr(daily$date, 1, 7) %in% months, , drop = FALSE],
    cran_names = function() cran,
    bioc_names = function() bioc,
    now = function() as.POSIXct(now, tz = "UTC"))
}
