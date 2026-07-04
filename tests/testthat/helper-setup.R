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
