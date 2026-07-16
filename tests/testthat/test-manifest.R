# Build a tiny, real summary DB on disk via the pipeline's own exporter
# (canonical schema + single SUMMARY_TABLE), so the integrity core is exercised
# against exactly the bytes a run publishes. `*.db` fixtures are gitignored, so
# each test builds its own.
build_summary_db <- function(n = 3L) {
  tmp <- tempfile(fileext = ".db")
  df <- data.frame(
    package        = paste0("r-pkg", seq_len(n)),
    package_lower  = paste0("r-pkg", seq_len(n)),
    origin         = rep("cran", n),
    canonical_name = paste0("pkg", seq_len(n)),
    total_30d      = seq_len(n) * 10L,
    total_90d      = seq_len(n) * 30L,
    total_365d     = seq_len(n) * 100L,
    rank_30d       = seq_len(n),
    rank_90d       = seq_len(n),
    rank_365d      = seq_len(n),
    avg_daily_30d  = seq_len(n) * 1.5,
    trend          = rep(NA_real_, n),
    first_date     = rep("2020-01-01", n),
    last_date      = rep("2026-07-14", n),
    identity_state = rep("live", n),
    stringsAsFactors = FALSE)
  export_summary_shard(tmp, df[, SUMMARY_COLS])
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_summary_db(3L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count
  expect_equal(core$tables, stats::setNames(list(3L), SUMMARY_TABLE))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this test
  # genuinely cross-checks the code path instead of re-running the same
  # library. Skip only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_summary_db(2L)
  on.exit(unlink(db))

  core <- summary_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields", {
  db <- build_summary_db(4L)
  on.exit(unlink(db), add = TRUE)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  write_manifest(
    tmp,
    list(
      tag            = "v20260714-000000",
      changed_shards = as.list(basename(db)),
      summary        = list(packages = 4L)),
    core = core)

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$tag, "v20260714-000000")
  expect_equal(parsed$summary$packages, 4L)
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed[["tables"]][[SUMMARY_TABLE]], 4L)
  expect_true(parsed$complete)
})

test_that("write_manifest without a core preserves the plain object and empty array", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  write_manifest(tmp, list(tag = "v...", changed_shards = as.list(character(0))))

  raw <- readLines(tmp, warn = FALSE)
  expect_true(any(grepl('"changed_shards"\\s*:\\s*\\[\\s*\\]', raw)))
  parsed <- jsonlite::fromJSON(tmp)
  expect_null(parsed$db_sha256)
})
