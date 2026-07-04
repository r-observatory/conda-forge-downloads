# conda-forge Downloads

Daily per-package download statistics for the `r-*` slice of [conda-forge](https://conda-forge.org/), the community-maintained conda channel that packages most of CRAN for installation with `conda`/`mamba`. The counts come from Anaconda's public [anaconda-package-data](https://github.com/anaconda/anaconda-package-data) dataset, a set of anonymous, publicly readable Parquet files on S3 that record CDN download events for every conda channel Anaconda serves. This pipeline aggregates the conda-forge `r-*` package rows to one download count per package per UTC day, resolves each name against the current CRAN package index, and publishes the result as SQLite shard files attached to a single rolling GitHub release tag (`current`).

> [!IMPORTANT]
> **What these numbers mean, and what they do not.**
>
> - **Counts are downloads served through Anaconda's CDN, best-effort deduped by Anaconda.** The exact bot-filtering and deduplication rules are not published. These counts miss independent third-party mirrors (for example prefix.dev, and corporate or university mirrors that sync conda-forge), so they are not absolute install counts, only a lower-bound view of CDN traffic.
> - **Per-platform splits are dropped.** Roughly 47% of R package downloads in the source data carry a blank platform field, so counts are summed across all platforms (`linux-64`, `osx-64`, `noarch`, and so on) rather than broken out per platform.
> - **`origin = 'other'` marks `r-*` names that are not CRAN packages.** This includes the `r-base` meta-package and other conda-native R tooling that ships on conda-forge but has no corresponding CRAN release.
> - **The daily grain and 30/90/365-day windows match `cran-downloads`, but the absolute numbers are not directly comparable across sources.** conda-forge, CRAN, r2u, COPR, and autoOBS each serve a different population over different infrastructure with different counting methods. Use each source for its own trend, not for cross-source magnitude comparisons.

## Data Access

All shards live as assets on the [`current` release](https://github.com/r-observatory/conda-forge-downloads/releases/tag/current). Each daily run uploads only the shards that changed; the rest remain unchanged.

### Recent data (last 400 days)

For most use cases this is the only file you need. It holds the rolling 400-day window of `conda_forge_downloads_daily` plus the full `conda_forge_downloads_summary` and `conda_forge_packages` tables.

```bash
gh release download current \
  --repo r-observatory/conda-forge-downloads \
  --pattern "conda-forge-downloads-recent.db"
```

```r
url <- "https://github.com/r-observatory/conda-forge-downloads/releases/download/current/conda-forge-downloads-recent.db"
download.file(url, "conda-forge-downloads-recent.db", mode = "wb")

library(RSQLite)
con <- dbConnect(SQLite(), "conda-forge-downloads-recent.db")

# Daily downloads for r-ggplot2 over the last 30 days
dbGetQuery(con, "
  SELECT date, count
  FROM conda_forge_downloads_daily
  WHERE package = 'r-ggplot2'
  ORDER BY date DESC LIMIT 30
")

# Top 20 packages by 30-day downloads
dbGetQuery(con, "
  SELECT package, canonical_name, total_30d, rank_30d
  FROM conda_forge_downloads_summary
  ORDER BY rank_30d LIMIT 20
")

dbDisconnect(con)
```

```python
import urllib.request, sqlite3
url = "https://github.com/r-observatory/conda-forge-downloads/releases/download/current/conda-forge-downloads-recent.db"
urllib.request.urlretrieve(url, "conda-forge-downloads-recent.db")

con = sqlite3.connect("conda-forge-downloads-recent.db")
for row in con.execute("""
    SELECT package, canonical_name, total_30d, rank_30d
    FROM conda_forge_downloads_summary
    ORDER BY rank_30d LIMIT 10"""):
    print(row)
con.close()
```

### Per-year archives

Each calendar year of the daily series has its own shard (history begins in April 2017, when Anaconda's dataset starts tracking conda-forge):

```bash
gh release download current \
  --repo r-observatory/conda-forge-downloads \
  --pattern "conda-forge-downloads-2026.db"
```

### Full history (all years)

```bash
gh release download current \
  --repo r-observatory/conda-forge-downloads \
  --pattern "conda-forge-downloads-*.db"
```

### Summary only

For top-package lists, ranks, and the current windows without the daily series:

```bash
gh release download current \
  --repo r-observatory/conda-forge-downloads \
  --pattern "conda-forge-downloads-summary.db"
```

### Manifest

`manifest.json` lists which shards changed in the most recent run, the source kind (`hourly` for a live read, `frozen` for a heartbeat when the source was unreachable), per-shard coverage, and freshness timestamps.

```bash
gh release download current \
  --pattern manifest.json \
  --repo r-observatory/conda-forge-downloads
cat manifest.json
```

## Example Queries

### Top packages by 30-day downloads

```sql
SELECT package, canonical_name, total_30d, rank_30d
  FROM conda_forge_downloads_summary
 ORDER BY rank_30d
 LIMIT 50;
```

### Daily series joined to summary identity

```sql
SELECT d.date, d.package, s.canonical_name, s.origin, d.count
  FROM conda_forge_downloads_daily d
  JOIN conda_forge_downloads_summary s ON s.package = d.package
 WHERE d.package = 'r-data.table'
 ORDER BY d.date DESC
 LIMIT 30;
```

### CRAN-only packages, ranked

```sql
SELECT package, canonical_name, total_30d, rank_30d
  FROM conda_forge_downloads_summary
 WHERE origin = 'cran'
 ORDER BY total_30d DESC
 LIMIT 50;
```

## Schema

### `conda_forge_downloads_daily`

One row per package per day. The count is the sum of Anaconda's hourly-binned `counts` column for that package and UTC day, across all platforms and versions. Present in `conda-forge-downloads-recent.db` (last 400 days) and each `conda-forge-downloads-YYYY.db` archive.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | conda-forge package name, e.g. `r-ggplot2` (PK part 1) |
| `date` | TEXT | The UTC day the downloads occurred, `YYYY-MM-DD` (PK part 2) |
| `count` | INTEGER | Downloads on that day, summed across all platforms and versions |

### `conda_forge_downloads_summary`

Per-package standing, rebuilt each run from the accumulated daily series. Present in `conda-forge-downloads-recent.db` and `conda-forge-downloads-summary.db`.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | conda-forge package name (PK) |
| `package_lower` | TEXT | Lowercased helper column for case-insensitive joins |
| `origin` | TEXT | `cran` if the stripped `r-` name matches a current CRAN package, else `other` (conda-forge carries no `bioconductor-*` packages, so `bioc` never appears here) |
| `canonical_name` | TEXT | The CRAN canonical-case name, e.g. `ggplot2`; `NULL` when `origin = 'other'` |
| `total_30d` | INTEGER | Downloads in the trailing 30 days ending on the latest date in the series |
| `total_90d` | INTEGER | Downloads in the trailing 90 days |
| `total_365d` | INTEGER | Downloads in the trailing 365 days |
| `rank_30d` | INTEGER | Rank by `total_30d` (1 = most downloaded) |
| `rank_90d` | INTEGER | Rank by `total_90d` |
| `rank_365d` | INTEGER | Rank by `total_365d` |
| `avg_daily_30d` | REAL | `total_30d` divided by 30 |
| `trend` | REAL | Percent change: last 30 days vs the prior 30 days; `NULL` until roughly 60 days of history exist |
| `first_date` | TEXT | Earliest date this package appears in the daily series (`YYYY-MM-DD`) |
| `last_date` | TEXT | Latest date this package appears in the daily series (`YYYY-MM-DD`) |

### `conda_forge_packages`

The package-name identity cache, carried inside `conda-forge-downloads-recent.db` so a transient CRAN name-index outage falls back to the prior run's mapping instead of blanking every origin.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | conda-forge package name (PK) |
| `origin` | TEXT | `cran` or `other`, as in the summary table |
| `canonical_name` | TEXT | CRAN canonical-case name, or `NULL` when `origin = 'other'` |

## How it works

A daily GitHub Actions job (05:00 UTC) reads Anaconda's public `anaconda-package-data` hourly Parquet files directly from S3 with an anonymous DuckDB connection (`httpfs`, no AWS credentials required), filtered to `data_source = 'conda-forge'` and package names matching `r-%`. Rows are aggregated to one `(package, date, count)` triple per UTC day, merged into the accumulated history, and resolved against the current CRAN package index (`available.packages()`) to assign each package an `origin` (`cran` or `other`) and, for CRAN packages, a canonical case-correct name. The affected year shard plus the rolling `conda-forge-downloads-recent.db` and `conda-forge-downloads-summary.db` are rebuilt, and only the changed shards are uploaded to the `current` release (with `manifest.json` uploaded last, so a crash mid-publish leaves the prior state authoritative). When the S3 source or the CRAN index is unreachable, the run degrades gracefully: a source outage produces a heartbeat that refreshes `last_checked` and leaves the release untouched, and a CRAN-index outage falls back to the cached `conda_forge_packages` mapping from the last successful run.

## Attribution

Download counts are read from Anaconda's public [anaconda-package-data](https://github.com/anaconda/anaconda-package-data) dataset on S3; the packages themselves are built and maintained by the [conda-forge](https://conda-forge.org/) community. This repository provides only the daily aggregation and packaging into SQLite. Please respect Anaconda's public data infrastructure and terms.

## License

The pipeline code in this repository is proprietary. Copyright (c) 2026 HJJB, LLC. All rights reserved; see [LICENSE](LICENSE). The underlying download counts originate from Anaconda's anaconda-package-data.
