## Resubmission of an archived package

ineAtlas was archived on 2026-06-29 for a policy violation on Internet access. This version fixes that:

* `get_atlas()`, `get_census()` and `get_tract_geom()` now fail gracefully when the online data repository cannot be reached. They return `NULL` with an informative message instead of throwing an error. This path is tested with mocked downloads.
* Tests that need Internet access are skipped on CRAN.
* The vignettes are precomputed, so building them needs no Internet access.

It also updates `get_tract_geom()` to cover the census tract boundaries for 2024 and 2025, added with the latest data release from the Spanish Statistical Office.

## Test environments

* local macOS 15.5 (aarch64), R 4.5.2
* GitHub Actions: macos-latest (release), windows-latest (release), ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission. Package was archived on CRAN.

  This is expected: it is a resubmission of the archived package, with the fixes listed above.

## Downstream dependencies

There are currently no downstream dependencies for this package.
