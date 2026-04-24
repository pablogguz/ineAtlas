# ineAtlas

R package that wraps processed INE Atlas data from the companion repo [ineAtlas.data](https://github.com/pablogguz/ineAtlas.data). Package code is in `R/`; the data build pipeline lives in `data-raw/`.

## Data pipeline (`data-raw/`)

INE releases Atlas data once a year, typically late October. Each release updates Dropbox raw CSVs, which are processed into the zipped outputs that `ineAtlas.data` ships.

Paths resolve from `$HOME` on macOS/Linux and `C:\Users\%USERNAME%` on Windows:

- **Raw scratch**: `~/Dropbox/ineAtlas_data/`
  - `raw_atlasdata/` — 10 INE Atlas CSVs (one per table)
  - `raw_census/C2021_Indicadores.csv` — 2021 Census
  - `raw_geometries/seccionado_{year}.zip` — INE census tract shapefiles
- **Output (git tracked)**: `~/Documents/GitHub/ineAtlas.data/data/`

### Scripts, in order

1. **`0. atlas_ine_download.R`** — pulls the 10 Atlas tables from INE and saves them with semantic names. `file_map` maps INE table code → output filename. URL: `https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/{code}.csv?nocab=1`.

   | code  | saved as                             | content |
   |-------|--------------------------------------|---------|
   | 30824 | `income_raw`                         | Income indicators (net/gross, per person/hh/equiv) |
   | 30825 | `income_sources_raw`                 | Income by source (wage, pension, benefits, ...) |
   | 30826 | `distribution_sex_abs`               | Income distribution by sex — absolute thresholds (€) |
   | 30827 | `distribution_sex_age_abs`           | ... by sex × age — absolute |
   | 30828 | `distribution_sex_nationality_abs`   | ... by sex × nationality — absolute |
   | 30829 | `distribution_sex_rel`               | ... by sex — relative (% of median) |
   | 30830 | `distribution_sex_age_rel`           | ... by sex × age — relative |
   | 30831 | `distribution_sex_nationality_rel`   | ... by sex × nationality — relative |
   | 30832 | `demographics_raw`                   | Demographics (age, household size, % Spanish, ...) |
   | 37677 | `gini_p80p20_raw`                    | Gini, P80/P20 |

2. **`1. process_ine_data.r`** — defines `process_ine_data(indicator_type)`. Sourced by script 2, not run directly. Handles long → wide reshape, Spanish → English translation, province join, and zip compression to `ineAtlas.data/data/{indicator}/`.

3. **`2. run_processing.r`** — orchestrator. Calls `process_ine_data()` for each of the 7 indicator types. Produces `{indicator}_municipality.zip`, `{indicator}_district.zip`, `{indicator}_tract.zip`.

4. **`3. process_geometries_tracts.r`** — reads `seccionado_{year}.zip` shapefiles, writes `census_tracts_{year}.gpkg.zip`. `years` at the top of the script controls which years to rebuild (default: most recent).

5. **`4. census_2021.r`** — processes the 2021 Census (`C2021_Indicadores.csv`) into tract/district/municipality-level outputs under `census_2021/`.

`_master.r` is legacy — it references scripts in `archive/` from an earlier pipeline design. Use scripts `0`–`4` instead.

### Running an update

When INE releases new data (check table `30824` first row for the latest year):

```r
source("data-raw/0. atlas_ine_download.R")   # refresh raw CSVs (10+ GB)
source("data-raw/2. run_processing.r")       # rebuild 7 indicator outputs
# Bump `years` in script 3 to include the new year, then:
source("data-raw/3. process_geometries_tracts.r")
# Census 2021 is static — only rerun if C2021_Indicadores.csv changes.
```

Then commit the updated zips in `ineAtlas.data/` and tag a new release there.

## Conventions

- Geographic codes are zero-padded strings: `prov_code` (2), `mun_code` (5), `district_code` (7), `tract_code` (10).
- Decimal separator in raw INE data is `,`; thousands `.`. Processing normalizes to numeric.
- Outputs are always zipped CSVs (max compression, no paths stored) to fit under GitHub file-size limits.
