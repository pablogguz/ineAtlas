# Fetch data from the ineAtlas data repository

Downloads and extracts compressed data files from the ineAtlas data
repository, providing access to various socioeconomic indicators at
different geographic levels.

## Usage

``` r
get_atlas(category, level, cache = TRUE, cache_dir = tempdir())
```

## Arguments

- category:

  Character string specifying the data category. Must be one of:
  "income", "income_sources", "demographics", "distribution_sex",
  "distribution_sex_age", "distribution_sex_nationality", or
  "gini_p80p20"

- level:

  Character string specifying the geographic level. Must be one of:
  "municipality", "district", or "tract"

- cache:

  Logical indicating whether to cache the extracted data. Default is
  TRUE. Cached data is stored uncompressed for faster access.

- cache_dir:

  Character string specifying the cache directory. Default is tempdir().

## Value

A tibble containing the requested data. Distribution data will include
additional columns for demographic breakdowns (sex, age, nationality).
The data is automatically extracted from compressed files and cached
locally if requested.

## Note

Data files are stored compressed on the repository to reduce size and
download times. The function handles decompression automatically.

## Examples

``` r
# \donttest{
# Get municipality level income data
income_data <- get_atlas("income", "municipality")
#> Downloading data from income_municipality.zip...
#> Caching data to ineatlas_income_municipality.csv
#> Data successfully loaded: 73251 rows, 11 columns

# Get district level demographics without caching
demo_data <- get_atlas("demographics", "district", cache = FALSE)
#> Downloading data from demographics_district.zip...
#> Data successfully loaded: 94662 rows, 13 columns

# Get income distribution indicators by sex
sex_dist <- get_atlas("distribution_sex", "municipality")
#> Downloading data from distribution_sex_municipality.zip...
#> Caching data to ineatlas_distribution_sex_municipality.csv
#> Data successfully loaded: 219753 rows, 15 columns

# Get inequality indicators including Gini coefficient
gini_data <- get_atlas("gini_p80p20", "municipality")
#> Downloading data from gini_p80p20_municipality.zip...
#> Caching data to ineatlas_gini_p80p20_municipality.csv
#> Data successfully loaded: 73251 rows, 7 columns
# }
```
