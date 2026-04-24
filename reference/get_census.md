# Fetch 2021 Census data from the ineAtlas data repository

Downloads and extracts compressed census data files from the ineAtlas
data repository, providing access to detailed demographic, socioeconomic
and housing indicators at different geographic levels from the 2021
Population and Housing Census.

## Usage

``` r
get_census(level, cache = TRUE, cache_dir = tempdir())
```

## Arguments

- level:

  Character string specifying the geographic level. Must be one of:
  "municipality", "district", or "tract"

- cache:

  Logical indicating whether to cache the extracted data. Default is
  TRUE. Cached data is stored uncompressed for faster access.

- cache_dir:

  Character string specifying the cache directory. Default is tempdir().

## Value

A tibble containing the requested census data at the specified
geographic level. The data includes demographic, socioeconomic and
housing indicators from the 2021 Population and Housing Census. The data
is automatically extracted from compressed files and cached locally if
requested.

## Note

Data files are stored compressed on the repository to reduce size and
download times. The function handles decompression automatically. Census
data is only available for 2021.

## Examples

``` r
# \donttest{
# Get municipality level census data
mun_data <- get_census("municipality")
#> Downloading data from census_2021_municipality.zip...
#> Caching data to ineatlas_census_2021_municipality.csv
#> Data successfully loaded: 8131 rows, 39 columns

# Get district level census data without caching
dist_data <- get_census("district", cache = FALSE)
#> Downloading data from census_2021_district.zip...
#> Data successfully loaded: 10479 rows, 40 columns

# Get census tract level data
tract_data <- get_census("tract")
#> Downloading data from census_2021_tract.zip...
#> Caching data to ineatlas_census_2021_tract.csv
#> Data successfully loaded: 36333 rows, 41 columns
# }
```
