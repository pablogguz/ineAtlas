# Get census tract boundary geometries

Downloads and extracts census tract boundary files from the ineAtlas
repository, returning an sf object with the geometries for the specified
year.

## Usage

``` r
get_tract_geom(year, cache = TRUE, cache_dir = tempdir())
```

## Arguments

- year:

  Numeric. Year of the census tract boundaries to retrieve (2015-2022)

- cache:

  Logical indicating whether to cache the extracted data. Default is
  TRUE. Cached data is stored uncompressed for faster access.

- cache_dir:

  Character string specifying the cache directory. Default is tempdir().

## Value

An sf object containing census tract boundaries with the following
columns:

- year: The reference year

- tract_code: Census tract identifier

- municipality: Municipality name

- province: Province name

- geometry: Census tract boundary geometry

## Examples

``` r
# \donttest{
# Get census tract boundaries for 2020
tracts_2020 <- get_tract_geom(2020)
#> Downloading geometries for 2020...
#> Caching geometries to ineatlas_tracts_2020.gpkg
#> Geometries successfully loaded: 36309 features

# Get boundaries without caching
tracts_2019 <- get_tract_geom(2019, cache = FALSE)
#> Downloading geometries for 2019...
#> Geometries successfully loaded: 36317 features
# }
```
