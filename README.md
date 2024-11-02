
# ineAtlas <img src="man/figures/logo.png" align="right" height="278" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pablogguz/ineAtlas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pablogguz/ineAtlas/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `ineAtlas` package is to provide easy access to granular socioeconomic indicators from the Spanish Statistical Office (INE) _Atlas de Distribución de Renta de los Hogares_ (Household Income Distribution Atlas). This dataset combines administrative tax data with population statistics to provide detailed information about income distribution and related socioeconomic indicators across Spain.

## Data structure

The data is organized into several categories and is available at three geographic levels:

### Geographic levels
- Municipality (_Municipio_)
- District (_Distrito_)
- Census tract (_Sección censal_)

### Available datasets
| Dataset | Description |
|---------|------------|
| Income | Income indicators including net/gross income per capita and household |
| Income sources | Income indicators by source (wages, pensions, benefits, etc.) |
| Demographics | Population characteristics including age structure and household composition |
| Distribution by sex | Income distribution indicators disaggregated by sex |
| Distribution by sex & age | Income distribution indicators by sex and age categories |
| Distribution by sex, age & nationality | Income distribution indicators by sex and nationality status |
| Inequality indicators | Inequality metrics including Gini coefficient and P80/P20 ratio |

## Installation

You can install the development version of ineAtlas from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("pablogguz/ineAtlas")
```

## Example

Here's a basic example of fetching census tract-level income data:

``` r
library(ineAtlas)

# Get municipality level income data
income_data <- get_atlas("income", "tract")

# View the first few rows
head(income_data)
```
## Contributing

Contributions are welcome! Please feel free to submit a pull request. For major changes, please open an issue first to discuss what you would like to change.

## Author

**Pablo García Guzmán**  
EBRD
