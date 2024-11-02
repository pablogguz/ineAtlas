#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Processes INE data files and separates them by geographic level
#*------------------------------------------------------------------------------

packages_to_load <- c(
    "tidyverse", 
    "arrow"
) 

package.check <- lapply(
  packages_to_load,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

lapply(packages_to_load, require, character=T)

# Set paths 
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw/")
proc <- paste0(root, "/proc/")

# Script starts ------

# First check all files for required columns
files <- list.files(raw, pattern = "\\.csv$", full.names = TRUE)
cat("Found", length(files), "total files\n")

# Function to check if file contains income indicators
has_income_indicators <- function(file_path) {
  # Read just the first line and parse it as header
  header <- readLines(file_path, n=1)
  cols <- strsplit(header, "\t")[[1]]
  return("Indicadores de renta media y mediana" %in% cols)
}

# Filter files and print info
income_files <- character(0)
cat("\nChecking files for income indicators:\n")
for(file in files) {
  if(has_income_indicators(file)) {
    cat("\nFound income indicators in:", basename(file))
    income_files <- c(income_files, file)
  }
}

cat("\n\nFound", length(income_files), "income indicator files\n")

# Load and process files
data_list <- list()
for(file in income_files) {
  df <- read_delim(
    file,
    delim = "\t",
    locale = locale(encoding = "UTF-8"),
    col_types = cols(.default = col_character())
  )
  data_list[[length(data_list) + 1]] <- df
}

# Combine files
all_data <- bind_rows(data_list)

# Convert Total to numeric (replace comma with dot first)
all_data <- all_data %>%
  mutate(
    # First remove any thousands separators (dots)
    Total = str_replace_all(Total, "\\.", ""),
    # Then replace comma with dot for decimal point
    Total = str_replace(Total, ",", "."),
    # Convert to numeric
    Total = as.numeric(Total)
  )

# Define indicator name mapping
indicator_names <- c(
  "Renta neta media por persona" = "net_income_pc",              # Net income per capita
  "Renta neta media por hogar" = "net_income_hh",               # Net income per household
  "Media de la renta por unidad de consumo" = "net_income_equiv",   # Mean income per consumption unit (equivalised)
  "Mediana de la renta por unidad de consumo" = "median_income_equiv", # Median income per consumption unit (equivalised)
  "Renta bruta media por persona" = "gross_income_pc",          # Gross income per capita
  "Renta bruta media por hogar" = "gross_income_hh"             # Gross income per household
)

# Create the three datasets
# 1. Municipality level (no district or section)
mun_data <- all_data %>%
  filter(is.na(Distritos) | Distritos == "") %>%
  select(
    Municipios,
    `Indicadores de renta media y mediana`,
    Periodo,
    Total
  ) %>%
  mutate(
    mun_code = str_extract(Municipios, "^\\d+"),
    mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
    year = Periodo,
    indicator = `Indicadores de renta media y mediana`,
    value = Total
  ) %>%
  select(mun_code, mun_name, year, indicator, value)

# 2. District level (has district but no section)
district_data <- all_data %>%
  filter(
    (!is.na(Distritos) & Distritos != "") & 
    (is.na(Secciones) | Secciones == "")
  ) %>%
  mutate(
    mun_code = str_extract(Municipios, "^\\d+"),
    mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
    district_code = str_extract(Distritos, "\\d+"),
    year = Periodo,
    indicator = `Indicadores de renta media y mediana`,
    value = Total
  ) %>%
  select(mun_code, mun_name, district_code, year, indicator, value)

# 3. Census tract level (has both district and section)
section_data <- all_data %>%
  filter(!is.na(Secciones) & Secciones != "") %>%
  mutate(
    mun_code = str_extract(Municipios, "^\\d+"),
    mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
    district_code = str_extract(Distritos, "\\d+"),
    section_code = str_extract(Secciones, "\\d+"),
    year = Periodo,
    indicator = `Indicadores de renta media y mediana`,
    value = Total
  ) %>%
  select(mun_code, mun_name, district_code, section_code, year, indicator, value)

# Convert to wide format - Municipality level
mun_data_wide <- mun_data %>%
  mutate(
    # Map Spanish indicators to English names
    indicator = recode(indicator, !!!indicator_names)
  ) %>%
  pivot_wider(
    id_cols = c(mun_code, mun_name, year),
    names_from = indicator,
    values_from = value
  )

# District level
district_data_wide <- district_data %>%
  mutate(
    indicator = recode(indicator, !!!indicator_names)
  ) %>%
  pivot_wider(
    id_cols = c(mun_code, mun_name, district_code, year),
    names_from = indicator,
    values_from = value
  )

# Section level
section_data_wide <- section_data %>%
  mutate(
    indicator = recode(indicator, !!!indicator_names)
  ) %>%
  pivot_wider(
    id_cols = c(mun_code, mun_name, district_code, section_code, year),
    names_from = indicator,
    values_from = value
  )

# Save the datasets
write_csv(mun_data_wide, file.path(gitdata, "/income/income_municipality.csv"))
write_csv(district_data_wide, file.path(gitdata, "/income/income_district.csv"))
write_csv(section_data_wide, file.path(gitdata, "/income/income_tract.csv"))

# Check province coverage
cat("\nChecking province coverage:")

# Municipality level
mun_provinces <- mun_data_wide %>%
  mutate(province_code = substr(mun_code, 1, 2)) %>%
  pull(province_code) %>%
  unique() %>%
  sort()

cat("\nMunicipality data covers", length(mun_provinces), "provinces (should be 52)")
if(length(mun_provinces) != 52) {
  cat("\nMissing province codes:", setdiff(sprintf("%02d", 1:52), mun_provinces))
}

# District level
district_provinces <- district_data_wide %>%
  mutate(province_code = substr(mun_code, 1, 2)) %>%
  pull(province_code) %>%
  unique() %>%
  sort()

cat("\nDistrict data covers", length(district_provinces), "provinces (should be 52)")
if(length(district_provinces) != 52) {
  cat("\nMissing province codes:", setdiff(sprintf("%02d", 1:52), district_provinces))
}

# Census tract level
section_provinces <- section_data_wide %>%
  mutate(province_code = substr(mun_code, 1, 2)) %>%
  pull(province_code) %>%
  unique() %>%
  sort()

cat("\nCensus tract data covers", length(section_provinces), "provinces (should be 52)")
if(length(section_provinces) != 52) {
  cat("\nMissing province codes:", setdiff(sprintf("%02d", 1:52), section_provinces))
}
