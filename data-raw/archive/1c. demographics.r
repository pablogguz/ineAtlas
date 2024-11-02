#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Processes demographics files
#*------------------------------------------------------------------------------

library(tidyverse)

# Set paths 
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw/")
proc <- paste0(root, "/proc/")

# Define indicator name mapping for income sources
indicator_names <- c(
  "Edad media de la población" = "mean_age",              # Mean age of population
  "Porcentaje de población menor de 18 años" = "pct_under18",    # Percentage under 18 years
  "Porcentaje de población de 65 y más años" = "pct_over65",     # Percentage 65 and over
  "Tamaño medio del hogar" = "mean_hh_size",             # Mean household size
  "Porcentaje de hogares unipersonales" = "pct_single_hh",      # Percentage single-person households
  "Población" = "population",                            # Total population
  "Porcentaje de población española" = "pct_spanish"      # Percentage Spanish nationality
)

# List all files
files <- list.files(raw, pattern = "\\.csv$", full.names = TRUE)
cat("Found", length(files), "total files\n")

# Load and check files
data_list <- list()
for(i in seq_along(files)) {
  # Read file with all columns as character
  df <- read_delim(
    files[i],
    delim = "\t",
    locale = locale(encoding = "UTF-8"),
    col_types = cols(.default = col_character())
  )
  
  # Check if this is an income source distribution file
  if("Indicadores demográficos" %in% names(df)) {
    cat("\nFound demographics file:", basename(files[i]), "\n")
    data_list[[length(data_list) + 1]] <- df
  }
}

# Print summary
cat("\nFound", length(data_list), "income source files\n")

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

print(unique(all_data$`Indicadores demográficos`))

# Create the three datasets
# 1. Municipality level (no district or section)
mun_data <- all_data %>%
  filter(is.na(Distritos) | Distritos == "") %>%
  select(
    Municipios,
    `Indicadores demográficos`,
    Periodo,
    Total
  ) %>%
  mutate(
    mun_code = str_extract(Municipios, "^\\d+"),
    mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
    year = Periodo,
    indicator = `Indicadores demográficos`,
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
    indicator = `Indicadores demográficos`,
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
    indicator = `Indicadores demográficos`,
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
write_csv(mun_data_wide, file.path(gitdata, "/demographics/demographics_municipality.csv"))
write_csv(district_data_wide, file.path(gitdata, "/demographics/demographics_district.csv"))
write_csv(section_data_wide, file.path(gitdata, "/demographics/demographics_tract.csv"))