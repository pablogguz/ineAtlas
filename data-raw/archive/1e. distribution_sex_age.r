#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Processes income heterogeneity files by sex and age
#*------------------------------------------------------------------------------

library(tidyverse)

# Set paths 
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw/")
proc <- paste0(root, "/proc/")

# Define mapping for income threshold categories
indicator_names <- c(
  "Población con ingresos por unidad de consumo por debajo de 5.000 Euros" = "equivinc_below_5000",
  "Población con ingresos por unidad de consumo por debajo de 7.500 Euros" = "equivinc_below_7500",
  "Población con ingresos por unidad de consumo por debajo de 10.000 Euros" = "equivinc_below_10000",
  "Población con ingresos por unidad de consumo por debajo 40% de la mediana" = "equivinc_below_40p_median",
  "Población con ingresos por unidad de consumo por debajo 50% de la mediana" = "equivinc_below_50p_median",
  "Población con ingresos por unidad de consumo por debajo 60% de la mediana" = "equivinc_below_60p_median",
  "Población con ingresos por unidad de consumo por encima 140% de la mediana" = "equivinc_above_140p_median",
  "Población con ingresos por unidad de consumo por encima 160% de la mediana" = "equivinc_above_160p_median",
  "Población con ingresos por unidad de consumo por encima 200% de la mediana" = "equivinc_above_200p_median"
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
  
  # Check if this is a sex and age heterogeneity file
  if("Distribución de la renta por unidad de consumo" %in% names(df) && 
     "Sexo" %in% names(df) && 
     "Tramos de edad" %in% names(df) && 
     !("Nacionalidad" %in% names(df))) {
    cat("\nFound sex and age heterogeneity file:", basename(files[i]), "\n")
    data_list[[length(data_list) + 1]] <- df
  }
}

# Print summary
cat("\nFound", length(data_list), "sex and age heterogeneity files\n")

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

# Create the three datasets
# 1. Municipality level (no district or section)
mun_data <- all_data %>%
  filter(is.na(Distritos) | Distritos == "") %>%
  select(
    Municipios,
    Sexo,
    `Tramos de edad`,
    `Distribución de la renta por unidad de consumo`,
    Periodo,
    Total
  ) %>%
  mutate(
    mun_code = str_extract(Municipios, "^\\d+"),
    mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
    year = Periodo,
    sex = Sexo,
    age_group = `Tramos de edad`,
    indicator = `Distribución de la renta por unidad de consumo`,
    value = Total
  ) %>%
  select(mun_code, mun_name, year, sex, age_group, indicator, value)

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
    sex = Sexo,
    age_group = `Tramos de edad`,
    indicator = `Distribución de la renta por unidad de consumo`,
    value = Total
  ) %>%
  select(mun_code, mun_name, district_code, year, sex, age_group, indicator, value)

# 3. Census tract level (has both district and section)
section_data <- all_data %>%
  filter(!is.na(Secciones) & Secciones != "") %>%
  mutate(
    mun_code = str_extract(Municipios, "^\\d+"),
    mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
    district_code = str_extract(Distritos, "\\d+"),
    section_code = str_extract(Secciones, "\\d+"),
    year = Periodo,
    sex = Sexo,
    age_group = `Tramos de edad`,
    indicator = `Distribución de la renta por unidad de consumo`,
    value = Total
  ) %>%
  select(mun_code, mun_name, district_code, section_code, year, sex, age_group, indicator, value)

# Convert to wide format - Municipality level
mun_data_wide <- mun_data %>%
  mutate(
    # Map Spanish indicators to English names
    indicator = recode(indicator, !!!indicator_names)
  ) %>%
  pivot_wider(
    id_cols = c(mun_code, mun_name, year, sex, age_group),
    names_from = indicator,
    values_from = value
  )

# District level
district_data_wide <- district_data %>%
  mutate(
    indicator = recode(indicator, !!!indicator_names)
  ) %>%
  pivot_wider(
    id_cols = c(mun_code, mun_name, district_code, year, sex, age_group),
    names_from = indicator,
    values_from = value
  )

# Section level
section_data_wide <- section_data %>%
  mutate(
    indicator = recode(indicator, !!!indicator_names)
  ) %>%
  pivot_wider(
    id_cols = c(mun_code, mun_name, district_code, section_code, year, sex, age_group),
    names_from = indicator,
    values_from = value
  )

# Create directories if they don't exist
dir.create(file.path(gitdata, "distribution_sex_age/"), recursive = TRUE, showWarnings = FALSE)

# Save the datasets
write_csv(mun_data_wide, file.path(gitdata, "distribution_sex_age/distribution_sex_age_municipality.csv"))
write_csv(district_data_wide, file.path(gitdata, "distribution_sex_age/distribution_sex_age_district.csv"))
write_csv(section_data_wide, file.path(gitdata, "distribution_sex_age/distribution_sex_age_tract.csv"))