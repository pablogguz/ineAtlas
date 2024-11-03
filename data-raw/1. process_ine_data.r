#* ------------------------------------------------------------------------------
#* Process INE data files by indicator type
#* Returns three datasets: municipality, district, and census tract level
#* ------------------------------------------------------------------------------

library(tidyverse)

# Set paths
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw/")
proc <- paste0(root, "/proc/")

# INE province codes mapping
province_codes <- c(
  "02" = "Albacete",
  "03" = "Alicante/Alacant",
  "04" = "Almería",
  "01" = "Araba/Álava",
  "33" = "Asturias",
  "05" = "Avila",
  "06" = "Badajoz",
  "07" = "Balears, Illes",
  "08" = "Barcelona",
  "48" = "Bizkaia",
  "09" = "Burgos",
  "10" = "Cáceres",
  "11" = "Cádiz",
  "39" = "Cantabria",
  "12" = "Castellón/Castelló",
  "13" = "Ciudad Real",
  "14" = "Córdoba",
  "15" = "Coruña, A",
  "16" = "Cuenca",
  "20" = "Gipuzkoa",
  "17" = "Girona",
  "18" = "Granada",
  "19" = "Guadalajara",
  "21" = "Huelva",
  "22" = "Huesca",
  "23" = "Jaén",
  "24" = "León",
  "25" = "Lleida",
  "27" = "Lugo",
  "28" = "Madrid",
  "29" = "Málaga",
  "30" = "Murcia",
  "31" = "Navarra",
  "32" = "Ourense",
  "34" = "Palencia",
  "35" = "Palmas, Las",
  "36" = "Pontevedra",
  "26" = "Rioja, La",
  "37" = "Salamanca",
  "38" = "Santa Cruz de Tenerife",
  "40" = "Segovia",
  "41" = "Sevilla",
  "42" = "Soria",
  "43" = "Tarragona",
  "44" = "Teruel",
  "45" = "Toledo",
  "46" = "Valencia/València",
  "47" = "Valladolid",
  "49" = "Zamora",
  "50" = "Zaragoza",
  "51" = "Ceuta",
  "52" = "Melilla"
)

process_ine_data <- function(indicator_type) {
  # Validate indicator type
  valid_types <- c(
    "income", "income_sources", "demographics",
    "distribution_sex", "distribution_sex_age",
    "distribution_sex_nationality", "gini_p80p20"
  )

  if (!indicator_type %in% valid_types) {
    stop("Invalid indicator type. Must be one of: ", paste(valid_types, collapse = ", "))
  }

  # Define indicator column and mapping based on type
  indicator_configs <- list(
    income = list(
      column = "Indicadores de renta media y mediana",
      required_cols = NULL,
      excluded_cols = NULL,
      mapping = c(
        "Renta neta media por persona" = "net_income_pc",
        "Renta neta media por hogar" = "net_income_hh",
        "Media de la renta por unidad de consumo" = "net_income_equiv",
        "Mediana de la renta por unidad de consumo" = "median_income_equiv",
        "Renta bruta media por persona" = "gross_income_pc",
        "Renta bruta media por hogar" = "gross_income_hh"
      )
    ),
    income_sources = list(
      column = "Distribución por fuente de ingresos",
      required_cols = NULL,
      excluded_cols = NULL,
      mapping = c(
        "Renta bruta media por persona" = "gross_income_pc",
        "Fuente de ingreso: salario" = "wage",
        "Fuente de ingreso: pensiones" = "pension",
        "Fuente de ingreso: prestaciones por desempleo" = "unemp_benefit",
        "Fuente de ingreso: otras prestaciones" = "other_benefits",
        "Fuente de ingreso: otros ingresos" = "other_income"
      )
    ),
    demographics = list(
      column = "Indicadores demográficos",
      required_cols = NULL,
      excluded_cols = NULL,
      mapping = c(
        "Edad media de la población" = "mean_age",
        "Porcentaje de población menor de 18 años" = "pct_under18",
        "Porcentaje de población de 65 y más años" = "pct_over65",
        "Tamaño medio del hogar" = "mean_hh_size",
        "Porcentaje de hogares unipersonales" = "pct_single_hh",
        "Población" = "population",
        "Porcentaje de población española" = "pct_spanish"
      )
    ),
    distribution_sex = list(
      column = "Distribución de la renta por unidad de consumo",
      required_cols = c("Sexo"),
      excluded_cols = c("Tramos de edad", "Nacionalidad"),
      mapping = c(
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
    ),
    distribution_sex_age = list(
      column = "Distribución de la renta por unidad de consumo",
      required_cols = c("Sexo", "Tramos de edad"),
      excluded_cols = c("Nacionalidad"),
      mapping = c(
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
    ),
    distribution_sex_nationality = list(
      column = "Distribución de la renta por unidad de consumo",
      required_cols = c("Sexo", "Nacionalidad"),
      excluded_cols = c("Tramos de edad"),
      mapping = c(
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
    ),
    gini_p80p20 = list(
      column = "Índice de Gini y Distribución de la renta P80/P20",
      required_cols = NULL,
      excluded_cols = NULL,
      mapping = c(
        "Índice de Gini" = "gini",
        "Distribución de la renta P80/P20" = "p80p20"
      )
    )
  )

  config <- indicator_configs[[indicator_type]]

  # Function to check if file contains indicators
  has_indicators <- function(file_path) {
    header <- readLines(file_path, n = 1)
    cols <- strsplit(header, "\t")[[1]]

    # Check main indicator column
    has_main_col <- config$column %in% cols

    # If no additional requirements, return main column check
    if (is.null(config$required_cols) && is.null(config$excluded_cols)) {
      return(has_main_col)
    }

    # Check required columns
    has_required <- all(config$required_cols %in% cols)

    # Check excluded columns
    no_excluded <- !any(config$excluded_cols %in% cols)

    return(has_main_col && has_required && no_excluded)
  }

  # Find relevant files
  files <- list.files(raw, pattern = "\\.csv$", full.names = TRUE)
  cat("Found", length(files), "total files\n")

  # Filter files
  indicator_files <- character(0)
  cat(sprintf("\nChecking files for %s indicators:\n", indicator_type))
  for (file in files) {
    if (has_indicators(file)) {
      cat("\nFound indicators in:", basename(file))
      indicator_files <- c(indicator_files, file)
    }
  }

  cat("\n\nFound", length(indicator_files), indicator_type, "files\n")

  if (grepl("distribution_", indicator_type)) {
    if (length(indicator_files) != 104) {
      cat(sprintf("\nWARNING: Found %d files but expected 104 (2 per province)\n", length(indicator_files)))
    }
  }

  # Load and process files
  cat("\nReading files...\n")

  data_list <- list()
  for (file in indicator_files) {
    df <- read_delim(
      file,
      delim = "\t",
      locale = locale(encoding = "UTF-8"),
      col_types = cols(.default = col_character())
    )
    data_list[[length(data_list) + 1]] <- df
  }

  # Combine files
  cat("\nBinding rows...\n")
  all_data <- bind_rows(data_list)

  # Convert Total to numeric
  all_data <- all_data %>%
    mutate(
      Total = str_replace_all(Total, "\\.", ""),
      Total = str_replace(Total, ",", "."),
      Total = as.numeric(Total)
    )

  # Determine additional grouping columns
  group_cols <- c()
  if (!is.null(config$required_cols)) {
    # Create mapping for column name translation
    col_mapping <- c(
      "Sexo" = "sex",
      "Nacionalidad" = "nationality",
      "Tramos de edad" = "age_group"
    )

    # Translate the required columns from Spanish to English
    group_cols <- sapply(config$required_cols, function(col) {
      if (col %in% names(col_mapping)) {
        col_mapping[col]
      } else {
        col
      }
    })
  }

  # Translate demographic columns if they exist
  if ("Sexo" %in% names(all_data)) {
    all_data <- all_data %>%
      mutate(
        sex = case_when(
          Sexo == "Mujeres" ~ "female",
          Sexo == "Hombres" ~ "male",
          TRUE ~ tolower(Sexo)
        )
      ) %>%
      select(-Sexo) # Remove original column
  }

  if ("Nacionalidad" %in% names(all_data)) {
    all_data <- all_data %>%
      mutate(
        nationality = case_when(
          Nacionalidad == "Española" ~ "spanish",
          Nacionalidad == "Extranjera" ~ "foreign",
          TRUE ~ tolower(Nacionalidad)
        )
      ) %>%
      select(-Nacionalidad) # Remove original column
  }

  if ("Tramos de edad" %in% names(all_data)) {
    all_data <- all_data %>%
      mutate(
        age_group = case_when(
          `Tramos de edad` == "Menos de 18 años" ~ "under_18",
          `Tramos de edad` == "De 18 a 64 años" ~ "18_64",
          `Tramos de edad` == "65 y más años" ~ "over_65",
          TRUE ~ gsub(" ", "_", tolower(`Tramos de edad`))
        )
      ) %>%
      select(-`Tramos de edad`) # Remove original column
  }

  # Print for debugging
  cat("\nUpdated group_cols:", paste(group_cols, collapse = ", "), "\n")

  cat("Processing geographic levels...")

  # Update required_cols in config lists where needed
  indicator_configs$distribution_sex$required_cols <- c("sex")
  indicator_configs$distribution_sex_age$required_cols <- c("sex", "age_group")
  indicator_configs$distribution_sex_nationality$required_cols <- c("sex", "nationality")

  # Update required_cols to match the new English column names
  if ("Sexo" %in% names(all_data)) {
    config$required_cols <- gsub("Sexo", "sex", config$required_cols)
  }
  if ("Nacionalidad" %in% names(all_data)) {
    config$required_cols <- gsub("Nacionalidad", "nationality", config$required_cols)
  }
  if ("Tramos de edad" %in% names(all_data)) {
    config$required_cols <- gsub("Tramos de edad", "age_group", config$required_cols)
  }

  # Process each geographic level
  # Municipality level
  mun_data <- all_data %>%
    filter(is.na(Distritos) | Distritos == "")

  # Select columns based on whether it's a distribution file
  if (length(group_cols) > 0) {
    # Use the updated English column names
    mun_data <- mun_data %>%
      select(
        Municipios,
        any_of(c("sex", "nationality", "age_group")),
        all_of(config$column),
        Periodo,
        Total
      )
  } else {
    mun_data <- mun_data %>%
      select(Municipios, all_of(config$column), Periodo, Total)
  }

  mun_data <- mun_data %>%
    mutate(
      mun_code = str_extract(Municipios, "^\\d+"),
      mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
      year = Periodo,
      indicator = .data[[config$column]],
      value = Total
    ) %>%
    select(
      mun_code, mun_name, year,
      any_of(c("sex", "nationality", "age_group")),
      indicator, value
    )

  # District level
  district_data <- all_data %>%
    filter((!is.na(Distritos) & Distritos != "") & (is.na(Secciones) | Secciones == ""))

  if (length(group_cols) > 0) {
    district_data <- district_data %>%
      select(
        Municipios,
        Distritos,
        any_of(c("sex", "nationality", "age_group")),
        all_of(config$column),
        Periodo,
        Total
      )
  } else {
    district_data <- district_data %>%
      select(Municipios, Distritos, all_of(config$column), Periodo, Total)
  }

  district_data <- district_data %>%
    mutate(
      mun_code = str_extract(Municipios, "^\\d+"),
      mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
      district_code = str_extract(Distritos, "\\d+"),
      year = Periodo,
      indicator = .data[[config$column]],
      value = Total
    ) %>%
    select(
      mun_code, mun_name, district_code, year,
      any_of(c("sex", "nationality", "age_group")),
      indicator, value
    )

  # Section level
  section_data <- all_data %>%
    filter(!is.na(Secciones) & Secciones != "")

  if (length(group_cols) > 0) {
    section_data <- section_data %>%
      select(
        Municipios,
        Distritos,
        Secciones,
        any_of(c("sex", "nationality", "age_group")),
        all_of(config$column),
        Periodo,
        Total
      )
  } else {
    section_data <- section_data %>%
      select(Municipios, Distritos, Secciones, all_of(config$column), Periodo, Total)
  }

  section_data <- section_data %>%
    mutate(
      mun_code = str_extract(Municipios, "^\\d+"),
      mun_name = str_trim(str_remove(Municipios, "^\\d+\\s")),
      district_code = str_extract(Distritos, "\\d+"),
      tract_code = str_extract(Secciones, "\\d+"),
      year = Periodo,
      indicator = .data[[config$column]],
      value = Total
    ) %>%
    select(
      mun_code, mun_name, district_code, tract_code, year,
      any_of(c("sex", "nationality", "age_group")),
      indicator, value
    )

  # Add province names and province codes
  # For municipality data
  mun_data <- mun_data %>%
      mutate(
          prov_code = substr(mun_code, 1, 2),
          prov_name = province_codes[prov_code]
      )

  # For district data
  district_data <- district_data %>%
      mutate(
          prov_code = substr(mun_code, 1, 2),
          prov_name = province_codes[prov_code]
      )

  # For section data
  section_data <- section_data %>%
      mutate(
          prov_code = substr(mun_code, 1, 2),
          prov_name = province_codes[prov_code]
      )

  # For distribution data, keep demographic columns in id_cols
  base_id_cols <- list(
    mun = c("mun_code", "mun_name", "prov_code", "prov_name", "year"),
    district = c("mun_code", "mun_name", "prov_code", "prov_name", "district_code", "year"),
    section = c("mun_code", "mun_name", "prov_code", "prov_name", "district_code", "tract_code", "year")
  )

  # Add the demographic columns to the id_cols if they exist
  demographic_cols <- intersect(
    c("sex", "nationality", "age_group"),
    names(mun_data)
  )

  # Convert to wide format
  mun_data_wide <- mun_data %>%
    mutate(indicator = recode(indicator, !!!config$mapping)) %>%
    pivot_wider(
      id_cols = c(base_id_cols$mun, all_of(demographic_cols)),
      names_from = indicator,
      values_from = value
    )

  district_data_wide <- district_data %>%
    mutate(indicator = recode(indicator, !!!config$mapping)) %>%
    pivot_wider(
      id_cols = c(base_id_cols$district, all_of(demographic_cols)),
      names_from = indicator,
      values_from = value
    )

  section_data_wide <- section_data %>%
    mutate(indicator = recode(indicator, !!!config$mapping)) %>%
    pivot_wider(
      id_cols = c(base_id_cols$section, all_of(demographic_cols)),
      names_from = indicator,
      values_from = value
    )

  # Save datasets with compression
  out_dir <- file.path(gitdata, indicator_type)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Function to save and compress data
  save_compressed <- function(data, filename) {
    # Create a temporary file for the CSV
    temp_csv <- tempfile(fileext = ".csv")
    write_csv(data, temp_csv)

    # Create zip file
    zip_filename <- paste0(tools::file_path_sans_ext(filename), ".zip")
    zip_path <- file.path(out_dir, zip_filename)

    # Remove existing zip if it exists
    if (file.exists(zip_path)) {
      unlink(zip_path)
    }

    # Compress the CSV file
    utils::zip(zip_path, temp_csv, flags = "-j9X") # -j: junk paths, -9: maximum compression, -X: no extras

    # Clean up temporary file
    unlink(temp_csv)

    # Log compression results
    orig_size <- file.size(temp_csv)
    zip_size <- file.size(zip_path)
    compression_ratio <- (1 - zip_size / orig_size) * 100
    cat(sprintf(
      "\nCompressed %s: %.1f MB -> %.1f MB (%.1f%% reduction)",
      filename, orig_size / 1e6, zip_size / 1e6, compression_ratio
    ))
  }

  # Save and compress each dataset
  save_compressed(mun_data_wide, paste0(indicator_type, "_municipality.csv"))
  save_compressed(district_data_wide, paste0(indicator_type, "_district.csv"))
  save_compressed(section_data_wide, paste0(indicator_type, "_tract.csv"))

  # Check province coverage
  cat("\nChecking province coverage:")

  check_provinces <- function(data, level) {
    # Get all indicator columns from the mapping
    indicator_columns <- unname(config$mapping)

    # Check provinces
    provinces <- data %>%
      mutate(province_code = substr(mun_code, 1, 2)) %>%
      pull(province_code) %>%
      unique() %>%
      sort()

    cat(sprintf(
      "\n%s data covers %d provinces (should be 52)",
      level, length(provinces)
    ))

    if (length(provinces) != 52) {
      missing_codes <- setdiff(names(province_codes), provinces)
      cat("\nMissing provinces:")
      for (code in missing_codes) {
        cat(sprintf("\n  - %s (%s)", province_codes[code], code))
      }
    }

    # For heterogeneity files, check data coverage by province
    if (grepl("distribution_", indicator_type)) {
      cat("\n\nChecking indicator coverage by province:")

      for (prov in provinces) {
        prov_data <- data %>%
          filter(substr(mun_code, 1, 2) == prov)

        # Check which indicators have all NA values for this province
        missing_indicators <- indicator_columns[sapply(indicator_columns, function(col) {
          all(is.na(prov_data[[col]]))
        })]

        if (length(missing_indicators) > 0) {
          cat(sprintf(
            "\n\nProvince %s (%s) missing data for:",
            province_codes[prov], prov
          ))
          for (ind in missing_indicators) {
            cat(sprintf("\n  - %s", ind))
          }
        }
      }
    }

    return(provinces)
  }

  mun_provinces <- check_provinces(mun_data_wide, "Municipality")
  # district_provinces <- check_provinces(district_data_wide, "District")
  # section_provinces <- check_provinces(section_data_wide, "Census tract")

  # Return the processed datasets invisibly
  return(invisible(list(
    municipality = mun_data_wide,
    district = district_data_wide,
    tract = section_data_wide
  )))
}
