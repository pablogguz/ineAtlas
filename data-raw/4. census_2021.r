#* ------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Process census tract-level data from the 2021 Census
#* ------------------------------------------------------------------------------

# Load required packages
packages_to_load <- c(
    "tidyverse",
    "sf",
    "zip",
    "httr"
)

package.check <- lapply(
    packages_to_load,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
        }
    }
)

lapply(packages_to_load, require, character = TRUE)

# Set paths
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw_census/")
out_dir <- file.path(gitdata, "census_2021")

province_codes <- c(
    "02" = "Albacete",
    "03" = "Alicante/Alacant",
    "04" = "Almería",
    "01" = "Araba/Álava",
    "33" = "Asturias",
    "05" = "Ávila",
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

# Load data
census <- read_csv(file.path(raw, "C2021_Indicadores.csv"))

# Create mapping dictionary for variable renaming
var_mapping <- c(
    # Population counts
    total_pop = "t1_1",

    # Gender distribution
    pct_female = "t2_1",
    pct_male = "t2_2",

    # Age structure
    mean_age = "t3_1",
    pct_under16 = "t4_1",
    pct_16to64 = "t4_2",
    pct_over64 = "t4_3",

    # Nationality and origin
    pct_foreign = "t5_1",
    pct_foreign_born = "t6_1",

    # Education
    pct_higher_ed_enrolled = "t7_1", # % enrolled in higher education
    pct_university_enrolled = "t8_1", # % enrolled in university
    pct_higher_ed_completed = "t9_1", # % completed higher education

    # Labor market status
    unemployment_rate = "t10_1", # unemployment rate (% of active population)
    employment_rate = "t11_1", # employment rate (% of 16+ population)
    activity_rate = "t12_1", # activity rate (% of 16+ population)

    # Pension and inactivity
    pct_disability_pension = "t13_1", # % receiving disability pension
    pct_retirement_pension = "t14_1", # % receiving retirement pension
    pct_other_inactive = "t15_1", # % other inactive
    pct_student = "t16_1", # % students

    # Marital status
    pct_single = "t17_1",
    pct_married = "t17_2",
    pct_widowed = "t17_3",
    pct_marital_unknown = "t17_4",
    pct_divorced_separated = "t17_5",

    # Housing
    total_dwellings = "t18_1",
    primary_dwellings = "t19_1",
    secondary_dwellings = "t19_2",
    owned_dwellings = "t20_1",
    rented_dwellings = "t20_2",
    other_tenure_dwellings = "t20_3",

    # Household composition
    total_households = "t21_1",
    hh_size1 = "t22_1", # 1-person households
    hh_size2 = "t22_2", # 2-person households
    hh_size3 = "t22_3", # 3-person households
    hh_size4 = "t22_4", # 4-person households
    hh_size5plus = "t22_5" # 5+ person households
)

# Process base data with geographic codes
census_base <- census %>%
    # Add geographic codes
    mutate(
        # Pad numbers with zeros
        cpro = str_pad(cpro, width = 2, pad = "0"),
        cmun = str_pad(cmun, width = 3, pad = "0"),
        dist = str_pad(dist, width = 2, pad = "0"),
        secc = str_pad(secc, width = 3, pad = "0"),

        # Create standard codes
        prov_code = cpro,
        mun_code = paste0(cpro, cmun),
        district_code = paste0(cpro, cmun, dist),
        tract_code = paste0(cpro, cmun, dist, secc)
    )

# Create tract level dataset
census_tract <- census_base %>%
    rename(!!!var_mapping) %>%
    select(
        tract_code, district_code, mun_code, prov_code,
        everything(),
        -ccaa, -cpro, -cmun, -dist, -secc # Remove original geographic columns
    ) %>%
    mutate(
        prov_name = province_codes[prov_code]
    )

# Identify variables to sum (counts) vs average (rates/percentages)
sum_vars <- c(
    "t1_1", # total population
    "t18_1", # total dwellings
    "t19_1", # primary dwellings
    "t19_2", # secondary dwellings
    "t20_1", # owned dwellings
    "t20_2", # rented dwellings
    "t20_3", # other tenure dwellings
    "t21_1", # total households
    "t22_1", # 1-person households
    "t22_2", # 2-person households
    "t22_3", # 3-person households
    "t22_4", # 4-person households
    "t22_5" # 5+ person households
)

# Get census variables (t*_*) excluding the geographic identifiers
census_vars <- names(census_base)[grep("^t\\d+_\\d+$", names(census_base))]
mean_vars <- setdiff(census_vars, sum_vars)

# Create district level dataset (aggregate)
census_district <- census_base %>%
    select(-tract_code) %>%
    group_by(prov_code, mun_code, district_code) %>%
    # First, sum the count variables
    summarise(
        across(all_of(sum_vars), sum, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    # Then join back with the weighted means
    left_join(
        census_base %>%
            select(-tract_code) %>%
            group_by(prov_code, mun_code, district_code) %>%
            summarise(
                across(all_of(mean_vars), ~ weighted.mean(.x, w = t1_1, na.rm = TRUE)),
                .groups = "drop"
            ),
        by = c("prov_code", "mun_code", "district_code")
    ) %>%
    rename(!!!var_mapping) %>%
    mutate(
        prov_name = province_codes[prov_code]
    )

# Create municipality level dataset (aggregate)
census_municipality <- census_base %>%
    select(-tract_code, -district_code) %>%
    group_by(prov_code, mun_code) %>%
    summarise(
        across(all_of(sum_vars), sum, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    left_join(
        census_base %>%
            select(-tract_code, -district_code) %>%
            group_by(prov_code, mun_code) %>%
            summarise(
                across(all_of(mean_vars), ~ weighted.mean(.x, w = t1_1, na.rm = TRUE)),
                .groups = "drop"
            ),
        by = c("prov_code", "mun_code")
    ) %>%
    rename(!!!var_mapping) %>%
    mutate(
        prov_name = province_codes[prov_code]
    )

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
    utils::zip(zip_path, temp_csv, flags = "-j9X")

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

# Create output directory
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save all three datasets
save_compressed(census_tract, "census_2021_tract.csv")
save_compressed(census_district, "census_2021_district.csv")
save_compressed(census_municipality, "census_2021_municipality.csv")
