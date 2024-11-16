
library(extrafont)
library(ggplot2)
library(tidyverse)
library(data.table)
library(scales)
library(ggtext)
library(ARDECO)
library(giscoR)
library(WDI)

username <- Sys.getenv("USERNAME")
root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")

loadfonts(device = "win", quiet = TRUE)

# WDI

# Read ----
wdi <- WDI(
  country = "all",
  indicator = c(
    "PA.NUS.PRVT.PP"
    ),
  start = 2021,
  end = 2021,
) 

wdi <- wdi %>%
  rename(ppp = PA.NUS.PRVT.PP) %>%
  mutate(iso2c = countrycode::countrycode(country, origin = "country.name", destination = "iso2c")) %>%
  drop_na()

# ARDECO
vec_vars <- c("SNPTD")

data_list <- map(
    vec_vars,
    ~ ardeco_get_dataset_data(
        variable = .x,
        version = 2021,
        level = '2'
    )
)

# Filter out non-data frame elements
data_list <- keep(data_list, is.data.frame)

# Reshape each data frame and merge them
reshaped_data_list <- map(data_list, function(df) {
    colnames(df) <- tolower(colnames(df))
    variable_name <- unique(df$variable)
    df <- df %>%
        select(-variable) %>%
        pivot_wider(names_from = unit, values_from = value, names_prefix = paste0(variable_name, "_"))
    return(df)
})

# Combine the reshaped data frames by merging on common columns
final_data <- reduce(reshaped_data_list, full_join, by = c("versions", "level", "nutscode", "year"))

# Display the final combined dataset
View(final_data)

# Remove non-alphanumeric characters from column names
remove_nonalpha <- function(df) {
  colnames(df) <- colnames(df) %>%
    str_replace_all(" ", "") %>%  # Remove spaces
    str_replace_all("[^[:alnum:]_]", "")  # Remove non-alphanumeric characters except underscore
  return(df)
}

final_data <- final_data %>%
  remove_nonalpha()

hh_income <- read_csv(
    paste0(root, "/RUVNH - Million EUR - Households net disposable income.csv")
) %>%
    select(NUTS, `2021`) %>%
    rename(nutscode = NUTS, hh_income = `2021`)

# Define non-euro exchange rates for 2021 (annual averages)
# Source: ECB reference rates
# https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/
exchange_rates <- tribble(
  ~iso2c, ~currency, ~rate,  # rate is LCU per EUR
  "BG", "BGN", 1.9558,      # Bulgarian lev
  "CZ", "CZK", 25.6400,     # Czech koruna
  "DK", "DKK", 7.4370,      # Danish krone
  "HR", "HRK", 7.5284,      # Croatian kuna
  "HU", "HUF", 358.5200,    # Hungarian forint
  "PL", "PLN", 4.5652,      # Polish zloty
  "RO", "RON", 4.9215,      # Romanian leu
  "SE", "SEK", 10.1465,     # Swedish krona
  "CH", "CHF", 1.0811,      # Swiss franc
  "NO", "NOK", 10.1633,     # Norwegian krone
  "IS", "ISK", 150.1500     # Icelandic krona
)

# Create Euro area indicator
euro_countries <- c("AT", "BE", "CY", "DE", "EE", "ES", "FI", "FR", "GR", "IE", "IT", 
                   "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")

# Merge
merged <- final_data %>%
    left_join(
        hh_income,
        by = "nutscode"
    ) %>%
    mutate(
        net_income_pc = 1000000*hh_income/`SNPTD_Persons`
    ) %>%
    filter(year == 2021) %>%
    select(nutscode, net_income_pc) %>%
    rename(NUTS_ID = nutscode) %>%
    mutate(
        iso2c = str_sub(NUTS_ID, 1, 2),
        iso2c = ifelse(str_sub(NUTS_ID, 1, 2)=="EL", "GR", iso2c),
        is_euro = iso2c %in% euro_countries
    ) %>%
    left_join(
        wdi,
        by = "iso2c"
    ) %>%
    left_join(
        exchange_rates,
        by = "iso2c"
    ) %>%
    mutate(
        rate = ifelse(country == "Croatia", 1, rate)  # adjust error in xr for HR
    ) %>%
    # For non-euro countries: EUR -> LCU -> PPP
    # For euro countries: just apply PPP
    mutate(
        net_income_pc = case_when(
            is_euro ~ net_income_pc/ppp,
            !is_euro ~ (net_income_pc * rate)/ppp,  # Convert EUR to LCU then to PPP
            TRUE ~ NA_real_
        )
    )

# Get NUTS2 geometry
nuts2_geometry <- gisco_get_nuts(
  year = 2021,
  resolution = "20",
  nuts_level = 2
) %>%
  select(NUTS_ID, geometry)

# Join with geometry
map_data_final <- nuts2_geometry %>%
  left_join(merged, by = "NUTS_ID")

# Create the map
plot <- ggplot(map_data_final) +
  geom_sf(
    aes(fill = cut(net_income_pc,
        breaks = c(-Inf, 10000, 14000, 18000, 22000, 26000, 30000, Inf),
        labels = c("<10k", "10-14k", "14-18k", "18-22k", "22-26k", "26-30k", ">30k")
    )),
    color = NA
  ) +
  scale_fill_manual(
    name = "Net income per \ncapita, 2021 \n(int. $, PPP)",
    values = c("#3b0012", "#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC", "#053061"),
    na.value = "grey80"
  ) +
  coord_sf(xlim = c(-12, 45), ylim = c(35, 72)) +
  labs(
    title = "Net income per capita across European regions, 2021",
    caption = "@pablogguz_ | The map shows net household income per capita adjusted for purchasing power parity at the NUTS2 level. Source: ARDECO Database, World Bank WDIs and author's calculations."
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto", size = 14),
    plot.title = element_text(size = 18, margin = margin(b = 20), face = "bold"),
    legend.position = c(0.9, 0.6),
    legend.title = element_text(size = 14),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_textbox_simple(
      size = 10,
      color = "grey40",
      margin = margin(t = 20),
      hjust = 0,
      halign = 0,
      lineheight = 1.2
    )
  ) +
  coord_sf(xlim = c(-22, 55), ylim = c(35, 72))

ggsave(
    "plot.png",
    plot,
    width = 8,
    height = 6.75,
    dpi = 300,
    bg = "white"
)


# Filter out specific capital regions
fortweet <- map_data_final %>%
  filter(NUTS_ID %in% c(
    "PL91",  # Warsaw (Warszawski stołeczny)
    "RO32",  # Bucharest (București-Ilfov)
    "PL21",  # Krakow (Małopolskie)
    "HU11",  # Budapest
    "EE00",  # Tallinn (all of Estonia is one NUTS2 region)
    "CZ01",  # Prague
    "SK01",  # Bratislava
    "BG41",  # Sofia (Yugozapaden)
    "LV00",  # Riga (all of Latvia is one NUTS2 region)
    "LT01",  # Vilnius region
    "SI04",  # Western Slovenia (Ljubljana)
    "HR05"   # City of Zagreb
  )) %>%
  mutate(
    city_name = case_when(
      NUTS_ID == "PL91" ~ "Warsaw",
      NUTS_ID == "RO32" ~ "Bucharest",
      NUTS_ID == "PL21" ~ "Krakow",
      NUTS_ID == "HU11" ~ "Budapest",
      NUTS_ID == "EE00" ~ "Estonia (Tallinn)",
      NUTS_ID == "CZ01" ~ "Prague",
      NUTS_ID == "SK01" ~ "Bratislava",
      NUTS_ID == "BG41" ~ "Sofia",
      NUTS_ID == "LV00" ~ "Latvia (Riga)",
      NUTS_ID == "LT01" ~ "Vilnius",
      NUTS_ID == "SI04" ~ "Western Slovenia (Ljubljana)",
      NUTS_ID == "HR05" ~ "Zagreb"
    )
  ) %>%
  as.data.frame() %>%
  select(NUTS_ID, city_name, net_income_pc)
