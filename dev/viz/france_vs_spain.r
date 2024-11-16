
# Load required libraries
library(extrafont)
library(ggplot2)
library(tidyverse)
library(data.table)
library(scales)
library(ggtext)
library(mapSpain)
library(maps)
library(sf)
library(mapproj)
library(readxl)
library(WDI)
library(patchwork)
library(Hmisc)

# Set up environment
username <- Sys.getenv("USERNAME")
root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/viz_other/")
loadfonts(device = "win", quiet = TRUE)

# ---------------------------------------------------------------------------

# Load Development Indicators PPP data ----

wdi <- WDI(
  country = c("ES","FR"),
  indicator = c("PA.NUS.PRVT.PP"),
  start = 2019,
  end = 2019
) 

prov_to_ccaa <- data.frame(
   prov_code = c(
       "01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
       "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
       "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
       "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
       "41", "42", "43", "44", "45", "46", "47", "48", "49", "50"
   ),
   ccaa_code = c(
       "16", "08", "10", "01", "07", "11", "04", "09", "07", "11",
       "01", "10", "08", "01", "12", "08", "09", "01", "08", "16",
       "01", "02", "01", "07", "09", "17", "12", "13", "01", "14",
       "15", "12", "03", "07", "05", "12", "07", "05", "06", "07",
       "01", "07", "09", "02", "08", "10", "07", "16", "07", "02"
   )
)

# Load Spanish atlas data ----
atlas_all <- merge(
   setDT(ineAtlas::get_atlas("income", "municipality")),
   setDT(ineAtlas::get_atlas("demographics", "municipality"))
) 

# Calculate average change 2019-2020 for municipalities with data in both years
growth_factor <- atlas_all %>%
  filter(!is.na(net_income_equiv)) %>%
  select(year, mun_code, net_income_equiv) %>%
  pivot_wider(names_from = year, 
              values_from = net_income_equiv,
              names_prefix = "income_") %>%
  filter(!is.na(income_2019) & !is.na(income_2020)) %>%
  summarise(
    avg_growth = mean(income_2020/income_2019, na.rm = TRUE)
  ) %>%
  pull(avg_growth)
  
# Check missing by province for 2019
# missing_by_prov_2019 <- atlas_all %>%
#   filter(year == 2019) %>%
#   group_by(prov_name) %>%
#   summarise(
#     total_munic = n(),
#     missing_munic = sum(is.na(net_income_equiv)),
#     pct_missing = round(missing_munic/total_munic * 100, 1)
#   ) %>%
#   arrange(desc(pct_missing))

# print(missing_by_prov_2019)

# Get data for 2019
atlas_2019 <- atlas_all %>%
  # First get 2019 data
  filter(year == 2019) %>%
  mutate(
    ratio = mean(net_income_equiv / net_income_pc, na.rm = TRUE)
  ) %>%
  mutate(
    income_equiv_final = case_when(
      !is.na(net_income_equiv) ~ net_income_equiv,
      TRUE ~ net_income_pc * ratio
    )
  ) 

# Get data for 2020
atlas_2020 <- atlas_all %>%
  filter(year == 2020) %>%
  mutate(
    ratio = mean(net_income_equiv / net_income_pc, na.rm = TRUE)
  ) %>%
  mutate(
    income_equiv_final = case_when(
      !is.na(net_income_equiv) ~ net_income_equiv,
      TRUE ~ net_income_pc * ratio
    )
  ) %>%
  mutate(
    income_equiv_final_2020 = income_equiv_final/as.numeric(growth_factor)
  ) %>%
  select(income_equiv_final_2020, mun_code)

# Combine
atlas <- atlas_2019 %>% 
    left_join(atlas_2020) %>% 
    mutate(
        income_equiv_final = coalesce(income_equiv_final, income_equiv_final_2020)
    )

# Show percentage of missing values 
calculate_missing_percentage <- function(df, variable) {
    # Handle cases where variable is passed as string or symbol
    var_name <- deparse(substitute(variable))
    
    # Calculate missing percentage
    missing_percentage <- df %>%
        summarise(missing_pct = mean(is.na(!!sym(var_name))) * 100)
    
    # Create output message
    result <- paste("Percentage of missing values in", var_name, ":", 
                   round(missing_percentage$missing_pct, 2), "%")
    
    # Print result
    print(result)
    
    # Return the percentage value invisibly for potential further use
    return(invisible(missing_percentage$missing_pct))
}

atlas_2020_raw <- atlas_all %>% filter(year==2020)
calculate_missing_percentage(atlas_2020_raw, net_income_equiv)
calculate_missing_percentage(atlas_2020, income_equiv_final_2020)
calculate_missing_percentage(atlas_2019, income_equiv_final)
calculate_missing_percentage(atlas_2019, net_income_pc)

# Winsorize spanish data 

tract_all <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
)

tract_2019 <- tract_all %>%
  # First get 2019 data
  filter(year == 2019) %>%
  mutate(
    ratio = mean(net_income_equiv / net_income_pc, na.rm = TRUE)
  ) %>%
  mutate(
    income_equiv_final = case_when(
      !is.na(net_income_equiv) ~ net_income_equiv,
      TRUE ~ net_income_pc * ratio
    )
  ) 

# Get data for 2020
tract_2020 <- tract_all %>%
  filter(year == 2020) %>%
  mutate(
    ratio = mean(net_income_equiv / net_income_pc, na.rm = TRUE)
  ) %>%
  mutate(
    income_equiv_final = case_when(
      !is.na(net_income_equiv) ~ net_income_equiv,
      TRUE ~ net_income_pc * ratio
    )
  ) %>%
  mutate(
    income_equiv_final_2020 = income_equiv_final/as.numeric(growth_factor)
  ) %>%
  select(income_equiv_final_2020, tract_code)

# Combine
tract <- tract_2019 %>% 
    left_join(tract_2020) %>% 
    mutate(
        income_equiv_final = coalesce(income_equiv_final, income_equiv_final_2020)
    )

spanish_percentiles <- tract %>%
  group_by(prov_code) %>%
  summarise(
      p5 = Hmisc::wtd.quantile(income_equiv_final, weights = population, probs = 0.05),
      p95 = Hmisc::wtd.quantile(income_equiv_final, weights = population, probs = 0.95)
  ) 

# winsorize using the tract-level data at the province level
atlas <- atlas %>%
  left_join(spanish_percentiles) %>%
  mutate(
    income_equiv_final = case_when(
      income_equiv_final < p5 ~ p5,
      income_equiv_final > p95 ~ p95,
      TRUE ~ income_equiv_final
    )
  )

tract <- tract %>%
  left_join(spanish_percentiles) %>%
  mutate(
    income_equiv_final = case_when(
      income_equiv_final < p5 ~ p5,
      income_equiv_final > p95 ~ p95,
      TRUE ~ income_equiv_final
    )
  )

# Get tract geometries
# esp_map <- ineAtlas::get_tract_geom(2019) %>%
#   left_join(
#     tract %>% 
#       mutate(
#           income_equiv_final = income_equiv_final / 0.68 # Adjust for PPP
#       )
#   ) %>%
#   mutate(
#     income_equiv_final = case_when(
#       income_equiv_final < 10000 ~ 10000,
#       income_equiv_final > 40000 ~ 40000,
#       TRUE ~ income_equiv_final
#     )
#   )

# Spanish municipalities
esp_map <- esp_get_munic_siane() %>%
    left_join(
        atlas %>% 
            mutate(
                income_equiv_final = income_equiv_final / 0.68, # Adjust for PPP
                LAU_CODE = mun_code
            ),
        by = "LAU_CODE"
    ) %>%
    mutate(
      income_equiv_final = case_when(
        income_equiv_final < 10000 ~ 10000,
        income_equiv_final > 40000 ~ 40000,
        TRUE ~ income_equiv_final
      )
    )

# Load French data ----

# French grid cells
france_map <- read_sf(paste0(root, "france/carreaux_1km_met.gpkg"))

# data.table for faster processing
france_dt <- setDT(st_drop_geometry(france_map))[
 , .(
   total_pop = sum(ind, na.rm = TRUE),
   total_ind_snv = sum(ind_snv, na.rm = TRUE)
 ), 
  by = .(commune = case_when(
   substr(lcog_geo, 1, 3) == "751" ~ "75056", # If it's Paris, use the commune code
   #substr(lcog_geo, 1, 5) == "69100" ~ "69123", # If it's Lyon, use the commune code
   
   TRUE ~ substr(lcog_geo, 1, 5)  # Otherwise proceed as normal
 ))
][
 , income_equiv_final := (total_ind_snv/total_pop)/0.8 # adjust for PPP
][
 , income_equiv_final := fifelse(income_equiv_final < 10000, 10000,
                        fifelse(income_equiv_final > 40000, 40000,
                               income_equiv_final))
]

# load French commune geometries
france_geom <- read_sf(paste0(root, "france/communes-20200101.shp"))

france_communes_final <- france_geom %>%
 right_join(france_dt, by = c("insee" = "commune")) %>%
 select(insee, nom, income_equiv_final, geometry)

# Show all insee codes with missing data 
missing_data <- france_geom %>% 
    as.data.frame() %>%
    select(-geometry) %>%
    left_join(france_dt, by = c("insee" = "commune")) %>%
    filter(is.na(income_equiv_final)) %>%
    select(insee, nom) %>% 
    distinct()

# paris_check <- france_geom %>% 
#   filter(grepl("Paris", nom, ignore.case = TRUE) | 
#          grepl("^75", insee))  # Paris department code is 75
# print(paris_check)

# paris_grid <- france_map %>%
#   filter(grepl("75056", lcog_geo))
# print(head(paris_grid))

# top_100_communes <- france_communes_final %>%
#   arrange(desc(income_equiv_final)) %>%
#   select(insee, nom, income_equiv_final) %>%
#   slice_head(n = 100)

# # Print the top 100, focusing on Paris
# print(top_100_communes)

# # Specifically check Paris
# paris_check <- france_communes_final %>%
#   filter(insee == "75056") %>%
#   select(insee, nom, income_equiv_final)

# print("Paris values:")
# print(paris_check)

# lyon_check <- france_communes_final %>%
#   filter(insee == "69123") %>%
#   select(insee, nom, income_equiv_final)

# Set up plotting parameters ----

# Define breaks and labels
income_breaks <- c(10000, 20000, 30000, 40000)
income_labels <- c("<$10k", "$20k", "$30k", ">$40k")

# Create common theme
common_theme <- theme_void() +
    theme(
        text = element_text(family = "Roboto", size = 12),
        plot.title = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold", margin = margin(b = 10)),
        plot.margin = margin(10, 10, 10, 10),
        plot.caption = element_textbox_simple(
            size = 10,
            color = "#868686",
            margin = margin(t = 10),
            hjust = 0,
            halign = 0,
            lineheight = 1.2
        )
    )

# Create plots ----

# France
france_plot <- ggplot(france_communes_final) +
    geom_sf(aes(fill = income_equiv_final), color = NA) +
    scale_fill_gradientn(
        name = "Disposable income per equivalent adult (USD, PPP)",
        colors = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#92C5DE", "#4393C3", "#2166AC"),
        na.value = "#e4e4e4",
        breaks = income_breaks,
        labels = income_labels,
        limits = c(10000, 40000),
        guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            label.position = "bottom",
            barwidth = 20,
            barheight = 0.8,
            ticks.linewidth = 1
        )
    ) +
    common_theme +
    labs(title = "France")

# Spain
spain_plot <- ggplot() +
    geom_sf(data = esp_map,
            aes(fill = income_equiv_final),
            color = NA) +
    scale_fill_gradientn(
        name = "Disposable income per equivalent adult (USD, PPP)",
        colors = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#92C5DE", "#4393C3", "#2166AC"),
        na.value = "#e4e4e4",
        breaks = income_breaks,
        labels = income_labels,
        limits = c(10000, 40000),
        guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            label.position = "bottom",
            barwidth = 20,
            barheight = 0.8,
            ticks.linewidth = 1
        )
    ) +
    common_theme +
    labs(title = "Spain")

# Combine plots and add final elements
combined_plot <- france_plot + spain_plot + 
    plot_layout(guides = "collect", widths = c(1, 1)) &
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.margin = margin(t = 10, b = 10),
        legend.box.spacing = unit(1, "cm")
    ) 

combined_plot <- combined_plot + 
    plot_annotation(
        title = "Disposable household income in 2019",
        caption = "@pablogguz_ | The map shows PPP-adjusted disposable household income per equivalent adult in France and Spain (2019). French data aggregated from 1km x 1km grid cells to communes using population weights. Spanish data shown at municipal level with two adjustments: i) for
        municipalities with missing equivalised income data, values are imputed using disposable per capita income multiplied by the national average
        ratio of equivalised to per capita income; ii) for municipalities with missing income data for 2019 (approximately 19% of the sample), data from 2020 is used instead, with values downscaled by the cross-municipality average growth factor from 2019 to 2020.  Equivalised income adjusts household income to account for differences in household size and composition using equivalence scales, which assign a weight of 1.0 to the first adult, 0.5 to additional adults over 14, and 0.3 to each child under 14. Sources: INSEE Filosofi, Spanish Statistical Office (INE), World Bank WDIs and authors' calculations.",
        theme = theme(
            text = element_text(family = "Roboto", size = 12),
            plot.title = element_text(size = 18, margin = margin(b = 10), face = "bold"),
            plot.subtitle = element_text(size = 14, margin = margin(b = 14), hjust = 0.5),
            plot.caption = element_textbox_simple(
                size = 7,
                color = "#8a8a8a",
                margin = margin(t = 10),
                hjust = 0,
                halign = 0,
                lineheight = 1.2
            )
        )
    )

# Save ----
ggsave(
    "france_spain_income.png",
    combined_plot,
    width = 11,
    height = 7,
    dpi = 300,
    bg = "white"
)
