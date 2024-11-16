# 1. Load required libraries
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
library(tigris)
library(readxl)
library(WDI)
library(patchwork)

# 2. Set up environment
username <- Sys.getenv("USERNAME")
root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
loadfonts(device = "win", quiet = TRUE)

# 3. Load raw data
# 3.1 World Development Indicators data
wdi <- WDI(
    country = c("ES","GB"),
    indicator = c("PA.NUS.PRVT.PP"),
    start = 2022,
    end = 2022
) 

# 3.2 Spanish Atlas data
atlas <- merge(
    setDT(ineAtlas::get_atlas("income", "municipality")),
    setDT(ineAtlas::get_atlas("demographics", "municipality"))
) %>%
    filter(year == 2022) 

# 3.3 UK data
lad_geom <- read_sf(paste0(root, "uk/lad_geometries.gpkg"))
lad_data <- read_xlsx(paste0(root, "uk/lad_householdincome.xlsx")) %>%
    mutate(
        household_income = `2022`/0.73, # Adjust for PPP
        LAD23CD = `LA code`
    ) %>%
    select(LAD23CD, household_income)

# 4. Process geographical data
# 4.1 Process Spanish map data
esp_map <- esp_get_munic_siane() %>%
    left_join(
        atlas %>% 
            mutate(
                net_income_pc = net_income_pc / 0.642, # Adjust for PPP
                LAU_CODE = mun_code
            ),
        by = "LAU_CODE"
    )

# 4.2 Process UK map data
lad_geom <- lad_geom %>%
    left_join(lad_data)

# 5. Apply income thresholds
# 5.1 UK thresholds
lad_geom <- lad_geom %>%
    mutate(
        household_income = case_when(
            as.numeric(household_income) < 10000 ~ 10000,
            as.numeric(household_income) > 40000 ~ 40000,
            TRUE ~ as.numeric(household_income)
        )
    )

# 5.2 Spain thresholds
esp_map <- esp_map %>%
    mutate(
        net_income_pc = case_when(
            net_income_pc < 10000 ~ 10000,
            net_income_pc > 40000 ~ 40000,
            TRUE ~ net_income_pc
        )
    )

# 6. Set up plotting parameters
# 6.1 Define breaks and labels
income_breaks <- c(10000, 20000, 30000, 40000)
income_labels <- c("<$10k", "$20k", "$30k", ">$40k")

# 6.2 Define common theme
common_theme <- theme_void() +
    theme(
        text = element_text(family = "Roboto", size = 12),
        plot.title = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold", margin = margin(b = 10)),
        plot.margin = margin(10, 10, 10, 10),
        plot.caption = element_textbox_simple(
            size = 10,
            color = "grey40",
            margin = margin(t = 20),
            hjust = 0,
            halign = 0,
            lineheight = 1.2
        )
    )

# 7. Create individual plots
# 7.1 UK plot
uk_plot <- ggplot(lad_geom) +
    geom_sf(aes(fill = household_income), color = NA) +
    coord_sf(
        xlim = c(10000, 655000),
        ylim = c(10000, 1220000),
        expand = FALSE,
        crs = 27700
    ) +
    scale_fill_gradientn(
        name = "Disposable household income per capita (USD, PPP)",
        colors = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#92C5DE", "#4393C3", "#2166AC"),
        na.value = "#e4e4e4",
        breaks = income_breaks,
        labels = income_labels,
        limits = c(10000, 40000),
        guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            label.position = "bottom",
            barwidth = 25,
            barheight = 0.8,
            ticks.linewidth = 1
        )
    ) +
    common_theme +
    labs(title = "United Kingdom")

# 7.2 Spain plot
spain_plot <- ggplot() +
    geom_sf(data = esp_map,
            aes(fill = net_income_pc),
            color = NA) +
    scale_fill_gradientn(
        name = "Disposable household income per capita (USD, PPP)",
        colors = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#92C5DE", "#4393C3", "#2166AC"),
        na.value = "#e4e4e4",
        breaks = income_breaks,
        labels = income_labels,
        limits = c(10000, 40000),
        guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            label.position = "bottom",
            barwidth = 25,
            barheight = 0.8,
            ticks.linewidth = 1
        )
    ) +
    common_theme +
    labs(title = "Spain")

# 8. Combine plots
combined_plot <- uk_plot + spain_plot + 
    plot_layout(guides = "collect", widths = c(1, 1)) &
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.margin = margin(t = 10, b = 10),
        legend.box.spacing = unit(1, "cm")
    )

# 9. Add final annotations
combined_plot <- combined_plot + 
    plot_annotation(
        title = "Median household income in 2022",
        caption = "@pablogguz_ | The map shows...",
        theme = theme(
            text = element_text(family = "Roboto", size = 12),
            plot.title = element_text(size = 18, margin = margin(b = 10), face = "bold"),
            plot.subtitle = element_text(size = 14, margin = margin(b = 14), hjust = 0.5),
            plot.caption = element_textbox_simple(
                size = 10,
                color = "grey40",
                margin = margin(t = 20),
                hjust = 0,
                halign = 0,
                lineheight = 1.2
            )
        )
    )

# 10. Save final plot
ggsave(
    "uk.png",
    combined_plot,
    width = 11,
    height = 6.75,
    dpi = 300,
    bg = "white"
)