
library(ineAtlas)
library(mapSpain)
library(tidyverse)
library(scales)
library(extrafont)
library(ggtext)
library(cowplot)
library(magick)
library(grid)
library(stringr)
library(ggtext)

# Get municipality level income data
mun_data <- get_census(
    level = "municipality"
) %>%
    mutate(
        pop_16_64 = total_pop * pct_16to64,
        pop_over_16 = total_pop*(1-pct_under16),
        total_employed = employment_rate * pop_over_16, 
        total_pensioners = pct_retirement_pension * pop_over_16,
        workers_per_pensioner = total_employed / total_pensioners
    ) %>%
    mutate(
        pct_higher_ed_completed = pct_higher_ed_completed * 100,
        pct_foreign_born = pct_foreign_born * 100,
        pct_single = pct_single * 100,
        pct_16to64 = pct_16to64 * 100,
        #employment_rate = employment_rate * 100,
        pct_retirement_pension = pct_retirement_pension * 100,
        unemployment_rate = unemployment_rate * 100
    ) %>%
    # Impute missing values with avg. at the province level
    group_by(prov_code) %>%
    mutate(
        across(
            c(pct_higher_ed_completed, pct_foreign_born, pct_single, pct_under16,
                pct_16to64, workers_per_pensioner,
              employment_rate, pct_retirement_pension, unemployment_rate),
            ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
        )
    ) %>%
    ungroup() %>%
    mutate(
        workers_per_pensioner = case_when(
            workers_per_pensioner < 0 ~ 0,
            workers_per_pensioner >= 6 ~ 6,
            TRUE ~ workers_per_pensioner
        )
    )
    
mun_data %>%
    filter(workers_per_pensioner > 25) %>%
    nrow()

# Get municipality geometries
mun_map <- esp_get_munic_siane() %>%
    left_join(
        mun_data,
        by = c("LAU_CODE" = "mun_code")
    )

breaks_scale <- c(0, 2, 4, 6)
breaks_labels <- c("0", "2", "4", "6")

# Create the main map
main_map <- ggplot(mun_map) +
    geom_sf(
        aes(fill = workers_per_pensioner),
        color = NA
    ) + 
    scale_fill_gradientn(
        name = "Workers per \npensioner",
        colors = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
        na.value = "#e4e4e4",
        breaks = breaks_scale,
        labels = breaks_labels,
        limits = c(0, 6),
        guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0,
            barheight = 10,  # Make bar taller for vertical orientation
            barwidth = 1,    # Make bar thinner for vertical orientation
            ticks.linewidth = 1
        )
    ) +
    labs(
        title = "Retirement pensioners as a share of 16+ population, 2021",
        caption =  "@pablogguz_ | The map shows the ratio of total employment to the number of 
        retirement pensioners across Spanish municipalities in 2021. Missing values have been
        imputed using province-level averages. Source: Spanish Statistical Office and author's calculations.",
        subtitle = ""
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Open Sans"),
        plot.title = element_text(
            size = 20, 
            face = "bold",
            margin = margin(b = 10)
        ),
        plot.subtitle = element_text(
            size = 14,
            margin = margin(b = 20)
        ),
        legend.position = c(0.2, 0.5),
        legend.title = element_text(size = 10, margin = margin(b = 10), hjust = 0),
        legend.text = element_text(size = 10),
        plot.caption = element_textbox_simple(
            size = 8,
            color = "grey40",
            margin = margin(t = 20),
            hjust = 0,
            halign = 0,
            lineheight = 1.2
        ),
        plot.margin = margin(20, 20, 20, 20)
    )

# Save with Twitter-optimized dimensions (1200 x 675 pixels)
ggsave(
    "census.png",
    main_map,
    width = 11,
    height = 6.75,
    dpi = 300,
    bg = "white"
)
