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
mun_data <- get_atlas(
    category = "income",
    level = "municipality"
) %>%
    filter(year == 2022)

# Get municipality geometries
mun_map <- esp_get_munic_siane() %>%
    left_join(
        mun_data,
        by = c("LAU_CODE" = "mun_code")
    )

# Create the main map
main_map <- ggplot(mun_map) +
    geom_sf(
        aes(fill = cut(net_income_pc,
            breaks = c(-Inf, 8000, 10000, 12000, 14000, 16000, Inf),
            labels = c("<8k", "8-10k", "10-12k", "12-14k", "14-16k", ">16k")
        )),
        color = NA
    ) + 
    scale_fill_manual(
        name = "Net income per\ncapita (€)",
        values = c("#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC", "#053061"),
        na.value = "grey80"
    ) +
    labs(
        title = "Household Income Distribution Atlas",
        caption =  "@pablogguz_ | The map shows net income per capita at the municipality level in 2022. Source: Spanish Statistical Office and author's calculations.",
        subtitle = "Net income per capita across Spanish municipalities, 2022"
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Open Sans"),
        plot.title = element_text(
            size = 24, 
            face = "bold",
            margin = margin(b = 10)
        ),
        plot.subtitle = element_text(
            size = 16,
            margin = margin(b = 20)
        ),
        legend.position = c(0.2, 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.caption = element_textbox_simple(
            size = 12,
            color = "grey40",
            margin = margin(t = 20),
            hjust = 0,
            halign = 0,
            lineheight = 1.2
        ),
        plot.margin = margin(20, 20, 20, 20)
    )

# Read and convert hex logo to raster
hex_logo <- image_read("man/figures/logo.png")
hex_raster <- as.raster(hex_logo)
hex_grob <- rasterGrob(hex_raster)

# Create the final composition
final_plot <- ggdraw() +
    draw_plot(main_map) +
    # Add hex logo to top-right corner
    draw_plot(hex_grob, x = 0.7, y = 0.7, width = 0.25, height = 0.25)

# Save with Twitter-optimized dimensions (1200 x 675 pixels)
ggsave(
    "twitter_preview.png",
    final_plot,
    width = 11,
    height = 6.75,
    dpi = 300,
    bg = "white"
)
