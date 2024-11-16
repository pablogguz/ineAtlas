
library(tidyverse)
library(ineAtlas)

census_data <- get_atlas(
    category = "income",
    level = "tract"
) %>%
    filter(year == 2022) %>%
    #filter(prov_name == "Madrid") %>%
    mutate(rank = 100*percent_rank(net_income_pc))

geometries <- get_tract_geom(year = 2022)

aranjuez <- geometries %>% 
    filter(municipality == "Aranjuez") %>%
    left_join(census_data)

# Plot net_income_pc in census tracts 
aranjuez_inner <- aranjuez %>%
    filter(row_number()!=30)

plot <- ggplot(aranjuez_inner) +
    geom_sf(
        aes(fill = cut(rank,
            breaks = c(-Inf, 20, 40, 60, 80, 100),
            labels = c("0-20th", "20-40th", "40-60th", "60-80th", "80-100th")
        )),
        color = NA
    ) +
    geom_sf_text(
        aes(label = round(rank)),
        color = "white",
        fontface = "bold",
        size = 3,
        family = "Open Sans"
    ) +
    labs(
        title = "Income distribution in Aranjuez",
        subtitle = "Census tract income per capita percentiles"
    ) +
    scale_fill_manual(
        name = "Net income per capita percentile",
        values = c("#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC"),
        na.value = "grey80",
        guide = guide_legend(
            label.position = "bottom",
            title.position = "top",
            title.hjust = 0.5
        )
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Open Sans", size = 14),
        plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, margin = margin(b = 20)),
        legend.position = c(0.7, 0.55),
        plot.margin = margin(20, 20, 20, 20),
        legend.direction = "horizontal",
        legend.box = "vertical"
    )

plot

ggsave(
    "aranjuez.png",
    plot,
    width = 6,
    height = 6.75,
    dpi = 300,
    bg = "white"
)


