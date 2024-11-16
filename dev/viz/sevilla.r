
library(tidyverse)
library(ineAtlas)

census_data <- get_atlas(
    category = "income",
    level = "tract"
) %>%
    filter(year == 2022) %>%
    mutate(rank = 100*percent_rank(net_income_pc))

geometries <- get_tract_geom(year = 2022)

plot_data <- geometries %>% =
    filter(municipality == "Sevilla") %>%
    left_join(census_data)

plot <- ggplot(plot_data) +
    geom_sf(
        aes(fill = cut(rank,
            breaks = c(-Inf, 20, 40, 60, 80, 90, 100),
            labels = c("0-20th", "20-40th", "40-60th", "60-80th", "80-90th", "90-100th")
        )),
        color = NA
    ) +
    labs(
        title = "Income distribution in Sevilla (2022)",
        subtitle = "Census tract income per capita percentiles",
        caption = "@pablogguz_ | The map shows net income per capita percentiles (calculated based on the national-level distribution)
        for each census tract in Sevilla for 2022. Source: Spanish Statistical Office and author's calculations."
    ) +
    scale_fill_manual(
        name = "Net income per \ncapita percentile",
        values = c("#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC", "#0e345a"),
        na.value = "grey80",
        guide = guide_legend(
            #label.position = "bottom",
            title.position = "top",
            title.hjust = 0.5
        )
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Open Sans", size = 14),
        plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, margin = margin(b = 20)),
        legend.position = c(0.8, 0.2),
        plot.margin = margin(20, 20, 20, 20),
        legend.direction = "vertical",
        legend.box = "vertical",
        plot.caption = element_textbox_simple(
            size = 10, 
            color = "grey40", 
            margin = margin(t = 20),
            hjust = 0  # This left-justifies the caption
        ),
    )

plot

ggsave(
    "plot.png",
    plot,
    width = 6,
    height = 6.75,
    dpi = 300,
    bg = "white"
)


