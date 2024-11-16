
library(extrafont)
library(ggplot2)
library(tidyverse)
library(data.table)
library(scales)
library(ggtext)
library(WDI)

loadfonts(device = "win", quiet = TRUE)

wdi <- WDI(
  country = "ES",
  indicator = c(
    "PA.NUS.PRVT.PP"
    ),
  start = 2022,
  end = 2022,
) 

atlas <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
) %>%
    filter(year == 2022) 

percentiles <- atlas[, .(
    p10 = Hmisc::wtd.quantile(net_income_equiv, weights = population, probs = 0.10),
    #p25 = Hmisc::wtd.quantile(net_income_equiv, weights = population, probs = 0.25),
    p50 = Hmisc::wtd.quantile(net_income_equiv, weights = population, probs = 0.50),
    #p75 = Hmisc::wtd.quantile(net_income_equiv, weights = population, probs = 0.75),
    p90 = Hmisc::wtd.quantile(net_income_equiv, weights = population, probs = 0.90),
    p99 = Hmisc::wtd.quantile(net_income_equiv, weights = population, probs = 0.99)
)]

mun <- merge(
    setDT(ineAtlas::get_atlas("income", "municipality")),
    setDT(ineAtlas::get_atlas("demographics", "municipality"))
) %>%
filter(population > 20000)

# Find municipalities closest to each percentile
mun[year == 2022, .(
    municipality = mun_name,
    income = net_income_equiv,
    population = population,
    province = prov_name
)][order(abs(income - percentiles$p10))][1:3] # For p10

mun[year == 2022, .(
    municipality = mun_name,
    income = net_income_equiv,
    population = population,
    province = prov_name
)][order(abs(income - percentiles$p50))][1:3] # For p50

mun[year == 2022, .(
    municipality = mun_name,
    income = net_income_equiv,
    population = population,
    province = prov_name
)][order(abs(income - percentiles$p90))][1:3] # For p90

mun[year == 2022, .(
    municipality = mun_name,
    income = net_income_equiv,
    population = population,
    province = prov_name
)][order(abs(income - percentiles$p99))][1:3] # For p99

# Create a more beautiful gradient fill
plot <- ggplot(atlas, aes(x = net_income_equiv)) +
    # Main density with gradient fill
    geom_density(aes(y = ..count.. / sum(..count..)),
        fill = "#2171b5",  # Darker blue base
        alpha = 0.7,  # Slightly more opaque
        color = NA
    ) +
    # Add a subtle overlay for depth
    geom_density(aes(y = ..count.. / sum(..count..)),
        fill = "#4292c6",  # Lighter blue overlay
        alpha = 0.3,
        color = NA
    ) +
    geom_vline(
        xintercept = unlist(percentiles),
        linetype = "dashed",
        color = "#636363",  # Darker gray for better contrast
        alpha = 0.5
    ) +
    annotate("text",
        x = unlist(percentiles)+900,
        y = 0,
        label = c(
            "p10",
            "p50",
            "p90",
            "p99"
        ),
        angle = 90,
        hjust = -0.2,
        size = 4,
        color = "#636363",
        family = "Open Sans"
    ) +
    # Municipality labels with enhanced styling
    annotate("text",
        x = c(9000, 20500, 30000, 49200),
        y = c(0.008, 0.0097, 0.008, 0.008),
        label = c(
            "Mazarrón\n(Murcia)",
            "Sagunto\n(Valencia)",
            "Oleiros\n(A Coruña)",
            "Pozuelo de Alarcón\n(Madrid)"
        ),
        size = 4,
        angle = 0,
        hjust = 0,
        color = "#4a4a4a",  # Darker text for better readability
        family = "Roboto",
        fontface = "bold"
    ) +
    # Income examples with enhanced styling
    annotate("text",
        x = 30000,
        y = c(0.006, 0.0055, 0.005),
        label = c(
            "Single: €2,417/month",
            "Couple: €3,625/month",
            "Family (2 kids): €5,075/month"
        ),
        size = 3.5,
        angle = 0,
        hjust = 0,
        color = "#4a4a4a",
        family = "Open Sans"
    ) +
    theme_minimal() +
    labs(
        x = "Net equivalised income",
        y = "Fraction of observations",
        title = "Equivalised disposable income distribution in Spain (2022)",
        subtitle = "A couple without children earning around €1.8k/month each is at the 90th percentile",
        caption = "@pablogguz_ | The chart shows the distribution of net equivalised income across census tracts in Spain for 2022. Source: Spanish Statistical Office and author's calculations."
    ) +
    scale_x_continuous(
        labels = scales::dollar_format(suffix = "", prefix = "€"),
        limits = c(3000, 61000)
    ) +
    theme(
        text = element_text(family = "Roboto", size = 14),
        panel.grid = element_blank(),
        axis.text = element_text(color = "#4a4a4a"),
        axis.title = element_text(color = "#2b2b2b", size = 12),
        plot.title = element_text(
            size = 18, 
            face = "bold", 
            margin = margin(b = 10),
            color = "#2b2b2b"
        ),
        plot.subtitle = element_text(
            size = 14, 
            margin = margin(b = 20),
            color = "#4a4a4a"
        ),
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "white", color = NA),
        plot.caption = element_textbox_simple(
            size = 10, 
            color = "grey40", 
            margin = margin(t = 20),
            hjust = 0
        )
    )

ggsave(
    "plot.png",
    plot,
    width = 11,
    height = 6.75,
    dpi = 300,
    bg = "white"
)