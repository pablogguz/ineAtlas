
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
        pct_higher_ed_completed = pct_higher_ed_completed * 100,
        pct_foreign_born = pct_foreign_born * 100,
        pct_single = pct_single * 100,
        #employment_rate = employment_rate * 100,
        pct_retirement_pension = pct_retirement_pension * 100,
        unemployment_rate = unemployment_rate * 100
    ) %>%
    # Impute missing values with avg. at the province level
    group_by(prov_code) %>%
    mutate(
        across(
            c(pct_higher_ed_completed, pct_foreign_born, pct_single, pct_under16,
                pct_16to64,
              employment_rate, pct_retirement_pension, unemployment_rate),
            ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
        )
    ) %>%
    ungroup() %>% 
    mutate(
        # recalculate emp. rate as % of pop. 16-64
        pop_over_16 = total_pop*(1-pct_under16),
        total_employed = employment_rate * pop_over_16, 
        total_pop_16_64 = total_pop * pct_16to64,
        employment_rate_new = 100*total_employed/total_pop_16_64
    ) %>%
    mutate(
        employment_rate = case_when(
            employment_rate_new < 50 ~ 50,
            employment_rate_new >= 80 ~ 80,
            TRUE ~ employment_rate_new
        )
    )

# Calculate avg. employment rate 
mun_data %>%
    summarise(
        avg_emp_rate = weighted.mean(employment_rate_new, w = total_pop, na.rm = TRUE)
    )

# Calculate provinces with highest emp. rates 
mun_data %>%
    group_by(prov_name) %>%
    summarise(
        avg_emp_rate = weighted.mean(employment_rate_new, w = total_pop, na.rm = TRUE)
    ) %>%
    arrange(desc(avg_emp_rate)) %>% 
    head(5)
    
# Get municipality geometries
mun_map <- esp_get_munic_siane() %>%
    left_join(
        mun_data,
        by = c("LAU_CODE" = "mun_code")
    )

# # Calculate no. of muns with emp. rates below 30%
# get_census(
#         level = "municipality"
#     ) %>%
#     filter(!is.na(employment_rate)) %>% 
#     nrow()

# get_census(
#         level = "municipality"
#     ) %>%
#     filter(!is.na(employment_rate)) %>% 
#     filter(employment_rate < .5) %>%
#     nrow()

mun_data %>% 
    filter(employment_rate < 60) %>% 
    nrow()

# # Create the main map
# main_map <- ggplot(mun_map) +
#     geom_sf(
#         aes(fill = cut(pct_retirement_pension,
#             breaks = c(-Inf, 20, 30, 40, 50, Inf),
#             labels = c("<20%", "20-30%", "30-40%", "40-50%", ">50%")
#         )),
#         color = NA
#     ) + 
#     scale_fill_manual(
#         name = "Per cent with higher \neducation completed (€)",
#         values = c("#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC", "#053061"),
#         na.value = "grey80"
#     ) +
#     labs(
#         title = "Insert",
#         caption =  "@pablogguz_ | The map shows net income per capita at the municipality level in 2022. Source: Spanish Statistical Office and author's calculations.",
#         subtitle = "Net income per capita across Spanish municipalities, 2022"
#     ) +
#     theme_void() +
#     theme(
#         text = element_text(family = "Open Sans"),
#         plot.title = element_text(
#             size = 24, 
#             face = "bold",
#             margin = margin(b = 10)
#         ),
#         plot.subtitle = element_text(
#             size = 16,
#             margin = margin(b = 20)
#         ),
#         legend.position = c(0.2, 0.5),
#         legend.title = element_text(size = 10),
#         legend.text = element_text(size = 10),
#         plot.caption = element_textbox_simple(
#             size = 8,
#             color = "grey40",
#             margin = margin(t = 20),
#             hjust = 0,
#             halign = 0,
#             lineheight = 1.2
#         ),
#         plot.margin = margin(20, 20, 20, 20)
#     )


breaks_scale <- c(50, 60, 70, 80)
breaks_labels <- c("<50%", "60%", "70%", ">80%")

# Create the main map
main_map <- ggplot(mun_map) +
    geom_sf(
        aes(fill = employment_rate),
        color = NA
    ) + 
    scale_fill_gradientn(
        name = "Employment \nrate (%)",
        colors = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#92C5DE", "#4393C3", "#2166AC", "#053061"),
        na.value = "#e4e4e4",
        breaks = breaks_scale,
        labels = breaks_labels,
        limits = c(49.5, 80.5),
        guide = guide_colorbar(
            title.position = "top",
            title.hjust = 0,
            barheight = 10,  # Make bar taller for vertical orientation
            barwidth = 1,    # Make bar thinner for vertical orientation
            ticks.linewidth = 1
        )
    ) +
    labs(
        title = "Employment rates across spanish municipalities, 2021",
        caption =  "@pablogguz_ | The map shows the employment rate across Spanish municipalities in 2021. The employment rate is calculated
        as a proportion of working-age population (16-64). Missing values have been
        imputed using province-level average employment rates. Source: Spanish Statistical Office and author's calculations.",
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
