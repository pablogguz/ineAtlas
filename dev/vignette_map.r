# usethis::use_vignette("mapping-with-ineatlas")

# # Add dependencies to DESCRIPTION
# usethis::use_package("mapSpain", "Suggests")
# usethis::use_package("ggplot2", "Suggests")
# usethis::use_package("dplyr", "Suggests")
# usethis::use_package("scales", "Suggests")

library(ineAtlas)
library(mapSpain)
library(tidyverse)
library(scales)
library(extrafont)
library(ggtext)

# Get municipality level income data
mun_data <- get_atlas(
    category = "income",
    level = "municipality"
) %>%
    filter(year == 2022)

# Preview the data
head(mun_data)

# Get municipality geometries
mun_map <- esp_get_munic_siane() %>%
    # Join with our income data
    left_join(
        mun_data,
        by = c("LAU_CODE" = "mun_code")
    )

# Preview the joined data
glimpse(mun_map)

# Create the map
ggplot(mun_map) +
    geom_sf(
        aes(fill = cut(net_income_pc,
            breaks = c(-Inf, 8000, 10000, 12000, 14000, 16000, Inf),
            labels = c("<8k", "8-10k", "10-12k", "12-14k", "14-16k", ">16k")
        )),
        color = NA
    ) + 
    scale_fill_manual(
        name = "Net income per \ncapita, 2022 (€)",
        values = c("#67001F", "#B2182B", "#D6604D", "#4393C3", "#2166AC", "#053061"),
        na.value = "grey80"
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Open Sans", size = 16),
        legend.position = c(0.2, 0.5)
    )

mun_data %>%
  arrange(desc(net_income_pc)) %>%
  select(mun_name, net_income_pc) %>%
  head(10) %>%
  mutate(
    net_income_pc = round(net_income_pc, 2)
  )

mun_data %>%
  arrange(net_income_pc) %>%
  select(mun_name, net_income_pc) %>%
  head(10) %>%
  mutate(
    net_income_pc = round(net_income_pc, 2)
  )
