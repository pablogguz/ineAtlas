library(tidyverse)
library(ineAtlas)
library(ggbeeswarm)
library(scales)
library(ggtext)

# Get municipality level income data
mun_data <- merge(
  setDT(ineAtlas::get_atlas("income", "municipality")),
  setDT(ineAtlas::get_atlas("demographics", "municipality"))
) %>% 
  filter(year == 2022) 
   
# Calculate weighted averages for different scenarios
scenarios <- data.frame(
  scenario = factor(c(
    "Excluding Madrid, Barcelona\n& Basque Country",
    "Excluding Madrid & Barcelona",
    "All municipalities"
  ), levels = c(
    "Excluding Madrid, Barcelona\n& Basque Country",
    "Excluding Madrid & Barcelona",
    "All municipalities"
  )),
  avg_income = c(
    # Without Madrid, Barcelona & Basque Country
    weighted.mean(mun_data[!substr(mun_code, 1, 2) %in% c("28", "08", "01", "20", "48")]$net_income_equiv, 
                 w = mun_data[!substr(mun_code, 1, 2) %in% c("28", "08", "01", "20", "48")]$population, 
                 na.rm = TRUE),
    # Without Madrid & Barcelona provinces
    weighted.mean(mun_data[!substr(mun_code, 1, 2) %in% c("28", "08")]$net_income_equiv, 
                 w = mun_data[!substr(mun_code, 1, 2) %in% c("28", "08")]$population, 
                 na.rm = TRUE),
    # All municipalities
    weighted.mean(mun_data$net_income_equiv, w = mun_data$population, na.rm = TRUE)
  )
)

# Rest of the plotting code remains the same
plot <- ggplot(scenarios, aes(x = avg_income, y = scenario)) +
  geom_segment(
    aes(x = mean(scenarios$avg_income), xend = avg_income, 
        y = scenario, yend = scenario),
    color = "gray80",
    linewidth = 1.5
  ) +
  geom_point(
    size = 16,
    color = "#2166AC",
    alpha = 0.8
  ) +
  geom_text(
    aes(label = (round(avg_income))),
    color = "white",
    size = 4
  ) +
  geom_vline(
    xintercept = mean(scenarios$avg_income),
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.5
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Open Sans"),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    plot.caption = element_textbox_simple(
      size = 10, 
      color = "grey40",
      margin = margin(t = 20),
      lineheight = 1.2
    ),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "How average income changes when excluding highest-income regions",
    subtitle = "Population-weighted average net income per capita (2022)",
    caption = "@pablogguz_ | Source: Spanish Statistical Office (INE) and author's calculations using ineAtlas package."
  )

ggsave(
    "plot.png",
    plot,
    width = 6,
    height = 6.75,
    dpi = 300,
    bg = "white"
)
