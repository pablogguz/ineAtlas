
library(extrafont)
library(ggplot2)
library(tidyverse)
library(data.table)
library(scales)
library(ggtext)

username <- Sys.getenv("USERNAME")
root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")

loadfonts(device = "win", quiet = TRUE)

atlas <- merge(
    setDT(ineAtlas::get_atlas("income", "municipality")),
    setDT(ineAtlas::get_atlas("demographics", "municipality"))
) %>%
    filter(year == 2022)

elections_raw <- readxl::read_xlsx(
    paste0(root, "/02_202307_1.xlsx"),
    range = "A6:BT8137"
) 

elections_proc <- elections_raw %>%
    mutate(
        mun_code = str_pad(as.character(`Código de Municipio`), width = 3, pad = "0"),
        prov_code = str_pad(as.character(`Código de Provincia`), width = 2, pad = "0")
    ) %>%
    mutate(
        mun_code = paste0(prov_code, mun_code)
    ) %>%
    select(
        -prov_code
    ) %>%
    mutate(
        pp = `PP`,
        psoe = `PSOE`,
        total_votes = `Total votantes`
    ) %>%
    mutate(
        share_pp = pp / total_votes,
        share_psoe = psoe / total_votes,
    ) %>%
    select(
        share_pp, share_psoe, mun_code, total_votes
    )

# Merge
plot_data <- elections_proc %>%
    left_join(
        atlas,
        by = "mun_code"
    ) %>%
    mutate(income_percentile = percent_rank(net_income_pc)) %>%
    select(income_percentile, share_pp, share_psoe, total_votes) %>%
    # Reshape to long format for easier plotting
    pivot_longer(
        cols = c(share_pp, share_psoe),
        names_to = "party",
        values_to = "vote_share"
    ) %>%
    drop_na()

# Create plot
final_plot <- ggplot() +
    geom_point(data = plot_data,
               aes(x = income_percentile * 100, 
                   y = vote_share * 100,
                   color = party),
               size = plot_data$total_votes/20000,
               alpha = 0.2) +
    geom_smooth(data = plot_data,
                aes(x = income_percentile * 100,
                    y = vote_share * 100,
                    color = party),
                method = "loess",
                se = FALSE,
                linewidth = 1.5) +
    scale_color_manual(
        values = c("share_pp" = "#0066CC", "share_psoe" = "#E31C1C"),
        labels = c("share_pp" = "PP", "share_psoe" = "PSOE"),
        name = NULL
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
        title = "Voting patterns and income at the census tract-level (Madrid, 2021)",
        x = "Equivalised net income percentile",
        y = "Vote share",
        caption = "@pablogguz_ | The chart shows the percent of total votes by party within each census tract in the Madrid 2021 regional election.
        Source: Regional Government of Madrid, Spanish Statistical Office and author's calculations."
    ) +
    theme_minimal() +
    theme(
        text = element_text(family = "Open Sans", size = 20),
        plot.title = element_text(size = 20, margin = margin(b = 20)),
        plot.caption = element_textbox_simple(
            size = 12, 
            color = "grey40", 
            margin = margin(t = 20),
            hjust = 0  # This left-justifies the caption
        ),
        legend.position = "top",        
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    )

ggsave(
    "plot.png",
    final_plot,
    width = 11,
    height = 6.75,
    dpi = 300,
    bg = "white"
)
