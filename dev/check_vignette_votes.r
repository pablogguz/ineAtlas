library(tidyverse)
library(ineAtlas)
library(jsonlite)
library(extrafont)
library(ggtext)

# Load elections data
elections_raw <- data.table::fread(
    "https://datos.comunidad.madrid/catalogo/dataset/08aac4de-ca28-4f9c-b45d-ef8457c4b5d2/resource/5e8cf4ad-b9f4-4ffd-a026-c27433e7815f/download/datos_electorales_elecciones_autonomicas_comunidad_de_madrid_2021.csv",
    sep = ";",
    encoding = "Latin-1"
) %>% as_tibble()

# Get income data from ineAtlas
income_data <- get_atlas("income", "tract") %>%
    # Filter for Madrid region
    filter(substr(mun_code, 1, 2) == "28") %>%
    filter(year == 2021)

# Process election data - assuming we need to aggregate to census tract level
elections_proc <- elections_raw %>%
    select(PP, `P.S.O.E.`, distrito, seccion, cod_muni, votos_electores) %>%
    rename(
        pp = PP,
        psoe = `P.S.O.E.`,
        total_votes = votos_electores
    ) %>%
    mutate(
        cod_muni = str_pad(as.character(cod_muni), width = 3, pad = "0"),
        distrito = str_pad(as.character(distrito), width = 2, pad = "0"),
        seccion = str_pad(as.character(seccion), width = 3, pad = "0"),
        tract_code = paste0("28", cod_muni, distrito, seccion),
        share_pp = pp / total_votes,
        share_psoe = psoe / total_votes,
        tract_code = as.character(tract_code)
    )

# Merge
plot_data <- elections_proc %>%
    left_join(
        income_data,
        by = "tract_code"
    ) %>%
    mutate(income_percentile = percent_rank(net_income_pc)) %>%
    select(income_percentile, share_pp, share_psoe, total_votes) %>%
    # Reshape to long format for easier plotting
    pivot_longer(
        cols = c(share_pp, share_psoe),
        names_to = "party",
        values_to = "vote_share"
    )

# Create plot
final_plot <- ggplot() +
    geom_point(data = plot_data,
               aes(x = income_percentile * 100, 
                   y = vote_share * 100,
                   color = party),
               size = plot_data$total_votes/200,
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

# Save with Twitter-optimized dimensions (1200 x 675 pixels)
ggsave(
    "twitter_votes.png",
    final_plot,
    width = 11,
    height = 6.75,
    dpi = 300,
    bg = "white"
)


#usethis::use_vignette("ineAtlas-income-voting")
