
# Calculate French median from original grid data
french_median <- france_map %>%
    as.data.frame() %>%
    select(-geom) %>%
    mutate(net_income_equiv = ind_snv / ind) %>%
    summarise(
        french_median = Hmisc::wtd.quantile((ind_snv / ind)/0.8, weights = ind, probs = 0.50)
    ) %>%
    pull(french_median)

spanish_median <- tract %>%
    mutate(
        income_equiv_final = income_equiv_final / 0.68
    ) %>%
    summarise(
        spanish_median = Hmisc::wtd.quantile(income_equiv_final, weights = population, probs = 0.50)
    ) %>%
    pull(spanish_median)



paris_income <- france_map %>%
    as.data.frame() %>%
    select(-geom) %>%
    filter(substr(lcog_geo, 1, 3) == "751") %>% # Paris arrondissements
    mutate(net_income_equiv = ind_snv / ind) %>%
    summarise(
        #paris_income = Hmisc::wtd.quantile((ind_snv / ind)/0.8, weights = ind, probs = 0.50)
        paris_income = weighted.mean((ind_snv / ind)/0.8, w = ind)
    ) %>%
    pull(paris_income)

# Calculate French median's position in Spanish distribution
tract_all <- merge(
    setDT(ineAtlas::get_atlas("income", "tract")),
    setDT(ineAtlas::get_atlas("demographics", "tract"))
)

growth_factor <- tract_all %>%
  filter(!is.na(net_income_equiv)) %>%
  select(year, mun_code, net_income_equiv) %>%
  pivot_wider(names_from = year, 
              values_from = net_income_equiv,
              names_prefix = "income_") %>%
  filter(!is.na(income_2019) & !is.na(income_2020)) %>%
  summarise(
    avg_growth = mean(income_2020/income_2019, na.rm = TRUE)
  ) %>%
  pull(avg_growth)

tract_2019 <- tract_all %>%
  # First get 2019 data
  filter(year == 2019) %>%
  mutate(
    ratio = mean(net_income_equiv / net_income_pc, na.rm = TRUE)
  ) %>%
  mutate(
    income_equiv_final = case_when(
      !is.na(net_income_equiv) ~ net_income_equiv,
      TRUE ~ net_income_pc * ratio
    )
  ) 

# Get data for 2020
tract_2020 <- tract_all %>%
  filter(year == 2020) %>%
  mutate(
    ratio = mean(net_income_equiv / net_income_pc, na.rm = TRUE)
  ) %>%
  mutate(
    income_equiv_final = case_when(
      !is.na(net_income_equiv) ~ net_income_equiv,
      TRUE ~ net_income_pc * ratio
    )
  ) %>%
  mutate(
    income_equiv_final_2020 = income_equiv_final/as.numeric(growth_factor)
  ) %>%
  select(income_equiv_final_2020, tract_code)

# Combine
tract <- tract_2019 %>% 
    left_join(tract_2020) %>% 
    mutate(
        income_equiv_final = coalesce(income_equiv_final, income_equiv_final_2020)
    )

pct_below_french <- tract %>%
    mutate(
        income_equiv_final = income_equiv_final / 0.68
    ) %>%
    mutate(
        below_france = income_equiv_final < paris_income
    ) %>%
    summarise(
        total_pop = sum(population, na.rm = TRUE),
        pop_below = sum(population[below_france], na.rm = TRUE),
        pct_below = pop_below / total_pop * 100
    ) %>%
    pull(pct_below)

print(paste("Percentage of Spanish population living in municipalities with income below French median:", 
            round(pct_below_french, 1), "%"))

# Calculate the opposite 
madrid_income <- atlas %>%
    filter(year == 2019) %>%
    filter(mun_code == "28079") %>%  # Madrid municipality code
    pull(income_equiv_final) %>%
    {. / 0.68}  # Convert to PPP

# Calculate percentage of French population below Madrid's median
french_below_madrid <- france_map %>%
    as.data.frame() %>%
    select(-geom) %>%
    mutate(
        income_per_person = (ind_snv / ind)/0.8,  # Convert to PPP
        below_madrid = income_per_person < madrid_income,
        weighted_below = ifelse(below_madrid, ind, 0)
    ) %>%
    summarise(
        total_pop = sum(ind, na.rm = TRUE),
        pop_below = sum(weighted_below, na.rm = TRUE),
        pct_below = pop_below / total_pop * 100
    )

print(paste0("Madrid's median income (PPP): ", round(madrid_income, 0)))
print(paste0("Percentage of French population living in areas with income below Madrid's median: ", 
            round(french_below_madrid$pct_below, 1), "%"))
