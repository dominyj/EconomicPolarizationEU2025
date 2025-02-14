rm(list = ls())

# Packages
library(here)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(plotly)
library(gridExtra)


# Load data
load(here("data/final/macro_data.Rdata"))

start_year <- 1995

gdp_growth <- macro_data_tidy %>%
  filter(variable_name == "gdp_real_pc_ppp") %>%
  group_by(country_iso3c) %>%
  arrange(country_iso3c, year) %>%
  mutate(variable_name = "gdp_growth",
         value = (value/dplyr::lag(value)-1),
         indicator = "GDP Growth of real GDP pc in PPP") %>%
  filter(!is.na(value))

eu_gdp <- macro_data_tidy %>%
  filter(variable_name %in% c("gdp_real_pc_ppp", "population"),
         year >= start_year) %>%
  pivot_wider(id_cols = c(country_iso3c, year),
              names_from = variable_name,
              values_from = value) %>%
  group_by(year) %>%
  summarize(
    eu_gdp_avg = weighted.mean(gdp_real_pc_ppp, w = population))

gdp_deviation <- macro_data_tidy %>%
  filter(variable_name %in% c("gdp_real_pc_ppp"),
         year >= start_year) %>%
  left_join(eu_gdp, by = "year") %>%
  mutate(variable_name = "gdp_pc_eu_deviation",
         value = value - eu_gdp_avg,
         indicator = "GDP Deviation from EU Average") %>%
  select(-eu_gdp_avg)

finance_data <- macro_data_tidy %>%
  filter(variable_name %in% c("fdi_percGDP")) %>%
  pivot_wider(
    id_cols = c(country, country_iso3c, year),
    names_from = "variable_name",
    values_from = "value"
  ) %>%
  mutate(
    abs_fdi_percGDP = abs(fdi_percGDP)
  ) %>%
  group_by(country, country_iso3c) %>%
  filter(year >= start_year) %>%
  mutate(fdi_volatility = sd(fdi_percGDP, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-c(fdi_percGDP)) %>%
  pivot_longer(
    cols = c(abs_fdi_percGDP, fdi_volatility),
    names_to = "variable_name",
    values_to = "value"
  )

debt_total_corp <- macro_data_tidy %>%
  filter(variable_name %in% c("debt_corp_f_percGDP",
                              "debt_corp_nf_percGDP")) %>%
  pivot_wider(
    id_cols = c(country, country_iso3c, year),
    names_from = "variable_name",
    values_from = "value"
  ) %>%
  mutate(
    value = debt_corp_f_percGDP + debt_corp_nf_percGDP,
    variable_name = "debt_total_corp",
    indicator = "Total debt of financial and non-financial corporations in % of GDP"
  ) %>%
  select(-c("debt_corp_f_percGDP",
            "debt_corp_nf_percGDP"))


sfb_percGDP <- macro_data_tidy %>%
  filter(variable_name %in% c("sfb_gdp", "sfb_household", "sfb_corp", "sfb_gov", "sfb_foreign")) %>%
  pivot_wider(
    id_cols = c(country, country_iso3c, year),
    names_from = "variable_name",
    values_from = "value"
  ) %>%
  pivot_longer(cols = c(sfb_household, sfb_corp, sfb_gov, sfb_foreign),
               names_to = "variable_name",
               values_to = "value") %>%
  mutate(value = value/sfb_gdp*100,
         variable_name = paste0(variable_name, "_percGDP"))

foreign_ownership_percGDP <- macro_data_tidy %>%
  filter(variable_name %in% c("foreign_ownership", "gdp_oecd")) %>%
  pivot_wider(
    id_cols = c(country, country_iso3c, year),
    names_from = "variable_name",
    values_from = "value"
  ) %>%
  mutate(value = foreign_ownership/gdp_oecd * 100,
         variable_name = "foreign_ownership_percGDP")


# Combine data
full_data <- rbind(macro_data_tidy, gdp_growth, gdp_deviation, finance_data, debt_total_corp, sfb_percGDP, foreign_ownership_percGDP, fill = TRUE) %>%
  filter(year >= start_year)

sort(unique(full_data$variable_name))


# save data
dir_path <- here("data/processed/")
dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
save(full_data, file = paste0(dir_path, "full_data.Rdata"))
