# Clear environment
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(plotly)

# Source additional setup files
source(here("R/00_country_setup.R"))
source(here("R/stylized_facts/99_stylized_facts_utils.R"))

# Import data
load(here("data/processed/full_data.Rdata"))

# Prepare data
macro_data <- full_data %>%
  left_join(country_groups %>% select(c_group, country_iso3c), by = "country_iso3c") %>%
  pivot_wider(id_cols = c(country, country_iso3c, c_group, year),
              names_from = variable_name,
              values_from = value) %>%
  filter(year >= 1999,
         year <= 2023)


# Plot Konfiguration erstellen
plot_config <- tribble(
  ~variable_name,               ~label,                                   ~show_deviation, ~show_variance, ~detailed_plots, ~stat_type,
  "current_account",            "Current Account Balance",                FALSE,          FALSE,         TRUE,           "mean",
  "current_account",            "Current Account Balance",                FALSE,          FALSE,         TRUE,           "weighted_mean",
  "unemployment",               "Unemployment Rate",                      FALSE,          FALSE,         TRUE,           "mean",
  "unemployment",               "Unemployment Rate",                      FALSE,          FALSE,         TRUE,           "weighted_mean",
  "adj_wage_share",            "Adjusted Wage Share",                    FALSE,          FALSE,         TRUE,           "mean",
  "adj_wage_share",            "Adjusted Wage Share",                    FALSE,          FALSE,         TRUE,           "weighted_mean",
  "public_debt_to_gdp",        "Public Debt (% GDP)",                   FALSE,          FALSE,         TRUE,           "mean",
  "public_debt_to_gdp",        "Public Debt (% GDP)",                   FALSE,          FALSE,         TRUE,           "weighted_mean",
  "gdp_real_pc_ppp",           "GDP per Capita (PPP)",                  TRUE,           FALSE,         TRUE,           "mean",
  "gdp_real_pc_ppp",           "GDP per Capita (PPP)",                  TRUE,           FALSE,         TRUE,           "weighted_mean",
  "industry_employment_share",  "Industry Employment Share",             FALSE,          FALSE,         TRUE,           "mean",
  "industry_employment_share",  "Industry Employment Share",             FALSE,          FALSE,         TRUE,           "weighted_mean",
  "fdi_percGDP",               "Foreign Direct Investment (in % of GDP)", FALSE,          FALSE,         TRUE,           "mean",
  "fdi_percGDP",               "Foreign Direct Investment (in % of GDP)", FALSE,          FALSE,         TRUE,           "weighted_mean",
  "fdi_current_dollar",        "Foreign Direct Investment (current US$)", FALSE,          FALSE,         TRUE,           "sum",
  "trade_exp_GDP",             "Exports (% GDP)",                       FALSE,          FALSE,         TRUE,           "mean",
  "trade_exp_GDP",             "Exports (% GDP)",                       FALSE,          FALSE,         TRUE,           "weighted_mean",
  "industry_output",           "Industrial Production",                  FALSE,          FALSE,         TRUE,           "mean",
  "industry_output",           "Industrial Production",                  FALSE,          FALSE,         TRUE,           "weighted_mean",
  "bond_yield",                "Government Bond Yield",                  FALSE,          FALSE,         TRUE,           "mean",
  "bond_yield",                "Government Bond Yield",                  FALSE,          FALSE,         TRUE,           "weighted_mean",
  "debt_corp_nf_percGDP",      "Non-Financial Corporate Debt (% GDP)",   FALSE,          FALSE,         TRUE,           "mean",
  "debt_corp_nf_percGDP",      "Non-Financial Corporate Debt (% GDP)",   FALSE,          FALSE,         TRUE,           "weighted_mean",
  "debt_corp_f_percGDP",       "Financial Corporate Debt (% GDP)",       FALSE,          FALSE,         TRUE,           "mean",
  "debt_corp_f_percGDP",       "Financial Corporate Debt (% GDP)",       FALSE,          FALSE,         TRUE,           "weighted_mean",
  "debt_gov_percGDP",          "Government Debt (% GDP)",               FALSE,          FALSE,         TRUE,           "mean",
  "debt_gov_percGDP",          "Government Debt (% GDP)",               FALSE,          FALSE,         TRUE,           "weighted_mean",
  "debt_hh_npish_percGDI",     "Household Debt (% GDI)",                FALSE,          FALSE,         TRUE,           "mean",
  "debt_hh_npish_percGDI",     "Household Debt (% GDI)",                FALSE,          FALSE,         TRUE,           "weighted_mean",
  "total_debt_percGDP",        "Total Debt (% GDP)",                    FALSE,          FALSE,         TRUE,           "mean",
  "total_debt_percGDP",        "Total Debt (% GDP)",                    FALSE,          FALSE,         TRUE,           "weighted_mean",
  "nulc",                      "Nominal Unit Labour Cost",              FALSE,          FALSE,         TRUE,           "mean",
  "nulc",                      "Nominal Unit Labour Cost",              FALSE,          FALSE,         TRUE,           "weighted_mean",
  "rulc",                      "Real Unit Labour Cost",                 FALSE,          FALSE,         TRUE,           "mean",
  "rulc",                     "Real Unit Labour Cost",                  FALSE,          FALSE,         TRUE,           "weighted_mean",
  "top50_wealth_share",    "Top 50% Wealth Share",         FALSE,          FALSE,         TRUE,           "mean",
  "top50_wealth_share",    "Top 50% Wealth Share",         FALSE,          FALSE,         TRUE,           "weighted_mean",
  "top10_wealth_share",    "Top 10% Wealth Share",         FALSE,          FALSE,         TRUE,           "mean",
  "top10_wealth_share",    "Top 10% Wealth Share",         FALSE,          FALSE,         TRUE,           "weighted_mean",
  "top1_wealth_share",     "Top 1% Wealth Share",          FALSE,          FALSE,         TRUE,           "mean",
  "top1_wealth_share",     "Top 1% Wealth Share",          FALSE,          FALSE,         TRUE,           "weighted_mean",
  "top50_income_share",    "Top 50% Income Share",         FALSE,          FALSE,         TRUE,           "mean",
  "top50_income_share",    "Top 50% Income Share",         FALSE,          FALSE,         TRUE,           "weighted_mean",
  "top10_income_share",    "Top 10% Income Share",         FALSE,          FALSE,         TRUE,           "mean",
  "top10_income_share",    "Top 10% Income Share",         FALSE,          FALSE,         TRUE,           "weighted_mean",
  "top1_income_share",     "Top 1% Income Share",          FALSE,          FALSE,         TRUE,           "mean",
  "top1_income_share",     "Top 1% Income Share",          FALSE,          FALSE,         TRUE,           "weighted_mean",
  "hs_eci",                "Complexity (ECI)",             FALSE,          FALSE,         TRUE,           "mean",
  "hs_eci",                "Complexity (ECI)",             FALSE,          FALSE,         TRUE,           "weighted_mean",
  "fdi_current_dollar",        "Foreign Direct Investment (current US$)", FALSE,          FALSE,         TRUE,           "mean",
  "fdi_current_dollar",        "Foreign Direct Investment (current US$)", FALSE,          FALSE,         TRUE,           "weighted_mean"
)

# Update only these variables
plot_config <- tribble(
  ~variable_name,               ~label,                                   ~show_deviation, ~show_variance, ~detailed_plots, ~stat_type,
  "hcpi",                       "Harmonised consumer price index (All-items)", FALSE,      FALSE,          TRUE,            "mean",
  "hcpi",                       "Harmonised consumer price index (All-items)", FALSE,      FALSE,          TRUE,            "weighted_mean",
  "hcpi_excluding",             "Harmonised consumer price index (Overall index excluding energy, food, alcohol and tobacco)", FALSE,          FALSE,         TRUE,           "mean",
  "hcpi_excluding",             "Harmonised consumer price index (Overall index excluding energy, food, alcohol and tobacco)", FALSE,          FALSE,         TRUE,           "weighted_mean"
)

# Update only these variables
plot_config <- tribble(
  ~variable_name,                    ~label,                                            ~show_deviation, ~show_variance, ~detailed_plots, ~stat_type,
  "hs_eci_distance_to_leader",       "Distance to Technology Frontier (HS-ECI)",         FALSE,          FALSE,          TRUE,           "mean",
  "hs_eci_distance_to_leader",       "Distance to Technology Frontier (HS-ECI)",         FALSE,          FALSE,          TRUE,           "weighted_mean",
  "hs_eci_distance_to_p95",          "Distance to 95th Percentile (HS-ECI)",            FALSE,          FALSE,          TRUE,           "mean",
  "hs_eci_distance_to_p95",          "Distance to 95th Percentile (HS-ECI)",            FALSE,          FALSE,          TRUE,           "weighted_mean",
  "sitc_eci_distance_to_leader",     "Distance to Technology Frontier (SITC-ECI)",       FALSE,          FALSE,          TRUE,           "mean",
  "sitc_eci_distance_to_leader",     "Distance to Technology Frontier (SITC-ECI)",       FALSE,          FALSE,          TRUE,           "weighted_mean",
  "sitc_eci_distance_to_p95",        "Distance to 95th Percentile (SITC-ECI)",          FALSE,          FALSE,          TRUE,           "mean",
  "sitc_eci_distance_to_p95",        "Distance to 95th Percentile (SITC-ECI)",          FALSE,          FALSE,          TRUE,           "weighted_mean"
)

# Update only these variables
plot_config <- tribble(
  ~variable_name,                    ~label,                                            ~show_deviation, ~show_variance, ~detailed_plots, ~stat_type,
  "foreign_ownership_percGDP",       "Foreign Ownership (in % of GDP)",         FALSE,          FALSE,          TRUE,           "mean",
  "foreign_ownership_percGDP",       "Foreign Ownership (in % of GDP)",         FALSE,          FALSE,          TRUE,           "weighted_mean"
)

# Create and save plots
for(i in 1:nrow(plot_config)) {
  plots <- generate_indicator_plot(
    data = macro_data,
    var_name = plot_config$variable_name[i],
    var_label = plot_config$label[i],
    show_deviation = plot_config$show_deviation[i],
    show_variance = plot_config$show_variance[i],
    detailed_plots = plot_config$detailed_plots[i],
    stat_type = plot_config$stat_type[i]
  )

  # Erstelle Pfad basierend auf Variable und stat_type
  var_path <- file.path(here("output/fig/exploratory/stylized_facts"),
                        plot_config$variable_name[i], plot_config$stat_type[i])

  save_plots(plots, plot_config$variable_name[i], path = var_path)
}
