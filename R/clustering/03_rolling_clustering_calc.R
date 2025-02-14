#------------------------------------------------------------------------------#
# Setup -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(cluster)
library(factoextra)

# Load data and helper functions
load(here("data/processed/full_data.Rdata"))
source(here("R/clustering/utils/panel_estimation.R"))
source(here("R/clustering/utils/standardized_distances.R"))
source(here("R/clustering/utils/cluster_analysis.R"))

# Data preparation
full_data <- full_data %>%
  filter(!(variable_name == "current_account" & country == "Romania"))

#------------------------------------------------------------------------------#
# Configuration -------------------------------------------------------------
#------------------------------------------------------------------------------#

# Variable definition
var_names <- c(
  "adj_wage_share",
  "unemployment",
  "current_account",
  "public_debt_to_gdp",
  "va_finance",
  "trade_exp_GDP",
  "abs_fdi_percGDP",
  "bond_yield",
  "debt_total_corp",
  "gdp_real_pc_ppp"
)

# Perform Rolling Window Clustering
results <- perform_rolling_window_clustering(
  data = full_data,
  start_year = 1999,
  end_year = 2023,
  window_size = 10,
  step_size = 5,
  var_names = var_names,
  se_type = "clustered",
  cce_type = "none",
  time_fe = TRUE,
  coef_select = "fe",
  n_clusters = 5,
  cluster_method = "ward"
)

#------------------------------------------------------------------------------#
# Save results ------------------------------------------------------------
#------------------------------------------------------------------------------#

save(results, file = here("data/processed/rolling_clusters.Rdata"))
