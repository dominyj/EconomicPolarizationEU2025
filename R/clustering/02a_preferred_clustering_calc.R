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
source(here("R/clustering/utils/visualization.R"))

# Data preparation
full_data <- full_data %>%
  filter(!(variable_name == "current_account" & country == "Romania"))

#------------------------------------------------------------------------------#
# Configuration -------------------------------------------------------------
#------------------------------------------------------------------------------#

config <- list(
  # Panel Schätzung
  panel = list(
    se_type = "clustered",
    cce_type = "none",
    time_fe = TRUE,
    group_fe = TRUE,
    coef_select = c("fe"),
    has_controls = FALSE
  ),
  # Clustering
  clustering = list(
    dist_method = "se_weighted",
    cluster_method = "ward",
    n_clusters = 5
  ),
  # Zeitraum
  time = list(
    start_year = 1999,
    end_year = 2023
  )
)

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

#------------------------------------------------------------------------------#
# Calculation --------------------------------------------------------------
#------------------------------------------------------------------------------#

# Calculate clustering results
results <- perform_cluster_analysis(full_data, var_names, config)

# Save results
save(results, file = here("data/processed/preferred_clustering.Rdata"))
