#------------------------------------------------------------------------------#
# Setup -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library(plm)

# Load data and helper functions
load(here("data/processed/full_data.Rdata"))
source(here("R/clustering/utils/panel_estimation.R"))
source(here("R/clustering/utils/standardized_distances.R"))
source(here("R/clustering/utils/cluster_analysis.R"))
source(here("R/clustering/utils/visualization.R"))

# Remove problematic observations
full_data <- full_data %>%
  filter(!(variable_name == "current_account" & country == "Romania"))

#------------------------------------------------------------------------------#
# Define configurations ------------------------------------------------------
#------------------------------------------------------------------------------#

# Configuration 1: keine Kontrollvariablen, neue Daten, alte Distanzberechnung (Cluster 3)
config1 <- list(
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
    dist_method = "standard",
    cluster_method = "ward",
    n_clusters = 5
  ),
  # Zeitraum
  time = list(
    start_year = 1999,
    end_year = 2023
  )
)

# Configuration 2: keine Kontrollvariablen, neue Daten, neue Distanzberechnung (Cluster 4)
config2 <- config1
config2$clustering$dist_method <- "se_weighted"

# Configuration 0: Original time period
config0 <- config1
config0$time$end_year <- 2016

# Variable definition
var_names <- c(
  "adj_wage_share",
  "unemployment",
  "current_account",
  "public_debt_to_gdp",
  "va_finance",
  "trade_exp_GDP",
  "gdp_growth",
  "gdp_real_pc_ppp"
)

#------------------------------------------------------------------------------#
# Calculate results ---------------------------------------------------------
#------------------------------------------------------------------------------#

# Calculate clustering for each configuration
results0 <- perform_cluster_analysis(full_data, var_names, config0)
results1 <- perform_cluster_analysis(full_data, var_names, config1)
results2 <- perform_cluster_analysis(full_data, var_names, config2)

# Save results
clustering_comparison <- list(
  original_period = results0,
  extended_standard = results1,
  extended_weighted = results2
)

save(clustering_comparison,
     file = here("data/processed/clustering_comparison.Rdata"))
