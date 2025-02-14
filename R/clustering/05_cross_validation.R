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
source(here("R/clustering/utils/visualization.R"))
source(here("R/clustering/utils/cluster_analysis.R"))
source(here("R/clustering/utils/utils.R"))


# List all variable names
sort(unique(full_data$variable_name))


full_data <- full_data %>%
  filter(!(variable_name == "current_account" & country == "Romania"))


# Konfiguration ----
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

# Variablendefinition separat ----
var_names <- c(
  "adj_wage_share",
  "unemployment",
  "current_account",
  "public_debt_to_gdp",
  "va_finance",
  "trade_exp_GDP",
  "abs_fdi_percGDP",
  # "hs_eci",
  "bond_yield",
  "debt_total_corp",
  "gdp_real_pc_ppp"
)




# Ausführung ----
results <- perform_cluster_analysis(full_data, var_names, config)
results$dendogram


selected_sizes <- c(1, (length(var_names)-1):length(var_names))

# Durchführung der Analyse für alle Kombinationen
for(i in selected_sizes) {
  var_combinations <- combn(var_names, i, simplify = FALSE)

  for(vars in var_combinations) {
    # Erstelle Dateiname mit Pfad
    filename <- create_filepath(vars, config, base_path = here("output/fig/exploratory/clustering/cross_validation"))

    # Führe Analyse durch
    results <- perform_cluster_analysis(full_data, vars, config)

    # Speichere Plot
    pdf(filename, width = 10, height = 12)
    print(results$dendogram)
    dev.off()

    # Gib Fortschritt aus
    message(sprintf("Saved plot: %s", filename))
  }
}
