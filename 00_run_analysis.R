#!/usr/bin/env Rscript

# =============================================================================
# Create Figures for:
# Economic Polarization in the European Union:
# Development Models in the Race for the Best Location
# =============================================================================
# This script reproduces all figures from the paper. It first restores the
# required package environment and then creates all figures in PDF, EPS,
# and EMF format.
# =============================================================================

# Restore environment from renv.lock
renv::restore()

# Create Figures
# 1. Stylized Facts (Fig 1 & 7)
source("R/stylized_facts/00_fig1_fig7.R")

# 2. Group Characteristics (Fig 2)
source("R/stylized_facts/02_fig2.R")

# 3. Method Comparison (Fig 3)
source("R/clustering/01b_compare_method_fig3.R")

# 4. Preferred Clustering (Fig 4)
source("R/clustering/02b_preferred_clustering_fig4.R")

# 5. Rolling Window Analysis (Fig 5)
source("R/clustering/03_rolling_clustering_fig5.R")

# 6. CCE Analysis (Fig 6)
source("R/clustering/04_cce_cluster_fig6.R")
