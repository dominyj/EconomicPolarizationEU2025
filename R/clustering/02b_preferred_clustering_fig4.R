#------------------------------------------------------------------------------#
# Setup -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)
library(devEMF)
library(grid)

# Source helper functions
source(here("R/clustering/utils/dendogram_utils.R"))

# Load results
load(here("data/processed/preferred_clustering.Rdata"))

#------------------------------------------------------------------------------#
# Prepare visualization ----------------------------------------------------
#------------------------------------------------------------------------------#

# Get colors
set2_colors <- brewer.pal(n = 8, name = "Set2")
cluster_colors <- set2_colors[c(2,4,1,3,5)]

# Create dendrogram
dend <- fviz_dend(results$clustering_results$clustering,
                  k = 5,
                  cex = 0.7,
                  main = "Hierarchical Clustering of FE Estimates",
                  ylab = "Height",
                  rect = FALSE,
                  color_labels_by_k = TRUE,
                  horiz = TRUE,
                  palette = cluster_colors,
                  ggtheme = theme_minimal() +
                    theme(
                      panel.grid = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_blank()
                    )
) +
  custom_rect_dendrogram(stats::as.dendrogram(results$clustering_results$clustering),
                         k = 5,
                         k_colors = cluster_colors,
                         rect_fill = TRUE,
                         rect_lty = 2,
                         rect_width_offset = 1) +
  scale_y_continuous(breaks = seq(0, 15, by = 5), trans = "reverse") +
  coord_flip()

# Get distance matrix for factor maps
dist_matrix <- results$clustering_results$dist_matrix

# Prepare cluster factor
cluster_factor <- cutree(results$clustering_results$clustering, k = 5)
cluster_names <- names(cluster_factor)

# Recode clusters and set factor levels
new_cluster_factor <- recode(cluster_factor,
                             `1` = 1,
                             `2` = 2,
                             `3` = 4,
                             `4` = 3,
                             `5` = 5)
names(new_cluster_factor) <- cluster_names
new_cluster_factor <- factor(new_cluster_factor,
                             levels = c(1, 3, 2, 4, 5),
                             labels = c("Core", "Periphery", "Workbench", "Finance", "Luxembourg"))


# Save plot
ggsave(file = here("output/fig/final/pdf/fig4.pdf"),
       plot = dend,
       width = 8, height = 5,
       bg = "white")
ggsave(file = here("output/fig/final/eps/fig4.eps"),
       plot = dend,
       width = 8, height = 5,
       device = cairo_ps)

emf(here("output/fig/final/emf/fig4.emf"), width = 9, height = 5, bg = "white")
grid.draw(dend)
dev.off()


