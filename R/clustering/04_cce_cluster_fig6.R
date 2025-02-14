#------------------------------------------------------------------------------#
# Setup -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)
library(devEMF)
library(grid)

# Source helper functions
source(here("R/clustering/utils/dendogram_utils.R"))

# Load results
load(here("data/processed/cce_clustering.Rdata"))

#------------------------------------------------------------------------------#
# Prepare visualization ----------------------------------------------------
#------------------------------------------------------------------------------#

# Get colors
set2_colors <- brewer.pal(n = 8, name = "Set2")
cluster_colors <- set2_colors[c(2,8,4,1,3)]

# Create dendrogram
dend <- fviz_dend(results$clustering_results$clustering,
                  k = 5,
                  cex = 0.7,
                  main = "Hierarchical Clustering of CCE Estimates",
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

# Get cluster assignments
cluster_factor <- cutree(results$clustering_results$clustering, k = 5)

# Create factor map
p2 <- fviz_cluster(
  list(
    data = as.matrix(dist_matrix),
    cluster = cluster_factor
  ),
  axes = c(1, 2),
  main = "Factor Map (Dim 1 vs 2)",
  geom = c("point", "text"),
  labelsize = 12,
  repel = TRUE,
  ggtheme = theme_minimal(),
  palette = set2_colors[c(2,4,3,8,1)]
) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

#------------------------------------------------------------------------------#
# Combine and save plots --------------------------------------------------
#------------------------------------------------------------------------------#

# Combine plots
combined_plots <- ggarrange(
  dend + theme(legend.position = "none"),
  p2 + theme(legend.position = "none"),
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)

# Save plot
ggsave(file = here("output/fig/final/pdf/fig6.pdf"),
       plot = combined_plots,
       width = 10, height = 6,
       bg = "white")
ggsave(file = here("output/fig/final/eps/fig6.eps"), plot = combined_plots,
       width = 10, height = 6,
       device = cairo_ps)

emf(here("output/fig/final/emf/fig6.emf"), width = 10, height = 6)
grid.draw(combined_plots)
dev.off()
