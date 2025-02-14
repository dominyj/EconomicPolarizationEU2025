#------------------------------------------------------------------------------
# Setup
#------------------------------------------------------------------------------
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(ggalluvial)
library(RColorBrewer)

# Load results
load(here("data/processed/clustering_comparison.Rdata"))
load(here("data/processed/old_clustering.Rdata"))  # Original results from previous paper

#------------------------------------------------------------------------------
# Prepare data for plotting
#------------------------------------------------------------------------------

# Function to prepare cluster data
prepare_cluster_data <- function(clust_obj, n_clusters = 5, method_name) {
  cluster_assignments <- cutree(clust_obj, k = n_clusters)

  data.frame(
    country_id = clust_obj$order,
    country = clust_obj$order.lab
  ) %>%
    arrange(country_id) %>%
    left_join(tibble(
      country_id = 1:length(cluster_assignments),
      cluster = cluster_assignments
    ), by = "country_id") %>%
    arrange(cluster) %>%
    mutate(method_type = method_name)
}

# Prepare data for all methods
plot_data <- bind_rows(
  prepare_cluster_data(clust_obj, n_clusters = 5, "original_results"),
  prepare_cluster_data(clustering_comparison$original_period$clustering_results$clustering,
                       5, "simple_model"),
  prepare_cluster_data(clustering_comparison$extended_standard$clustering_results$clustering,
                       5, "new_data"),
  prepare_cluster_data(clustering_comparison$extended_weighted$clustering_results$clustering,
                       5, "new_method")
)


# Define cluster group names
cluster_names <- tribble(
  ~method_type,        ~cluster,  ~c_group,
  "original_results",  1,         "Core",
  "original_results",  2,         "Periphery",
  "original_results",  3,         "Workbench/Finance",
  "original_results",  4,         "Workbench",
  "original_results",  5,         "Luxembourg",

  "simple_model",      1,         "Core",
  "simple_model",      2,         "Workbench",
  "simple_model",      3,         "Periphery",
  "simple_model",      4,         "Workbench/Finance",
  "simple_model",      5,         "Luxembourg",

  "new_data",         1,         "Core",
  "new_data",         2,         "Workbench",
  "new_data",         3,         "Workbench/Finance",
  "new_data",         4,         "Periphery",
  "new_data",         5,         "Luxembourg",

  "new_method",       1,         "Core",
  "new_method",       2,         "Workbench",
  "new_method",       3,         "Workbench/Finance",
  "new_method",       4,         "Periphery",
  "new_method",       5,         "Luxembourg"
)

# Join cluster names and set factor levels
plot_data <- plot_data %>%
  left_join(cluster_names, by = c("method_type", "cluster"))

plot_data$method_type <- factor(plot_data$method_type,
                                levels = c("original_results", "simple_model", "new_data", "new_method"))

plot_data$c_group <- factor(plot_data$c_group,
                            levels = c("Core", "Periphery", "Workbench", "Workbench/Finance", "Luxembourg"))

#------------------------------------------------------------------------------
# Create plot
#------------------------------------------------------------------------------

# Get colors
set2_colors <- brewer.pal(n = 8, name = "Set2")

# Create Sankey plot
sankey_plot <- ggplot(plot_data,
                      aes(x = method_type,
                          stratum = c_group,
                          alluvium = country,
                          fill = c_group,
                          label = c_group)) +
  geom_flow(stat = "alluvium",
            lode.guidance = "zigzag",
            color = "darkgray") +
  geom_stratum() +
  geom_text(stat = "alluvium",
            aes(label = country),
            size = 3,
            min.y = 1) +
  # Add group labels
  annotate("text", x = 0.75, y = 23, label = "CORE",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  annotate("text", x = 0.75, y = 3, label = "WORKBENCH/\nFINANCE",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans", lineheight = 0.8) +
  annotate("text", x = 0.75, y = 16.5, label = "PERIPHERY",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  annotate("text", x = 0.75, y = 9, label = "WORKBENCH",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  # Add time period labels
  annotate("text", x = c(1, 2, 3, 4), y = -0.5,
           label = c("1999-2016", "1999-2016", "1999-2023", "1999-2023"),
           size = 3.5) +
  # Add method labels
  annotate("text", x = c(1, 2, 3, 4), y = 27.7,
           label = c("a) Original Clustering", "b) Simplified Model",
                     "c) + Extended Data", "d) + SE-Adjusted"),
           size = 4, fontface = "bold") +
  annotate("text", x = 1, y = 27.2,
           label = "Gräbner et al. (2020b)",
           size = 3.5) +
  # Theme customization
  theme_minimal() +
  labs(x = "Time Period",
       y = NULL,
       fill = NULL,
       title = "Methodological Evolution of Country Classifications") +
  scale_fill_manual(values = c(
    "Core" = set2_colors[2],
    "Workbench/Finance" = set2_colors[1],
    "Periphery" = set2_colors[3],
    "Workbench" = set2_colors[4],
    "Luxembourg" = set2_colors[5]
  )) +
  scale_x_discrete(expand = c(0.11, 0),
                   labels = NULL) +
  scale_y_continuous(expand = c(0, 0.6)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5, r = -15, b = 5, l = 5, unit = "pt")
  )

sankey_plot

# Save plot
ggsave(file = here("output/fig/final/pdf/fig3.pdf"),
       plot = sankey_plot,
       width = 12, height = 9)
 ggsave(file = here("output/fig/final/eps/fig3.eps"), plot = sankey_plot,
       width = 12, height = 9,
       device = cairo_ps)

emf(here("output/fig/final/emf/fig3.emf"), width = 12, height = 9)
print(sankey_plot)
dev.off()

