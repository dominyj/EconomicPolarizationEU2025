#------------------------------------------------------------------------------#
# Setup -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(ggalluvial)
library(RColorBrewer)

source(here("R/clustering/utils/visualization.R"))

# Load results
load(here("data/processed/rolling_clusters.Rdata"))

#------------------------------------------------------------------------------#
# Prepare data for visualization -------------------------------------------
#------------------------------------------------------------------------------#

# Define cluster names
cluster_names <- tribble(
  ~period, ~cluster, ~c_group,
  "1999",  1,        "Core",
  "1999",  2,        "Finance",
  "1999",  3,        "Workbench",
  "1999",  4,        "Periphery",
  "1999",  5,        "Luxembourg",

  "2004",  1,        "Core",
  "2004",  2,        "Workbench",
  "2004",  3,        "Workbench",
  "2004",  4,        "Periphery",
  "2004",  5,        "Luxembourg",

  "2009",  1,        "Core",
  "2009",  2,        "Workbench",
  "2009",  3,        "Finance",
  "2009",  4,        "Periphery",
  "2009",  5,        "Luxembourg",

  "2014",  1,        "Core",
  "2014",  2,        "Finance",
  "2014",  3,        "Workbench",
  "2014",  4,        "Periphery",
  "2014",  5,        "Luxembourg",
)

# Prepare plot data
plot_data <- prepare_plot_data(results)
plot_data <- plot_data %>%
  left_join(cluster_names, by = c("period", "cluster"))

sankey_data <- plot_data %>%
  mutate(period_range = paste0(period_start, "-", period_end)) %>%
  select(period, period_range, country, c_group) %>%
  mutate(c_group = factor(c_group,
                          levels = c("Luxembourg", "Finance", "Core", "Periphery", "Workbench")))

sankey_data <- sankey_data %>%
  group_by(country) %>%
  mutate(id = cur_group_id()) %>%
  ungroup()

# Get colors
set2_colors <- brewer.pal(n = 8, name = "Set2")

#------------------------------------------------------------------------------#
# Create plot ------------------------------------------------------------
#------------------------------------------------------------------------------#

sankey_plot <- ggplot(sankey_data,
                      aes(x = period_range,
                          stratum = c_group,
                          alluvium = id,
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
  annotate("text", x = 0.75, y = 20, label = "CORE",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  annotate("text", x = 0.75, y = 25, label = "FINANCE",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  annotate("text", x = 0.75, y = 14.5, label = "PERIPHERY",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  annotate("text", x = 0.75, y = 6.5, label = "WORKBENCH",
           angle = 90, hjust = 0.5, vjust = 0.5, size = 5,
           fontface = "bold", family = "sans") +
  # Theme customization
  theme_minimal() +
  labs(x = "Time Period",
       y = NULL,
       fill = NULL,
       title = "Structural Changes in European Country Groups, 1999-2023") +
  scale_fill_manual(values = c(
    "Core" = set2_colors[2],
    "Finance" = set2_colors[1],
    "Periphery" = set2_colors[3],
    "Workbench" = set2_colors[4],
    "Luxembourg" = set2_colors[5]
  )) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 5, r = -15, b = 5, l = 5, unit = "pt"))

#------------------------------------------------------------------------------#
# Save plot --------------------------------------------------------------
#------------------------------------------------------------------------------#

ggsave(file = here("output/fig/final/pdf/fig5.pdf"),
       plot = sankey_plot,
       width = 12, height = 9)
ggsave(file = here("output/fig/final/eps/fig5.eps"), plot = sankey_plot,
       width = 12, height = 9,
       device = cairo_ps)

emf(here("output/fig/final/emf/fig5.emf"), width = 12, height = 9)
print(sankey_plot)
dev.off()
