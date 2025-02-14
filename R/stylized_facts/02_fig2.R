# Clear environment
rm(list = ls())

# Load required packages
library(here)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)
library(RColorBrewer)
library(purrr)
library(ggpubr)
library(scales)
library(devEMF)

# Source setup files
source(here("R/00_country_setup.R"))
source(here("R/stylized_facts/99_stylized_facts_utils.R"))

# Import data
load(here("data/processed/full_data.Rdata"))

macro_data_tidy <- full_data

# 1. Data Preparation
# ------------------
# Prepare base dataset with country groups and time filter
macro_data <- macro_data_tidy %>%
  left_join(country_groups %>% select(c_group, country_iso3c), by = "country_iso3c") %>%
  filter(year >= 2018, year <= 2023)

plot_data <- macro_data

# 2. Configure Group Comparisons
# ----------------------------
# Define variable configurations for each country group
group_configs <- list(
  # Core countries: Focus on macroeconomic stability and competitiveness
  Core = tribble(
    ~var_name,          ~title,                ~y_label,              ~reference_groups,                                 ~log_scale,
    "gdp_pc_eu_deviation",  "GDP p.c.",       "EU mean deviation (2021 USD, PPP)", list(c("Periphery", "Workbench")),    FALSE,
    "hs_eci",           "Complexity",          "ECI",                 NULL,                                              FALSE,
    "current_account",  "Current Account",     "% of GDP",            list(c("Periphery", "Workbench")),                 FALSE,
    "bond_yield",       "Bond Yield",          "Gov. Bonds in %",     NULL,                                              FALSE
  ),

  # Periphery countries: Focus on structural challenges
  Periphery = tribble(
    ~var_name,           ~title,              ~y_label,               ~reference_groups,              ~log_scale,
    "unemployment",      "Unemployment",       "% of Labor Force",     NULL,                          FALSE,
    "debt_gov_percGDP",  "Government Debt",    "% of GDP",            NULL,                           FALSE,
    "total_debt_percGDP", "Total Debt",        "% of GDP",            NULL,                           FALSE,
    "trade_exp_GDP",     "Exports",            "% of GDP",            list(c("Core", "Workbench")),   FALSE
  ),

  # Finance countries: Focus on financial sector characteristics
  Finance = tribble(
    ~var_name,            ~title,              ~y_label,               ~reference_groups, ~log_scale,
    "abs_fdi_percGDP",    "FDI Flows",         "|Inflows + Outflows| % GDP", NULL,       TRUE,
    "fdi_volatility",     "FDI Volatility",     "Standard Deviation",   NULL,            TRUE,
    "debt_total_corp",    "Corporate Debt",     "% of GDP",            NULL,             TRUE,
    "va_finance",         "Financial Sector",   "Share in Gross Output", NULL,           FALSE
  ),

  # Workbench countries: Focus on industrial and wage dynamics
  Workbench = tribble(
    ~var_name,            ~title,              ~y_label,               ~reference_groups, ~log_scale,
    "gdp_pc_eu_deviation",  "GDP p.c.",        "EU mean deviation (2021 USD, PPP)", NULL, FALSE,
    "fdi_percGDP",         "FDI",              "Net Inflows % GDP",    list(c("Core", "Periphery")), FALSE,
    "industry_employment_share", "Industry Share",    "% of Total Employment", NULL,       FALSE,
    "adj_wage_share",      "Adj. Wage Share",        "% of GDP",       list(c("Core", "Periphery")), FALSE
  )
)

# Get unique variables from config for data validation
unique_vars <- unique(c(
  group_configs$Core$var_name,
  group_configs$Periphery$var_name,
  group_configs$Finance$var_name,
  group_configs$Workbench$var_name
))

# Basic data validation checks
filter(plot_data, variable_name == "fdi_percGDP") %>%
  slice(1)

first_rows <- plot_data %>%
  filter(variable_name %in% unique_vars) %>%
  group_by(variable_name) %>%
  slice(1)

as.data.frame(first_rows)

# 3. Reference Group Function
# -------------------------
# Function to create reference groups for comparison
create_reference_groups <- function(data, var_name, focus_group, ref_groups) {
  # Get all unique groups from data
  all_groups <- unique(data$c_group)

  if (is.null(ref_groups)) {
    # If no specific reference groups, use all groups except focus group
    ref_groups <- setdiff(all_groups, focus_group)
    missing_groups <- character(0)
    filter_groups <- all_groups
  } else {
    # Determine missing groups for explicit references
    missing_groups <- setdiff(all_groups, c(focus_group, unlist(ref_groups)))
    filter_groups <- c(focus_group, unlist(ref_groups))
  }

  # Create comparison groups with appropriate labels
  return_data <- data %>%
    filter(c_group %in% filter_groups) %>%
    mutate(
      comparison_group = if_else(
        c_group == focus_group,
        focus_group,
        if (length(missing_groups) > 0) {
          paste0("Other\n (excl. ", paste(missing_groups, collapse = ", "), ")")
        } else {
          "Other"
        }
      )
    )
  return(return_data)
}

# 4. Apply Reference Groups
# -----------------------
# Initialize empty dataframe for results
plot_data_grouped <- data.frame()

# Process each focus group
for (group in names(group_configs)) {
  config <- group_configs[[group]]

  # Process each variable in the configuration
  for (i in 1:nrow(config)) {
    filtered_data <- plot_data %>%
      filter(variable_name == config$var_name[i])

    # Apply reference group function and add to results
    processed_data <- create_reference_groups(
      data = filtered_data,
      var_name = config$var_name[i],
      focus_group = group,
      ref_groups = config$reference_groups[[i]]
    ) %>%
      mutate(original_focus_group = group)

    plot_data_grouped <- bind_rows(plot_data_grouped, processed_data)
  }
}

# Define color palette
set2_colors <- brewer.pal(n = 8, name = "Set2")

# Create function to generate plots for each group configuration
create_plots <- function(data, config) {
  plots <- map2(1:nrow(config), config$y_label, function(i, y_lab) {
    # Prepare data and factor levels for plotting
    var_data <- data %>%
      filter(variable_name == config$var_name[i]) %>%
      mutate(comparison_group = factor(comparison_group,
                                       levels = c(unique(original_focus_group),
                                                  setdiff(unique(comparison_group), unique(original_focus_group)))))

    # Define color scheme for groups
    group_colors <- c(
      "Core" = set2_colors[2],
      "Finance" = set2_colors[1],
      "Periphery" = set2_colors[3],
      "Workbench" = set2_colors[4],
      "Other" = "grey70"
    )
    # Add colors for additional "Other" groups
    other_groups <- grep("Other\\n \\(excl", unique(var_data$comparison_group), value = TRUE)
    group_colors <- c(group_colors, setNames(rep("grey70", length(other_groups)), other_groups))

    # Create base plot
    p <- ggplot(var_data, aes(x = comparison_group, y = value, fill = comparison_group)) +
      geom_boxplot(width = 0.7, alpha = 0.9, outliers = FALSE) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
      stat_boxplot(geom = "errorbar", width = 0.3)

    # Define y-axis scale based on variable type
    y_scale <- if(config$log_scale[i]) {
      scale_y_log10()
    } else if(config$var_name[i] == "gdp_pc_eu_deviation") {
      scale_y_continuous(labels = function(x) paste0(x/1000, "k"))
    } else if(config$var_name[i] == "fdi_percGDP") {
      scale_y_continuous(
        trans = scales::pseudo_log_trans(sigma = 0.5),
        breaks = c(-100, -50, -25, -10, 0, 10, 25, 50, 100, 200)
      )
    } else {
      scale_y_continuous()
    }

    # Add scale and styling to plot
    p + y_scale +
      scale_fill_manual(values = group_colors) +
      theme_bw() +
      ggtitle(config$title[i]) +
      ylab(y_lab) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7.8),
        axis.title.y = element_text(size = 9),
        panel.border = element_blank(),
        axis.line = element_line()
      )
  })

  # Combine plots with consistent layout
  wrap_plots(plots, nrow = 1) +
    plot_layout(guides = "collect") &
    theme(plot.margin = margin(5, 5, 5, 5))
}

# Create individual group plots
group_plots <- map2(group_configs, names(group_configs), function(config, group_name) {
  # Filter data for current group
  group_data <- plot_data_grouped %>%
    filter(original_focus_group == group_name &
             comparison_group %in% c(group_name, "Other", grep("Other\n \\(excl", comparison_group, value = TRUE)))

  create_plots(group_data, config)
})

# Create final 2x2 panel combining all group plots
combined_group_fig <- ggarrange(
  # Add plots with specific margins and styling
  group_plots[[1]] + theme(legend.position = "none",
                           plot.margin = margin(t = 25, r = 25, b = 20, l = 0, unit = "pt")),
  group_plots[[2]] + theme(legend.position = "none",
                           plot.margin = margin(t = 25, r = 10, b = 20, l = 5, unit = "pt")),
  group_plots[[3]] + theme(legend.position = "none",
                           plot.margin = margin(t = 25, r = 25, b = 5, l = 0, unit = "pt")),
  group_plots[[4]] + theme(legend.position = "none",
                           plot.margin = margin(t = 25, r = 10, b = 5, l = 5, unit = "pt")),
  labels = c("a) Core", "b) Periphery", "c) Finance", "d) Workbench"),
  ncol = 2,
  nrow = 2
)

# Display combined figure
combined_group_fig

# Save plots in different formats
# PDF version
ggsave(file = here("output/fig/final/pdf/fig2.pdf"),
       plot = combined_group_fig,
       width = 12, height = 8)

# EPS version
ggsave(file = here("output/fig/final/eps/fig2.eps"),
       plot = combined_group_fig,
       width = 12, height = 8,
       device = cairo_ps)

# EMF version
emf(here("output/fig/final/emf/fig2.emf"), width = 12, height = 8)
print(combined_group_fig)
dev.off()






