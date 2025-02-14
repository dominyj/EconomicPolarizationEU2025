# Clear environment
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(plotly)
library(ggpubr)
library(scales)
library(devEMF)


# Source additional setup files
source(here("R/00_country_setup.R"))
source(here("R/stylized_facts/99_stylized_facts_utils.R"))

# Import data
load(here("data/processed/full_data.Rdata"))

# Prepare data
macro_data <- full_data %>%
  left_join(country_groups %>% select(c_group, country_iso3c), by = "country_iso3c") %>%
  pivot_wider(id_cols = c(country, country_iso3c, c_group, year),
              names_from = variable_name,
              values_from = value) %>%
  filter(year >= 1999,
         year <= 2023)


# Plot Configuration
plot_config <- tribble(
  ~variable_name,               ~label,                                   ~show_deviation, ~show_variance, ~detailed_plots, ~stat_type,
  "current_account",            "Current Account Balance",                FALSE,          FALSE,         FALSE,           "weighted_mean",
  "unemployment",               "Unemployment Rate",                      FALSE,          FALSE,         FALSE,           "weighted_mean",
  "gdp_real_pc_ppp",           "GDP per Capita (PPP)",                  TRUE,           FALSE,         FALSE,           "mean",
  "gdp_real_pc_ppp",           "GDP per Capita (PPP)",                  TRUE,           FALSE,         FALSE,           "weighted_mean",
  "industry_employment_share",  "Industry Employment Share",             FALSE,          FALSE,         FALSE,           "weighted_mean",
  "hs_eci_distance_to_p95",          "Distance to 95th Percentile (HS-ECI)",            FALSE,          FALSE,          FALSE,           "weighted_mean",

)

plot_list <- list()

# Create and save plots
for(i in 1:nrow(plot_config)) {
  plot_list[[paste0(i, "_", plot_config$variable_name[[i]])]] <- generate_indicator_plot(
    data = macro_data,
    var_name = plot_config$variable_name[i],
    var_label = plot_config$label[i],
    show_deviation = plot_config$show_deviation[i],
    show_variance = plot_config$show_variance[i],
    detailed_plots = plot_config$detailed_plots[i],
    stat_type = plot_config$stat_type[i]
  )

}



# Create individual plots
# Fig 1a - GDP per capita (population-weighted)
fig1a <- plot_list$`4_gdp_real_pc_ppp`+
  labs(title = "GDP per capita (deviation from EU average)",
       subtitle = "population-weighted averages",
       y = "constant 2021 USD, PPP")+
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1000,
                                                    suffix = "k"),
                     expand = expansion(mult = c(0.03, 0.15)))

# Fig 1b - GDP per capita (unweighted)
fig1b <- plot_list$`3_gdp_real_pc_ppp`+
  labs(title = "GDP per capita (deviation from EU average)",
       subtitle = "unweighted averages",
       y = "constant 2021 USD, PPP")+
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1000,
                                                    suffix = "k"),
                     expand = expansion(mult = c(0.03, 0.15)))

# Fig 1c - Unemployment rate
fig1c <- plot_list$`2_unemployment`+
  labs(title = "Unemployment rate",
       subtitle = "population-weighted averages",
       y = "% of active population")+
  scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                     suffix = "%",
                                                     accuracy = 1),
                     expand = expansion(mult = c(0.03, 0.15)))

# Fig 1d - Current account balance
fig1d <- plot_list$`1_current_account`+
  labs(title = "Current account balance",
       subtitle = "population-weighted averages",
       y = "% of GDP")+
  scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                     suffix = "%",
                                                     accuracy = 1),
                     expand = expansion(mult = c(0.03, 0.15)))

# Combine all four plots into 2x2 panel
combined_fig1 <- ggarrange(
  fig1a + theme(legend.position = "none"),
  fig1b + theme(legend.position = "none"),
  fig1c + theme(legend.position = "none"),
  fig1d + theme(legend.position = "none"),
  labels = c("a)", "b)", "c)", "d)"),
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "bottom"
)

# Add caption with source information for all panels
final_fig1 <- annotate_figure(combined_fig1,
                              bottom = text_grob("Source: World Development Indicators (World Bank) for panels a) and b); AMECO for panels c) and d)",
                                                 hjust = 1, x = 0.99,
                                                 size = 10))

# Save final figure
ggsave(file = here("output/fig/final/pdf/fig1.pdf"), plot = final_fig1,
       width = 12, height = 9)
ggsave(file = here("output/fig/final/eps/fig1.eps"), plot = final_fig1,
       width = 12, height = 9,
       device = cairo_ps)

emf(here("output/fig/final/emf/fig1.emf"), width = 12, height = 9)
print(final_fig1)
dev.off()




# Fig 8
fig8a <- plot_list$`5_industry_employment_share`+
  labs(title = "Industry Employment Share",
       subtitle = "population-weighted averages",
       y = "% of total employment")+
  scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                     suffix = "%",
                                                     accuracy = 1),
                     expand = expansion(mult = c(0.03, 0.15)))

fig8b <- plot_list$`6_hs_eci_distance_to_p95`+
  labs(title = "Economic Complexity Relative to Technology Frontier",
       subtitle = "population-weighted averages",
       y = "Distance to 95th percentile (HS-based ECI)")+
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.15)))

combined_fig8 <- ggarrange(
  fig8a + theme(legend.position = "none"),
  fig8b + theme(legend.position = "none"),
  labels = c("a)", "b)"),
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)

# Add caption with source information for all panels
final_fig8 <- annotate_figure(combined_fig8,
                              bottom = text_grob("Source: World Development Indicators (World Bank) for panel a); own calculations based on Atlas of Economic Complexity, Harvard Growth Lab for panel b)",
                                                 hjust = 1, x = 0.99,
                                                 size = 10))

# Save final figure
ggsave(file = here("output/fig/final/pdf/fig7.pdf"), plot = final_fig8,
       width = 12, height = 5)
ggsave(file = here("output/fig/final/eps/fig7.eps"), plot = final_fig8,
       width = 12, height = 5,
       device = cairo_ps)

emf(here("output/fig/final/emf/fig7.emf"), width = 12, height = 5)
print(final_fig8)
dev.off()
