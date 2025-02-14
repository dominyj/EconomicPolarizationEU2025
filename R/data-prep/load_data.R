# Setup -------------------------------------------------------------------
# Clear environment
rm(list = ls())

# Load required packages
library(rdbnomics)
library(data.table)
library(here)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

# Source additional setup files
source(here("R/00_country_setup.R"))
source(here("R/data-prep/source/ameco_utils.R"))
source(here("R/data-prep/source/wdi_utils.R"))
source(here("R/data-prep/source/eurostat_utils.R"))
source(here("R/data-prep/source/oecd_utils.R"))
source(here("R/data-prep/utils/rdbnomics_utils.R"))
source(here("R/data-prep/source/growth_lab_eci_utils.R"))
source(here("R/data-prep/source/eu_klems_utils.R"))
source(here("R/data-prep/utils/custom_countrycode.R"))

# Countries ----------------------------------------------------------
countries_considered <- country_groups$country_iso3c


# rdbnomics
# Load variable dictionary -----------------------------------------------
variable_dictionary <- fread(here("data/meta/macro_variable_dictionary.csv")) %>%
  mutate(indicator = map2_chr(dbnomics_id, source, extract_indicator))

# Configuration
reload_config <- list(
  ameco = FALSE,
  wdi = FALSE,
  eurostat = FALSE,
  oecd = FALSE,
  eci = FALSE,
  klems = FALSE
)

# Load data from each source
ameco_data <- load_ameco_data(
  reload = reload_config$ameco,
  variable_dictionary = variable_dictionary,
  countries_considered = countries_considered
)

wdi_data <- load_wdi_data(
  reload = reload_config$wdi,
  variable_dictionary = variable_dictionary,
  countries_considered = countries_considered
)

eurostat_data <- load_eurostat_data(
  reload = reload_config$eurostat,
  variable_dictionary = variable_dictionary,
  countries_considered = countries_considered
)

oecd_data <- load_oecd_data(
  reload = reload_config$oecd,
  variable_dictionary = variable_dictionary,
  countries_considered = countries_considered
)

eci_data_tidy <- load_eci_data(
  reload = reload_config$eci,
  countries_considered = countries_considered
)

klems_tidy <- load_klems_data(
  reload = reload_config$klems,
  countries_considered = country_groups$country_iso3c
)


# Combine all rdb data
rdb_data_tidy <- combine_rdb_data(ameco_data, wdi_data, eurostat_data, oecd_data)

# Combine all datasets
macro_data_tidy <- rbind(rdb_data_tidy, eci_data_tidy, klems_tidy, fill = TRUE)

# Save final results
dir_path <- here("data/final/")
dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
save(macro_data_tidy, file = paste0(dir_path, "macro_data.Rdata"))
fwrite(macro_data_tidy, file = paste0(dir_path, "macro_data.csv"))
