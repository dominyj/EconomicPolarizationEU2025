# Load required packages
library(countrycode)

# Define country and country groups
country_groups <- tribble(
  ~country,                ~c_group,
  "Austria",                    "Core",
  "Belgium",                    "Core",
  "Denmark",                    "Core",
  "Finland",                    "Core",
  "Germany",                    "Core",
  "Sweden",                     "Core",
  "Bulgaria",                   "Workbench",
  "Romania",                    "Workbench",
  "Czech Republic",             "Workbench",
  "Estonia",                    "Workbench",
  "Latvia",                     "Workbench",
  "Lithuania",                  "Workbench",
  "Hungary",                    "Workbench",
  "Poland",                     "Workbench",
  "Slovenia",                   "Workbench",
  "Slovakia",                   "Workbench",
  "Croatia",                    "Workbench",
  "Luxembourg",                 "Finance",
  "Netherlands",                "Finance",
  "Malta",                      "Finance",
  "Ireland",                    "Finance",
  "Cyprus",                     "Finance",
  "France",                     "Periphery",
  "Greece",                     "Periphery",
  "Italy",                      "Periphery",
  "Portugal",                   "Periphery",
  "Spain",                      "Periphery"
) %>%
  mutate(country_iso3c = countrycode(country, "country.name", "iso3c"))
