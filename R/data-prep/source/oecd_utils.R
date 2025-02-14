#' Process OECD raw data into tidy format
#' @param raw_data Data frame containing raw OECD data from DBnomics
#' @param dictionary Data frame containing variable definitions for OECD indicators
#' @return Processed and tidied OECD data frame
#' @importFrom dplyr rename mutate left_join filter select %>%
#' @importFrom stringr str_replace
#' @importFrom countrycode countrycode
process_oecd_data <- function(raw_data, dictionary) {
  tidy_data <- raw_data %>%
    rename(country_iso3c = REF_AREA,
           unit = `Unit of measure`) %>%
    mutate(
      country = countrycode(country_iso3c, "iso3c", "country.name"),
      indicator = str_replace(series_code, "(A\\.[A-Z]{3}\\.|[A-Z]{3}\\.)", "\\*\\.")
    ) %>%
    left_join(dictionary, by = "indicator") %>%
    filter(!is.na(value)) %>%
    select(country, country_iso3c, variable_name, indicator, period, value,
           provider_code, indexed_at, downloaded_at, series_name, series_code, unit,
           '@frequency', dataset_code, dataset_name)

  return(tidy_data)
}

#' Load and process OECD data with automatic batching
#' @param reload Boolean indicating whether to fetch new data (TRUE) or use cached data (FALSE)
#' @param cache_path Path where cached data is stored
#' @param variable_dictionary Data frame containing variable definitions
#' @param countries_considered Vector of ISO3C country codes
#' @return Processed OECD data frame containing:
#'   \item{country}{Full country name}
#'   \item{country_iso3c}{ISO3C country code}
#'   \item{variable_name}{Name of the economic variable}
#'   \item{indicator}{DBnomics indicator code}
#'   \item{period}{Time period of observation}
#'   \item{value}{Observed value}
#'   \item{unit}{Unit of measurement}
#' @importFrom dplyr filter select mutate bind_rows %>%
#' @importFrom here here
#' @examples
#' \dontrun{
#' data <- load_oecd_data(
#'   variable_dictionary = my_dictionary,
#'   countries_considered = c("DEU", "FRA", "ITA")
#' )
#' }
#' @export
load_oecd_data <- function(reload = FALSE,
                           cache_path = here("data/cache/oecd_data.Rdata"),
                           variable_dictionary,
                           countries_considered) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  if (!reload && file.exists(cache_path)) {
    load(cache_path)
    return(oecd_tidy_data)
  }

  oecd_dictionary <- variable_dictionary %>%
    filter(source == "OECD") %>%
    select(-unit)

  oecd_ids <- create_dbnomics_ids(
    templates = oecd_dictionary$dbnomics_id,
    code_formats = oecd_dictionary$countrycode,
    countries_considered
  )

  oecd_raw_data <- batch_rdb(oecd_ids)
  oecd_tidy_data <- process_oecd_data(oecd_raw_data, oecd_dictionary)

  save(oecd_tidy_data, file = cache_path)
  return(oecd_tidy_data)
}
