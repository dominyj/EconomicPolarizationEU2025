#' Process AMECO raw data into tidy format
#' @param raw_data Data frame containing raw AMECO data from DBnomics
#' @param dictionary Data frame containing variable definitions for AMECO indicators
#' @return Processed and tidied AMECO data frame
#' @importFrom dplyr rename mutate left_join filter select %>%
process_ameco_data <- function(raw_data, dictionary) {
  raw_data %>%
    rename(country = Country) %>%
    mutate(
      indicator = dataset_code,
      country_iso3c = custom_countrycode(geo, "ameco", "iso3c")
    ) %>%
    left_join(dictionary, by = "indicator") %>%
    filter(!is.na(value)) %>%
    select(country, country_iso3c, variable_name, indicator, period, value,
           provider_code, indexed_at, downloaded_at, series_name, series_code, unit,
           '@frequency', dataset_code, dataset_name)
}

#' Load and process AMECO data with automatic batching
#' @param reload Boolean indicating whether to fetch new data (TRUE) or use cached data (FALSE)
#' @param cache_path Path where cached data is stored
#' @param variable_dictionary Data frame containing variable definitions
#' @param countries_considered Vector of ISO3C country codes
#' @return Processed AMECO data frame containing:
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
#' data <- load_ameco_data(
#'   variable_dictionary = my_dictionary,
#'   countries_considered = c("DEU", "FRA", "ITA")
#' )
#' }
#' @export
load_ameco_data <- function(reload = FALSE,
                            cache_path = here("data/cache/ameco_data.Rdata"),
                            variable_dictionary,
                            countries_considered) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  if (!reload && file.exists(cache_path)) {
    load(cache_path)
    return(ameco_tidy_data)
  }

  ameco_dictionary <- variable_dictionary %>%
    filter(source == "AMECO") %>%
    select(-unit)

  ameco_ids <- create_dbnomics_ids(
    templates = ameco_dictionary$dbnomics_id,
    code_formats = ameco_dictionary$countrycode,
    countries_considered
  )

  ameco_raw_data <- batch_rdb(ameco_ids)
  ameco_tidy_data <- process_ameco_data(ameco_raw_data, ameco_dictionary)

  save(ameco_tidy_data, file = cache_path)
  return(ameco_tidy_data)
}
