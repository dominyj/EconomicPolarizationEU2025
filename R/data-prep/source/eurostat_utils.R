#' #' Process Eurostat raw data into tidy format
#' @param raw_data Data frame containing raw Eurostat data from DBnomics
#' @param dictionary Data frame containing variable definitions for Eurostat indicators
#' @return Processed and tidied Eurostat data frame
#' @importFrom dplyr mutate left_join filter select %>%
#' @importFrom countrycode countrycode
process_eurostat_data <- function(raw_data, dictionary) {
  raw_data %>%
    mutate(
      country_iso3c = countrycode(geo, "eurostat", "iso3c"),
      country = countrycode(geo, "eurostat", "country.name"),
      indicator = dataset_code,
      unit = `Unit of measure`
    ) %>%
    left_join(dictionary, by = "indicator") %>%
    filter(!is.na(value)) %>%
    select(country, country_iso3c, variable_name, indicator, period, value,
           provider_code, indexed_at, downloaded_at, series_name, series_code, unit,
           '@frequency', dataset_code, dataset_name)
}

#' Load and process Eurostat data
#' @param reload Boolean indicating whether to fetch new data (TRUE) or use cached data (FALSE)
#' @param cache_path Path where cached data is stored
#' @param variable_dictionary Data frame containing variable definitions
#' @param countries_considered Vector of ISO3C country codes
#' @return Processed Eurostat data frame containing:
#'   \item{country}{Full country name}
#'   \item{country_iso3c}{ISO3C country code}
#'   \item{variable_name}{Name of the economic variable}
#'   \item{indicator}{DBnomics indicator code}
#'   \item{period}{Time period of observation}
#'   \item{value}{Observed value}
#'   \item{unit}{Unit of measurement}
#' @importFrom dplyr filter select mutate %>%
#' @importFrom here here
#' @examples
#' \dontrun{
#' data <- load_eurostat_data(
#'   variable_dictionary = my_dictionary,
#'   countries_considered = c("DEU", "FRA", "ITA")
#' )
#' }
#' @export
load_eurostat_data <- function(reload = FALSE,
                               cache_path = here("data/cache/eurostat_data.Rdata"),
                               variable_dictionary,
                               countries_considered) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  if (!reload && file.exists(cache_path)) {
    load(cache_path)
    return(eurostat_tidy_data)
  }

  eurostat_dictionary <- variable_dictionary %>%
    filter(source == "Eurostat") %>%
    select(-unit)

  eurostat_ids <- create_dbnomics_ids(
    templates = eurostat_dictionary$dbnomics_id,
    code_formats = eurostat_dictionary$countrycode,
    countries_considered
  )

  eurostat_raw_data <- batch_rdb(eurostat_ids)
  eurostat_tidy_data <- process_eurostat_data(eurostat_raw_data, eurostat_dictionary)

  save(eurostat_tidy_data, file = cache_path)
  return(eurostat_tidy_data)
}
