#' #' Process WDI raw data into tidy format
#' @param raw_data Data frame containing raw WDI data from DBnomics
#' @param dictionary Data frame containing variable definitions for WDI indicators
#' @return Processed and tidied WDI data frame
#' @importFrom dplyr mutate left_join filter select %>%
#' @importFrom countrycode countrycode
process_wdi_data <- function(raw_data, dictionary) {
  raw_data %>%
    mutate(
      country_iso3c = custom_countrycode(country, "wb", "iso3c"),
      country = countrycode(country, "wb", "country.name")
    ) %>%
    left_join(dictionary, by = "indicator") %>%
    filter(!is.na(value)) %>%
    select(country, country_iso3c, variable_name, indicator, period, value,
           provider_code, indexed_at, downloaded_at, series_name, series_code, unit,
           '@frequency', dataset_code, dataset_name)
}

#' Load and process World Development Indicators (WDI) data
#' @param reload Boolean indicating whether to fetch new data (TRUE) or use cached data (FALSE)
#' @param cache_path Path where cached data is stored
#' @param variable_dictionary Data frame containing variable definitions
#' @param countries_considered Vector of ISO3C country codes
#' @return Processed WDI data frame containing:
#'   \item{country}{Full country name}
#'   \item{country_iso3c}{ISO3C country code}
#'   \item{variable_name}{Name of the economic variable}
#'   \item{indicator}{DBnomics indicator code}
#'   \item{period}{Time period of observation}
#'   \item{value}{Observed value}
#'   \item{unit}{Unit of measurement}
#' @importFrom dplyr filter mutate %>%
#' @importFrom here here
#' @examples
#' \dontrun{
#' data <- load_wdi_data(
#'   variable_dictionary = my_dictionary,
#'   countries_considered = c("DEU", "FRA", "ITA")
#' )
#' }
#' @export
load_wdi_data <- function(reload = FALSE,
                          cache_path = here("data/cache/wdi_data.Rdata"),
                          variable_dictionary,
                          countries_considered) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)

  if (!reload && file.exists(cache_path)) {
    load(cache_path)
    return(wdi_tidy_data)
  }

  wdi_dictionary <- variable_dictionary %>%
    filter(source == "WDI")

  wdi_ids <- create_dbnomics_ids(
    templates = wdi_dictionary$dbnomics_id,
    code_formats = wdi_dictionary$countrycode,
    countries_considered
  )

  wdi_raw_data <- batch_rdb(wdi_ids)
  wdi_tidy_data <- process_wdi_data(wdi_raw_data, wdi_dictionary)

  save(wdi_tidy_data, file = cache_path)
  return(wdi_tidy_data)
}
