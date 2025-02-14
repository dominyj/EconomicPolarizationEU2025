#' Download EU KLEMS national accounts data
#'
#' @param data_dir Directory where data should be stored
#' @return Path to the downloaded file or NULL if download fails
#' @export
download_klems_data <- function(data_dir = "data/raw/EU_KLEMS") {
  # Create directory if needed
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  # Define file path
  file_path <- file.path(data_dir, "national accounts.dta")

  # Try to download the file
  tryCatch({
    # Download using download.file
    download.file(
      url = "https://www.dropbox.com/s/78sucgpgnh5amju/national%20accounts.dta?dl=1",
      destfile = file_path,
      mode = "wb"
    )
    message("Successfully downloaded EU KLEMS data")
    return(file_path)
  }, error = function(e) {
    warning("Failed to download EU KLEMS data: ", e$message)
    return(NULL)
  })
}

#' Process raw EU KLEMS data
#'
#' @param data Raw EU KLEMS data frame
#' @return Processed data frame in standard format
#' @importFrom dplyr select filter mutate %>%
process_klems_data <- function(data, countries_considered) {

  # Filter out EU/EA aggregates first
  filtered_data <- data %>%
    filter(!str_detect(geo_code, "^(EU|EA)"))

  filtered_data %>%
    select(geo_code, geo_name, year, nace_r2_code, nace_r2_name, VA_CP) %>%
    filter(nace_r2_name %in% c("Total - all NACE activities",
                               "Financial and insurance activities")) %>%
    mutate(
      country_iso3c = countrycode(geo_code, "eurostat", "iso3c"),
      country = countrycode(geo_code, "eurostat", "country.name")
    ) %>%
    filter(country_iso3c %in% countries_considered) %>%
    arrange(geo_code, nace_r2_code, year) %>%
    pivot_wider(
      id_cols = c(country, country_iso3c, year),
      names_from = nace_r2_code,
      values_from = VA_CP
    ) %>%
    mutate(
      value = K/TOT*100,
      variable_name = "va_finance",
      indicator = "Share of Value-added Finance to Total",
      provider_code = "EU_KLEMS",
      indexed_at = NA,
      downloaded_at = Sys.time(),
      series_name = "EU KLEMS",
      series_code = "EU_KLEMS",
      unit = "percentage",
      '@frequency' = "Annual",
      dataset_code = NA,
      dataset_name = "EU KLEMS"
    ) %>%
    select(country, country_iso3c, variable_name, indicator, year, value,
           provider_code, indexed_at, downloaded_at, series_name, series_code,
           unit, '@frequency', dataset_code, dataset_name)
}

#' Load and process EU KLEMS data
#'
#' @param reload Boolean indicating whether to fetch new data (TRUE) or use cached data (FALSE)
#' @param countries_considered Vector of ISO3C country codes to filter data for
#' @param cache_path Path where cached data is stored
#' @return Processed EU KLEMS data frame
#' @export
load_klems_data <- function(reload = FALSE,
                            countries_considered = NULL,
                            cache_path = here("data/cache/klems_data.Rdata")) {
  raw_path <- here("data/raw/EU_KLEMS/national_data")
  raw_data_path <- here("data/raw/EU_KLEMS/national accounts.dta")

  # First check if raw data exists
  if (!file.exists(raw_data_path)) {
    message("Raw data not found, attempting download")
    raw_data_path <- download_klems_data()
    if (is.null(raw_data_path)) {
      stop("Neither download nor local file available")
    }
  }


  # Read and process data
  klems_raw <- haven::read_dta(raw_data_path)
  klems_tidy <- process_klems_data(klems_raw, countries_considered)

  # Save to cache
  save(klems_tidy, file = cache_path)
  return(klems_tidy)
}
