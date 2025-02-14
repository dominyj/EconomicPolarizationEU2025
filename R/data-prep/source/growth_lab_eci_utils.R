#' Load and process ECI data
#'
#' @param reload Boolean indicating whether to fetch new data (TRUE) or use cached data (FALSE)
#' @param countries_considered Vector of ISO3C country codes to filter data for
#' @return Processed ECI data frame
#' @export
load_eci_data <- function(reload = FALSE,
                          countries_considered = NULL) {

  # Define cache path
  cache_path <- here("data/cache/eci_data.Rdata")

  # Check if cached data exists and reload is not forced
  if (!reload && file.exists(cache_path)) {
    load(cache_path)

    # If countries are specified, filter the cached data
    if (!is.null(countries_considered)) {
      eci_data_tidy <- eci_data_tidy %>%
        filter(country_iso3c %in% countries_considered)
    }

    return(eci_data_tidy)
  }

  # Get raw data
  eci_data_raw <- get_raw_eci_data(update = reload)

  # Convert to standardized format
  eci_data_all <- convert_to_rdb_format(eci_data_raw)

  # Calculate distances and combine
  eci_data_all <- bind_rows(
    eci_data_all,
    calculate_eci_distances(eci_data_all, "hs_eci"),
    calculate_eci_distances(eci_data_all, "sitc_eci")
  )

  # Filter for specified countries if provided
  if (!is.null(countries_considered)) {
    eci_data_tidy <- eci_data_all %>%
      filter(country_iso3c %in% countries_considered)
  } else {
    eci_data_tidy <- eci_data_all
  }

  # Save to cache
  save(eci_data_tidy, file = cache_path)

  return(eci_data_tidy)
}

#' Get raw ECI Ranking Data
#' @inheritParams load_eci_data
#' @param update logical, whether to force update (default: FALSE)
#' @param data_dir character, directory for data storage
#' @return tibble with raw ECI data including timestamps
get_raw_eci_data <- function(update = FALSE,
                             data_dir = "data/raw/eci_ranking") {
  raw_file <- file.path(data_dir, "rankings.rds")

  if (!update && file.exists(raw_file)) {
    message("Loading existing raw ECI data")
    return(readRDS(raw_file))
  }

  message("Downloading fresh ECI data")
  stata_file <- download_raw_eci_data(data_dir)
  data_with_timestamps <- add_timestamps_eci(stata_file)
  saveRDS(data_with_timestamps, raw_file)

  return(data_with_timestamps)
}

#' Download raw ECI Ranking Data
#'
#' @param raw_dir character, directory for raw data storage
#' @return path to downloaded file
#' @export
download_raw_eci_data <- function(raw_dir = "data/raw/eci_ranking") {
  if (!require("dataverse")) {
    stop("Package 'dataverse' is required. Please install it first.")
  }

  # Create directory if needed
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  # Set up Dataverse connection
  Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

  # Define file paths
  data_file <- file.path(raw_dir, "rankings.dta")
  meta_file <- file.path(raw_dir, "data_dictionary.pdf")

  # Download files
  tryCatch({
    writeBin(
      get_file("rankings.tab", "doi:10.7910/DVN/XTAQMC"),
      data_file
    )
    writeBin(
      get_file("data_dictionary.pdf", "doi:10.7910/DVN/XTAQMC"),
      meta_file
    )
    message("Raw ECI ranking data successfully downloaded")
    return(data_file)
  }, error = function(e) {
    stop("Error downloading files: ", e$message)
  })
}

#' Add timestamps to ECI data
#'
#' @param data_file path to the raw data file
#' @return tibble with timestamps added
#' @export
add_timestamps_eci <- function(data_file) {
  if (!require("haven")) stop("Package 'haven' is required")
  if (!require("dplyr")) stop("Package 'dplyr' is required")

  # Get metadata timestamp
  indexed_at <- get_eci_metadata()

  # Read and add timestamps
  read_dta(data_file) %>%
    mutate(
      downloaded_at = Sys.time(),
      indexed_at = indexed_at
    )
}

#' Get ECI dataset metadata from Dataverse
#'
#' @return POSIXct timestamp of last dataset modification
#' @export
get_eci_metadata <- function() {
  if (!require("dataverse")) {
    stop("Package 'dataverse' is required. Please install it first.")
  }

  # Get dataset metadata
  tryCatch({
    dataset <- get_dataset("doi:10.7910/DVN/XTAQMC")

    # Extract the publication date from metadata
    indexed_at <- as.POSIXct(dataset$distributionDate)

    return(indexed_at)
  }, error = function(e) {
    stop("Error fetching metadata: ", e$message)
  })
}

#' Convert ECI data to RDB format
#'
#' @param data raw ECI data with timestamps
#' @return tibble in RDB-compatible format
#' @export
convert_to_rdb_format <- function(data) {
  if (!require("tidyr")) stop("Package 'tidyr' is required")
  if (!require("countrycode")) stop("Package 'countrycode' is required")
  if (!require("readr")) stop("Package 'readr' is required")

  # Versuche Länderklassifikationen zu laden
  country_classifications <- tryCatch({
    # Prüfe ob Datei existiert
    file_path <- "data/raw/growth_lab_classification/location_country.csv"
    if (!file.exists(file_path)) {
      # Versuche Download
      download_growth_lab_classification()
    }
    # Lade Datei
    readr::read_csv(file_path, show_col_types = FALSE)
  }, error = function(e) {
    message("Could not load country classifications, falling back to countrycode package")
    NULL
  })

  # Verarbeite Daten
  processed_data <- data %>%
    mutate(sitc_eci_rank = as.double(sitc_eci_rank)) %>%
    pivot_longer(
      cols = any_of(c("sitc_eci", "hs_eci", "growth_proj", "sitc_eci_rank","hs_eci_rank")),
      names_to = "variable_name",
      values_to = "value"
    ) %>%
    filter(!is.na(value))

  # Füge Ländercode-Informationen hinzu
  if (!is.null(country_classifications)) {
    processed_data <- processed_data %>%
      left_join(
        country_classifications %>%
          select(country_id, iso3_code),
        by = c("country_id")
      ) %>%
      mutate(
        country = countrycode(iso3_code, "iso3c", "country.name"),
        country_iso3c = iso3_code
      )
  } else {
    # Fallback zur countrycode-Funktion
    processed_data <- processed_data %>%
      mutate(
        country = countrycode(country_id, "un", "country.name"),
        country_iso3c = countrycode(country_id, "un", "iso3c")
      )
  }

  # Rest der Standardisierung
  processed_data %>%
    mutate(
      indicator = case_when(
        variable_name == "sitc_eci" ~ "Economic Complexity Index (SITC)",
        variable_name == "hs_eci" ~ "Economic Complexity Index (HS)",
        variable_name == "growth_proj" ~ "Growth Projections",
        variable_name == "sitc_eci_rankings" ~ "Economic Complexity Ranking (SITC)",
        variable_name == "hs_eci_rankings" ~ "Economic Complexity Ranking (HS)"
      ),
      provider_code = "HARVARD_GROWTH_LAB",
      series_name = "Growth Projections and Complexity Rankings",
      series_code = "ECI",
      unit = case_when(
        str_detect(variable_name, "rankings$") ~ "rank",
        variable_name == "growth_proj" ~ "percent",
        TRUE ~ "index"
      ),
      `@frequency` = "annual",
      dataset_code = "AECD",
      dataset_name = "Atlas of Economic Complexity Dataverse"
    ) %>%
    select(
      country,
      country_iso3c,
      variable_name,
      indicator,
      year,
      value,
      provider_code,
      indexed_at,
      downloaded_at,
      series_name,
      series_code,
      unit,
      `@frequency`,
      dataset_code,
      dataset_name
    )
}

#' Process ECI Ranking Data
#'
#' @param input_file character, path to raw data file
#' @param output_file character, path where to save processed data
#' @return invisible(TRUE) if successful
#' @export
process_eci_data <- function(input_file,
                             output_file = "data/processed/eci_rankings.rds") {

  # Add timestamps to raw data
  data_with_timestamps <- add_timestamps_eci(input_file)

  # Standardize format
  processed_data <- standardize_eci_format(data_with_timestamps)

  # Create directory if needed
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  # Save processed data
  saveRDS(processed_data, file = output_file)

  message(sprintf("ECI ranking data successfully processed and saved to %s",
                  output_file))

  invisible(TRUE)
}



# Calculate distance to leader and p95 for both ECI measures
calculate_eci_distances <- function(data, eci_var) {
  # Erstelle Basis für beide Distanzmaße
  base_data <- data %>%
    filter(variable_name == eci_var) %>%
    group_by(year) %>%
    mutate(
      max_eci = max(value, na.rm = TRUE),
      p95_eci = quantile(value, 0.95, na.rm = TRUE),
      distance_to_leader = value - max_eci,
      distance_to_p95 = value - p95_eci
    ) %>%
    ungroup()

  # Funktion für die Formatierung der einzelnen Distanzmaße
  format_distance <- function(distance_data, distance_type, distance_values) {
    new_var_name <- paste0(eci_var, "_distance_to_", distance_type)

    distance_data %>%
      select(country, country_iso3c, year) %>%
      mutate(
        distance_value = distance_values,
        variable_name = new_var_name,
        indicator = paste0(str_to_upper(eci_var), " Distance to ", str_to_title(distance_type), " (own calculations)"),
        value = distance_value,
        provider_code = "HARVARD_GROWTH_LAB_OWN_CALC",
        indexed_at = first(data$indexed_at),
        downloaded_at = first(data$downloaded_at),
        series_name = paste0("Distance to ", str_to_title(distance_type), " based on ", str_to_upper(eci_var)),
        series_code = new_var_name,
        unit = first(data$unit),
        `@frequency` = first(data$`@frequency`),
        dataset_code = first(data$dataset_code),
        dataset_name = paste0(first(data$dataset_name), " (with own calculations)")
      ) %>%
      select(-distance_value) %>%
      select(names(data))
  }

  # Erstelle beide Distanzmaße
  leader_distance <- format_distance(base_data, "leader", base_data$distance_to_leader)
  p95_distance <- format_distance(base_data, "p95", base_data$distance_to_p95)

  # Kombiniere beide Datensätze
  bind_rows(leader_distance, p95_distance)
}

#' Download Harvard Growth Lab Classification Data
#'
#' @param update logical, whether to force update (default: FALSE)
#' @param data_dir character, directory for data storage
#' @return invisible(TRUE)
#' @export
download_growth_lab_classification <- function(update = FALSE,
                                               data_dir = "data/raw/growth_lab_classification") {

  if (!require("dataverse")) {
    stop("Package 'dataverse' is required")
  }

  # Liste der zu downloadenden Dateien
  files_to_download <- data.frame(
    filename = c(
      "location_country.csv",
      "location_group.csv",
      "location_group_member.csv",
      "product_hs12.csv",
      "product_hs92.csv",
      "product_sitc.csv",
      "product_services_unilateral.csv"
    ),
    originalname = c(
      "location_country.tab",
      "location_group.tab",
      "location_group_member.tab",
      "product_hs12.tab",
      "product_hs92.tab",
      "product_sitc.tab",
      "product_services_unilateral.tab"
    )
  )

  # Prüfe ob Update notwendig
  if (!update && all(file.exists(file.path(data_dir, files_to_download$filename)))) {
    message("Classification files already exist. Use update = TRUE to force download.")
    return(invisible(TRUE))
  }

  # Erstelle Verzeichnis falls notwendig
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  # Set up Dataverse connection
  Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

  # Download files
  tryCatch({
    # Lade auch data dictionary
    writeBin(
      get_file("data_dictionary.pdf", "doi:10.7910/DVN/3BAL1O"),
      file.path(data_dir, "data_dictionary.pdf")
    )

    # Lade alle Datendateien
    for (i in seq_len(nrow(files_to_download))) {
      message(sprintf("Downloading %s...", files_to_download$filename[i]))
      writeBin(
        get_file(files_to_download$originalname[i], "doi:10.7910/DVN/3BAL1O"),
        file.path(data_dir, files_to_download$filename[i])
      )
    }

    message("All classification files successfully downloaded")
    invisible(TRUE)

  }, error = function(e) {
    stop("Error downloading files: ", e$message)
  })
}
