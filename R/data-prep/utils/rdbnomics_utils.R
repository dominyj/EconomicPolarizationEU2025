#' Batch download function for DBnomics
#' @param ids Vector of DBnomics IDs
#' @param batch_size Maximum characters per batch (default 8000)
#' @param sleep_time Seconds between batches (default 1)
#' @inheritParams rdb
#' @return Combined data frame from all batches
batch_rdb <- function(ids, batch_size = 7000, sleep_time = 1, ...) {
  chars_per_id <- mean(nchar(ids))
  ids_per_batch <- floor(batch_size / chars_per_id)
  n_batches <- ceiling(length(ids) / ids_per_batch)
  batch_results <- list()

  for (i in 1:n_batches) {
    start_idx <- (i-1) * ids_per_batch + 1
    end_idx <- min(i * ids_per_batch, length(ids))
    message(sprintf("Downloading batch %d of %d", i, n_batches))
    batch_results[[i]] <- rdb(ids[start_idx:end_idx], ...) %>%
      mutate(downloaded_at = Sys.time())
    Sys.sleep(sleep_time)
  }

  bind_rows(batch_results)
}


#' Combine data from all sources
#' @param ameco_data AMECO data frame
#' @param wdi_data WDI data frame
#' @param eurostat_data Eurostat data frame
#' @param oecd_data OECD data frame
#' @return Combined and processed data frame
#' @export
combine_rdb_data <- function(ameco_data, wdi_data, eurostat_data, oecd_data) {
  rbind(ameco_data, wdi_data, eurostat_data, oecd_data) %>%
    mutate(year = year(period)) %>%
    select(country, country_iso3c, variable_name, indicator, year, value,
           provider_code, indexed_at, downloaded_at, series_name, series_code, unit,
           '@frequency', dataset_code, dataset_name)
}

create_dbnomics_ids <- function(templates, code_formats, countries) {
  result_ids <- character()

  for (i in seq_along(templates)) {
    converted_countries <- custom_countrycode(countries,
                                              origin = "iso3c",
                                              destination = code_formats[i])

    new_ids <- sapply(converted_countries, function(x) {
      gsub("\\*", x, templates[i])
    })
    result_ids <- c(result_ids, new_ids)
  }

  return(unname(result_ids))
}


extract_indicator <- function(dbnomics_id, source) {
  if (source == "AMECO") {
    # Erster Versuch: Zwischen AMECO/ und nächstem /
    indicator <- str_extract(dbnomics_id, "(?<=AMECO/)[^/]+")

    # Wenn das nicht funktioniert, nimm das Ende nach dem letzten .
    if (is.na(indicator)) {
      indicator <- str_extract(dbnomics_id, "[^.]+$")
    }
    return(indicator)

  } else if (source == "WDI") {
    # Extrahiere Teil zwischen A- und -*
    indicator <- str_extract(dbnomics_id, "(?<=A-)[^-]+(?=-\\*)")
    return(indicator)

  } else if (source == "Eurostat") {
    indicator <- str_extract(dbnomics_id, "(?<=Eurostat/)[^/]+")
    return(indicator)

  } else if (source == "OECD") {
    indicator <- str_extract(dbnomics_id, "\\*\\..*$")
    return(indicator)

  }

  return(NA)  # Für unbekannte Quellen
}
