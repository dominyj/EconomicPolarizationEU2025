# Cluster Analysis Functions
# This file contains functions for performing hierarchical clustering

#------------------------------------------------------------------------------
# Required Packages
#------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(countrycode)

#------------------------------------------------------------------------------
# Core Clustering Functions
#------------------------------------------------------------------------------

#' Perform Complete Cluster Analysis
#'
#' Main function for performing the entire clustering analysis
#' @param data Panel data
#' @param var_names Variables to include in analysis
#' @param config Configuration parameters
#' @return List of results including filtered data, estimates, clustering, dendogram
#' @export
perform_cluster_analysis <- function(data, var_names, config) {
  # 1. Data preparation
  filtered_data <- prepare_clustering_data(
    data = data,
    start_year = config$time$start_year,
    end_year = config$time$end_year,
    var_names = var_names
  )

  # 2. Panel estimation
  panel_estimates <- filtered_data %>%
    split(.$variable_name) %>%
    map_dfr(estimate_panel,
            .id = "variable_name",
            se_type = config$panel$se_type,
            cce_type = config$panel$cce_type,
            time_fe = config$panel$time_fe,
            coef_select = config$panel$coef_select)

  # 3. Perform clustering
  clustering_results <- perform_clustering(
    data = transform_panel_to_clustering(panel_estimates),
    dist_method = config$clustering$dist_method,
    cluster_method = config$clustering$cluster_method,
    n_clusters = config$clustering$n_clusters
  )

  # 4. Create visualization
  documented_dendo <- create_documented_dendogram(
    clustering_results = clustering_results,
    filtered_data = filtered_data,
    panel_estimates = panel_estimates,
    clustering_params = config$clustering,
    config = config
  )

  # Return results
  list(
    filtered_data = filtered_data,
    panel_estimates = panel_estimates,
    clustering_results = clustering_results,
    dendogram = documented_dendo
  )
}


#' Perform Hierarchical Clustering
#'
#' Execute the hierarchical clustering analysis with specified parameters
#' @param data Wide format data
#' @param dist_method Either "standard" or "se_weighted"
#' @param cluster_method Clustering algorithm (e.g., "ward", "complete")
#' @param n_clusters Number of clusters to extract
#' @return List containing clustering results
#' @export
perform_clustering <- function(data,
                               dist_method = c("standard", "se_weighted"),
                               cluster_method = "ward",
                               n_clusters = 6) {

  dist_method <- match.arg(dist_method)

  # Calculate distances and weights based on method
  if(dist_method == "se_weighted") {
    scaled_estimates <- scale_with_se(data)
    weights <- calculate_variable_weights(scaled_estimates, dist_method = dist_method)
    dist_matrix <- dist_weighted_by_se(scaled_estimates,
                                       method = "mean",
                                       na_handling = "infinite_se")
  } else {
    scaled_data <- scale(select(data, matches("^(fe|cce)_.*_est$")))
    dist_matrix <- dist(scaled_data)
    weights <- calculate_variable_weights(scaled_data, dist_method = dist_method)
  }

  # Convert country codes
  attr(dist_matrix, "Labels") <- countrycode(
    attr(dist_matrix, "Labels"),
    "iso3c",
    "country.name"
  )

  # Perform clustering
  clustering <- agnes(dist_matrix, method = cluster_method)
  cluster_assignments <- cutree(clustering, k = n_clusters)

  list(
    clustering = clustering,
    cluster_assignments = cluster_assignments,
    country_order = clustering$order,
    dist_matrix = dist_matrix,
    variable_weights = weights
  )
}

#' Filter panel data for clustering analysis
#' @param data Data frame containing the panel data
#' @param start_year Starting year of the window
#' @param end_year Optional end year (exclusive)
#' @param variables Character vector of variable names to include
#' @return Filtered data frame
prepare_clustering_data <- function(data,
                                    start_year,
                                    end_year,
                                    var_names) {

  filtered_data <- data %>%
    dplyr::filter(year >= start_year,
                  year <= end_year,
                  variable_name %in% var_names)

  return(filtered_data)
}

#' Transform Panel Estimates to Clustering Format
#'
#' @param panel_estimates Data frame from estimate_panel()
#' @param id_col Name of the identifier column
#' @return Wide format data frame ready for clustering
#' @export
transform_panel_to_clustering <- function(panel_estimates,
                                          id_col = "country_iso3c") {
  panel_estimates %>%
    pivot_wider(
      id_cols = all_of(id_col),
      names_from = c(variable_name, type),
      values_from = c(estimate, std_error),
      names_glue = "{type}_{variable_name}_{ifelse(.value == 'estimate', 'est', 'se')}"
    ) %>%
    column_to_rownames(id_col)
}

#' Perform Rolling Window Clustering
#'
#' @param data Input panel data
#' @param start_year Start of analysis period
#' @param end_year End of analysis period
#' @param window_size Size of rolling window
#' @param step_size Step size for window
#' @param var_names Variables to include
#' @param n_clusters Number of clusters
#' @param additional parameters for estimation and clustering
#' @return List of clustering results for each window
#' @export
perform_rolling_window_clustering <- function(data,
                                              start_year,
                                              end_year,
                                              window_size = 5,
                                              step_size = 1,
                                              var_names,
                                              n_clusters = 6,
                                              se_type = "clustered",
                                              cce_type = "none",
                                              time_fe = TRUE,
                                              coef_select = "fe",
                                              cluster_method = "ward") {

  # Generate sequence of starting years
  start_years <- seq(start_year, end_year - window_size + 1, by = step_size)

  # Process each window
  results <- list()
  for(year in start_years) {
    # Prepare data and estimate panels
    clustering_data <- prepare_window_estimates(
      data = data,
      start_year = year,
      end_year = year + window_size,
      var_names = var_names,
      se_type = se_type,
      cce_type = cce_type,
      time_fe = time_fe,
      coef_select = coef_select
    )

    # Perform clustering
    clustering_result <- perform_clustering(
      data = clustering_data,
      dist_method = "se_weighted",
      cluster_method = cluster_method,
      n_clusters = n_clusters
    )

    # Add window information
    clustering_result$window_start <- year
    clustering_result$window_end <- year + window_size - 1

    # Store results
    results[[as.character(year)]] <- clustering_result
  }

  return(results)
}

#' Prepare Window Estimates
#'
#' Helper function for rolling window analysis
#' @param data Panel data
#' @param start_year Window start
#' @param end_year Window end
#' @param var_names Variables to include
#' @param estimation parameters
#' @return Data frame ready for clustering
#' @export
prepare_window_estimates <- function(data,
                                     start_year,
                                     end_year,
                                     var_names,
                                     se_type = "clustered",
                                     cce_type = "none",
                                     time_fe = TRUE,
                                     coef_select = "fe") {

  # Filter data
  window_data <- prepare_clustering_data(
    data = data,
    start_year = start_year,
    end_year = end_year,
    var_names = var_names
  )

  # Estimate panels
  panel_estimates <- window_data %>%
    split(.$variable_name) %>%
    map_dfr(
      ~estimate_panel(
        data = .x,
        se_type = se_type,
        cce_type = cce_type,
        time_fe = time_fe,
        coef_select = coef_select
      ),
      .id = "variable_name"
    )

  # Transform to clustering format
  clustering_data <- transform_panel_to_clustering(panel_estimates)

  return(clustering_data)
}
