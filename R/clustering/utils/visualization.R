# Visualization Functions for Clustering Analysis
# This file contains functions for creating dendrograms and other visualizations

#------------------------------------------------------------------------------
# Required Packages
#------------------------------------------------------------------------------
library(ggplot2)
library(factoextra)
library(dendextend)
library(ggpubr)

#------------------------------------------------------------------------------
# Dendrogram Functions
#------------------------------------------------------------------------------

#' Create Documented Dendogram
#'
#' Generate dendrogram with comprehensive documentation
#' @param clustering_results Results from clustering analysis
#' @param filtered_data Filtered input data
#' @param panel_estimates Panel estimation results
#' @param clustering_params Clustering parameters
#' @param config Configuration settings
#' @param vars_per_line Variables per line in title
#' @return ggplot object containing the dendrogram
#' @export
create_documented_dendogram <- function(clustering_results,
                                        filtered_data,
                                        panel_estimates,
                                        clustering_params,
                                        config,
                                        vars_per_line = 1) {

  clustered_types <- unique(panel_estimates$type)
  var_info <- get_variable_info(filtered_data)

  title <- create_dendogram_title(
    var_info = var_info,
    clustered_types = clustered_types,
    clustering_params = clustering_params,
    config = config,
    weights = clustering_results$variable_weights,
    vars_per_line = vars_per_line
  )

  fviz_dend(
    clustering_results$clustering,
    main = title,
    xlab = "Countries",
    ylab = "Distance",
    k = clustering_params$n_clusters,
    cex = 0.75,
    rect = TRUE,
    rect_fill = TRUE,
    color_labels_by_k = TRUE,
    horiz = TRUE
  )
}

#' Create Custom Rectangle Dendrogram
#'
#' Helper function for custom dendrogram visualization
#' @param dend Dendrogram object
#' @param k Number of clusters
#' @param k_colors Colors for clusters
#' @param additional parameters for customization
#' @return ggplot2 layer
#' @export
custom_rect_dendrogram <- function(dend, k = NULL, h = NULL, k_colors = NULL,
                                   palette = NULL, rect_fill = FALSE, rect_lty = 2,
                                   lower_rect = -1.5, rect_width_offset = 3.5, ...) {

  if (missing(k_colors) & !is.null(palette))
    k_colors <- palette

  prop_k_height <- 0.5
  if (!dendextend::is.dendrogram(dend))
    stop("x is not a dendrogram object.")

  k <- if (!is.null(h)) {
    length(unique(dendextend::cutree(dend, h = h)))
  } else {
    k
  }

  tree_heights <- dendextend::heights_per_k.dendrogram(dend)[-1]
  tree_order <- stats::order.dendrogram(dend)

  if (is.null(k))
    stop("specify k")
  if (k < 2) {
    stop(gettextf("k must be between 2 and %d", length(tree_heights)),
         domain = NA)
  }

  cluster <- dendextend::cutree(dend, k = k)
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  which <- 1L:k

  # Create rectangle coordinates
  df <- map_df(seq_along(which), function(n) {
    next_k_height <- tree_heights[names(tree_heights) == k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1
    }

    tibble(
      xmin = m[which[n]] + 0.66,
      ymin = lower_rect - rect_width_offset,
      xmax = m[which[n] + 1] + 0.33,
      ymax = tree_heights[names(tree_heights) == k] * prop_k_height +
        next_k_height * (1 - prop_k_height)
    )
  })

  # Handle colors
  color <- k_colors
  if (length(color) == 1 && color == "cluster")
    color <- "default"
  if (ggpubr:::.is_col_palette(color))
    color <- ggpubr:::.get_pal(color, k = k)
  else if (length(color) > 1 & length(color) < k) {
    color <- rep(color, k)[1:k]
  }

  if (rect_fill) {
    fill <- color
    alpha <- 0.2
  } else {
    fill <- "transparent"
    alpha <- 0
  }

  df$color <- color
  df$cluster <- as.factor(paste0("c", 1:k))

  ggplot2::geom_rect(
    data = df,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    fill = fill,
    color = color,
    linetype = rect_lty,
    alpha = alpha,
    ...
  )
}

#------------------------------------------------------------------------------
# Plot Data Preparation
#------------------------------------------------------------------------------

#' Prepare Plot Data
#'
#' Transform clustering results for visualization
#' @param rolling_results Results from rolling window analysis
#' @return Data frame ready for plotting
#' @export
prepare_plot_data <- function(rolling_results) {
  map_df(names(rolling_results), function(period) {
    res <- rolling_results[[period]]

    # Get countries in dendrogram order
    countries <- res$clustering$order.lab

    # Reorder clusters according to dendrogram order
    clusters_reordered <- res$cluster_assignments[res$clustering$order]

    # Create data frame for this period
    tibble(
      period = period,
      country = countries,
      position = 1:length(countries),
      cluster = clusters_reordered,
      period_start = res$window_start,
      period_end = res$window_end
    )
  })
}

#' Create Dendogram Title
#'
#' Generate comprehensive title for dendrogram
#' @param var_info Variable information
#' @param additional parameters for title creation
#' @return String containing formatted title
#' @export
create_dendogram_title <- function(var_info,
                                   clustered_types,
                                   clustering_params,
                                   config,
                                   weights,
                                   vars_per_line = 4) {

  all_var_names <- c(
    "adj_wage_share", "unemployment", "current_account", "public_debt_to_gdp",
    "va_finance", "trade_exp_GDP", "abs_fdi_percGDP", "hs_eci", "bond_yield",
    "debt_total_corp", "gdp_growth", "gdp_real_pc_ppp"
  )

  # Check for missing variables
  included_vars <- unique(weights$variable)
  missing_vars <- setdiff(all_var_names, included_vars)
  missing_text <- if(length(missing_vars) > 0) {
    sprintf("Missing variables: %s", paste(missing_vars, collapse = ", "))
  } else {
    NULL
  }

  # Get time period info
  overall_min_year <- min(var_info$min_year)
  overall_max_year <- max(var_info$max_year)

  # Create variable information text
  vars_with_info <- weights %>%
    left_join(var_info, by = c("variable" = "variable_name")) %>%
    mutate(var_text = if_else(
      min_year != overall_min_year | max_year != overall_max_year,
      sprintf("%-15s (%s w: %.3f; %d-%d)",
              variable, type, avg_weight, min_year, max_year),
      sprintf("%-15s (%s w: %.3f)",
              variable, type, avg_weight)
    )) %>%
    arrange(desc(avg_weight))

  var_text <- c(
    "Variables and their relative weights in clustering:",
    split(vars_with_info$var_text,
          ceiling(seq_along(vars_with_info$var_text)/vars_per_line)) %>%
      sapply(paste, collapse = "\n") %>%
      paste(collapse = "\n")
  )

  # Create metadata text
  meta_text <- c(
    sprintf("Period: %d-%d", overall_min_year, overall_max_year),
    sprintf("Estimation: CCE(%s), %s SE",
            config$panel$cce_type,
            config$panel$se_type),
    sprintf("Clustering: %s distance, %s method",
            clustering_params$dist_method,
            clustering_params$cluster_method)
  )

  # Combine all text elements
  all_text <- if(!is.null(missing_text)) {
    c(missing_text, "", meta_text, "", var_text)
  } else {
    c(meta_text, "", var_text)
  }

  paste(all_text, collapse = "\n")
}

get_variable_info <- function(filtered_data) {
  filtered_data %>%
    group_by(variable_name) %>%
    summarise(
      min_year = min(year),
      max_year = max(year)
    )
}
