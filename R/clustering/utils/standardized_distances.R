#' Scale Fixed Effects and Standard Errors
#'
#' Standardizes fixed effects and adjusts standard errors accordingly
#'
#' @param data_with_se A data frame containing fixed effects (*_est) and standard errors (*_se)
#' @return A data frame with standardized fixed effects and adjusted standard errors
#' @examples
#' \dontrun{
#' scaled_data <- scale_with_se(fe_data)
#' }
scale_with_se <- function(data_with_se) {
  # Identify pairs of est and se columns based on the pattern type_varname_est/se
  est_cols <- grep("_est$", names(data_with_se), value = TRUE)
  base_names <- sub("_est$", "", est_cols)

  scaled_data <- data_with_se
  for(base in base_names) {
    est_col <- paste0(base, "_est")
    se_col <- paste0(base, "_se")

    est_mean <- mean(data_with_se[[est_col]], na.rm = TRUE)
    est_sd <- sd(data_with_se[[est_col]], na.rm = TRUE)

    # Calculate scaled values
    scaled_data[[est_col]] <- (data_with_se[[est_col]] - est_mean) / est_sd
    scaled_data[[se_col]] <- data_with_se[[se_col]] / est_sd
  }
  scaled_data
}

#' Calculate SE-Weighted Distances
#'
#' @param scaled_data A data frame with standardized estimates (*_est) and
#'   standard errors (*_se)
#' @param method Character string specifying the distance method. Either "mean" for
#'   mean absolute t-statistics or "rms" for root mean square t-statistics.
#' @param na_handling Character string specifying how to handle NAs. Either "omit" to
#'   exclude NA values from calculation or "infinite_se" to treat them as having
#'   infinite standard error (zero contribution to distance).
#' @return A dist object containing the SE-weighted distances between countries
dist_weighted_by_se <- function(scaled_data,
                                method = c("mean", "rms"),
                                na_handling = c("omit", "infinite_se")) {
  method <- match.arg(method)
  na_handling <- match.arg(na_handling)

  # Identify pairs of est and se columns
  est_cols <- grep("_est$", names(scaled_data), value = TRUE)
  base_names <- sub("_est$", "", est_cols)
  se_cols <- paste0(base_names, "_se")

  if (length(est_cols) != length(se_cols)) {
    stop("Number of estimate columns must match number of SE columns")
  }

  n_countries <- nrow(scaled_data)
  distances <- matrix(0, nrow = n_countries, ncol = n_countries)
  rownames(distances) <- rownames(scaled_data)
  colnames(distances) <- rownames(scaled_data)

  for(i in 1:n_countries) {
    for(j in 1:n_countries) {
      if(i < j) {
        t_stats <- numeric(length(base_names))
        for(v in seq_along(base_names)) {
          est_col <- paste0(base_names[v], "_est")
          se_col <- paste0(base_names[v], "_se")

          # Get values
          est_i <- scaled_data[[est_col]][i]
          est_j <- scaled_data[[est_col]][j]
          se_i <- scaled_data[[se_col]][i]
          se_j <- scaled_data[[se_col]][j]

          # Handle missing values
          if(na_handling == "infinite_se" &&
             (is.na(est_i) || is.na(est_j) || is.na(se_i) || is.na(se_j))) {
            t_stats[v] <- 0  # Missing value leads to zero contribution
          } else {
            diff <- abs(est_i - est_j)
            combined_se <- sqrt(se_i^2 + se_j^2)
            t_stats[v] <- diff / combined_se
          }
        }

        # Calculate distance based on chosen method
        distances[i,j] <- if(method == "mean") {
          if(na_handling == "omit") {
            mean(t_stats, na.rm = TRUE)
          } else {
            mean(t_stats)  # All values are defined (0 or actual t-stat)
          }
        } else {  # method == "rms"
          if(na_handling == "omit") {
            sqrt(mean(t_stats^2, na.rm = TRUE))
          } else {
            sqrt(mean(t_stats^2))
          }
        }
        distances[j,i] <- distances[i,j]
      }
    }
  }
  as.dist(distances)
}

#' Calculate Average Weights from Standard Errors
#'
#' @param scaled_data A data frame with standardized standard errors (columns with '_se' suffix)
#' @return A tibble with variable names and their normalized weights (1/combined_se) that sum to 1
calculate_variable_weights <- function(scaled_data, dist_method = "se_weighted") {
  if (dist_method == "standard") {
    var_info <- data.frame(
      variable <- gsub("^fe_|_est$", "", colnames(scaled_data)),
      type = sub("^([^_]+).*", "\\1", colnames(scaled_data))
    )
    weights <- rep(1, ncol(scaled_data))
  } else {
    # Select SE columns
    se_cols <- grep("_se$", names(scaled_data), value = TRUE)
    base_names <- sub("_se$", "", se_cols)

    # Extract variable names and types
    var_info <- data.frame(
      full_name = base_names,
      type = sub("^([^_]+)_.*", "\\1", base_names),
      variable = sub("^[^_]+_(.*)", "\\1", base_names)
    )

    # Calculate weights for each variable
    weights <- sapply(seq_along(se_cols), function(i) {
      se <- scaled_data[[se_cols[i]]]
      # Calculate combined SE for all pairs
      pairs <- combn(length(se), 2)
      combined_ses <- sqrt(se[pairs[1,]]^2 + se[pairs[2,]]^2)
      # Calculate average weight
      mean(1/combined_ses, na.rm = TRUE)
    })

  }
  # Create output tibble with type information
  tibble(
    variable = var_info$variable,
    type = var_info$type,
    avg_weight = weights / sum(weights)
  ) %>%
    arrange(desc(avg_weight))

}
