#' Estimate Panel Models with Fixed Effects and Common Correlated Effects
#'
#' @param formula A formula object specifying the model
#' @param data A data frame containing the model variables
#' @param group_var Character string specifying the group identifier variable
#' @param time_var Character string specifying the time variable
#' @param se_type Type of standard errors: "normal", "robust", or "clustered"
#' @param group_fe Logical; include group fixed effects
#' @param cce_type Type of CCE estimation: "none", "standard", or "interactive"
#' @param time_fe Logical; include time fixed effects
#' @param coef_select Character vector specifying which coefficients to return: "all", "fe", "cce", "interaction"
#'
#' @return A tibble containing the estimated coefficients, standard errors, and types
#' @export
#'
#' @examples
#' estimate_panel(gdp ~ population, data = panel_data,
#'               group_var = "country", time_var = "year")
estimate_panel <- function(
    data,
    formula = value ~ 0,
    group_var = "country_iso3c",
    time_var = "year",
    se_type = c("clustered", "normal", "robust"),
    group_fe = TRUE,
    cce_type = c("none", "standard", "interactive"),
    time_fe = TRUE,
    coef_select = c("all", "fe", "cce", "interaction")
) {
  # Input validation
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
  if (!all(c(group_var, time_var) %in% names(data))) {
    stop("group_var and time_var must exist in data")
  }
  if (!is.character(group_var) || !is.character(time_var)) {
    stop("group_var and time_var must be character strings")
  }

  se_type <- match.arg(se_type)
  cce_type <- match.arg(cce_type)
  coef_select <- match.arg(coef_select, several.ok = TRUE)

  # Extract dependent variable
  dep_var <- all.vars(formula)[1]
  if (!dep_var %in% names(data)) stop("Dependent variable not found in data")

  # Check for invalid combinations
  if (cce_type %in% c("interactive", "standard") && time_fe) {
    warning("Time fixed effects are ignored when using CCE estimation (time effects are implicitly captured by cross-sectional means)")
    time_fe <- FALSE
  }

  # Extract and clean RHS variables
  rhs_vars <- all.vars(formula[[3]]) %>%
    .[!grepl(group_var, .)]
  has_controls <- length(rhs_vars) > 0


  # Identify numeric variables from formula
  numeric_vars <- if (length(rhs_vars) > 0) {
    rhs_vars[sapply(data[rhs_vars], is.numeric)]
  } else {
    character(0)
  }

  # Prepare data
  work_data <- data %>%
    group_by(!!sym(time_var)) %>%
    mutate(
      y_mean = mean(!!sym(dep_var), na.rm = TRUE),
      across(all_of(numeric_vars),
             ~mean(., na.rm = TRUE),
             .names = "{.col}_mean")
    ) %>%
    ungroup()

  # Build formula
  fe_terms <- c(
    if(group_fe) paste0("factor(", group_var, ")"),
    if(time_fe) paste0("factor(", time_var, ")"),
    if(cce_type == "standard") c(
      "y_mean",
      if(length(numeric_vars) > 0) paste0(numeric_vars, "_mean")
    ),
    if(cce_type == "interactive") c(
      paste0("y_mean:factor(", group_var, ")"),
      if(length(numeric_vars) > 0) paste0(numeric_vars, "_mean")
    )
  )

  # Create formula without intercept
  if (length(fe_terms) > 0) {
    additional_terms <- paste(fe_terms, collapse = " + ")
    right_side <- paste("~ 0 + . +", additional_terms)
    model_formula <- update(formula, as.formula(right_side))
  } else {
    model_formula <- update(formula, ~0 + .)
  }

  # Print model specification
  message(sprintf("Estimating panel model:\nFormula: %s\nStandard errors: %s",
                  deparse1(model_formula), se_type))

  # Estimate model
  model <- lm(model_formula, data = work_data)

  # Create panel data structure for plm
  pdata <- pdata.frame(work_data, index = c(group_var, time_var))

  # Calculate variance-covariance matrix
  vcov_matrix <- switch(se_type,
                        "robust" = sandwich::vcovHC(model),
                        "clustered" = vcovHC(model, type = "HC1",
                                             cluster = "group",
                                             pdata = pdata),
                        vcov(model)
  )


  # Prepare results
  coefs <- coef(model)
  ses <- sqrt(diag(vcov_matrix))

  results <- tibble(
    coefficient = names(coefs),
    estimate = coefs,
    std_error = ses,
    type = case_when(
      grepl(paste0("factor\\(", group_var, "\\)[A-Z]+:y_mean"), coefficient) ~ "cce",
      grepl(paste0("^factor\\(", group_var, "\\)[A-Z]+$"), coefficient) ~ "fe",
      grepl(paste0("factor\\(", group_var, "\\)[A-Z]+:"), coefficient) ~ "interaction",
      TRUE ~ "other"
    )
  ) %>%
    filter(type %in% c("fe", "cce", "interaction")) %>%
    mutate(
      !!group_var := str_extract(coefficient,
                                 paste0("(?<=", group_var, "\\))[A-Z]+(?=:|$)")),
      coefficient = NULL
    ) %>%
    select(!!sym(group_var), type, estimate, std_error)  # Sort by group variable

  # Filter coefficients
  if (!("all" %in% coef_select)) {
    results <- results %>%
      filter(type %in% coef_select)
  }

  return(results)
}
