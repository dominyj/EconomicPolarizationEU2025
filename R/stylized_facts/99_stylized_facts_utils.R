#' Get Plot Variables and Labels
#'
#' @description
#' Creates a mapping between variable names and their display labels for plotting.
#' Can use automatic labels from the data or manual overrides.
#'
#' @param data Optional dataframe containing variable information
#' @param manual_labels Optional list of manual label overrides
#'
#' @return A list mapping variable names to their display labels
#' @export
get_plot_variables <- function(data = NULL, manual_labels = NULL) {
  if(is.null(manual_labels)) {
    manual_labels <- list()
  }

  if(!is.null(data)) {
    # Create automatic labels from unique variable names
    auto_labels <- data %>%
      distinct(variable_name) %>%
      pull(variable_name) %>%
      set_names() %>%
      map(~data %>%
            filter(variable_name == .x) %>%
            pull(series_name) %>%
            first())

    # Override auto labels with manual labels where provided
    labels <- modifyList(auto_labels, manual_labels)

    return(labels)
  }

  # Return only manual labels if no data provided
  return(manual_labels)
}

#' Calculate Indicator Statistics
#'
#' @description
#' Prepares statistical summaries for indicators, including weighted means and deviations.
#'
#' @param data Input data frame
#' @param var_name Name of variable to analyze
#' @param group_var Grouping variable (default: "c_group")
#' @param show_deviation Whether to show deviation from mean
#' @param stat_type Type of statistic ("weighted_mean", "mean", or "sum")
#' @param total_data Optional reference data for deviations
#'
#' @return Data frame with calculated statistics
#' @export
prepare_indicator_stats <- function(data,
                                    var_name,
                                    group_var = "c_group",
                                    show_deviation = TRUE,
                                    stat_type = "weighted_mean",
                                    total_data = NULL) {

  # Helper function for statistic calculation
  calc_stat <- function(x, weights, type) {
    if(type == "weighted_mean") {
      weighted.mean(x, weights, na.rm = TRUE)
    } else if(type == "mean") {
      mean(x, na.rm = TRUE)
    } else if(type == "sum") {
      sum(x, na.rm = TRUE)
    }
  }

  # Calculate weighted statistics
  weighted_stats <- data %>%
    group_by(!!sym(group_var), year) %>%
    summarise(
      stat_value = calc_stat(!!sym(var_name), population, stat_type),
      n = n(),
      sd = if(n > 1) sd(!!sym(var_name), na.rm = TRUE) else 0,
      total_population = sum(population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sd_lower = stat_value - sd,
      sd_upper = stat_value + sd
    )

  # Calculate deviations if requested
  if (show_deviation) {
    total_avg <- if (!is.null(total_data)) {
      total_data %>%
        group_by(year) %>%
        summarise(
          total_stat = calc_stat(!!sym(var_name), population, stat_type),
          .groups = "drop"
        )
    } else {
      data %>%
        group_by(year) %>%
        summarise(
          total_stat = calc_stat(!!sym(var_name), population, stat_type),
          .groups = "drop"
        )
    }

    weighted_stats <- weighted_stats %>%
      left_join(total_avg, by = "year") %>%
      mutate(
        plot_value = stat_value - total_stat,
        sd_lower = sd_lower - total_stat,
        sd_upper = sd_upper - total_stat
      )
  } else {
    weighted_stats <- weighted_stats %>%
      mutate(plot_value = stat_value)
  }

  return(weighted_stats)
}

#' Create Plot Labels
#'
#' @description
#' Generates standardized labels for plots based on variable and display settings.
#'
#' @param var_label Variable label
#' @param show_deviation Whether deviation is shown
#' @param show_variance Whether variance is shown
#' @param group_var Grouping variable
#' @param stat_type Type of statistic used
#'
#' @return List of plot labels
#' @export
create_plot_labels <- function(var_label, show_deviation, show_variance, group_var, stat_type = "weighted_mean") {
  stat_desc <- case_when(
    stat_type == "weighted_mean" ~ "Population-weighted",
    stat_type == "mean" ~ "Unweighted",
    stat_type == "sum" ~ "Sum of"
  )

  list(
    y_label = if(show_deviation) paste("Deviation from mean:", var_label) else var_label,
    title = paste(
      if(show_deviation) "Deviation from EU average:" else "Development of:",
      var_label
    ),
    subtitle = if(show_variance && group_var == "c_group")
      paste(stat_desc, if(stat_type == "sum") "values" else "averages",
            if(stat_type != "sum") "with ± 1 standard deviation" else "")
    else
      paste(stat_desc, if(stat_type == "sum") "values" else "averages"),
    legend_title = if(group_var == "c_group") "Country groups" else "Countries"
  )
}

#' Create Indicator Plot
#'
#' @description
#' Creates a standardized plot for economic indicators with consistent styling.
#'
#' @param plot_data Prepared data for plotting
#' @param labels Plot labels
#' @param group_var Grouping variable
#' @param show_deviation Whether to show deviation
#' @param show_variance Whether to show variance
#'
#' @return ggplot object
#' @export
create_indicator_plot <- function(plot_data,
                                  labels,
                                  group_var,
                                  show_deviation = TRUE,
                                  show_variance = TRUE) {

  needs_zero_line <- show_deviation ||
    (min(plot_data$plot_value, na.rm = TRUE) < 0 &&
       max(plot_data$plot_value, na.rm = TRUE) > 0)

  library(RColorBrewer)
  set2_colors <- brewer.pal(n = 8, name = "Set2")

  # Create base plot
  p <- ggplot(plot_data, aes(x = year, y = plot_value,
                             color = !!sym(group_var),
                             shape = !!sym(group_var))) +
    {if(needs_zero_line) geom_hline(yintercept = 0, linetype = "solid", color = "gray30")} +
    # Add time period annotations
    annotate("rect", xmin = 2018, xmax = 2024,
             ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.3) +
    geom_vline(xintercept = c(2018), linetype = "solid", color = "darkgrey") +
    geom_vline(xintercept = c(2008, 2020, 2022, 2018), linetype = "dashed", color = "darkgrey") +
    # Add text annotations
    annotate("text", x = 2018, y = max(plot_data$plot_value, na.rm = TRUE)*1.12,
             label = "Extended analysis\nperiod", angle = 90, vjust = 1.3, size = 2) +
    # Add historical event markers
    geom_vline(xintercept = 1999, linetype = "dashed", color = "darkgrey") +
    geom_vline(xintercept = 2008, linetype = "dashed", color = "darkgrey") +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "darkgrey") +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "darkgrey") +
    # Add main plot elements
    geom_line(size = 1) +
    geom_point(size = 2) +
    # Add event annotations
    annotate("text", x = 1999, y = max(plot_data$plot_value, na.rm = TRUE)*1.12,
             label = "Euro Introduction", angle = 90, vjust = 1.5, size = 2) +
    annotate("text", x = 2008, y = max(plot_data$plot_value, na.rm = TRUE)*1.12,
             label = "Financial Crisis", angle = 90, vjust = 1.5, size = 2) +
    annotate("text", x = 2020, y = max(plot_data$plot_value, na.rm = TRUE)*1.12,
             label = "Start of Covid-19\npandemic", angle = 90, vjust = 1.3, size = 2) +
    annotate("text", x = 2022, y = max(plot_data$plot_value, na.rm = TRUE)*1.12,
             label = "Russian invasion\nof Ukraine", angle = 90, vjust = 1.3, size = 2) +
    # Configure legend and colors
    guides(color = guide_legend(override.aes = list(fill = NA)))+
    scale_color_manual(values = c(
      "Core" = set2_colors[2],
      "Finance" = set2_colors[1],
      "Periphery" = set2_colors[3],
      "Workbench" = set2_colors[4],
      "Luxembourg" = set2_colors[5]
    ))+
    scale_y_continuous(expand = expansion(mult = c(0.03, 0.1)))+
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)))

  # Add variance ribbons if requested
  if(show_variance && group_var == "c_group") {
    p <- p + geom_ribbon(
      aes(
        ymin = sd_lower,
        ymax = sd_upper,
        fill = !!sym(group_var),
        color = NULL
      ),
      alpha = 0.1,
      show.legend = FALSE
    )
  }

  # Add theme and labels
  p + theme_minimal() +
    labs(
      title = labels$title,
      subtitle = labels$subtitle,
      x = "",
      y = labels$y_label,
      color = labels$legend_title,
      shape = labels$legend_title
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = unit(c(1, 2, 1, 1), "lines"),
      panel.grid.minor.x = element_blank()
    )
}

#' Generate Disaggregated Plot
#'
#' @description
#' Creates a plot showing disaggregated data for a specific country group.
#'
#' @param data Input data frame
#' @param var_name Variable name
#' @param var_label Variable label
#' @param group Group to analyze
#' @param show_deviation Whether to show deviation
#' @param stat_type Type of statistic
#'
#' @return ggplot object
#' @export
generate_disaggregated_plot <- function(data,
                                        var_name,
                                        var_label,
                                        group,
                                        show_deviation = TRUE,
                                        stat_type = "weighted_mean") {

  group_data <- data %>% filter(c_group == group)

  plot_data <- prepare_indicator_stats(
    group_data,
    var_name,
    group_var = "country",
    show_deviation,
    stat_type = stat_type,
    total_data = data
  )

  labels <- create_plot_labels(
    paste(var_label, "-", group),
    show_deviation,
    show_variance = FALSE,
    group_var = "country",
    stat_type = stat_type
  )

  create_indicator_plot(
    plot_data,
    labels,
    "country",
    show_deviation,
    show_variance = FALSE
  )
}

#' Generate Indicator Plot
#'
#' @description
#' Main function to generate indicator plots with optional detailed breakdowns.
#'
#' @param data Input data frame
#' @param var_name Variable name
#' @param var_label Variable label (optional)
#' @param group_var Grouping variable
#' @param show_deviation Whether to show deviation
#' @param show_variance Whether to show variance
#' @param detailed_plots Whether to create detailed plots
#' @param stat_type Type of statistic
#'
#' @return ggplot object or list of plots
#' @export
generate_indicator_plot <- function(data,
                                    var_name,
                                    var_label = NULL,
                                    group_var = "c_group",
                                    show_deviation = TRUE,
                                    show_variance = TRUE,
                                    detailed_plots = FALSE,
                                    stat_type = "weighted_mean") {

  if (is.null(var_label)) var_label <- var_name

  plot_data <- prepare_indicator_stats(data, var_name, group_var, show_deviation, stat_type)

  labels <- create_plot_labels(var_label, show_deviation, show_variance, group_var, stat_type)

  main_plot <- create_indicator_plot(plot_data, labels, group_var, show_deviation, show_variance)

  if (detailed_plots) {
    groups <- unique(data[[group_var]])
    detail_plots <- map(groups, ~generate_disaggregated_plot(
      data, var_name, var_label, .x, show_deviation, stat_type
    )) %>% set_names(groups)

    return(list(main = main_plot, details = detail_plots))
  }

  return(main_plot)
}

#' Save Plots to Files
#'
#' @description
#' Saves plots to files in specified formats and locations.
#'
#' @param plot_object Plot or list of plots to save
#' @param var_name Variable name for file naming
#' @param path Output path
#' @param width Plot width
#' @param height Plot height
#'
#' @return NULL (invisibly)
#' @export
save_plots <- function(plot_object,
                       var_name,
                       path = paste0(here("output/fig/exploratory", "stylized_facts/"), var_name),
                       width = 10,
                       height = 6) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (is.ggplot(plot_object)) {
    filename <- file.path(path, paste0("01_", var_name, "_groups.pdf"))
    ggsave(filename, plot_object, width = width, height = height)
    return(invisible(NULL))
  }

  filename_main <- file.path(path, paste0("01_", var_name, "_groups.pdf"))
  ggsave(filename_main, plot_object$main, width = width, height = height)

  walk2(
    plot_object$details,
    names(plot_object$details),
    ~ggsave(
      file.path(path, paste0("02_", var_name, "_", .y, ".pdf")),
      .x,
      width = width,
      height = height
    )
  )
}
