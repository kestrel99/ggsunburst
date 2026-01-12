#' Create a Trajectory Sunburst Chart
#'
#' Create a sunburst chart showing how groups flow between categories over time.
#' Each segment in the inner ring represents a group of subjects with that
#' category at the first timepoint. Moving outward, each group subdivides based
#' on the category at subsequent timepoints, showing transitions.
#'
#' @param data A data frame with longitudinal data (one row per subject per timepoint).
#' @param subject Column name for subject identifier.
#' @param time Column name for timepoint.
#' @param category Column name for category variable (e.g., grade, response).
#' @param time_order Order of timepoints from inner (earliest) to outer (latest).
#' @param category_order Order of categories for consistent segment ordering.
#' @param inner_radius Inner radius of innermost ring (default 0.25).
#' @param segment_gap Gap between top-level category segments (default 0.02).
#' @param border_color Color of borders between segments (default "white").
#' @param border_width Width of borders (default 0.5).
#' @param show_labels What labels to show: "percent", "count", "category", "none",
#'   or combinations like c("category", "percent").
#' @param label_size Size for labels (default 3).
#' @param label_threshold Minimum segment width (radians) to show labels (default 0.15).
#' @param ring_labels Show ring (timepoint) labels? Default TRUE.
#' @param legend_title Title for the fill legend.
#' @param palette Color palette: named vector, "ctcae", "severity", or NULL.
#' @param na_color Color for missing values (default "grey90").
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' # Track AE grade changes
#' set.seed(42)
#' ae_data <- create_example_trajectory_data(n_subjects = 80, seed = 42)
#'
#' sunburst_trajectory(
#'   ae_data,
#'   subject = "subject",
#'   time = "visit",
#'   category = "grade",
#'   palette = "ctcae",
#'   title = "AE Grade Trajectories Over Time"
#' )
sunburst_trajectory <- function(
    data,
    subject,
    time,
    category,
    time_order = NULL,
    category_order = NULL,
    inner_radius = 0.25,
    segment_gap = 0.02,
    border_color = "white",
    border_width = 0.5,
    show_labels = "percent",
    label_size = 3,
    label_threshold = 0.15,
    ring_labels = TRUE,
    legend_title = NULL,
    palette = NULL,
    na_color = "grey90",
    title = NULL,
    subtitle = NULL
) {
  # Prepare trajectory data
  plot_data <- prepare_trajectory_data(
    data = data,
    subject = subject,
    time = time,
    category = category,
    time_order = time_order,
    category_order = category_order,
    inner_radius = inner_radius,
    segment_gap = segment_gap
  )
  
  # Build base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$category
      ),
      color = border_color,
      linewidth = border_width
    ) +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::theme_void()
  
  # Add fill scale
  p <- add_trajectory_fill_scale(p, plot_data$category, palette, legend_title, na_color)
  
  # Add labels
  if (!identical(show_labels, "none") && !identical(show_labels, FALSE)) {
    p <- add_trajectory_labels(
      p, plot_data,
      show_labels = show_labels,
      label_size = label_size,
      label_threshold = label_threshold
    )
  }
  
  # Add ring labels
  if (ring_labels) {
    # Get timepoint names from data
    time_col <- data[[time]]
    if (is.factor(time_col)) {
      time_names <- levels(time_col)
    } else {
      time_names <- sort(unique(time_col))
    }
    if (!is.null(time_order)) {
      time_names <- time_order
    }
    
    p <- add_trajectory_ring_labels(p, plot_data, time_names, inner_radius)
  }
  
  # Add titles
  if (!is.null(title) || !is.null(subtitle)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  }
  
  # Set limits
  max_y <- max(plot_data$ymax) * 1.12
  p <- p + ggplot2::ylim(0, max_y)
  
  p
}


#' Trajectory Sunburst for Adverse Events
#'
#' Specialized wrapper for visualizing AE grade trajectories over time.
#'
#' @param data A data frame with AE data.
#' @param subject Column name for subject identifier.
#' @param time Column name for timepoint/visit.
#' @param grade Column name for AE grade.
#' @param time_order Order of timepoints from inner to outer.
#' @param palette Color palette (default "ctcae").
#' @param ... Additional arguments passed to \code{sunburst_trajectory}.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' ae_data <- create_example_trajectory_data(n_subjects = 60, seed = 123)
#'
#' sunburst_ae_trajectory(
#'   ae_data,
#'   subject = "subject",
#'   time = "visit",
#'   grade = "grade",
#'   title = "AE Grade Changes Over Time"
#' )
sunburst_ae_trajectory <- function(
    data,
    subject,
    time,
    grade,
    time_order = NULL,
    palette = "ctcae",
    ...
) {
  # CTCAE grade order
  grade_order <- as.character(0:5)
  
  # Filter to grades that exist
  existing_grades <- as.character(sort(unique(as.numeric(data[[grade]]))))
  existing_grades <- existing_grades[!is.na(existing_grades)]
  grade_order <- grade_order[grade_order %in% existing_grades]
  
  sunburst_trajectory(
    data = data,
    subject = subject,
    time = time,
    category = grade,
    time_order = time_order,
    category_order = grade_order,
    palette = palette,
    legend_title = "Grade",
    ...
  )
}


#' Add Fill Scale for Trajectory Plot
#' @noRd
add_trajectory_fill_scale <- function(p, fill_values, palette, legend_title, na_color) {
  levels_vec <- levels(fill_values)
  if (is.null(levels_vec)) {
    levels_vec <- sort(unique(as.character(fill_values)))
  }
  # Remove "Missing" from display if present
  display_levels <- levels_vec[levels_vec != "Missing"]
  
  if (is.null(palette)) {
    p + ggplot2::scale_fill_discrete(name = legend_title, na.value = na_color)
  } else if (is.character(palette) && length(palette) == 1) {
    if (palette == "ctcae") {
      p + scale_fill_ctcae(name = legend_title)
    } else if (palette == "severity") {
      p + scale_fill_severity(name = legend_title, levels = display_levels)
    } else {
      p + ggplot2::scale_fill_brewer(name = legend_title, palette = palette, na.value = na_color)
    }
  } else {
    # Custom palette
    p + ggplot2::scale_fill_manual(name = legend_title, values = palette, na.value = na_color)
  }
}


#' Add Labels to Trajectory Plot
#' @noRd
add_trajectory_labels <- function(p, data, show_labels, label_size, label_threshold) {
  # Filter by segment width
  label_data <- data[data$segment_width >= label_threshold, ]
  
  if (nrow(label_data) == 0) return(p)
  
  # Build label text
  label_data$label_text <- ""
  
  if ("percent" %in% show_labels || identical(show_labels, "percent") ||
      identical(show_labels, "all")) {
    label_data$label_text <- paste0(round(label_data$proportion * 100), "%")
  }
  
  if ("count" %in% show_labels || identical(show_labels, "count")) {
    if (nchar(label_data$label_text[1]) > 0) {
      label_data$label_text <- paste0(label_data$label_text, "\n(", label_data$count, ")")
    } else {
      label_data$label_text <- as.character(label_data$count)
    }
  }
  
  if ("category" %in% show_labels) {
    if (nchar(label_data$label_text[1]) > 0) {
      label_data$label_text <- paste0(label_data$category, "\n", label_data$label_text)
    } else {
      label_data$label_text <- as.character(label_data$category)
    }
  }
  
  p +
    ggplot2::geom_text(
      data = label_data,
      mapping = ggplot2::aes(
        x = .data$x_mid,
        y = .data$y_mid,
        label = .data$label_text
      ),
      size = label_size,
      lineheight = 0.85,
      color = "black",
      inherit.aes = FALSE
    )
}


#' Add Ring Labels to Trajectory Plot
#' @noRd
add_trajectory_ring_labels <- function(p, data, time_names, inner_radius) {
  n_rings <- length(time_names)
  ring_width <- (1 - inner_radius) / n_rings
  
  ring_info <- data.frame(
    ring = time_names,
    x = 0,
    y = inner_radius + (seq_len(n_rings) - 0.5) * ring_width,
    stringsAsFactors = FALSE
  )
  
  p +
    ggplot2::geom_text(
      data = ring_info,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$ring),
      size = 2.8,
      fontface = "bold",
      color = "grey25",
      inherit.aes = FALSE
    )
}
