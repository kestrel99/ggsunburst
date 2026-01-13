#' Create a Trajectory Sunburst Chart
#'
#' Create a sunburst chart showing how groups flow between categories over time.
#' Each segment in the inner ring represents a group of subjects with that
#' category at the first timepoint. Moving outward, each group subdivides based
#' on the category at subsequent timepoints, showing transitions.
#'
#' @param data A data frame with longitudinal data (one row per subject per timepoint).
#'   Does not need to be rectangular - subjects may have different numbers of
#'   timepoints (dropouts, late entries, missing visits).
#' @param subject Column name for subject identifier.
#' @param time Column name for timepoint.
#' @param category Column name for category variable (e.g., grade, response).
#' @param time_order Order of timepoints from inner (earliest) to outer (latest).
#' @param category_order Order of categories for consistent segment ordering.
#' @param missing_handling How to handle subjects with missing timepoints:
#'   \itemize{
#'     \item \code{"dropout"} (default): Subjects who lack data at a timepoint
#'       have their segments drawn invisibly (no fill, no border) to preserve
#'       angular space. This creates an uneven outer border showing where
#'       dropouts occurred.
#'     \item \code{"carry_forward"}: Last observation carried forward (LOCF) -
#'       missing timepoints use the most recent observed category.
#'     \item \code{"show_missing"}: Missing timepoints shown as "Missing" category,
#'       subjects continue to subsequent timepoints if data exists.
#'     \item \code{"complete_only"}: Only include subjects with data at all timepoints.
#'   }
#' @param discontinued_label Label for discontinued/dropout subjects (default "Discontinued").
#' @param missing_label Label for missing observations (default "Missing").
#' @param inner_radius Inner radius of innermost ring (default 0.25).
#' @param segment_gap Gap between top-level category segments (default 0.02).
#' @param border_color Color of borders between segments (default "white").
#' @param border_width Width of borders (default 0.5).
#' @param show_labels What labels to show: "percent", "count", "category", "none",
#'   or combinations like c("category", "percent").
#' @param label_orientation How to orient labels: "auto" (default, chooses best
#'   fit), "radial" (text radiates from center), "tangential" (text follows arc),
#'   or "horizontal" (always horizontal).
#' @param label_size Base size for labels (default 3.5). With fit_labels=TRUE,
#'   this is the target size that gets adjusted per segment.
#' @param label_min_size Minimum label size when auto-fitting (default 2).
#' @param label_max_size Maximum label size when auto-fitting (default 5).
#' @param fit_labels Automatically fit labels to segments? TRUE (default) adjusts
#'   size and hides labels that don't fit. FALSE uses fixed size with threshold.
#' @param label_threshold Minimum segment width (radians) to show labels when
#'   fit_labels=FALSE (default 0.15).
#' @param label_color Color for labels (default "black").
#' @param ring_labels Show ring (timepoint) labels? Default TRUE.
#' @param legend_title Title for the fill legend.
#' @param palette Color palette: named vector, "ctcae", "severity", or NULL.
#' @param discontinued_color Color for discontinued subjects (default "grey70").
#' @param missing_color Color for missing observations (default "grey85").
#' @param na_color Color for NA values (default "grey90").
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' # Create data with dropouts
#' set.seed(42)
#' ae_data <- create_example_trajectory_data(
#'   n_subjects = 80, seed = 42,
#'   dropout_rate = 0.15
#' )
#'
#' # Default: dropouts are simply omitted from subsequent rings
#' sunburst_trajectory(
#'   ae_data,
#'   subject = "subject",
#'   time = "visit",
#'   category = "grade",
#'   palette = "ctcae",
#'   missing_handling = "dropout",
#'   title = "AE Grade Trajectories (dropouts omitted)"
#' )
#'
#' # Use LOCF for missing values (impute last observed)
#' sunburst_trajectory(
#'   ae_data,
#'   subject = "subject",
#'   time = "visit",
#'   category = "grade",
#'   missing_handling = "carry_forward",
#'   palette = "ctcae"
#' )
sunburst_trajectory <- function(
    data,
    subject,
    time,
    category,
    time_order = NULL,
    category_order = NULL,
    missing_handling = c("dropout", "carry_forward", "show_missing", "complete_only"),
    discontinued_label = "Discontinued",
    missing_label = "Missing",
    inner_radius = 0.25,
    segment_gap = 0.02,
    border_color = "white",
    border_width = 0.5,
    show_labels = "percent",
    label_orientation = "auto",
    label_size = 3.5,
    label_min_size = 2,
    label_max_size = 5,
    fit_labels = TRUE,
    label_threshold = 0.15,
    label_color = "black",
    ring_labels = TRUE,
    legend_title = NULL,
    palette = NULL,
    discontinued_color = NA,
    missing_color = "grey85",
    na_color = "grey90",
    title = NULL,
    subtitle = NULL
) {
  missing_handling <- match.arg(missing_handling)
  
 # Prepare trajectory data
  plot_data <- prepare_trajectory_data(
    data = data,
    subject = subject,
    time = time,
    category = category,
    time_order = time_order,
    category_order = category_order,
    missing_handling = missing_handling,
    discontinued_label = discontinued_label,
    missing_label = missing_label,
    inner_radius = inner_radius,
    segment_gap = segment_gap
  )
  
  # Split data into visible and invisible (discontinued) segments
  is_discontinued <- as.character(plot_data$category) == discontinued_label
  visible_data <- plot_data[!is_discontinued, , drop = FALSE]
  invisible_data <- plot_data[is_discontinued, , drop = FALSE]
  
  # Build base plot with visible segments
  p <- ggplot2::ggplot()
 
  # Draw invisible (discontinued) segments first - no fill, no border
  # These preserve the angular space but are not visible
  if (nrow(invisible_data) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = invisible_data,
        mapping = ggplot2::aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        fill = NA,
        color = NA
      )
  }
  
  # Draw visible segments with fill and border
  if (nrow(visible_data) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = visible_data,
        mapping = ggplot2::aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax,
          fill = .data$category
        ),
        color = border_color,
        linewidth = border_width
      )
  }
  
  p <- p +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::theme_void()
  
  # Add fill scale (exclude discontinued from legend)
  p <- add_trajectory_fill_scale(
    p, visible_data$category, palette, legend_title, na_color,
    discontinued_label = discontinued_label,
    discontinued_color = discontinued_color,
    missing_label = missing_label,
    missing_color = missing_color
  )
  
  # Add labels with new orientation and fitting system (only for visible segments)
  if (!identical(show_labels, "none") && !identical(show_labels, FALSE) && nrow(visible_data) > 0) {
    # Determine label type
    label_type <- if (is.character(show_labels) && length(show_labels) == 1) {
      show_labels
    } else if ("percent" %in% show_labels && "category" %in% show_labels) {
      "both"
    } else if ("percent" %in% show_labels) {
      "percent"
    } else if ("count" %in% show_labels) {
      "count"
    } else {
      "percent"
    }
    
    p <- add_sunburst_labels(
      p, visible_data,
      label_type = label_type,
      orientation = label_orientation,
      base_size = label_size,
      min_size = label_min_size,
      max_size = label_max_size,
      fit_labels = fit_labels,
      label_color = label_color,
      label_threshold = label_threshold
    )
  }
  
  # Add ring labels
  if (ring_labels) {
    # Get timepoint names from prepared data attribute or from data
    time_names <- attr(plot_data, "time_order")
    if (is.null(time_names)) {
      time_col <- data[[time]]
      if (is.factor(time_col)) {
        time_names <- levels(time_col)
      } else {
        time_names <- sort(unique(time_col))
      }
      if (!is.null(time_order)) {
        time_names <- time_order
      }
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
add_trajectory_fill_scale <- function(
    p, fill_values, palette, legend_title, na_color,
    discontinued_label = "Discontinued",
    discontinued_color = NA,
    missing_label = "Missing",
    missing_color = "grey85"
) {
  levels_vec <- levels(fill_values)
  if (is.null(levels_vec)) {
    levels_vec <- sort(unique(as.character(fill_values)))
  }
  
  # Check for special categories
  # Note: discontinued is drawn invisibly, so don't include in legend
  has_discontinued <- discontinued_label %in% levels_vec
  has_missing <- missing_label %in% levels_vec
  
  # Remove discontinued from display levels (it's invisible)
  display_levels <- setdiff(levels_vec, discontinued_label)
  
  # Build custom palette if needed
  if (is.null(palette)) {
    # Default palette with special colors
    n_regular <- length(display_levels) - has_missing
    regular_levels <- setdiff(display_levels, missing_label)
    regular_colors <- scales::hue_pal()(max(1, n_regular))
    
    color_vec <- setNames(regular_colors[seq_along(regular_levels)], regular_levels)
    if (has_missing) color_vec[missing_label] <- missing_color
    
    p + ggplot2::scale_fill_manual(
      name = legend_title, 
      values = color_vec, 
      na.value = na_color,
      breaks = display_levels  # Exclude discontinued from legend
    )
    
  } else if (is.character(palette) && length(palette) == 1) {
    if (palette == "ctcae") {
      # Get CTCAE colors
      ctcae <- ctcae_colors()
      if (has_missing) ctcae[missing_label] <- missing_color
      p + ggplot2::scale_fill_manual(
        name = legend_title, 
        values = ctcae, 
        na.value = na_color,
        breaks = display_levels
      )
      
    } else if (palette == "severity") {
      # Severity scale for regular categories
      regular_levels <- setdiff(display_levels, missing_label)
      sev_colors <- severity_colors(length(regular_levels))
      names(sev_colors) <- regular_levels
      if (has_missing) sev_colors[missing_label] <- missing_color
      p + ggplot2::scale_fill_manual(
        name = legend_title, 
        values = sev_colors, 
        na.value = na_color,
        breaks = display_levels
      )
      
    } else {
      # Brewer palette
      regular_levels <- setdiff(display_levels, missing_label)
      n_regular <- length(regular_levels)
      brewer_colors <- scales::brewer_pal(palette = palette)(max(3, n_regular))[seq_len(n_regular)]
      color_vec <- setNames(brewer_colors, regular_levels)
      if (has_missing) color_vec[missing_label] <- missing_color
      p + ggplot2::scale_fill_manual(
        name = legend_title, 
        values = color_vec, 
        na.value = na_color,
        breaks = display_levels
      )
    }
  } else {
    # Custom palette
    custom_palette <- palette
    if (has_missing && !(missing_label %in% names(custom_palette))) {
      custom_palette[missing_label] <- missing_color
    }
    p + ggplot2::scale_fill_manual(
      name = legend_title, 
      values = custom_palette, 
      na.value = na_color,
      breaks = display_levels
    )
  }
}


#' Generate severity colors
#' @noRd
severity_colors <- function(n) {
  if (n <= 5) {
    c("#d9d9d9", "#a6d96a", "#fdae61", "#f46d43", "#d73027")[1:n]
  } else {
    scales::seq_gradient_pal("#a6d96a", "#d73027")(seq(0, 1, length.out = n))
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
