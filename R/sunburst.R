#' Create a Sunburst Chart
#'
#' Create a sunburst (multilevel pie) chart from categorical/longitudinal data.
#' The chart displays data as concentric rings where each ring represents a
#' timepoint (earliest at center, latest at outer edge) and each segment
#' represents a category (e.g., AE grade). Segment width shows the proportion
#' in each category.
#'
#' @param data A data frame containing the source data.
#' @param rings Column name for the ring variable (timepoints). Inner ring is
#'   first/earliest, outer ring is last/latest.
#' @param segments Column name for the segment variable (categories/grades).
#' @param values Optional column name for counts/weights.
#' @param ring_order How to order rings: "appearance", "alphabetical", or
#'   a character vector from inner to outer.
#' @param segment_order How to order segments: "appearance", "alphabetical",
#'   "numeric", "frequency", or a character vector.
#' @param ring_gap Gap between rings (default 0 for no gap, just white border).
#' @param segment_gap Gap between segments in radians (default 0.01).
#' @param inner_radius Inner radius of innermost ring (default 0.25).
#' @param border_color Color of borders between rings/segments (default "white").
#' @param border_width Width of borders (default 0.8).
#' @param show_labels What labels to show: "all", "segments", "counts",
#'   "percent", "none", or a combination like c("segments", "percent").
#' @param label_color Color for labels (default "black" or "auto" for contrast).
#' @param label_size Size for labels (default 3.5).
#' @param label_threshold Minimum proportion to show labels (default 0.05).
#' @param ring_labels Show ring (timepoint) labels? Default TRUE.
#' @param ring_label_position Where to place ring labels: "inside" (default),
#'   "outside", or "legend".
#' @param legend_title Title for the fill legend.
#' @param palette Color palette: a named vector, "ctcae", "severity", or NULL.
#' @param na_color Color for NA/missing values.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' # Example with AE-like data
#' set.seed(42)
#' n_patients <- 50
#' visits <- c("Baseline", "Week 4", "Week 8", "Week 12")
#'
#' ae_data <- data.frame(
#'   patient = rep(paste0("PT", 1:n_patients), each = length(visits)),
#'   visit = factor(rep(visits, n_patients), levels = visits),
#'   grade = factor(sample(0:4, n_patients * length(visits), replace = TRUE,
#'                         prob = c(0.3, 0.35, 0.2, 0.1, 0.05)))
#' )
#'
#' sunburst(
#'   ae_data,
#'   rings = "visit",
#'   segments = "grade",
#'   palette = "ctcae",
#'   legend_title = "AE Grade",
#'   title = "Adverse Event Grade Distribution Over Time"
#' )
sunburst <- function(
    data,
    rings,
    segments,
    values = NULL,
    ring_order = "appearance",
    segment_order = "numeric",
    ring_gap = 0,
    segment_gap = 0.01,
    inner_radius = 0.25,
    border_color = "white",
    border_width = 0.8,
    show_labels = "percent",
    label_color = "auto",
    label_size = 3.5,
    label_threshold = 0.05,
    ring_labels = TRUE,
    ring_label_position = "inside",
    legend_title = NULL,
    palette = NULL,
    na_color = "grey80",
    title = NULL,
    subtitle = NULL
) {
  # Prepare data
  plot_data <- prepare_sunburst_data(
    data = data,
    rings = rings,
    segments = segments,
    values = values,
    ring_order = ring_order,
    segment_order = segment_order,
    ring_gap = ring_gap,
    segment_gap = segment_gap,
    inner_radius = inner_radius
  )

  # Build base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$segment_id
      ),
      color = border_color,
      linewidth = border_width
    ) +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::theme_void()

  # Add fill scale
  p <- add_fill_scale(p, plot_data$segment_id, palette, legend_title, na_color)

  # Add segment labels (counts/percentages in cells)
  if (!identical(show_labels, "none") && !identical(show_labels, FALSE)) {
    p <- add_cell_labels(
      p, plot_data,
      show_labels = show_labels,
      label_color = label_color,
      label_size = label_size,
      label_threshold = label_threshold
    )
  }

  # Add ring labels (timepoint names)
  if (ring_labels && ring_label_position != "legend") {
    p <- add_ring_labels_categorical(
      p, plot_data,
      position = ring_label_position,
      inner_radius = inner_radius
    )
  }

  # Add titles
  if (!is.null(title) || !is.null(subtitle)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  }

  # Set y limits to accommodate labels
  max_y <- max(plot_data$ymax) * 1.15
  p <- p + ggplot2::ylim(0, max_y)

  p
}


#' Sunburst Chart for Adverse Events
#'
#' A specialized wrapper around \code{sunburst()} optimized for visualizing
#' adverse event grade distribution over time. Each ring represents a timepoint,
#' each segment represents a grade category.
#'
#' @param data A data frame with AE data.
#' @param time Column name for time point or visit.
#' @param grade Column name for AE grade (typically 0-5 for CTCAE).
#' @param subject Optional column name for subject ID (for unique patient counts).
#' @param time_order Order of timepoints from inner to outer. Default uses
#'   factor levels or appearance order.
#' @param grade_labels Named vector for grade labels in legend.
#' @param palette Color palette: "ctcae" (default), "severity", or custom.
#' @param show_percent Show percentages in segments? Default TRUE.
#' @param show_counts Show counts in segments? Default FALSE.
#' @param ... Additional arguments passed to \code{sunburst()}.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' # Simulated adverse event data
#' set.seed(123)
#' n_patients <- 60
#' visits <- c("Screening", "Baseline", "Week 4", "Week 8", "Week 12")
#'
#' ae_data <- expand.grid(
#'   USUBJID = paste0("SUBJ-", sprintf("%03d", 1:n_patients)),
#'   AVISIT = factor(visits, levels = visits),
#'   stringsAsFactors = FALSE
#' )
#' ae_data$AETOXGR <- sample(0:4, nrow(ae_data), replace = TRUE,
#'                           prob = c(0.35, 0.30, 0.20, 0.10, 0.05))
#'
#' sunburst_ae(
#'   ae_data,
#'   time = "AVISIT",
#'   grade = "AETOXGR",
#'   title = "Adverse Event Grade Distribution Over Time"
#' )
sunburst_ae <- function(
    data,
    time,
    grade,
    subject = NULL,
    time_order = NULL,
    grade_labels = NULL,
    palette = "ctcae",
    show_percent = TRUE,
    show_counts = FALSE,
    ...
) {
  # Determine time order from data
  if (is.null(time_order)) {
    time_col <- data[[time]]
    if (is.factor(time_col)) {
      time_order <- levels(time_col)
    } else {
      time_order <- unique(time_col)
    }
  }

  # Determine what labels to show
  show_labels <- "none"
  if (show_percent && show_counts) {
    show_labels <- c("percent", "counts")
  } else if (show_percent) {
    show_labels <- "percent"
  } else if (show_counts) {
    show_labels <- "counts"
  }

  # Build the plot
  sunburst(
    data = data,
    rings = time,
    segments = grade,
    ring_order = time_order,
    segment_order = "numeric",
    palette = palette,
    show_labels = show_labels,
    legend_title = "Grade",
    ...
  )
}


#' Add Cell Labels (Counts/Percentages)
#' @noRd
add_cell_labels <- function(
    p, data,
    show_labels,
    label_color,
    label_size,
    label_threshold
) {
  # Filter data by threshold
  label_data <- data[data$proportion >= label_threshold, ]

  if (nrow(label_data) == 0) return(p)

  # Build label text
  label_data$label_text <- ""

  if ("percent" %in% show_labels || identical(show_labels, "percent") ||
      identical(show_labels, "all")) {
    label_data$label_text <- paste0(round(label_data$proportion * 100), "%")
  }

  if ("counts" %in% show_labels || identical(show_labels, "counts")) {
    if (nchar(label_data$label_text[1]) > 0) {
      label_data$label_text <- paste0(label_data$label_text, "\n(n=", label_data$count, ")")
    } else {
      label_data$label_text <- paste0("n=", label_data$count)
    }
  }

  if ("segments" %in% show_labels) {
    if (nchar(label_data$label_text[1]) > 0) {
      label_data$label_text <- paste0(label_data$segment_id, "\n", label_data$label_text)
    } else {
      label_data$label_text <- as.character(label_data$segment_id)
    }
  }

  # Determine label colors
  if (identical(label_color, "auto")) {
    # Use contrasting colors based on fill
    label_data$text_color <- "black"
  } else {
    label_data$text_color <- label_color
  }

  p +
    ggplot2::geom_text(
      data = label_data,
      mapping = ggplot2::aes(
        x = .data$x_mid,
        y = .data$y_mid,
        label = .data$label_text
      ),
      color = label_data$text_color,
      size = label_size,
      lineheight = 0.85,
      inherit.aes = FALSE
    )
}


#' Add Ring Labels (Timepoint Names)
#' @noRd
add_ring_labels_categorical <- function(p, data, position, inner_radius) {
  # Get unique rings with their radial positions
  rings <- levels(data$ring_id)

  ring_info <- data.frame(
    ring = rings,
    stringsAsFactors = FALSE
  )

  # Calculate radial position for each ring label
  for (i in seq_along(rings)) {
    ring_data <- data[data$ring_id == rings[i], ]
    if (position == "inside") {
      # Place label in the middle of the ring at a fixed angle
      ring_info$y[i] <- (ring_data$ymin[1] + ring_data$ymax[1]) / 2
    } else {
      # Place outside the ring
      ring_info$y[i] <- ring_data$ymax[1] + 0.03
    }
  }

  # Place labels at the top (angle = 0)
  ring_info$x <- 0

  p +
    ggplot2::geom_text(
      data = ring_info,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$ring
      ),
      size = 3,
      fontface = "bold",
      color = "grey25",
      inherit.aes = FALSE
    )
}


#' Add Fill Scale Based on Palette
#' @noRd
add_fill_scale <- function(p, fill_values, palette, legend_title, na_color) {
  unique_fills <- levels(fill_values)
  if (is.null(unique_fills)) {
    unique_fills <- sort(unique(as.character(fill_values)))
  }

  if (is.null(palette)) {
    # Use ggplot2 default
    p <- p + ggplot2::scale_fill_discrete(
      name = legend_title,
      na.value = na_color
    )
  } else if (is.character(palette) && length(palette) == 1) {
    # Named palette
    if (palette == "ctcae") {
      p <- p + scale_fill_ctcae(name = legend_title)
    } else if (palette == "severity") {
      p <- p + scale_fill_severity(name = legend_title, levels = unique_fills)
    } else {
      # Try as RColorBrewer palette
      p <- p + ggplot2::scale_fill_brewer(
        name = legend_title,
        palette = palette,
        na.value = na_color
      )
    }
  } else if (is.character(palette)) {
    # Custom color vector
    if (!is.null(names(palette))) {
      # Named vector - use as values
      p <- p + ggplot2::scale_fill_manual(
        name = legend_title,
        values = palette,
        na.value = na_color
      )
    } else {
      # Unnamed vector - match to unique fills
      colors <- setNames(palette[seq_along(unique_fills)], unique_fills)
      p <- p + ggplot2::scale_fill_manual(
        name = legend_title,
        values = colors,
        na.value = na_color
      )
    }
  }

  p
}
