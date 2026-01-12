#' Prepare Data for Sunburst Charts
#'
#' Convert various data formats into the internal format required for
#' sunburst charts. Handles both hierarchical (nested) data and
#' longitudinal (wide-format) data.
#'
#' @param data A data frame containing the source data.
#' @param rings Column name (string) for the ring variable (e.g., timepoint/visit).
#'   Each unique value becomes a concentric ring, from innermost (earliest) to
#'   outermost (latest).
#' @param segments Column name (string) for the segment variable (e.g., grade,
#'   category). Each unique value becomes a slice of the sunburst.
#' @param values Optional column name for counts/weights. If NULL, rows are counted.
#' @param ring_order How to order rings: "appearance" (default), "alphabetical",
#'   "reverse", or a character vector of ring names.
#' @param segment_order How to order segments: "appearance" (default),
#'   "alphabetical", "frequency", or a character vector of segment names.
#' @param ring_gap Numeric gap between rings (default 0, use 0 for no gap).
#' @param segment_gap Numeric gap between segments in radians (default 0.02).
#' @param inner_radius Inner radius of the innermost ring (default 0.3).
#' @param border_width Width of border lines between rings (default 0.5).
#'
#' @return A data frame with columns for plotting: segment_id, ring_id,
#'   xmin, xmax, ymin, ymax, fill, count, proportion, and label positioning.
#'
#' @export
#'
#' @examples
#' # AE grade distribution over time
#' df <- data.frame(
#'   visit = rep(c("Baseline", "Week 4", "Week 8"), each = 100),
#'   grade = sample(0:4, 300, replace = TRUE, prob = c(0.3, 0.35, 0.2, 0.1, 0.05))
#' )
#' prepared <- prepare_sunburst_data(
#'   df,
#'   rings = "visit",
#'   segments = "grade"
#' )
prepare_sunburst_data <- function(
    data,
    rings,
    segments,
    values = NULL,
    ring_order = "appearance",
    segment_order = "appearance",
    ring_gap = 0,
    segment_gap = 0.02,
    inner_radius = 0.3,
    border_width = 0.5
) {
  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # Convert to data frame if tibble
  data <- as.data.frame(data)

  # Validate columns exist
  if (!rings %in% names(data)) {
    cli::cli_abort("Column {.val {rings}} not found in data.")
  }
  if (!segments %in% names(data)) {
    cli::cli_abort("Column {.val {segments}} not found in data.")
  }

  # Aggregate data: count by ring and segment
  agg_data <- aggregate_counts(data, rings, segments, values)

  # Order rings and segments
  agg_data <- order_rings(agg_data, ring_order)
  agg_data <- order_segments_by_category(agg_data, segment_order)

  # Calculate proportions within each ring
  agg_data <- calculate_proportions(agg_data)

  # Calculate geometry
  calculate_sunburst_geometry_categorical(
    agg_data,
    ring_gap = ring_gap,
    segment_gap = segment_gap,
    inner_radius = inner_radius
  )
}


#' Aggregate Counts by Ring and Segment
#' @noRd
aggregate_counts <- function(data, rings, segments, values) {
  # Preserve factor levels if present
  ring_col <- data[[rings]]
  seg_col <- data[[segments]]
  
  ring_is_factor <- is.factor(ring_col)
  ring_levels <- if (ring_is_factor) levels(ring_col) else NULL
  
  # Create grouping data frame
  df <- data.frame(
    ring = as.character(ring_col),
    segment = as.character(seg_col),
    stringsAsFactors = FALSE
  )

  if (!is.null(values) && values %in% names(data)) {
    df$value <- data[[values]]
  } else {
    df$value <- 1
  }

  # Aggregate
  agg <- aggregate(value ~ ring + segment, data = df, FUN = sum)
  names(agg)[3] <- "count"
  
  # Restore factor levels for ring if they existed
  if (ring_is_factor && !is.null(ring_levels)) {
    agg$ring <- factor(agg$ring, levels = ring_levels)
  }

  agg
}


#' Order Rings (Timepoints)
#' @noRd
order_rings <- function(data, ring_order) {
  # Get rings - preserve factor levels if present
  if (is.factor(data$ring)) {
    rings <- levels(data$ring)
  } else {
    rings <- unique(data$ring)
  }

  if (is.character(ring_order) && length(ring_order) == 1) {
    ordered_rings <- switch(
      ring_order,
      "appearance" = rings,  # Keep original order (factor levels or appearance)
      "alphabetical" = sort(rings),
      "reverse" = rev(rings),
      rings
    )
  } else if (is.character(ring_order) && length(ring_order) > 1) {
    # Custom order provided
    if (!all(rings %in% ring_order)) {
      cli::cli_warn(
        "Not all rings found in custom order. Missing rings appended."
      )
      ordered_rings <- c(
        ring_order[ring_order %in% rings],
        setdiff(rings, ring_order)
      )
    } else {
      ordered_rings <- ring_order[ring_order %in% rings]
    }
  } else {
    ordered_rings <- rings
  }

  data$ring <- factor(data$ring, levels = ordered_rings)
  data
}


#' Order Segments (Categories)
#' @noRd
order_segments_by_category <- function(data, segment_order) {
  segments <- unique(data$segment)

  if (is.character(segment_order) && length(segment_order) == 1) {
    ordered_segments <- switch(
      segment_order,
      "appearance" = segments,
      "alphabetical" = sort(segments),
      "numeric" = {
        # Try to sort numerically
        num_vals <- suppressWarnings(as.numeric(segments))
        if (all(!is.na(num_vals))) {
          segments[order(num_vals)]
        } else {
          sort(segments)
        }
      },
      "frequency" = {
        freq <- tapply(data$count, data$segment, sum)
        names(sort(freq, decreasing = TRUE))
      },
      segments
    )
  } else if (is.character(segment_order) && length(segment_order) > 1) {
    if (!all(segments %in% segment_order)) {
      cli::cli_warn(
        "Not all segments in custom order. Missing segments appended."
      )
      ordered_segments <- c(
        segment_order[segment_order %in% segments],
        setdiff(segments, segment_order)
      )
    } else {
      ordered_segments <- segment_order[segment_order %in% segments]
    }
  } else {
    ordered_segments <- segments
  }

  data$segment <- factor(data$segment, levels = ordered_segments)
  data
}


#' Calculate Proportions Within Each Ring
#' @noRd
calculate_proportions <- function(data) {
  # Calculate total per ring
  ring_totals <- tapply(data$count, data$ring, sum)
  data$ring_total <- ring_totals[as.character(data$ring)]
  data$proportion <- data$count / data$ring_total

  # Sort by ring then segment for consistent ordering
  data <- data[order(data$ring, data$segment), ]
  rownames(data) <- NULL

  data
}


#' Calculate Sunburst Geometry for Categorical Data
#' @noRd
calculate_sunburst_geometry_categorical <- function(
    data,
    ring_gap,
    segment_gap,
    inner_radius
) {
  rings <- levels(data$ring)
  segments <- levels(data$segment)

  n_rings <- length(rings)
  n_segments <- length(segments)

  # Calculate ring dimensions (no gap = rings touch with just border)
  ring_width <- (1 - inner_radius - (n_rings - 1) * ring_gap) / n_rings

  # Total angular space accounting for gaps between segments
  total_angle <- 2 * pi - (n_segments * segment_gap)

  # Build geometry data frame
  result_list <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    ring_idx <- which(rings == as.character(data$ring[i]))
    seg_idx <- which(segments == as.character(data$segment[i]))

    # Radial position (y in polar coords) - inner to outer by time
    y_start <- inner_radius + (ring_idx - 1) * (ring_width + ring_gap)
    y_end <- y_start + ring_width

    # Angular position based on cumulative proportion within this ring
    # Get all segments for this ring, in segment order
    ring_data <- data[data$ring == data$ring[i], ]
    ring_data <- ring_data[order(match(ring_data$segment, segments)), ]

    # Calculate cumulative proportions
    ring_data$cum_prop <- cumsum(ring_data$proportion)
    ring_data$start_prop <- c(0, ring_data$cum_prop[-nrow(ring_data)])

    # Find this segment's position
    this_seg_row <- which(ring_data$segment == data$segment[i])
    start_prop <- ring_data$start_prop[this_seg_row]
    end_prop <- ring_data$cum_prop[this_seg_row]

    # Angular positions
    x_start <- start_prop * total_angle + (seg_idx - 1) * segment_gap
    x_end <- x_start + (end_prop - start_prop) * total_angle

    # Mid points for labels
    x_mid <- (x_start + x_end) / 2
    y_mid <- (y_start + y_end) / 2

    result_list[[i]] <- data.frame(
      ring_id = data$ring[i],
      segment_id = data$segment[i],
      count = data$count[i],
      proportion = data$proportion[i],
      ring_total = data$ring_total[i],
      xmin = x_start,
      xmax = x_end,
      ymin = y_start,
      ymax = y_end,
      x_mid = x_mid,
      y_mid = y_mid,
      # Label positioning
      label_x = y_mid * cos(x_mid - pi/2),
      label_y = y_mid * sin(x_mid - pi/2),
      label_angle = calculate_label_angle(x_mid),
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, result_list)

  # Ensure factor levels are preserved
  result$ring_id <- factor(result$ring_id, levels = rings)
  result$segment_id <- factor(result$segment_id, levels = segments)

  result
}


#' Calculate Label Angle for Readability
#' @noRd
calculate_label_angle <- function(angle_rad) {
  # Convert to degrees and adjust for readability
  angle_deg <- (angle_rad * 180 / pi) - 90

  # Flip labels on the left side so they're not upside down
  ifelse(angle_deg > 90 & angle_deg < 270,
         angle_deg - 180,
         angle_deg)
}


#' Prepare Adverse Event Data for Sunburst
#'
#' A convenience function to prepare adverse event data for sunburst
#' visualization showing grade distribution over time.
#'
#' @param data A data frame with AE data.
#' @param time Column name for time point or visit.
#' @param grade Column name for AE grade (typically 0-5 for CTCAE).
#' @param subject Optional column name for subject (used for counting unique patients).
#' @param count_unique If TRUE and subject is provided, count unique patients per
#'   grade/time combination. If FALSE, count rows.
#' @param time_order How to order timepoints: "appearance" (default), or a
#'   character vector of timepoint names from earliest to latest.
#' @param grade_order How to order grades: "numeric" (default, 0-5), or
#'   a character vector.
#' @param ... Additional arguments passed to \code{prepare_sunburst_data}.
#'
#' @return A data frame ready for sunburst plotting.
#'
#' @export
#'
#' @examples
#' # Simulated AE data
#' ae_data <- data.frame(
#'   USUBJID = rep(paste0("SUBJ-", 1:50), each = 4),
#'   VISIT = rep(c("Baseline", "Week 2", "Week 4", "Week 8"), 50),
#'   AETOXGR = sample(0:4, 200, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.08, 0.02))
#' )
#' prepared <- prepare_ae_data(
#'   ae_data,
#'   time = "VISIT",
#'   grade = "AETOXGR"
#' )
prepare_ae_data <- function(
    data,
    time,
    grade,
    subject = NULL,
    count_unique = TRUE,
    time_order = "appearance",
    grade_order = "numeric",
    ...
) {
  # Input validation
  required_cols <- c(time, grade)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Required column(s) not found: {.val {missing_cols}}"
    )
  }

  # Create working copy
  df <- data.frame(
    ring = data[[time]],
    segment = as.character(data[[grade]]),
    stringsAsFactors = FALSE
  )

  # Handle unique patient counting
  if (!is.null(subject) && subject %in% names(data) && count_unique) {
    df$subject <- data[[subject]]
    # Remove duplicates within time/grade
    df <- unique(df[, c("ring", "segment", "subject")])
    df$subject <- NULL
  }

  # Determine grade order
  if (identical(grade_order, "numeric")) {
    unique_grades <- unique(df$segment)
    num_grades <- suppressWarnings(as.numeric(unique_grades))
    if (all(!is.na(num_grades))) {
      grade_order <- as.character(sort(num_grades))
    } else {
      grade_order <- "appearance"
    }
  }

  prepare_sunburst_data(
    df,
    rings = "ring",
    segments = "segment",
    ring_order = time_order,
    segment_order = grade_order,
    ...
  )
}
