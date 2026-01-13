#' Label Utilities for Sunburst Charts
#'
#' Functions for controlling text orientation, sizing, and positioning
#' within sunburst segments.

#' Calculate Optimal Label Parameters for Segments
#'
#' Determines the best label size, angle, and position for each segment
#' based on its dimensions.
#'
#' @param data Prepared sunburst data with geometry columns.
#' @param orientation Label orientation: "auto" (default), "radial", "tangential",
#'   "horizontal", or "curved".
#' @param base_size Base font size (default 3.5).
#' @param min_size Minimum font size (default 2).
#' @param max_size Maximum font size (default 5).
#' @param fit_method How to fit labels: "scale" (adjust size to fit),
#'   "filter" (hide labels that don't fit), or "both" (default).
#' @param padding Padding factor inside segment (default 0.8, meaning use 80% of space).
#'
#' @return Data frame with added columns: label_size, label_angle, label_hjust,
#'   label_vjust, show_label.
#'
#' @export
calculate_label_params <- function(
    data,
    orientation = "auto",
    base_size = 3.5,
    min_size = 2,
    max_size = 5,
    fit_method = "both",
    padding = 0.8
) {
 

 # Calculate segment dimensions
  data$arc_length <- (data$xmax - data$xmin) * data$y_mid
  data$radial_height <- data$ymax - data$ymin
  data$segment_width <- data$xmax - data$xmin  # Angular width in radians
  
 
 # Determine orientation for each segment
  data$computed_angle <- vapply(seq_len(nrow(data)), function(i) {
    compute_label_angle(
      x_mid = data$x_mid[i],
      arc_length = data$arc_length[i],
      radial_height = data$radial_height[i],
      orientation = orientation
    )
  }, numeric(1))
  
  # Calculate optimal size based on segment dimensions
  data$computed_size <- vapply(seq_len(nrow(data)), function(i) {
    compute_label_size(
      arc_length = data$arc_length[i],
      radial_height = data$radial_height[i],
      angle = data$computed_angle[i],
      base_size = base_size,
      min_size = min_size,
      max_size = max_size,
      padding = padding
    )
  }, numeric(1))
  
  # Determine text alignment
  alignment <- compute_label_alignment(data$x_mid, data$computed_angle)
  data$computed_hjust <- alignment$hjust
  data$computed_vjust <- alignment$vjust
  
  # Determine which labels to show
  data$show_label <- compute_label_visibility(
    data,
    fit_method = fit_method,
    min_size = min_size
  )
  
  data
}


#' Compute Label Angle for a Segment
#' @noRd
compute_label_angle <- function(x_mid, arc_length, radial_height, orientation) {
  # Handle NA/NULL/missing values
 if (is.na(x_mid) || is.null(x_mid) || length(x_mid) == 0) {
    return(0)
  }
  if (is.na(arc_length) || is.null(arc_length) || length(arc_length) == 0) {
    arc_length <- 0.1  # Default value
  }
  if (is.na(radial_height) || is.null(radial_height) || length(radial_height) == 0) {
    radial_height <- 0.1  # Default value
  }
  
  # Base angle (converted from radians, adjusted for polar coordinates)
  base_angle_deg <- (x_mid * 180 / pi) - 90
  
  # Normalize to -180 to 180
  while (base_angle_deg > 180) base_angle_deg <- base_angle_deg - 360
  while (base_angle_deg < -180) base_angle_deg <- base_angle_deg + 360
  
  switch(
    orientation,
    "radial" = {
      # Text reads from center outward
      if (base_angle_deg > 90 || base_angle_deg < -90) {
        base_angle_deg + 180
      } else {
        base_angle_deg
      }
    },
    "tangential" = {
      # Text follows the arc
      tangent_angle <- base_angle_deg + 90
      # Flip if on left side for readability
      if (tangent_angle > 90 || tangent_angle < -90) {
        tangent_angle - 180
      } else {
        tangent_angle
      }
    },
    "horizontal" = {
      # Always horizontal
      0
    },
    "auto" = {
      # Choose based on segment shape
      aspect_ratio <- arc_length / radial_height
      
      # Handle edge case where radial_height is 0
      if (!is.finite(aspect_ratio)) {
        aspect_ratio <- 1
      }
      
      if (aspect_ratio > 1.5) {
        # Wide segment: use tangential
        tangent_angle <- base_angle_deg + 90
        if (tangent_angle > 90) tangent_angle <- tangent_angle - 180
        if (tangent_angle < -90) tangent_angle <- tangent_angle + 180
        tangent_angle
      } else if (aspect_ratio < 0.7) {
        # Tall segment: use radial
        if (base_angle_deg > 90 || base_angle_deg < -90) {
          base_angle_deg + 180
        } else {
          base_angle_deg
        }
      } else {
        # Square-ish: use radial but ensure readability
        if (base_angle_deg > 90 || base_angle_deg < -90) {
          base_angle_deg + 180
        } else {
          base_angle_deg
        }
      }
    },
    # Default: radial with flip
    {
      if (base_angle_deg > 90 || base_angle_deg < -90) {
        base_angle_deg + 180
      } else {
        base_angle_deg
      }
    }
  )
}


#' Compute Label Size Based on Segment Dimensions
#' @noRd
compute_label_size <- function(
    arc_length,
    radial_height,
    angle,
    base_size,
    min_size,
    max_size,
    padding
) {
  # Estimate available space based on angle
  angle_rad <- abs(angle) * pi / 180
  
  # For angled text, calculate effective bounding box
  # Approximate: use the smaller of arc_length and radial_height
  # adjusted for angle
  if (abs(angle) < 45) {
    # More horizontal: width matters more
    available_space <- arc_length * padding
  } else {
    # More vertical: height matters more
    available_space <- radial_height * padding
  }
  
  # Scale size based on available space
 
 # Assume base_size works for a "standard" segment of ~0.3 units
  standard_space <- 0.3
  scale_factor <- available_space / standard_space
  
  computed_size <- base_size * sqrt(scale_factor)  # sqrt for more gentle scaling
  
  # Clamp to min/max
 
  max(min_size, min(max_size, computed_size))
}


#' Compute Label Alignment
#' @noRd
compute_label_alignment <- function(x_mid, angles) {
  n <- length(x_mid)
  hjust <- rep(0.5, n)
  vjust <- rep(0.5, n)
  
  for (i in seq_len(n)) {
    angle_deg <- angles[i]
    
    # Adjust hjust based on angle for better centering
    if (abs(angle_deg) > 60 && abs(angle_deg) < 120) {
      # Nearly vertical text
      vjust[i] <- 0.5
    }
  }
  
  list(hjust = hjust, vjust = vjust)
}


#' Compute Label Visibility
#' @noRd
compute_label_visibility <- function(data, fit_method, min_size) {
  show <- rep(TRUE, nrow(data))
  
  if (fit_method %in% c("filter", "both")) {
    # Hide labels for very small segments
    min_arc <- 0.08  # Minimum arc length to show label
    min_height <- 0.05  # Minimum radial height
    
    show <- show & (data$arc_length >= min_arc)
    show <- show & (data$radial_height >= min_height)
  }
  
  if (fit_method %in% c("scale", "both")) {
    # Hide if computed size fell to minimum and segment is still too small
    too_small <- (data$computed_size <= min_size * 1.1) & (data$arc_length < 0.1)
    show <- show & !too_small
  }
  
  show
}


#' Add Labels to Sunburst Plot with Advanced Control
#'
#' Add labels to a sunburst plot with control over orientation, sizing,
#' and automatic fitting.
#'
#' @param p A ggplot2 sunburst object.
#' @param data Prepared sunburst data with geometry.
#' @param label_col Column name for label text (default "category" or "proportion").
#' @param label_type Type of label: "percent", "count", "category", or "custom".
#' @param orientation Label orientation: "auto", "radial", "tangential", "horizontal".
#' @param base_size Base font size.
#' @param min_size Minimum font size.
#' @param max_size Maximum font size.
#' @param fit_labels Fit labels to segments? TRUE (default), FALSE, or "size_only".
#' @param label_color Label color: fixed color, "auto" for contrast, or "by_fill".
#' @param label_threshold Minimum proportion to show labels (used if fit_labels=FALSE).
#' @param fontface Font face: "plain", "bold", "italic".
#' @param family Font family.
#'
#' @return Modified ggplot2 object.
#'
#' @export
add_sunburst_labels <- function(
    p,
    data,
    label_col = NULL,
    label_type = "percent",
    orientation = "auto",
    base_size = 3.5,
    min_size = 2,
    max_size = 5,
    fit_labels = TRUE,
    label_color = "black",
    label_threshold = 0.05,
    fontface = "plain",
    family = ""
) {
  # Build label text
  if (is.null(label_col)) {
    data$label_text <- switch(
      label_type,
      "percent" = paste0(round(data$proportion * 100), "%"),
      "count" = as.character(data$count),
      "category" = as.character(data$category),
      "both" = paste0(data$category, "\n", round(data$proportion * 100), "%"),
      paste0(round(data$proportion * 100), "%")
    )
  } else {
    data$label_text <- as.character(data[[label_col]])
  }
  
  # Calculate label parameters
  if (isTRUE(fit_labels) || fit_labels == "size_only") {
    fit_method <- if (isTRUE(fit_labels)) "both" else "scale"
    data <- calculate_label_params(
      data,
      orientation = orientation,
      base_size = base_size,
      min_size = min_size,
      max_size = max_size,
      fit_method = fit_method
    )
  } else {
    # Simple threshold-based filtering
    # First calculate arc_length if not present
    if (!"arc_length" %in% names(data)) {
      data$arc_length <- (data$xmax - data$xmin) * data$y_mid
    }
    if (!"radial_height" %in% names(data)) {
      data$radial_height <- data$ymax - data$ymin
    }
    
    data$computed_angle <- vapply(seq_len(nrow(data)), function(i) {
      compute_label_angle(data$x_mid[i], data$arc_length[i], 
                          data$radial_height[i], orientation)
    }, numeric(1))
    data$computed_size <- base_size
    data$computed_hjust <- 0.5
    data$computed_vjust <- 0.5
    data$show_label <- data$proportion >= label_threshold
  }
  
  # Filter to visible labels
  label_data <- data[data$show_label, ]
  
  if (nrow(label_data) == 0) {
    return(p)
  }
  
  # Determine colors
  if (identical(label_color, "auto")) {
    # Would need fill colors to compute contrast - default to black
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
        label = .data$label_text,
        angle = .data$computed_angle,
        size = .data$computed_size
      ),
      hjust = label_data$computed_hjust,
      vjust = label_data$computed_vjust,
      color = label_data$text_color,
      fontface = fontface,
      family = family,
      lineheight = 0.85,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_size_identity()
}


#' Label Orientation Options
#'
#' Helper to describe available label orientation options.
#'
#' @return A character vector of orientation options with descriptions.
#'
#' @export
#'
#' @examples
#' label_orientations()
label_orientations <- function() {
  c(
    "auto" = "Automatically choose based on segment shape (default)",
    "radial" = "Text reads from center outward along radius",
    "tangential" = "Text follows the arc/circumference",
    "horizontal" = "Text is always horizontal (0 degrees)",
    "curved" = "Text curves along the arc (not yet implemented)"
  )
}
