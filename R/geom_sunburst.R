#' Sunburst Geom for ggplot2
#'
#' A geom for drawing sunburst chart segments. This is primarily used
#' internally by \code{sunburst()} but can be used directly for custom plots.
#'
#' @param mapping Aesthetic mapping created by \code{aes()}.
#' @param data The data to be displayed (from \code{prepare_sunburst_data}).
#' @param stat The statistical transformation to use.
#' @param position Position adjustment.
#' @param na.rm Remove missing values?
#' @param border_color Color of segment borders (default "white").
#' @param border_width Width of segment borders (default 0.8).
#' @param show.legend Include in legend?
#' @param inherit.aes Inherit aesthetics from ggplot call?
#' @param ... Other arguments passed to layer.
#'
#' @return A ggplot2 layer.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Create example data
#' df <- data.frame(
#'   visit = rep(c("V1", "V2", "V3"), each = 100),
#'   grade = sample(0:4, 300, replace = TRUE)
#' )
#' plot_data <- prepare_sunburst_data(df, "visit", "grade")
#'
#' ggplot(plot_data) +
#'   geom_sunburst() +
#'   coord_polar(theta = "x") +
#'   theme_void()
geom_sunburst <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    border_color = "white",
    border_width = 0.8,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  # Default aesthetics for sunburst
  default_mapping <- ggplot2::aes(
    xmin = .data$xmin,
    xmax = .data$xmax,
    ymin = .data$ymin,
    ymax = .data$ymax,
    fill = .data$segment_id
  )

  if (!is.null(mapping)) {
    mapping <- modifyList(default_mapping, mapping)
  } else {
    mapping <- default_mapping
  }

  ggplot2::geom_rect(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    color = border_color,
    linewidth = border_width,
    ...
  )
}


#' Create a Basic Sunburst Plot from Prepared Data
#'
#' A convenience function to quickly create a sunburst plot from
#' data that has already been prepared with \code{prepare_sunburst_data()}.
#'
#' @param data A data frame from \code{prepare_sunburst_data()}.
#' @param border_color Color of borders between segments.
#' @param border_width Width of borders.
#' @param ... Additional arguments passed to theme.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   time = rep(c("T1", "T2", "T3"), each = 50),
#'   category = sample(LETTERS[1:4], 150, replace = TRUE)
#' )
#' plot_data <- prepare_sunburst_data(df, "time", "category")
#' plot_sunburst(plot_data)
plot_sunburst <- function(
    data,
    border_color = "white",
    border_width = 0.8,
    ...
) {
  ggplot2::ggplot(data) +
    geom_sunburst(
      border_color = border_color,
      border_width = border_width
    ) +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::theme_void(...) +
    ggplot2::ylim(0, max(data$ymax) * 1.15)
}


#' Add Segment Labels to Sunburst
#'
#' Add percentage or count labels to sunburst segments.
#'
#' @param p A ggplot2 sunburst object.
#' @param data The prepared sunburst data.
#' @param type Type of label: "percent", "count", or "both".
#' @param threshold Minimum proportion to show label (default 0.05).
#' @param size Font size for labels.
#' @param color Label color.
#'
#' @return A modified ggplot2 object.
#'
#' @export
add_segment_labels <- function(
    p,
    data,
    type = "percent",
    threshold = 0.05,
    size = 3.5,
    color = "black"
) {
  label_data <- data[data$proportion >= threshold, ]

  if (nrow(label_data) == 0) return(p)

  label_data$label <- switch(
    type,
    "percent" = paste0(round(label_data$proportion * 100), "%"),
    "count" = as.character(label_data$count),
    "both" = paste0(round(label_data$proportion * 100), "%\n(n=", label_data$count, ")"),
    paste0(round(label_data$proportion * 100), "%")
  )

  p +
    ggplot2::geom_text(
      data = label_data,
      mapping = ggplot2::aes(
        x = .data$x_mid,
        y = .data$y_mid,
        label = .data$label
      ),
      size = size,
      color = color,
      lineheight = 0.85,
      inherit.aes = FALSE
    )
}


#' Add Ring Labels to Sunburst
#'
#' Add timepoint/ring labels to a sunburst chart.
#'
#' @param p A ggplot2 sunburst object.
#' @param data The prepared sunburst data.
#' @param position Label position: "inside" or "outside".
#' @param size Font size for labels.
#' @param color Label color.
#' @param fontface Font face ("plain", "bold", "italic").
#'
#' @return A modified ggplot2 object.
#'
#' @export
add_ring_labels <- function(
    p,
    data,
    position = "inside",
    size = 3,
    color = "grey25",
    fontface = "bold"
) {
  rings <- levels(data$ring_id)

  ring_info <- data.frame(
    ring = rings,
    x = 0,  # Top of the chart
    stringsAsFactors = FALSE
  )

  for (i in seq_along(rings)) {
    ring_data <- data[data$ring_id == rings[i], ]
    if (position == "inside") {
      ring_info$y[i] <- (ring_data$ymin[1] + ring_data$ymax[1]) / 2
    } else {
      ring_info$y[i] <- ring_data$ymax[1] + 0.03
    }
  }

  p +
    ggplot2::geom_text(
      data = ring_info,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$ring
      ),
      size = size,
      color = color,
      fontface = fontface,
      inherit.aes = FALSE
    )
}
