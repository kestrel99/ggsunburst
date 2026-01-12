#' Sunburst Theme
#'
#' A minimal theme optimized for sunburst charts, based on \code{theme_void()}
#' with customizations for legend and title positioning.
#'
#' @param base_size Base font size.
#' @param legend_position Legend position: "right" (default), "left", "bottom",
#'   "top", or "none".
#' @param title_position Title position: "center" (default) or "left".
#'
#' @return A ggplot2 theme object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- create_example_ae_data(n_patients = 50, seed = 42)
#'
#' sunburst_ae(df, "AVISIT", "AETOXGR") +
#'   theme_sunburst(legend_position = "bottom")
theme_sunburst <- function(
    base_size = 11,
    legend_position = "right",
    title_position = "center"
) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      # Legend
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        size = base_size,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(size = base_size * 0.9),
      legend.key.size = ggplot2::unit(0.8, "lines"),

      # Titles
      plot.title = ggplot2::element_text(
        size = base_size * 1.3,
        face = "bold",
        hjust = if (title_position == "center") 0.5 else 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        hjust = if (title_position == "center") 0.5 else 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.8,
        hjust = 1,
        margin = ggplot2::margin(t = 10)
      ),

      # Margin
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}


#' Add Center Label to Sunburst
#'
#' Add a label or annotation to the center of a sunburst chart.
#'
#' @param p A ggplot2 object (sunburst chart).
#' @param label Text to display in center.
#' @param size Font size.
#' @param color Text color.
#' @param fontface Font face ("plain", "bold", "italic", "bold.italic").
#'
#' @return A modified ggplot2 object.
#'
#' @export
#'
#' @examples
#' df <- create_example_ae_data(n_patients = 50, seed = 42)
#' p <- sunburst_ae(df, "AVISIT", "AETOXGR")
#' add_center_label(p, "N=50\nPatients", size = 5)
add_center_label <- function(
    p,
    label,
    size = 4,
    color = "grey30",
    fontface = "bold"
) {
  center_data <- data.frame(x = 0, y = 0, label = label)

  p +
    ggplot2::geom_text(
      data = center_data,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = size,
      color = color,
      fontface = fontface,
      lineheight = 0.9,
      inherit.aes = FALSE
    )
}


#' Calculate Summary Statistics for Sunburst Data
#'
#' Compute summary statistics for data being visualized in a sunburst chart.
#'
#' @param data Data frame with raw data (before preparation).
#' @param rings Column name for rings (timepoints).
#' @param segments Column name for segments (categories).
#'
#' @return A list with summary statistics including counts and proportions
#'   by ring and segment.
#'
#' @export
#'
#' @examples
#' df <- create_example_ae_data(n_patients = 50, seed = 42)
#' sunburst_summary(df, "AVISIT", "AETOXGR")
sunburst_summary <- function(data, rings, segments) {
  # Overall distribution
  segment_dist <- table(data[[segments]])
  ring_dist <- table(data[[rings]])

  # Cross-tabulation
  cross_tab <- table(data[[rings]], data[[segments]])

  # Proportions within each ring
  prop_by_ring <- prop.table(cross_tab, margin = 1)

  list(
    n_total = nrow(data),
    n_rings = length(unique(data[[rings]])),
    n_segments = length(unique(data[[segments]])),
    segment_distribution = segment_dist,
    ring_distribution = ring_dist,
    counts_by_ring_segment = cross_tab,
    proportions_by_ring = round(prop_by_ring, 3)
  )
}


#' Export Sunburst Chart
#'
#' Convenience function to save a sunburst chart with appropriate defaults.
#'
#' @param plot A ggplot2 sunburst chart.
#' @param filename Output filename.
#' @param width Width in inches (default 10).
#' @param height Height in inches (default 10).
#' @param dpi Resolution (default 300).
#' @param bg Background color (default "white").
#' @param ... Additional arguments passed to \code{ggsave()}.
#'
#' @return Invisibly returns the filename.
#'
#' @export
save_sunburst <- function(
    plot,
    filename,
    width = 10,
    height = 10,
    dpi = 300,
    bg = "white",
    ...
) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = bg,
    ...
  )

  invisible(filename)
}


#' Create Sunburst with Multiple Panels
#'
#' Create a grid of sunburst charts for different subgroups.
#'
#' @param data A data frame with the source data.
#' @param rings Column name for rings (timepoints).
#' @param segments Column name for segments (categories).
#' @param facet Column name for faceting variable.
#' @param ncol Number of columns in the facet grid.
#' @param palette Color palette.
#' @param ... Additional arguments passed to sunburst plotting.
#'
#' @return A ggplot2 object with facets.
#'
#' @export
sunburst_faceted <- function(
    data,
    rings,
    segments,
    facet,
    ncol = NULL,
    palette = "ctcae",
    ...
) {
  # Get unique facet values
  facet_vals <- unique(data[[facet]])
  n_facets <- length(facet_vals)

  if (is.null(ncol)) {
    ncol <- ceiling(sqrt(n_facets))
  }

  # Prepare data for each facet
  all_data <- lapply(facet_vals, function(fv) {
    subset_df <- data[data[[facet]] == fv, ]
    prep <- prepare_sunburst_data(
      subset_df,
      rings = rings,
      segments = segments,
      ...
    )
    prep$facet_var <- fv
    prep
  })

  plot_data <- do.call(rbind, all_data)

  # Build faceted plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$segment_id
      ),
      color = "white",
      linewidth = 0.5
    ) +
    ggplot2::coord_polar(theta = "x", start = 0) +
    ggplot2::facet_wrap(~ facet_var, ncol = ncol) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    )

  # Add fill scale
  if (palette == "ctcae") {
    p <- p + scale_fill_ctcae()
  } else if (!is.null(palette)) {
    p <- p + ggplot2::scale_fill_manual(values = palette)
  }

  p
}
