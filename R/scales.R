#' CTCAE Grade Color Scale
#'
#' A color scale designed for Common Terminology Criteria for Adverse Events
#' (CTCAE) grades 0-5. Colors are chosen to be colorblind-friendly and
#' intuitively map to severity.
#'
#' @param ... Arguments passed to \code{scale_fill_manual}.
#' @param name Legend title (default "Grade").
#' @param na.value Color for NA values (default "grey90").
#' @param include_grade_5 Include Grade 5 (Death) in scale? Default TRUE.
#'
#' @return A ggplot2 scale object.
#'
#' @details
#' The default color mapping is:
#' \itemize{
#'   \item Grade 0 (None): Light grey (#E8E8E8)
#'   \item Grade 1 (Mild): Light green (#93C47D)
#'   \item Grade 2 (Moderate): Yellow (#FFD966)
#'   \item Grade 3 (Severe): Orange (#E69138)
#'   \item Grade 4 (Life-threatening): Red (#CC0000)
#'   \item Grade 5 (Death): Dark purple/black (#1C1C1C)
#' }
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   x = 1:6,
#'   grade = factor(0:5)
#' )
#'
#' ggplot(df, aes(x, y = 1, fill = grade)) +
#'   geom_tile() +
#'   scale_fill_ctcae()
scale_fill_ctcae <- function(
    ...,
    name = "Grade",
    na.value = "grey90",
    include_grade_5 = TRUE
) {
  colors <- ctcae_colors(include_grade_5 = include_grade_5)

  ggplot2::scale_fill_manual(
    name = name,
    values = colors,
    na.value = na.value,
    labels = ctcae_labels(include_grade_5 = include_grade_5),
    ...
  )
}


#' CTCAE Color Palette
#'
#' Returns the color palette for CTCAE grades.
#'
#' @param include_grade_5 Include Grade 5?
#' @return Named character vector of colors.
#' @export
ctcae_colors <- function(include_grade_5 = TRUE) {
  colors <- c(
    "0" = "#E8E8E8",  # Light grey - No AE
    "1" = "#93C47D",  # Light green - Mild
    "2" = "#FFD966",  # Yellow - Moderate
    "3" = "#E69138",  # Orange - Severe
    "4" = "#CC0000"   # Red - Life-threatening
  )

  if (include_grade_5) {
    colors["5"] <- "#1C1C1C"  # Near black - Death
  }

  colors
}


#' CTCAE Grade Labels
#'
#' Returns standard labels for CTCAE grades.
#'
#' @param include_grade_5 Include Grade 5?
#' @return Named character vector of labels.
#' @export
ctcae_labels <- function(include_grade_5 = TRUE) {
  labels <- c(
    "0" = "0 - None",
    "1" = "1 - Mild",
    "2" = "2 - Moderate",
    "3" = "3 - Severe",
    "4" = "4 - Life-threatening"
  )

  if (include_grade_5) {
    labels["5"] <- "5 - Death"
  }

  labels
}


#' General Severity Color Scale
#'
#' A color scale for general severity categories (Low, Medium, High, etc.)
#' or any ordered categorical variable.
#'
#' @param ... Arguments passed to \code{scale_fill_manual}.
#' @param name Legend title.
#' @param levels Character vector of level names (in order from low to high).
#' @param low_color Color for lowest severity.
#' @param high_color Color for highest severity.
#' @param na.value Color for NA values.
#'
#' @return A ggplot2 scale object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   x = 1:3,
#'   severity = factor(c("Low", "Medium", "High"),
#'                     levels = c("Low", "Medium", "High"))
#' )
#'
#' ggplot(df, aes(x, y = 1, fill = severity)) +
#'   geom_tile() +
#'   scale_fill_severity(levels = c("Low", "Medium", "High"))
scale_fill_severity <- function(
    ...,
    name = NULL,
    levels = NULL,
    low_color = "#93C47D",
    high_color = "#CC0000",
    na.value = "grey90"
) {
  if (is.null(levels)) {
    # Common default levels
    levels <- c("None", "Low", "Mild", "Medium", "Moderate",
                "High", "Severe", "Critical")
  }

  # Generate color gradient
  n <- length(levels)
  colors <- grDevices::colorRampPalette(c(low_color, "#FFD966", high_color))(n)
  names(colors) <- levels

  ggplot2::scale_fill_manual(
    name = name,
    values = colors,
    na.value = na.value,
    ...
  )
}


#' Sequential Color Scale for Sunburst
#'
#' A sequential color scale suitable for sunburst charts with many levels.
#'
#' @param ... Arguments passed to \code{scale_fill_gradientn}.
#' @param name Legend title.
#' @param palette Palette name: "viridis", "plasma", "inferno", "magma",
#'   "blues", "reds", "greens", or "oranges".
#' @param na.value Color for NA values.
#'
#' @return A ggplot2 scale object.
#'
#' @export
scale_fill_sunburst_seq <- function(
    ...,
    name = NULL,
    palette = "viridis",
    na.value = "grey90"
) {
  if (palette %in% c("viridis", "plasma", "inferno", "magma")) {
    ggplot2::scale_fill_viridis_d(
      name = name,
      option = palette,
      na.value = na.value,
      ...
    )
  } else {
    # RColorBrewer palettes
    ggplot2::scale_fill_brewer(
      name = name,
      palette = switch(
        palette,
        "blues" = "Blues",
        "reds" = "Reds",
        "greens" = "Greens",
        "oranges" = "Oranges",
        "Blues"
      ),
      na.value = na.value,
      ...
    )
  }
}


#' Diverging Color Scale for Sunburst
#'
#' A diverging color scale for sunburst charts showing change from baseline
#' or deviation from a neutral value.
#'
#' @param ... Arguments passed to \code{scale_fill_manual}.
#' @param name Legend title.
#' @param levels Character vector of level names.
#' @param mid_level The neutral/middle level name.
#' @param low_color Color for values below mid.
#' @param mid_color Color for mid value.
#' @param high_color Color for values above mid.
#' @param na.value Color for NA values.
#'
#' @return A ggplot2 scale object.
#'
#' @export
scale_fill_sunburst_div <- function(
    ...,
    name = NULL,
    levels = c("Much Worse", "Worse", "Same", "Better", "Much Better"),
    mid_level = "Same",
    low_color = "#CC0000",
    mid_color = "#FFFFBF",
    high_color = "#1A9850",
    na.value = "grey90"
) {
  n <- length(levels)
  mid_idx <- which(levels == mid_level)

  if (length(mid_idx) == 0) {
    mid_idx <- ceiling(n / 2)
  }

  # Generate colors
  n_low <- mid_idx - 1
  n_high <- n - mid_idx

  colors_low <- if (n_low > 0) {
    grDevices::colorRampPalette(c(low_color, mid_color))(n_low + 1)[1:n_low]
  } else {
    character(0)
  }

  colors_high <- if (n_high > 0) {
    grDevices::colorRampPalette(c(mid_color, high_color))(n_high + 1)[2:(n_high + 1)]
  } else {
    character(0)
  }

  colors <- c(colors_low, mid_color, colors_high)
  names(colors) <- levels

  ggplot2::scale_fill_manual(
    name = name,
    values = colors,
    na.value = na.value,
    ...
  )
}
