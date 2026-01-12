#' ggsunburst: Sunburst Charts for Hierarchical and Longitudinal Data
#'
#' @description
#' Create sunburst (multilevel pie) charts using the ggplot2 framework.
#' Sunburst charts display hierarchical data as concentric rings, where each
#' ring represents a level in the hierarchy. This package is particularly
#' suited for visualizing changes in adverse event grades over time at the
#' individual patient level, but can be used for any hierarchical or

#' longitudinal categorical data.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{sunburst}}: Create a sunburst chart from hierarchical data
#'   \item \code{\link{sunburst_ae}}: Specialized function for adverse event visualization
#'   \item \code{\link{geom_sunburst}}: ggplot2 geom for sunburst segments
#'   \item \code{\link{prepare_sunburst_data}}: Convert data to sunburst format
#' }
#'
#' @section Color Scales:
#' \itemize{
#'   \item \code{\link{scale_fill_ctcae}}: CTCAE grade color scale
#'   \item \code{\link{scale_fill_severity}}: General severity color scale
#' }
#'
#' @docType package
#' @name ggsunburst-package
#' @aliases ggsunburst
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data := sym
#' @importFrom scales rescale
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom stats setNames
#' @importFrom grDevices colorRampPalette
"_PACKAGE"

# Suppress R CMD check notes about undefined global variables
utils::globalVariables(c(

  ".", "xmin", "xmax", "ymin", "ymax", "fill", "label", "x", "y",

  "ring", "segment", "value", "angle", "hjust", "n", "prop",
  "cum_prop", "start", "end", "mid", "r_inner", "r_outer",

  "label_x", "label_y", "label_angle", "segment_id", "ring_id"
))
