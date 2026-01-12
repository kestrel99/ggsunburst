#' Prepare Trajectory Data for Sunburst Charts
#'
#' Convert longitudinal data into a hierarchical trajectory format for sunburst
#' visualization. Each subject's path through categories over time is tracked,
#' creating nested segments where inner ring groups subdivide in outer rings
#' based on category transitions.
#'
#' @param data A data frame containing longitudinal data with one row per
#'   subject per timepoint.
#' @param subject Column name (string) for subject identifier.
#' @param time Column name (string) for timepoint. Must be orderable.
#' @param category Column name (string) for the category variable (e.g., grade).
#' @param time_order Character vector specifying timepoint order from inner
#'   (earliest) to outer (latest). If NULL, uses factor levels or sorted unique values.
#' @param category_order Character vector specifying category order. If NULL,
#'   uses factor levels, numeric order, or sorted unique values.
#' @param inner_radius Inner radius of innermost ring (default 0.25).
#' @param segment_gap Small gap between top-level segments in radians (default 0.02).
#'
#' @return A data frame with columns for plotting: path (trajectory string),
#'   ring, category, count, xmin, xmax, ymin, ymax, and label positioning.
#'
#' @export
#'
#' @examples
#' # Track AE grade changes over time
#' df <- data.frame(
#'   subject = rep(1:100, each = 4),
#'   visit = rep(c("Baseline", "Week 4", "Week 8", "Week 12"), 100),
#'   grade = sample(0:3, 400, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05))
#' )
#' prepared <- prepare_trajectory_data(
#'   df,
#'   subject = "subject",
#'   time = "visit
#' ",
#'   category = "grade"
#' )
prepare_trajectory_data <- function(
    data,
    subject,
    time,
    category,
    time_order = NULL,
    category_order = NULL,
    inner_radius = 0.25,
    segment_gap = 0.02
) {
  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  
  data <- as.data.frame(data)
  
  required_cols <- c(subject, time, category)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Column(s) not found: {.val {missing_cols}}")
  }
  
  # Determine time order
  if (is.null(time_order)) {
    time_col <- data[[time]]
    if (is.factor(time_col)) {
      time_order <- levels(time_col)
    } else {
      time_order <- sort(unique(time_col))
    }
  }
  
 

  # Determine category order
  if (is.null(category_order)) {
    cat_col <- data[[category]]
    if (is.factor(cat_col)) {
      category_order <- levels(cat_col)
    } else {
      # Try numeric sort
      num_vals <- suppressWarnings(as.numeric(unique(cat_col)))
      if (all(!is.na(num_vals))) {
        category_order <- as.character(sort(num_vals))
      } else {
        category_order <- sort(unique(as.character(cat_col)))
      }
    }
  }
  
  # Build trajectory matrix: one row per subject, one column per timepoint
  trajectory_matrix <- build_trajectory_matrix(
    data, subject, time, category, time_order
  )
  
  # Build hierarchical segment structure
  segments <- build_hierarchical_segments(
    trajectory_matrix, 
    time_order, 
    category_order
  )
  
  # Calculate geometry
  calculate_trajectory_geometry(
    segments,
    n_rings = length(time_order),
    inner_radius = inner_radius,
    segment_gap = segment_gap,
    category_order = category_order
  )
}


#' Build Trajectory Matrix
#' @noRd
build_trajectory_matrix <- function(data, subject, time, category, time_order) {
  # Create wide format: subjects as rows, timepoints as columns
  subjects <- unique(data[[subject]])
  n_subjects <- length(subjects)
  n_times <- length(time_order)
  
  # Initialize matrix
  traj_matrix <- matrix(NA_character_, nrow = n_subjects, ncol = n_times)
  rownames(traj_matrix) <- subjects
  colnames(traj_matrix) <- time_order
  
  # Fill in values
  for (i in seq_len(nrow(data))) {
    subj <- as.character(data[[subject]][i])
    tp <- as.character(data[[time]][i])
    cat_val <- as.character(data[[category]][i])
    
    if (tp %in% time_order) {
      traj_matrix[subj, tp] <- cat_val
    }
  }
  
  traj_matrix
}


#' Build Hierarchical Segments from Trajectories
#' @noRd
build_hierarchical_segments <- function(traj_matrix, time_order, category_order) {
  n_times <- length(time_order)
  segments_list <- vector("list", n_times)
  
  for (ring in seq_len(n_times)) {
    if (ring == 1) {
      # First ring: just count by initial category
      categories <- traj_matrix[, 1]
      categories[is.na(categories)] <- "Missing"
      
      counts <- table(factor(categories, levels = c(category_order, "Missing")))
      counts <- counts[counts > 0]
      
      segments_list[[ring]] <- data.frame(
        ring = ring,
        path = names(counts),
        category = names(counts),
        count = as.integer(counts),
        parent_path = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      # Subsequent rings: subdivide each parent path
      parent_segments <- segments_list[[ring - 1]]
      
      child_list <- lapply(seq_len(nrow(parent_segments)), function(p) {
        parent_path <- parent_segments$path[p]
        
        # Find subjects matching this parent path
        if (ring == 2) {
          # Parent path is just the first category
          matching_subjects <- traj_matrix[, 1] == parent_path
        } else {
          # Parent path is trajectory up to previous ring
          path_parts <- strsplit(parent_path, " > ")[[1]]
          matching_subjects <- rep(TRUE, nrow(traj_matrix))
          for (t in seq_along(path_parts)) {
            matching_subjects <- matching_subjects & 
              (traj_matrix[, t] == path_parts[t] | 
                 (is.na(traj_matrix[, t]) & path_parts[t] == "Missing"))
          }
        }
        matching_subjects[is.na(matching_subjects)] <- FALSE
        
        if (sum(matching_subjects) == 0) {
          return(NULL)
        }
        
        # Get categories at current ring for matching subjects
        current_cats <- traj_matrix[matching_subjects, ring, drop = TRUE]
        current_cats[is.na(current_cats)] <- "Missing"
        
        counts <- table(factor(current_cats, levels = c(category_order, "Missing")))
        counts <- counts[counts > 0]
        
        if (length(counts) == 0) {
          return(NULL)
        }
        
        data.frame(
          ring = ring,
          path = paste(parent_path, names(counts), sep = " > "),
          category = names(counts),
          count = as.integer(counts),
          parent_path = parent_path,
          stringsAsFactors = FALSE
        )
      })
      
      segments_list[[ring]] <- do.call(rbind, child_list)
    }
  }
  
  do.call(rbind, segments_list)
}


#' Calculate Trajectory Geometry
#' @noRd
calculate_trajectory_geometry <- function(
    segments, 
    n_rings, 
    inner_radius, 
    segment_gap,
    category_order
) {
  # Calculate ring dimensions
  ring_width <- (1 - inner_radius) / n_rings
  
  # Get first ring segments for initial angular allocation
  ring1 <- segments[segments$ring == 1, ]
  n_top_segments <- nrow(ring1)
  
  # Total angle minus gaps
 
  total_angle <- 2 * pi - (n_top_segments * segment_gap)
  total_count <- sum(ring1$count)
  
  # Calculate angular positions for first ring
  ring1$proportion <- ring1$count / total_count
  ring1$cum_prop <- cumsum(ring1$proportion)
  ring1$start_prop <- c(0, ring1$cum_prop[-nrow(ring1)])
  
  # Order ring1 by category order
  ring1_order <- match(ring1$category, c(category_order, "Missing"))
  ring1_order[is.na(ring1_order)] <- length(category_order) + 1
  ring1 <- ring1[order(ring1_order), ]
  ring1$cum_prop <- cumsum(ring1$proportion)
  ring1$start_prop <- c(0, ring1$cum_prop[-nrow(ring1)])
  
  result_list <- vector("list", nrow(segments))
  
  for (i in seq_len(nrow(segments))) {
    seg <- segments[i, ]
    ring_idx <- seg$ring
    
    # Radial position
    y_start <- inner_radius + (ring_idx - 1) * ring_width
    y_end <- y_start + ring_width
    
    if (ring_idx == 1) {
      # First ring: use calculated proportions
      seg_row <- which(ring1$path == seg$path)
      start_prop <- ring1$start_prop[seg_row]
      end_prop <- ring1$cum_prop[seg_row]
      
      # Add segment index for gap
      seg_idx <- seg_row
      x_start <- start_prop * total_angle + (seg_idx - 1) * segment_gap
      x_end <- x_start + (end_prop - start_prop) * total_angle
    } else {
      # Find parent's angular extent
      parent_path <- seg$parent_path
      parent_row <- which(result_list_to_df(result_list)$path == parent_path)
      
      if (length(parent_row) == 0) {
        # Parent not yet processed, find in segments
        parent_seg <- segments[segments$path == parent_path, ]
        # This shouldn't happen if we process in order, but handle gracefully
        next
      }
      
      parent_geom <- result_list[[parent_row]]
      parent_x_start <- parent_geom$xmin
      parent_x_end <- parent_geom$xmax
      parent_width <- parent_x_end - parent_x_start
      
      # Find all siblings (same parent) and order by category
      siblings <- segments[segments$parent_path == parent_path & segments$ring == ring_idx, ]
      siblings_order <- match(siblings$category, c(category_order, "Missing"))
      siblings_order[is.na(siblings_order)] <- length(category_order) + 1
      siblings <- siblings[order(siblings_order), ]
      
      # Calculate proportions within parent
      siblings$proportion <- siblings$count / sum(siblings$count)
      siblings$cum_prop <- cumsum(siblings$proportion)
      siblings$start_prop <- c(0, siblings$cum_prop[-nrow(siblings)])
      
      # Find this segment's position among siblings
      sib_row <- which(siblings$path == seg$path)
      start_prop <- siblings$start_prop[sib_row]
      end_prop <- siblings$cum_prop[sib_row]
      
      x_start <- parent_x_start + start_prop * parent_width
      x_end <- parent_x_start + end_prop * parent_width
    }
    
    # Mid points for labels
    x_mid <- (x_start + x_end) / 2
    y_mid <- (y_start + y_end) / 2
    
    result_list[[i]] <- data.frame(
      ring = ring_idx,
      path = seg$path,
      category = seg$category,
      count = seg$count,
      parent_path = seg$parent_path,
      xmin = x_start,
      xmax = x_end,
      ymin = y_start,
      ymax = y_end,
      x_mid = x_mid,
      y_mid = y_mid,
      label_angle = calculate_label_angle(x_mid),
      stringsAsFactors = FALSE
    )
  }
  
  result <- do.call(rbind, result_list)
  
  # Add ring total and proportions
  for (r in unique(result$ring)) {
    ring_total <- sum(result$count[result$ring == r])
    result$ring_total[result$ring == r] <- ring_total
  }
  result$proportion <- result$count / result$ring_total
  
  # Add segment width for label filtering
  result$segment_width <- result$xmax - result$xmin
  
  # Ensure category is a factor with proper levels
  result$category <- factor(result$category, levels = c(category_order, "Missing"))
  
  result
}


#' Helper to convert result list to data frame
#' @noRd
result_list_to_df <- function(result_list) {
  non_null <- result_list[!sapply(result_list, is.null)]
  if (length(non_null) == 0) {
    return(data.frame(path = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, non_null)
}


#' Calculate Label Angle for Readability
#' @noRd
calculate_label_angle <- function(angle_rad) {
  angle_deg <- (angle_rad * 180 / pi) - 90
  ifelse(angle_deg > 90 & angle_deg < 270,
         angle_deg - 180,
         angle_deg)
}


#' Prepare Adverse Event Trajectory Data
#'
#' Convenience function for preparing AE grade trajectory data.
#'
#' @param data A data frame with AE data.
#' @param subject Column name for subject identifier.
#' @param time Column name for timepoint/visit.
#' @param grade Column name for AE grade.
#' @param time_order Order of timepoints (inner to outer).
#' @param ... Additional arguments passed to \code{prepare_trajectory_data}.
#'
#' @return A data frame ready for trajectory sunburst plotting.
#'
#' @export
prepare_ae_trajectory <- function(
    data,
    subject,
    time,
    grade,
    time_order = NULL,
    ...
) {
  # Default grade order for CTCAE
  grade_order <- as.character(0:5)
  
  prepare_trajectory_data(
    data = data,
    subject = subject,
    time = time,
    category = grade,
    time_order = time_order,
    category_order = grade_order,
    ...
  )
}
