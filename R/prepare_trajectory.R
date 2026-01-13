#' Prepare Trajectory Data for Sunburst Charts
#'
#' Convert longitudinal data into a hierarchical trajectory format for sunburst
#' visualization. Each subject's path through categories over time is tracked,
#' creating nested segments where inner ring groups subdivide in outer rings
#' based on category transitions.
#'
#' @param data A data frame containing longitudinal data with one row per
#'   subject per timepoint. Does not need to be rectangular - subjects may
#'   have different numbers of timepoints (dropouts, late entries, missing visits).
#' @param subject Column name (string) for subject identifier.
#' @param time Column name (string) for timepoint. Must be orderable.
#' @param category Column name (string) for the category variable (e.g., grade).
#' @param time_order Character vector specifying timepoint order from inner
#'   (earliest) to outer (latest). If NULL, uses factor levels or sorted unique values.
#' @param category_order Character vector specifying category order. If NULL,
#'   uses factor levels, numeric order, or sorted unique values.
#' @param missing_handling How to handle subjects with missing timepoints:
#'   \itemize{
#'     \item \code{"dropout"} (default): Subjects who lack data at a timepoint
#'       are marked as discontinued. When plotted, these segments are drawn
#'       invisibly to preserve angular space, creating uneven outer borders.
#'     \item \code{"carry_forward"}: Last observation carried forward (LOCF) -
#'       missing timepoints use the most recent observed category.
#'     \item \code{"show_missing"}: Missing timepoints shown as "Missing" category,
#'       subjects continue to subsequent timepoints if data exists.
#'     \item \code{"complete_only"}: Only include subjects with data at all timepoints.
#'   }
#' @param discontinued_label Label for discontinued/dropout subjects (default "Discontinued").
#' @param missing_label Label for missing observations (default "Missing").
#' @param inner_radius Inner radius of innermost ring (default 0.25).
#' @param segment_gap Small gap between top-level segments in radians (default 0.02).
#'
#' @return A data frame with columns for plotting: path (trajectory string),
#'   ring, category, count, xmin, xmax, ymin, ymax, and label positioning.
#'
#' @export
#'
#' @examples
#' # Data with dropouts
#' df <- data.frame(
#'   subject = c(rep(1:80, each = 4), rep(81:100, each = 2)),
#'   visit = c(rep(c("Baseline", "Week 4", "Week 8", "Week 12"), 80),
#'             rep(c("Baseline", "Week 4"), 20)),
#'   grade = sample(0:3, 360, replace = TRUE, prob = c(0.4, 0.35, 0.2, 0.05))
#' )
#'
#' # Show dropouts as "Discontinued"
#' prepared <- prepare_trajectory_data(
#'   df, subject = "subject", time = "visit", category = "grade",
#'   missing_handling = "dropout"
#' )
#'
#' # Use LOCF for missing
#' prepared_locf <- prepare_trajectory_data(
#'   df, subject = "subject", time = "visit", category = "grade",
#'   missing_handling = "carry_forward"
#' )
prepare_trajectory_data <- function(
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
    segment_gap = 0.02
) {
  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
 
  missing_handling <- match.arg(missing_handling)
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
  
  # Handle missing data according to specified method
  trajectory_matrix <- handle_missing_trajectories(
    trajectory_matrix,
    missing_handling = missing_handling,
    discontinued_label = discontinued_label,
    missing_label = missing_label
  )
  
  # Update category order to include special categories if needed
  special_cats <- c()
  if (missing_handling == "dropout" && any(trajectory_matrix == discontinued_label, na.rm = TRUE)) {
    special_cats <- c(special_cats, discontinued_label)
  }
  if (missing_handling == "show_missing" && any(trajectory_matrix == missing_label, na.rm = TRUE)) {
    special_cats <- c(special_cats, missing_label)
  }
  full_category_order <- c(category_order, special_cats)
  
  # Build hierarchical segment structure
  segments <- build_hierarchical_segments(
    trajectory_matrix, 
    time_order, 
    full_category_order
  )
  
  # Calculate geometry
  result <- calculate_trajectory_geometry(
    segments,
    n_rings = length(time_order),
    inner_radius = inner_radius,
    segment_gap = segment_gap,
    category_order = full_category_order
  )
  
  # Store metadata
  attr(result, "missing_handling") <- missing_handling
  attr(result, "discontinued_label") <- discontinued_label
  attr(result, "missing_label") <- missing_label
  attr(result, "n_subjects") <- nrow(trajectory_matrix)
  attr(result, "time_order") <- time_order
  
  result
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


#' Handle Missing Trajectories
#'
#' Process trajectory matrix to handle subjects with incomplete data.
#'
#' @param traj_matrix Character matrix with subjects as rows, timepoints as columns.
#' @param missing_handling Method for handling missing data.
#' @param discontinued_label Label for discontinued subjects.
#' @param missing_label Label for missing observations.
#'
#' @return Modified trajectory matrix.
#' @noRd
handle_missing_trajectories <- function(
    traj_matrix,
    missing_handling,
    discontinued_label,
    missing_label
) {
 n_subjects <- nrow(traj_matrix)
  n_times <- ncol(traj_matrix)
  
  if (missing_handling == "complete_only") {
    # Keep only subjects with data at all timepoints
    complete_rows <- apply(traj_matrix, 1, function(x) all(!is.na(x)))
    traj_matrix <- traj_matrix[complete_rows, , drop = FALSE]
    
    if (nrow(traj_matrix) == 0) {
      cli::cli_warn("No subjects have complete data at all timepoints.")
    } else {
      n_dropped <- n_subjects - nrow(traj_matrix)
      if (n_dropped > 0) {
        cli::cli_inform("{n_dropped} subject{?s} excluded due to incomplete data.")
      }
    }
    
  } else if (missing_handling == "carry_forward") {
    # Last observation carried forward (LOCF)
    for (i in seq_len(nrow(traj_matrix))) {
      last_obs <- NA_character_
      for (j in seq_len(n_times)) {
        if (!is.na(traj_matrix[i, j])) {
          last_obs <- traj_matrix[i, j]
        } else if (!is.na(last_obs)) {
          traj_matrix[i, j] <- last_obs
        }
      }
    }
    
  } else if (missing_handling == "show_missing") {
    # Replace NA with missing_label, keep subjects in subsequent timepoints
    traj_matrix[is.na(traj_matrix)] <- missing_label
    
  } else if (missing_handling == "dropout") {
    # Mark as discontinued at first missing timepoint
    # and propagate discontinued status forward
    # These segments will be drawn invisibly to preserve angular space
    for (i in seq_len(nrow(traj_matrix))) {
      discontinued <- FALSE
      for (j in seq_len(n_times)) {
        if (discontinued) {
          traj_matrix[i, j] <- discontinued_label
        } else if (is.na(traj_matrix[i, j])) {
          traj_matrix[i, j] <- discontinued_label
          discontinued <- TRUE
        }
      }
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
      # Keep NA handling in case complete_only removed all but some
      categories[is.na(categories)] <- "Missing"
      
      # Use category_order which now includes special categories
      all_levels <- unique(c(category_order, unique(categories)))
      counts <- table(factor(categories, levels = all_levels))
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
        parent_category <- parent_segments$category[p]
        
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
        
        all_levels <- unique(c(category_order, unique(current_cats)))
        counts <- table(factor(current_cats, levels = all_levels))
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
  
  # Order ring1 by category order (category_order already includes special categories)
  ring1_order <- match(ring1$category, category_order)
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
      siblings_order <- match(siblings$category, category_order)
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
  
  # Ensure category is a factor with proper levels (category_order already includes special categories)
  result$category <- factor(result$category, levels = unique(category_order))
  
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
