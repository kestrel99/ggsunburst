#' Example Adverse Event Data
#'
#' A simulated dataset of adverse event grades over time for demonstration
#' purposes. The data represents a hypothetical clinical trial with 60 patients
#' followed over 5 visits.
#'
#' @format A data frame with 300 rows and 4 columns:
#' \describe{
#'   \item{USUBJID}{Unique subject identifier (character)}
#'   \item{AVISIT}{Analysis visit name (factor with 5 levels)}
#'   \item{AVISITN}{Analysis visit number (numeric, 0-4)}
#'   \item{AETOXGR}{AE toxicity grade per CTCAE (integer, 0-4)}
#' }
#'
#' @source Simulated data
#'
#' @examples
#' data(ae_example)
#' head(ae_example)
#'
#' sunburst_ae(
#'   ae_example,
#'   time = "AVISIT",
#'   grade = "AETOXGR"
#' )
"ae_example"


#' Create Example AE Data
#'
#' Generate simulated adverse event data for testing and examples.
#'
#' @param n_patients Number of patients (default 60).
#' @param n_visits Number of visits (default 5).
#' @param visit_names Character vector of visit names. If NULL, generates
#'   default names.
#' @param grade_probs Probability vector for grades 0-4 (default weighted
#'   toward lower grades).
#' @param seed Random seed for reproducibility.
#'
#' @return A data frame with columns USUBJID, AVISIT, AVISITN, AETOXGR.
#'
#' @export
#'
#' @examples
#' ae_data <- create_example_ae_data(n_patients = 50, seed = 42)
#' sunburst_ae(ae_data, "AVISIT", "AETOXGR")
create_example_ae_data <- function(
    n_patients = 60,
    n_visits = 5,
    visit_names = NULL,
    grade_probs = c(0.30, 0.35, 0.20, 0.10, 0.05),
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(visit_names)) {
    visit_names <- c("Baseline", "Week 2", "Week 4", "Week 8", "Week 12")[1:n_visits]
  }

  n_visits <- length(visit_names)

  df <- expand.grid(
    USUBJID = paste0("SUBJ-", sprintf("%03d", 1:n_patients)),
    AVISIT = factor(visit_names, levels = visit_names),
    stringsAsFactors = FALSE
  )

  df$AVISITN <- as.numeric(df$AVISIT) - 1
  df$AETOXGR <- sample(0:4, nrow(df), replace = TRUE, prob = grade_probs)

  # Add some time-based pattern - grades tend to improve over time
  for (subj in unique(df$USUBJID)) {
    idx <- which(df$USUBJID == subj)
    # Sort by visit
    idx <- idx[order(df$AVISITN[idx])]
    # Later visits have slightly lower grades on average
    for (j in seq_along(idx)[-1]) {
      if (runif(1) < 0.3) {  # 30% chance of improvement
        df$AETOXGR[idx[j]] <- max(0, df$AETOXGR[idx[j]] - 1)
      }
    }
  }

  df <- df[order(df$USUBJID, df$AVISITN), ]
  rownames(df) <- NULL

  df
}


#' Create Example Trajectory Data
#'
#' Generate simulated longitudinal data where subjects transition between
#' categories over time. Suitable for trajectory sunburst visualization.
#'
#' @param n_subjects Number of subjects (default 60).
#' @param n_timepoints Number of timepoints (default 4).
#' @param timepoint_names Names for timepoints. If NULL, generates defaults.
#' @param categories Category levels (default 0:4 for CTCAE grades).
#' @param initial_probs Initial probability distribution across categories.
#' @param transition_tendency How likely subjects are to change category:
#'   "stable" (low change), "moderate", or "dynamic" (high change).
#' @param seed Random seed for reproducibility.
#'
#' @return A data frame with subject, visit, and grade columns.
#'
#' @export
#'
#' @examples
#' # Generate trajectory data
#' traj_data <- create_example_trajectory_data(n_subjects = 50, seed = 42)
#'
#' # Visualize trajectories
#' sunburst_trajectory(
#'   traj_data,
#'   subject = "subject",
#'   time = "visit",
#'   category = "grade",
#'   palette = "ctcae"
#' )
create_example_trajectory_data <- function(
    n_subjects = 60,
    n_timepoints = 4,
    timepoint_names = NULL,
    categories = as.character(0:4),
    initial_probs = c(0.30, 0.35, 0.20, 0.10, 0.05),
    transition_tendency = "moderate",
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  if (is.null(timepoint_names)) {
    timepoint_names <- c("Baseline", "Week 4", "Week 8", "Week 12", "Week 16", "Week 24")[1:n_timepoints]
  }
  n_timepoints <- length(timepoint_names)
  
  # Transition probability based on tendency
  stay_prob <- switch(
    transition_tendency,
    "stable" = 0.7,
    "moderate" = 0.5,
    "dynamic" = 0.3,
    0.5
  )
  
  n_cats <- length(categories)
  
  # Initialize results
  results <- vector("list", n_subjects)
  
  for (subj in seq_len(n_subjects)) {
    grades <- character(n_timepoints)
    
    # Initial category
    grades[1] <- sample(categories, 1, prob = initial_probs[1:n_cats])
    
    # Transitions for subsequent timepoints
    for (t in 2:n_timepoints) {
      current_cat <- grades[t - 1]
      current_idx <- which(categories == current_cat)
      
      if (runif(1) < stay_prob) {
        # Stay in same category
        grades[t] <- current_cat
      } else {
        # Transition - prefer adjacent categories
        trans_probs <- rep(0.1, n_cats)
        trans_probs[current_idx] <- 0
        
        # Higher probability for adjacent categories
        if (current_idx > 1) {
          trans_probs[current_idx - 1] <- 0.4  # Improvement
        }
        if (current_idx < n_cats) {
          trans_probs[current_idx + 1] <- 0.3  # Worsening
        }
        
        trans_probs <- trans_probs / sum(trans_probs)
        grades[t] <- sample(categories, 1, prob = trans_probs)
      }
    }
    
    results[[subj]] <- data.frame(
      subject = paste0("SUBJ-", sprintf("%03d", subj)),
      visit = factor(timepoint_names, levels = timepoint_names),
      grade = grades,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, results)
}


#' Create Example Response Trajectory Data
#'
#' Generate simulated tumor response data with realistic transitions.
#'
#' @param n_subjects Number of subjects.
#' @param n_timepoints Number of assessment timepoints.
#' @param seed Random seed.
#'
#' @return A data frame with subject, timepoint, and response columns.
#'
#' @export
create_example_response_trajectory <- function(
    n_subjects = 50,
    n_timepoints = 4,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  timepoint_names <- paste0("Week ", c(0, 8, 16, 24, 32, 48)[1:n_timepoints])
  responses <- c("CR", "PR", "SD", "PD")
  
  results <- vector("list", n_subjects)
  
  for (subj in seq_len(n_subjects)) {
    resp <- character(n_timepoints)
    
    # Initial response (mostly SD)
    resp[1] <- sample(responses, 1, prob = c(0.05, 0.20, 0.55, 0.20))
    
    for (t in 2:n_timepoints) {
      current <- resp[t - 1]
      
      # Transition probabilities depend on current state
      if (current == "CR") {
        # CR tends to stay CR or occasionally PR
        resp[t] <- sample(responses, 1, prob = c(0.85, 0.10, 0.03, 0.02))
      } else if (current == "PR") {
        # PR can improve to CR, stay, or worsen
        resp[t] <- sample(responses, 1, prob = c(0.15, 0.60, 0.15, 0.10))
      } else if (current == "SD") {
        # SD can go any direction
        resp[t] <- sample(responses, 1, prob = c(0.08, 0.25, 0.47, 0.20))
      } else {
        # PD tends to stay PD
        resp[t] <- sample(responses, 1, prob = c(0.02, 0.08, 0.15, 0.75))
      }
    }
    
    results[[subj]] <- data.frame(
      subject = paste0("PT", sprintf("%03d", subj)),
      timepoint = factor(timepoint_names, levels = timepoint_names),
      response = factor(resp, levels = responses),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, results)
}


#' Create Example Categorical Time Series
#'
#' Generate generic categorical data changing over time.
#'
#' @param n_units Number of units/subjects.
#' @param n_periods Number of time periods.
#' @param categories Category levels.
#' @param period_names Names for time periods.
#' @param seed Random seed.
#'
#' @return A data frame with unit, period, and category columns.
#'
#' @export
create_example_categorical_ts <- function(
    n_units = 50,
    n_periods = 4,
    categories = c("Low", "Medium", "High"),
    period_names = NULL,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(period_names)) {
    period_names <- paste0("T", seq_len(n_periods))
  }

  df <- expand.grid(
    unit = paste0("U", sprintf("%03d", 1:n_units)),
    period = factor(period_names, levels = period_names),
    stringsAsFactors = FALSE
  )

  df$category <- sample(
    categories, nrow(df),
    replace = TRUE
  )

  df$category <- factor(df$category, levels = categories)

  df
}
