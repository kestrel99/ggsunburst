# Tests for data generation functions

test_that("create_example_trajectory_data generates correct structure", {
 traj_data <- create_example_trajectory_data(n_subjects = 50, seed = 42)
  
  expect_s3_class(traj_data, "data.frame")
  expect_true(all(c("subject", "visit", "grade") %in% names(traj_data)))
  expect_equal(length(unique(traj_data$subject)), 50)
  expect_s3_class(traj_data$visit, "factor")
})

test_that("create_example_trajectory_data respects n_timepoints", {
  traj_data <- create_example_trajectory_data(
    n_subjects = 20, 
    n_timepoints = 3,
    seed = 42
  )
  
  expect_equal(nlevels(traj_data$visit), 3)
  # Without dropouts, should have n_subjects * n_timepoints rows
  expect_equal(nrow(traj_data), 20 * 3)
})

test_that("create_example_trajectory_data handles dropout_rate", {
  # With dropouts
  dropout_data <- create_example_trajectory_data(
    n_subjects = 100, 
    dropout_rate = 0.3,
    seed = 42
  )
  
  # Should have fewer rows than complete data
  expect_lt(nrow(dropout_data), 100 * 4)
  
  # Baseline should have all subjects
  baseline_count <- sum(dropout_data$visit == "Baseline")
  expect_equal(baseline_count, 100)
  
  # Later visits should have fewer
  week12_count <- sum(dropout_data$visit == "Week 12")
  expect_lt(week12_count, 100)
})

test_that("create_example_trajectory_data uses custom categories", {
  custom_cats <- c("Low", "Medium", "High")
  traj_data <- create_example_trajectory_data(
    n_subjects = 30,
    categories = custom_cats,
    initial_probs = c(0.5, 0.3, 0.2),
    seed = 42
  )
  
  expect_true(all(traj_data$grade %in% custom_cats))
})

test_that("create_example_trajectory_data transition_tendency affects stability", {
  set.seed(123)
  
  stable_data <- create_example_trajectory_data(
    n_subjects = 100, 
    transition_tendency = "stable",
    seed = 123
  )
  
  dynamic_data <- create_example_trajectory_data(
    n_subjects = 100, 
    transition_tendency = "dynamic",
    seed = 123
  )
  
 # Count transitions (changes between consecutive visits)
  count_transitions <- function(data) {
    subjects <- unique(data$subject)
    transitions <- 0
    for (subj in subjects) {
      subj_data <- data[data$subject == subj, ]
      subj_data <- subj_data[order(subj_data$visit), ]
      if (nrow(subj_data) > 1) {
        for (i in 2:nrow(subj_data)) {
          if (subj_data$grade[i] != subj_data$grade[i-1]) {
            transitions <- transitions + 1
          }
        }
      }
    }
    transitions
  }
  
  stable_transitions <- count_transitions(stable_data)
  dynamic_transitions <- count_transitions(dynamic_data)
  
  # Dynamic should have more transitions than stable
 expect_gt(dynamic_transitions, stable_transitions)
})

test_that("create_example_response_trajectory generates tumor response data", {
  resp_data <- create_example_response_trajectory(n_subjects = 40, seed = 42)
  
  expect_s3_class(resp_data, "data.frame")
  expect_true(all(c("subject", "timepoint", "response") %in% names(resp_data)))
  expect_true(all(resp_data$response %in% c("CR", "PR", "SD", "PD")))
})
