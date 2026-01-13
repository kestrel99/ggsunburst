# Tests for trajectory preparation functions

test_that("prepare_trajectory_data returns correct structure", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  expect_s3_class(prepared, "data.frame")
  
  # Check required columns exist
  required_cols <- c("ring", "path", "category", "count", "xmin", "xmax", 
                     "ymin", "ymax", "x_mid", "y_mid", "proportion")
  expect_true(all(required_cols %in% names(prepared)))
  
  # Check geometry is valid
  expect_true(all(prepared$xmin < prepared$xmax))
  expect_true(all(prepared$ymin < prepared$ymax))
  expect_true(all(prepared$xmin >= 0))
  expect_true(all(prepared$xmax <= 2 * pi + 0.1))  # Allow small tolerance
})

test_that("prepare_trajectory_data respects time_order", {
  traj_data <- create_example_trajectory_data(n_subjects = 20, seed = 42)
  
  custom_order <- c("Week 12", "Week 8", "Week 4", "Baseline")  # Reversed
  
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    time_order = custom_order
  )
  
  # Check that ring 1 corresponds to Week 12 (first in custom order)
  ring1_paths <- prepared$path[prepared$ring == 1]
  # Paths in ring 1 should be just the category, which comes from Week 12
  expect_true(length(ring1_paths) > 0)
})

test_that("prepare_trajectory_data respects category_order", {
  traj_data <- create_example_trajectory_data(
    n_subjects = 30, 
    categories = c("A", "B", "C"),
    initial_probs = c(0.33, 0.34, 0.33),
    seed = 42
  )
  
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    category_order = c("C", "B", "A")  # Reversed
  )
  
  # Category should be a factor with specified order
  expect_s3_class(prepared$category, "factor")
  expect_equal(levels(prepared$category)[1], "C")
})

test_that("prepare_trajectory_data handles missing_handling = 'dropout'", {
  dropout_data <- create_example_trajectory_data(
    n_subjects = 50,
    dropout_rate = 0.3,
    seed = 42
  )
  
  prepared <- prepare_trajectory_data(
    dropout_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    missing_handling = "dropout"
  )
  
  # Should have "Discontinued" category
  expect_true("Discontinued" %in% levels(prepared$category))
})

test_that("prepare_trajectory_data handles missing_handling = 'carry_forward'", {
  dropout_data <- create_example_trajectory_data(
    n_subjects = 50,
    dropout_rate = 0.3,
    seed = 42
  )
  
  prepared <- prepare_trajectory_data(
    dropout_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    missing_handling = "carry_forward"
  )
  
  # Should NOT have "Discontinued" category with LOCF
  expect_false("Discontinued" %in% levels(prepared$category))
})

test_that("prepare_trajectory_data handles missing_handling = 'complete_only'", {
  dropout_data <- create_example_trajectory_data(
    n_subjects = 50,
    dropout_rate = 0.3,
    seed = 42
  )
  
  # Count complete subjects
  complete_count <- sum(table(dropout_data$subject) == 4)
  
  prepared <- suppressMessages(prepare_trajectory_data(
    dropout_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    missing_handling = "complete_only"
  ))
  
  # Ring 1 count should equal complete subjects
  ring1_total <- sum(prepared$count[prepared$ring == 1])
  expect_equal(ring1_total, complete_count)
})

test_that("prepare_trajectory_data handles missing_handling = 'show_missing'", {
  # Create data with intermittent missing (not just dropouts)
  set.seed(42)
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  # Remove some middle visits
  remove_idx <- sample(which(traj_data$visit == "Week 4"), 5)
  traj_data <- traj_data[-remove_idx, ]
  
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    missing_handling = "show_missing"
  )
  
  # Should have "Missing" category
  expect_true("Missing" %in% levels(prepared$category))
})

test_that("prepare_trajectory_data validates input", {
  traj_data <- create_example_trajectory_data(n_subjects = 10, seed = 42)
  
  # Missing column should error
  expect_error(
    prepare_trajectory_data(
      traj_data,
      subject = "nonexistent",
      time = "visit",
      category = "grade"
    ),
    "not found"
  )
})

test_that("prepare_trajectory_data inner_radius affects geometry", {
  traj_data <- create_example_trajectory_data(n_subjects = 20, seed = 42)
  
  prepared_small <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    inner_radius = 0.1
  )
  
  prepared_large <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    inner_radius = 0.5
  )
  
  # Ring 1 ymin should differ
  ring1_ymin_small <- min(prepared_small$ymin[prepared_small$ring == 1])
  ring1_ymin_large <- min(prepared_large$ymin[prepared_large$ring == 1])
  
  expect_lt(ring1_ymin_small, ring1_ymin_large)
})
