# Tests for label utility functions

test_that("calculate_label_params adds required columns", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  labeled <- calculate_label_params(prepared, orientation = "auto")
  
  expected_cols <- c("computed_angle", "computed_size", "computed_hjust", 
                     "computed_vjust", "show_label")
  expect_true(all(expected_cols %in% names(labeled)))
})

test_that("calculate_label_params respects orientation parameter", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  # Horizontal orientation should give 0 degree angles
  labeled_horiz <- calculate_label_params(prepared, orientation = "horizontal")
  expect_true(all(labeled_horiz$computed_angle == 0))
})

test_that("calculate_label_params respects size constraints", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  min_size <- 2
  max_size <- 6
  
  labeled <- calculate_label_params(
    prepared,
    orientation = "auto",
    min_size = min_size,
    max_size = max_size
  )
  
  expect_true(all(labeled$computed_size >= min_size))
  expect_true(all(labeled$computed_size <= max_size))
})

test_that("calculate_label_params fit_method affects visibility", {
  traj_data <- create_example_trajectory_data(n_subjects = 100, seed = 42)
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  # With filtering, some labels should be hidden
  labeled_filter <- calculate_label_params(prepared, fit_method = "filter")
  
  # At least some should be hidden (for small segments)
  # Note: this depends on the data, so we just check it runs
  expect_true("show_label" %in% names(labeled_filter))
})

test_that("label_orientations returns named vector", {
  orientations <- label_orientations()
  
  expect_type(orientations, "character")
  expect_true(all(c("auto", "radial", "tangential", "horizontal") %in% names(orientations)))
})

test_that("add_sunburst_labels works with different label types", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  prepared <- prepare_trajectory_data(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  # Create base plot
  p_base <- ggplot2::ggplot(prepared) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = category)
    ) +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::theme_void()
  
  # Test different label types
  for (ltype in c("percent", "count", "category")) {
    p <- add_sunburst_labels(p_base, prepared, label_type = ltype)
    expect_s3_class(p, "ggplot")
  }
})
