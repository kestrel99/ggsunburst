# Tests for plotting functions

test_that("sunburst_trajectory returns ggplot object", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  p <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("sunburst_trajectory accepts palette options", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  # CTCAE palette
  p1 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    palette = "ctcae"
  )
  expect_s3_class(p1, "ggplot")
  
  # Severity palette
  p2 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    palette = "severity"
  )
  expect_s3_class(p2, "ggplot")
  
  # Custom palette
  custom_pal <- c("0" = "grey90", "1" = "green", "2" = "yellow", 
                  "3" = "orange", "4" = "red")
  p3 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    palette = custom_pal
  )
  expect_s3_class(p3, "ggplot")
})

test_that("sunburst_trajectory handles label_orientation options", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  orientations <- c("auto", "radial", "tangential", "horizontal")
  
  for (orient in orientations) {
    p <- sunburst_trajectory(
      traj_data,
      subject = "subject",
      time = "visit",
      category = "grade",
      label_orientation = orient
    )
    expect_s3_class(p, "ggplot")
  }
})

test_that("sunburst_trajectory handles show_labels options", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  # No labels
  p1 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    show_labels = "none"
  )
  expect_s3_class(p1, "ggplot")
  
  # Percent labels
  p2 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    show_labels = "percent"
  )
  expect_s3_class(p2, "ggplot")
  
  # Count labels
  p3 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    show_labels = "count"
  )
  expect_s3_class(p3, "ggplot")
})

test_that("sunburst_trajectory handles fit_labels parameter", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  # Auto-fit enabled
  p1 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    fit_labels = TRUE
  )
  expect_s3_class(p1, "ggplot")
  
  # Auto-fit disabled
  p2 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    fit_labels = FALSE,
    label_threshold = 0.05
  )
  expect_s3_class(p2, "ggplot")
})

test_that("sunburst_trajectory handles missing_handling options", {
  dropout_data <- create_example_trajectory_data(
    n_subjects = 50,
    dropout_rate = 0.25,
    seed = 42
  )
  
  methods <- c("dropout", "carry_forward", "complete_only")
  
  for (method in methods) {
    p <- suppressMessages(sunburst_trajectory(
      dropout_data,
      subject = "subject",
      time = "visit",
      category = "grade",
      missing_handling = method
    ))
    expect_s3_class(p, "ggplot")
  }
})

test_that("sunburst_trajectory adds titles correctly", {
  traj_data <- create_example_trajectory_data(n_subjects = 20, seed = 42)
  
  p <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    title = "Test Title",
    subtitle = "Test Subtitle"
  )
  
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Title")
  expect_equal(p$labels$subtitle, "Test Subtitle")
})

test_that("sunburst_trajectory handles ring_labels parameter", {
  traj_data <- create_example_trajectory_data(n_subjects = 20, seed = 42)
  
  # With ring labels
  p1 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    ring_labels = TRUE
  )
  expect_s3_class(p1, "ggplot")
  
  # Without ring labels
  p2 <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade",
    ring_labels = FALSE
  )
  expect_s3_class(p2, "ggplot")
})

test_that("sunburst_ae_trajectory works with AE-specific parameters", {
  traj_data <- create_example_trajectory_data(n_subjects = 30, seed = 42)
  
  p <- sunburst_ae_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    grade = "grade"
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("add_center_label adds annotation", {
  traj_data <- create_example_trajectory_data(n_subjects = 20, seed = 42)
  
  p <- sunburst_trajectory(
    traj_data,
    subject = "subject",
    time = "visit",
    category = "grade"
  )
  
  p_labeled <- add_center_label(p, "N=20")
  
  expect_s3_class(p_labeled, "ggplot")
  # Check that an annotation layer was added
  expect_gt(length(p_labeled$layers), length(p$layers))
})
