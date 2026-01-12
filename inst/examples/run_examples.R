#!/usr/bin/env Rscript
# Example usage of ggsunburst package
# This script demonstrates the main features

library(ggsunburst)
library(ggplot2)

# Set output directory
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

cat("=== ggsunburst Package Examples ===\n\n")

# -----------------------------------------------------------------------------
# Example 1: Basic AE sunburst
# -----------------------------------------------------------------------------
cat("1. Creating basic AE sunburst chart...\n")

ae_data <- create_example_ae_data(n_patients = 25, seed = 42)

p1 <- sunburst_ae(
  ae_data,
  subject = "USUBJID",
  time = "AVISIT",
  grade = "AETOXGR",
  title = "Adverse Event Grades Over Time",
  subtitle = "Individual patient trajectories by visit"
)

ggsave(file.path(output_dir, "01_basic_ae_sunburst.png"), p1, 
       width = 10, height = 10, dpi = 150)
cat("   Saved: 01_basic_ae_sunburst.png\n\n")

# -----------------------------------------------------------------------------
# Example 2: Sorted by different criteria
# -----------------------------------------------------------------------------
cat("2. Creating sorted variants...\n")

# Sort by maximum grade
p2a <- sunburst_ae(
  ae_data, "USUBJID", "AVISIT", "AETOXGR",
  sort_by = "max_grade",
  title = "Sorted by Maximum Grade (decreasing)"
)

# Sort by last grade
p2b <- sunburst_ae(
  ae_data, "USUBJID", "AVISIT", "AETOXGR",
  sort_by = "last_grade",
  title = "Sorted by Final Grade"
)

ggsave(file.path(output_dir, "02a_sorted_max_grade.png"), p2a,
       width = 10, height = 10, dpi = 150)
ggsave(file.path(output_dir, "02b_sorted_last_grade.png"), p2b,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 02a_sorted_max_grade.png, 02b_sorted_last_grade.png\n\n")

# -----------------------------------------------------------------------------
# Example 3: Small dataset with labels
# -----------------------------------------------------------------------------
cat("3. Creating chart with cell labels...\n")

small_ae <- create_example_ae_data(n_patients = 10, seed = 123)

p3 <- sunburst_ae(
  small_ae, "USUBJID", "AVISIT", "AETOXGR",
  show_grade_labels = TRUE,
  title = "AE Grades with Cell Labels",
  subtitle = "Suitable for smaller patient populations"
)

ggsave(file.path(output_dir, "03_with_labels.png"), p3,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 03_with_labels.png\n\n")

# -----------------------------------------------------------------------------
# Example 4: Custom colors
# -----------------------------------------------------------------------------
cat("4. Creating chart with custom colors...\n")

# Green-based gradient
custom_colors <- c(
  "0" = "#f7fcf5",
  "1" = "#c7e9c0",
  "2" = "#74c476",
  "3" = "#238b45",
  "4" = "#00441b"
)

p4 <- sunburst_ae(
  ae_data, "USUBJID", "AVISIT", "AETOXGR",
  palette = custom_colors,
  title = "Custom Green Color Palette"
)

ggsave(file.path(output_dir, "04_custom_colors.png"), p4,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 04_custom_colors.png\n\n")

# -----------------------------------------------------------------------------
# Example 5: With center label
# -----------------------------------------------------------------------------
cat("5. Adding center label...\n")

p5 <- sunburst_ae(
  ae_data, "USUBJID", "AVISIT", "AETOXGR",
  title = "Study XYZ-001"
) |>
  add_center_label("N=25\nPatients", size = 5)

ggsave(file.path(output_dir, "05_center_label.png"), p5,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 05_center_label.png\n\n")

# -----------------------------------------------------------------------------
# Example 6: General sunburst with wide format
# -----------------------------------------------------------------------------
cat("6. Creating general sunburst from wide format...\n")

set.seed(456)
wide_data <- data.frame(
  region = c("North", "South", "East", "West", "Central", 
             "Northeast", "Southeast", "Northwest"),
  Q1 = sample(c("Low", "Medium", "High"), 8, replace = TRUE),
  Q2 = sample(c("Low", "Medium", "High"), 8, replace = TRUE),
  Q3 = sample(c("Low", "Medium", "High"), 8, replace = TRUE),
  Q4 = sample(c("Low", "Medium", "High"), 8, replace = TRUE)
)

p6 <- sunburst(
  wide_data,
  segments = "region",
  rings = c("Q1", "Q2", "Q3", "Q4"),
  palette = c("Low" = "#1a9850", "Medium" = "#fee08b", "High" = "#d73027"),
  legend_title = "Performance",
  title = "Regional Performance by Quarter"
)

ggsave(file.path(output_dir, "06_wide_format.png"), p6,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 06_wide_format.png\n\n")

# -----------------------------------------------------------------------------
# Example 7: Long format data
# -----------------------------------------------------------------------------
cat("7. Creating sunburst from long format...\n")

set.seed(789)
long_data <- data.frame(
  patient = rep(paste0("PT", sprintf("%02d", 1:15)), each = 5),
  timepoint = factor(rep(c("Day 0", "Day 7", "Day 14", "Day 28", "Day 56"), 15),
                     levels = c("Day 0", "Day 7", "Day 14", "Day 28", "Day 56")),
  response = factor(sample(c("CR", "PR", "SD", "PD"), 75, replace = TRUE,
                          prob = c(0.15, 0.35, 0.35, 0.15)),
                   levels = c("CR", "PR", "SD", "PD"))
)

p7 <- sunburst(
  long_data,
  segments = "patient",
  rings = "timepoint",
  fill = "response",
  palette = c("CR" = "#1b7837", "PR" = "#7fbf7b", 
              "SD" = "#d9d9d9", "PD" = "#d73027"),
  legend_title = "Response",
  title = "Tumor Response Over Time"
)

ggsave(file.path(output_dir, "07_long_format.png"), p7,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 07_long_format.png\n\n")

# -----------------------------------------------------------------------------
# Example 8: Faceted sunbursts
# -----------------------------------------------------------------------------
cat("8. Creating faceted sunburst for multiple AE terms...\n")

set.seed(321)
multi_ae <- data.frame(
  USUBJID = rep(paste0("SUBJ-", sprintf("%02d", 1:15)), each = 16),
  AVISIT = rep(rep(c("Baseline", "Week 4", "Week 8", "Week 12"), each = 4), 15),
  AETERM = rep(c("Nausea", "Fatigue", "Headache", "Diarrhea"), 60),
  AETOXGR = sample(0:3, 240, replace = TRUE, prob = c(0.45, 0.30, 0.18, 0.07))
)

p8 <- sunburst_ae_faceted(
  multi_ae,
  subject = "USUBJID",
  time = "AVISIT",
  grade = "AETOXGR",
  facet = "AETERM",
  ncol = 2
)

ggsave(file.path(output_dir, "08_faceted.png"), p8,
       width = 12, height = 12, dpi = 150)
cat("   Saved: 08_faceted.png\n\n")

# -----------------------------------------------------------------------------
# Example 9: Customized with ggplot2 additions
# -----------------------------------------------------------------------------
cat("9. Creating fully customized chart...\n")

p9 <- sunburst_ae(ae_data, "USUBJID", "AVISIT", "AETOXGR") +
  labs(
    title = "Study ABC-123: Adverse Event Profile",
    subtitle = "CTCAE grade distribution across visits",
    caption = "Data cutoff: January 2025"
  ) +
  theme_sunburst(
    base_size = 11,
    legend_position = "right"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    plot.caption = element_text(size = 9, color = "grey50")
  )

ggsave(file.path(output_dir, "09_customized.png"), p9,
       width = 11, height = 10, dpi = 150)
cat("   Saved: 09_customized.png\n\n")

# -----------------------------------------------------------------------------
# Example 10: Different inner radius and gaps
# -----------------------------------------------------------------------------
cat("10. Demonstrating geometry parameters...\n")

# Larger inner radius (more donut-like)
p10a <- sunburst(
  wide_data,
  segments = "region",
  rings = c("Q1", "Q2", "Q3", "Q4"),
  inner_radius = 0.4,
  ring_gap = 0.03,
  segment_gap = 0.02,
  palette = c("Low" = "#1a9850", "Medium" = "#fee08b", "High" = "#d73027"),
  title = "Larger Inner Radius (0.4)"
)

# Smaller inner radius with larger gaps
p10b <- sunburst(
  wide_data,
  segments = "region",
  rings = c("Q1", "Q2", "Q3", "Q4"),
  inner_radius = 0.1,
  ring_gap = 0.1,
  segment_gap = 0.03,
  palette = c("Low" = "#1a9850", "Medium" = "#fee08b", "High" = "#d73027"),
  title = "Larger Ring Gaps"
)

ggsave(file.path(output_dir, "10a_large_inner.png"), p10a,
       width = 10, height = 10, dpi = 150)
ggsave(file.path(output_dir, "10b_large_gaps.png"), p10b,
       width = 10, height = 10, dpi = 150)
cat("   Saved: 10a_large_inner.png, 10b_large_gaps.png\n\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("=== All examples completed! ===\n")
cat(sprintf("Output files saved to: %s/\n", normalizePath(output_dir)))
cat("\nFiles created:\n")
cat(paste(" -", list.files(output_dir, pattern = "\\.png$"), collapse = "\n"))
cat("\n")
