# ggsunburst

Trajectory-based sunburst visualizations for longitudinal data in R.

## Overview

The `ggsunburst` package creates sunburst charts that show how groups of subjects flow between categories over time. Unlike traditional sunburst charts that display hierarchical proportions at a single point, trajectory sunbursts reveal **longitudinal patterns** and **transitions**.

### Reading the Chart

- **Inner ring** = earliest timepoint (e.g., Baseline)
- **Outer rings** = subsequent timepoints (e.g., Week 4, Week 8, Week 12)
- **Segment width** = proportion of subjects following that trajectory
- **Colors** = category at each timepoint
- **Follow any radial slice** from center to edge = one unique trajectory path

## Installation

```r
# Install from local tarball
install.packages("ggsunburst_0.1.0.tar.gz", repos = NULL, type = "source")

# Or install development version
# devtools::install_github("username/ggsunburst")
```

## Quick Start

```r
library(ggsunburst)

# Generate example trajectory data
traj_data <- create_example_trajectory_data(
  n_subjects = 80,
  n_timepoints = 4,
  seed = 42
)

# Create trajectory sunburst
sunburst_trajectory(
  traj_data,
  subject = "subject",
  time = "visit",
  category = "grade",
  palette = "ctcae",
  title = "AE Grade Trajectories Over Time"
)
```

## Key Features

### Handling Missing Data (Non-Rectangular Datasets)

Real clinical data often has dropouts. The `missing_handling` parameter controls display:
 
| Option | Description |
|--------|-------------|
| `"dropout"` (default) | Discontinued segments drawn invisibly, creating uneven outer border |
| `"carry_forward"` | LOCF imputation - missing values use last observed category |
| `"show_missing"` | Missing shown as separate category |
| `"complete_only"` | Only subjects with all timepoints included |

```r
# Data with 15% dropout rate
dropout_data <- create_example_trajectory_data(
  n_subjects = 100,
  dropout_rate = 0.15,
  seed = 123
)

# Show gaps where patients dropped out
sunburst_trajectory(
  dropout_data,
  subject = "subject",
  time = "visit",
  category = "grade",
  missing_handling = "dropout",
  palette = "ctcae"
)
```

### Label Orientation and Auto-Fit

Control how labels are oriented and sized within segments:

```r
sunburst_trajectory(
  dropout_data,
  subject = "subject",
  time = "visit",
  category = "grade",
  label_orientation = "auto",  # "auto", "radial", "tangential", "horizontal"
  fit_labels = TRUE,           # Auto-size labels to fit segments
  label_size = 3.5,            # Base font size
  label_min_size = 2,          # Minimum size
  label_max_size = 5           # Maximum size
)
```

### Color Palettes

Built-in palettes for clinical data:

```r
# CTCAE grading colors
sunburst_trajectory(..., palette = "ctcae")

# Severity gradient
sunburst_trajectory(..., palette = "severity")

# Custom palette
custom_colors <- c("0" = "grey90", "1" = "green", "2" = "yellow", 
                   "3" = "orange", "4" = "red")
sunburst_trajectory(..., palette = custom_colors)
```

### Center Labels

Add annotations to the center:

```r
sunburst_trajectory(...) |>
  add_center_label("N=80\nPatients", size = 5)
```

## Specialized Functions

### AE Trajectory Sunburst

You can also make use of the adverse event convenience wrapper with CTCAE defaults...

```r

ae_data <- create_example_ae_data()

sunburst_ae_trajectory(
  ae_data,
  subject = "USUBJID",
  time = "AVISIT",
  grade = "AETOXGR",
  title = "Adverse Event Grade Trajectories"
)
```

### Tumor Response Trajectories

```r
response_data <- create_example_response_trajectory(n_subjects = 60, seed = 42)

sunburst_trajectory(
  response_data,
  subject = "subject",
  time = "timepoint",
  category = "response",
  category_order = c("CR", "PR", "SD", "PD"),
  palette = c("CR" = "#1a9850", "PR" = "#91cf60", 
              "SD" = "#d9d9d9", "PD" = "#d73027")
)
```

## Integration with `ggplot2`

Returns a `ggplot2` object for further customization:

```r
sunburst_trajectory(...) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(caption = "Data source: Clinical trial XYZ")
```

## Requirements
 
- R >= 4.0.0
- `ggplot2` >= 3.4.0
- `dplyr`
- `tidyr`
- `rlang`
- `scales`
- `cli`

## License

GPL2

## Contributing

Contributions welcome! Please open an issue or submit a pull request.
