# ggsunburst

Create sunburst (multilevel pie) charts using ggplot2 for visualizing **trajectories** through categorical states over time. Useful for things like:

- **Adverse event grade changes** - Track how patients transition between grades
- **Treatment response flow** - Visualize transitions over assessments
- **Any longitudinal categorical data** - Show how groups flow between categories

## Sunburst plots

Each **segment in the inner ring** represents a group of subjects with that category at baseline. Moving outward, each segment **subdivides** based on the category at subsequent timepoints, showing how the group transitions. Segments never overlap and the the hierarchical structure is preserved. See (https://www.page-meeting.org/wp-content/uploads/pdf_assets/11030-abstract.pdf)[this PAGE poster] for an example for how plots like this might be used in practice. 

## Installation

```r
install.packages("path/to/ggsunburst_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Quick Start

```r
library(ggsunburst)

# Generate example trajectory data
traj_data <- create_example_trajectory_data(n_subjects = 60, seed = 42)

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

### Reading the Chart

- **Inner ring (Baseline)**: Initial distribution across grades
- **Second ring (Week 4)**: Each baseline group splits based on Week 4 grades
- **Outer ring (Week 12)**: Final subdivision showing complete trajectories
- **Segment width**: Proportional to number of patients in that trajectory
- **Colors**: Category at that specific timepoint

### For AE Data

```r

# Generate example trajectory data
ae_data <- create_example_ae_data(n_patients = 60, seed = 42)

sunburst_ae_trajectory(
  ae_data,
  subject = "USUBJID",
  time = "AVISIT",
  grade = "AETOXGR",
  title = "AE Grade Changes Over Time"
)
```

### For Response Data

```r

# Generate example trajectory data
resp_data <- create_example_response_trajectory(n_subjects = 60, seed = 42)

sunburst_trajectory(
  resp_data,
  subject = "subject",
  time = "timepoint",
  category = "response",
  category_order = c("CR", "PR", "SD", "PD"),
  palette = c("CR" = "#1b7837", "PR" = "#7fbf7b", 
              "SD" = "#d9d9d9", "PD" = "#d73027"),
  title = "Treatment Response Trajectories"
)
```

## Main Functions

| Function | Description |
|----------|-------------|
| `sunburst_trajectory()` | Create trajectory sunburst from any longitudinal data |
| `sunburst_ae_trajectory()` | Specialized for AE data with CTCAE colors |
| `prepare_trajectory_data()` | Convert longitudinal data to sunburst geometry |
| `scale_fill_ctcae()` | CTCAE grade color scale (0-5) |
| `add_center_label()` | Add text to chart center |

## Features

- **Hierarchical flow**: Groups subdivide outward without overlap
- **Proportional sizing**: Segment width = number of subjects
- **CTCAE colors**: Built-in severity color scale
- **Flexible categories**: Works with any categorical variable
- **ggplot2 integration**: Full customization with themes and labels

## Requirements

- R ≥ 4.0.0
- ggplot2 ≥ 3.4.0
- dplyr, tidyr, rlang, scales, cli

## License

MIT License - Occams Coöperatie U.A.
