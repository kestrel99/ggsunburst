# Generate example datasets for ggsunburst package
# Run this script to create the data/ directory contents

# Create data directory if needed
if (!dir.exists("data")) dir.create("data")

# Generate AE example data
set.seed(42)
n_patients <- 30
visit_names <- c("Screening", "Baseline", "Week 2", "Week 4", "Week 8", "Week 12")

ae_example <- expand.grid(
  USUBJID = paste0("SUBJ-", sprintf("%03d", 1:n_patients)),
  AVISIT = factor(visit_names, levels = visit_names),
  stringsAsFactors = FALSE
)

ae_example$AVISITN <- as.numeric(ae_example$AVISIT) - 1

# Base grade probabilities
grade_probs <- c(0.35, 0.30, 0.20, 0.10, 0.05)
ae_example$AETOXGR <- sample(0:4, nrow(ae_example), replace = TRUE, prob = grade_probs)

# Add some patient-level correlation
for (subj in unique(ae_example$USUBJID)) {
  idx <- ae_example$USUBJID == subj
  baseline_grade <- ae_example$AETOXGR[idx][2]  # Use baseline visit
  for (i in which(idx)[3:6]) {
    if (runif(1) < 0.6) {  # 60% chance of correlated grade
      ae_example$AETOXGR[i] <- sample(
        0:4, 1,
        prob = dnorm(0:4, mean = baseline_grade, sd = 1)
      )
    }
  }
}

ae_example$AETOXGR <- pmin(pmax(ae_example$AETOXGR, 0), 4)
ae_example <- ae_example[order(ae_example$USUBJID, ae_example$AVISITN), ]
rownames(ae_example) <- NULL

# Save
save(ae_example, file = "data/ae_example.rda", compress = "xz")

# Generate hierarchical example
set.seed(123)
regions <- c("North", "South", "East", "West")
categories <- c("Electronics", "Clothing", "Food")
subcategories <- list(
  Electronics = c("Phones", "Computers", "Tablets"),
  Clothing = c("Shirts", "Pants", "Shoes"),
  Food = c("Produce", "Dairy", "Bakery")
)

hierarchy_example <- do.call(rbind, lapply(regions, function(reg) {
  do.call(rbind, lapply(categories, function(cat) {
    n_sub <- sample(1:3, 1)
    subs <- sample(subcategories[[cat]], n_sub)
    data.frame(
      region = reg,
      category = cat,
      subcategory = subs,
      value = round(runif(n_sub, 100, 1000)),
      stringsAsFactors = FALSE
    )
  }))
}))
rownames(hierarchy_example) <- NULL

save(hierarchy_example, file = "data/hierarchy_example.rda", compress = "xz")

cat("Example datasets created in data/\n")
