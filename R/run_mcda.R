source("R/mcda_functions.R")
library(tidyverse)

# Load predefined objectives
objectives <- read_csv("data/objectives.csv")

# 1. FACILITATE WEIGHTING SESSION
cat("Let's determine what matters most...\n")
weights <- create_weights_interactive(objectives)
write_csv(weights, "config/workshop_weights.csv")

# 2. QUICKLY SCORE INTERVENTIONS (manual entry or CSV)
# Option A: Manual entry during workshop
interventions <- data.frame(
  intervention = c("Seed kits for women co-ops", "Central processing plant"),
  nutrition = c(7, 5),
  livelihoods = c(8, 9),
  environment = c(6, 3),
  equity = c(10, 2),
  feasibility = c(9, 2)
)

# Option B: Load from CSV if you pre-populate during breaks
# interventions <- read_csv("data/02_interventions.csv")

# 3. INSTANT RESULTS
results <- calculate_mcda_scores(interventions, weights)
print(results)

# 4. GENERATE REPORT
rmarkdown::render("mcda_report.Rmd", 
                  output_file = "output/mcda_report.html",
                  params = list(results = results, weights = weights))