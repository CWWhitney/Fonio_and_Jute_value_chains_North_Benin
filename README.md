# Decision Support Tool

A simple, interactive tool for prioritizing value chain interventions using Multi-Criteria Decision Analysis (MCDA).

## How It Works

1. **Edit** the `data/interventions_matrix.csv` spreadsheet during your workshop
2. **Run** the analysis by opening `Index.Rmd` and clicking "Knit"
3. **View** the results with ranked interventions and visual heatmaps

## Editing the Data

### Setting Priorities (Row 2)
- Keep `type = "weight"`
- Set importance weights (0-100) for each objective
- Total should ideally equal 100

### Adding Interventions (Subsequent rows)
- Use `type = "intervention"`
- Add new interventions as needed
- Score expected impact (0-100) for each objective

### Adding/Removing Objectives
- Add new columns for additional objectives
- Remove columns for objectives you don't need
- The tool automatically adapts

## Output

The report provides:
- **Ranked list** of interventions by total score
- **Heatmap** showing complete decision matrix
- **Bubble chart** visualizing feasibility and impact (weighted scores of variables in each category)
- **Key insights** and recommendations

## Requirements

- R and RStudio
- tidyverse package: `install.packages("tidyverse")`

## Tips

- Use 0-100 scale for consistent scoring (requires a discussion about what '0' and what '100' mean for each variable)
- Involve stakeholders in both weighting and scoring
- Re-knit frequently as data changes during workshops
