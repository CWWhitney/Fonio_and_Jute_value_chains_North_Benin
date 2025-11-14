process_intervention_data <- function(unified_matrix) {
  # Extract objective weights and category definitions
  objective_weights <- unified_matrix %>%
    filter(type == "score") %>%
    select(objective, category, weight)
  
  # Extract intervention scores
  score_matrix <- unified_matrix %>%
    filter(type == "score") %>%
    select(-type, -category, -weight)
  
  # Calculate weighted scores 
  results <- score_matrix %>%
    pivot_longer(cols = -objective, names_to = "intervention", values_to = "score") %>%
    left_join(objective_weights, by = "objective") %>%
    mutate(weighted_score = score * weight/100) %>% 
    group_by(intervention, category) %>%
    summarise(
      category_score = sum(weighted_score),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = category, values_from = category_score) %>%
    mutate(total_score = impact + feasibility) %>%
    arrange(desc(total_score))
  
  # Prepare data for heatmap
  full_heatmap_data <- unified_matrix %>%
    mutate(label = case_when(
      type == "weight" ~ "PRIORITY WEIGHTS",
      type == "score" ~ objective
    ))
  
  intervention_cols <- setdiff(names(full_heatmap_data), 
                               c("type", "objective", "category", "label"))
  
  full_heatmap_data <- full_heatmap_data %>%
    select(label, all_of(intervention_cols)) %>%
    pivot_longer(cols = -label, names_to = "intervention", values_to = "score") %>%
    mutate(score_display = ifelse(label == "PRIORITY WEIGHTS", score, score))
  
  # Return both results and heatmap data
  return(list(
    results = results,
    heatmap_data = full_heatmap_data
  ))
}