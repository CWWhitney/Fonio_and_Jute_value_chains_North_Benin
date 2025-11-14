create_full_heatmap_data <- function(unified_matrix){
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
  
  return(full_heatmap_data)
  
}