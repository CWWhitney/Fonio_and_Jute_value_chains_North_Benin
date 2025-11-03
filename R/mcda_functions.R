calculate_mcda_scores <- function(interventions_df, weights_df) {
  interventions_df %>%
    pivot_longer(cols = -intervention, names_to = "objective", values_to = "score") %>%
    left_join(weights_df, by = "objective") %>%
    mutate(weighted_score = score * weight) %>%
    group_by(intervention) %>%
    summarise(total_score = sum(weighted_score)) %>%
    arrange(desc(total_score))
}

create_weights_interactive <- function(objectives_df) {
  cat("=== STAKEHOLDER WEIGHTING SESSION ===\n")
  cat("Distribute 100 points across objectives:\n\n")
  
  weights <- list()
  for(i in 1:nrow(objectives_df)) {
    obj <- objectives_df$objective[i]
    desc <- objectives_df$description[i]
    cat(sprintf("%s (%s): ", obj, desc))
    weight <- as.numeric(readline())
    weights[[obj]] <- weight
  }
  
  # Normalize to 100
  weight_df <- data.frame(objective = names(weights), weight = unlist(weights))
  weight_df$weight <- (weight_df$weight / sum(weight_df$weight)) * 100
  
  return(weight_df)
}