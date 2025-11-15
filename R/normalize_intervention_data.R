normalize_intervention_data <- function(unified_matrix, 
                                        total_participants, 
                                        high_vote_score = 3) {
  max_possible_per_intervention <- total_participants * high_vote_score
  
  normalized_matrix <- unified_matrix %>%
    mutate(across(5:13, # the columns that are vote scores
                  ~ ifelse(type == "score", 
                           (.x / max_possible_per_intervention) * 10,  # Convert to 1-10 scale
                           .x)))  # Keep weight rows as-is
  
  return(normalized_matrix)
}

normalize_intervention_data_crincrin <- function(unified_matrix, 
                                        total_participants, 
                                        high_vote_score = 3) {
  max_possible_per_intervention <- total_participants * high_vote_score
  
  normalized_matrix <- unified_matrix %>%
    mutate(across(5:12, # the columns that are vote scores
                  ~ ifelse(type == "score", 
                           (.x / max_possible_per_intervention) * 10,  # Convert to 1-10 scale
                           .x)))  # Keep weight rows as-is
  
  return(normalized_matrix)
}