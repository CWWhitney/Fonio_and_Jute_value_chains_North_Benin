# Combined Bubble Plot ##########
# Add interpretaion 
# Plots below
create_gap_analysis_plot <- function(matrices_list) {
  # Process all datasets and combine with group labels
  all_results <- map2_dfr(matrices_list, names(matrices_list), function(matrix, group_name) {
    # Extract objective weights and scores
    objective_weights <- matrix %>%
      filter(type == "score") %>%
      select(objective, category, weight)
    
    score_matrix <- matrix %>%
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
      mutate(group = group_name,
             intervention_clean = str_replace_all(intervention, "_", " "))
    
    return(results)
  })
  
  # Calculate differences from overall average
  gap_analysis <- all_results %>%
    group_by(intervention_clean) %>%
    mutate(avg_impact = mean(impact),
           avg_feasibility = mean(feasibility),
           impact_gap = impact - avg_impact,
           feasibility_gap = feasibility - avg_feasibility) %>%
    ungroup()
  
  ggplot(gap_analysis, aes(x = feasibility_gap, y = impact_gap, color = group)) +
    geom_point(aes(size = abs(impact_gap + feasibility_gap)), alpha = 0.7) +
    geom_text_repel(aes(label = paste(intervention_clean)), 
                    size = 2, max.overlaps = 20, segment.size = 0.1) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_size_continuous(range = c(2, 6), name = "Total Deviation") +
    labs(title = "Performance Gaps from Overall Average",
         x = "Feasibility Gap", y = "Impact Gap") +
    theme_minimal()
}

## Fonio Boukoumbe ##########

matrices_list_fonio_boukoumbe <- list(
  # "All" = normalized_matrix_fonio_all,
  "Men_Boukoumbe" = normalized_matrix_fonio_men_boukoumbe,
  "Women_Boukoumbe" = normalized_matrix_fonio_women_boukoumbe
  # "Men_Natitingou" = normalized_matrix_fonio_men_natitingou,
  # "Women_Natitingou" = normalized_matrix_fonio_women_natitingou
)

combined_gap_analysis_fonio_boukoumbe <- create_gap_analysis_plot(matrices_list_fonio_boukoumbe)
ggsave(plot = combined_gap_analysis_fonio_boukoumbe, 
       filename = "figures/combined_gap_analysis_fonio_boukoumbe.png", 
       width = 25, height = 15, units = "cm")

## Fonio Natitingou ##########

matrices_list_fonio_natitingou <- list(
  # "All" = normalized_matrix_fonio_all,
  "Men_Natitingou" = normalized_matrix_fonio_men_natitingou,
  "Women_Natitingou" = normalized_matrix_fonio_women_natitingou
)

combined_gap_analysis_fonio_natitingou <- create_gap_analysis_plot(matrices_list_fonio_natitingou)
ggsave(plot = combined_gap_analysis_fonio_natitingou, 
       filename = "figures/combined_gap_analysis_fonio_natitingou.png", 
       width = 25, height = 15, units = "cm")

