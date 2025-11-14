create_comparative_bubble_plot <- function(matrices_list) {
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
  
  # Create comparative plot
  ggplot(all_results, aes(x = feasibility, y = impact, size = total_score, color = group)) +
    geom_point(alpha = 0.7) +
    geom_text_repel(aes(label = intervention_clean), 
                    size = 2.5, max.overlaps = 15, 
                    segment.size = 0.2, box.padding = 0.3) +
    scale_size_continuous(range = c(2, 8), name = "Total Score") +
    scale_color_brewer(palette = "Set1", name = "Group") +
    labs(title = "Comparative Strategic Investment Map",
         subtitle = "Impact vs Feasibility by Gender and Region",
         x = "Feasibility (Weighted Composite)", y = "Impact (Weighted Composite)") +
    theme_minimal() +
    facet_wrap(~ group, ncol = 2)  # Small multiples for each group
}

# Usage with your datasets
matrices_list_fonio <- list(
  "All" = normalized_matrix_fonio_all,
  "Men_Boukoumbe" = normalized_matrix_fonio_men_boukoumbe,
  "Men_Natitingou" = normalized_matrix_fonio_men_natitingou,
  "Women_Boukoumbe" = normalized_matrix_fonio_women_boukoumbe,
  "Women_Natitingou" = normalized_matrix_fonio_women_natitingou
)

comparative_plot_fonio <- create_comparative_bubble_plot(matrices_list_fonio)
ggsave(plot = comparative_plot_fonio, 
       filename = "figures/comparative_plot_fonio.png", 
       width = 17, height = 22, units = "cm")

