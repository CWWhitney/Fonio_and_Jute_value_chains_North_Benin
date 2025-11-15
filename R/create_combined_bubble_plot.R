# Combined Bubble Plot ##########
# Plots below
create_combined_bubble_plot <- function(matrices_list) {
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
      pivot_longer(cols = -objective, names_to = "intervention", 
                   values_to = "score") %>%
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
    geom_point(alpha = 0.6) +
    geom_text_repel(aes(label = paste(intervention_clean)), 
                    size = 2, max.overlaps = 20, segment.size = 0.1) +
    scale_size_continuous(range = c(2, 8), name = "Total Score") +
    scale_color_brewer(palette = "Set1", name = "Group") +
    labs(title = "Strategic Investment Map: Cross-Group Comparison",
         subtitle = "Each point shows intervention performance by demographic group",
         x = "Feasibility", y = "Impact") +
    theme_minimal()
}

## Fonio Boukoumbe ##########

matrices_list_fonio_boukoumbe <- list(
  # "All" = normalized_matrix_fonio_all,
  "Boukoumbe Men" = normalized_matrix_fonio_men_boukoumbe,
  "Boukoumbe Women" = normalized_matrix_fonio_women_boukoumbe
  # "Men_Natitingou" = normalized_matrix_fonio_men_natitingou,
  # "Women_Natitingou" = normalized_matrix_fonio_women_natitingou
)

combined_bubble_plot_fonio_boukoumbe <- create_combined_bubble_plot(matrices_list_fonio_boukoumbe)
ggsave(plot = combined_bubble_plot_fonio_boukoumbe, 
       filename = "figures/combined_bubble_plot_fonio_boukoumbe.png", 
        width = 15, height = 10, units = "cm")

## Fonio Natitingou ##########

matrices_list_fonio_natitingou <- list(
  # "All" = normalized_matrix_fonio_all,
  "Natitingou Men" = normalized_matrix_fonio_men_natitingou,
  "Natitingou Women" = normalized_matrix_fonio_women_natitingou
)

combined_bubble_plot_fonio_natitingou <- create_combined_bubble_plot(matrices_list_fonio_natitingou)
ggsave(plot = combined_bubble_plot_fonio_natitingou, 
       filename = "figures/combined_bubble_plot_fonio_natitingou.png", 
       width = 15, height = 10, units = "cm")


## crincrin Boukoumbe ##########

matrices_list_crincrin_boukoumbe <- list(
  # "All" = normalized_matrix_crincrin_all,
  "Boukoumbe Men" = normalized_matrix_crincrin_men_boukoumbe,
  "Boukoumbe Women" = normalized_matrix_crincrin_women_boukoumbe
  # "Men_Natitingou" = normalized_matrix_crincrin_men_natitingou,
  # "Women_Natitingou" = normalized_matrix_crincrin_women_natitingou
)

combined_bubble_plot_crincrin_boukoumbe <- create_combined_bubble_plot(matrices_list_crincrin_boukoumbe)
ggsave(plot = combined_bubble_plot_crincrin_boukoumbe, 
       filename = "figures/combined_bubble_plot_crincrin_boukoumbe.png", 
       width = 15, height = 10, units = "cm")

## crincrin Natitingou ##########

matrices_list_crincrin_natitingou <- list(
  # "All" = normalized_matrix_crincrin_all,
  "Natitingou Men" = normalized_matrix_crincrin_men_natitingou,
  "Natitingou Women" = normalized_matrix_crincrin_women_natitingou
)

combined_bubble_crincrin_natitingou <- create_combined_bubble_plot(matrices_list_crincrin_natitingou)
ggsave(plot = combined_bubble_plot_crincrin_natitingou, 
       filename = "figures/combined_bubble_plot_crincrin_natitingou.png", 
       width = 15, height = 10, units = "cm")
