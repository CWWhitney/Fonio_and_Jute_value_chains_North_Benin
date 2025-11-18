create_comparative_bubble_plot <- function(matrices_list) {
  # Process all datasets and combine with group labels
  all_results <- map2_dfr(matrices_list, names(matrices_list), 
                          function(matrix, group_name) {
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
  
  # Create gender column for coloring
  all_results <- all_results %>%
    mutate(gender = if_else(str_detect(group, "Women"), "Women", "Men"))
  
  # Create comparative plot with custom colors
  ggplot(all_results, aes(x = feasibility, y = impact, size = total_score, color = gender)) +
    geom_point(alpha = 0.7) +
    geom_text_repel(aes(label = intervention_clean), 
                    size = 2.5, max.overlaps = 15, 
                    segment.size = 0.2, box.padding = 0.3) +
    scale_size_continuous(range = c(2, 8), name = "Total Score") +
    scale_color_manual(values = c("Men" = "blue", "Women" = "#e377c2"), name = "Gender") +
    labs(title = "Comparative Strategic Investment Map",
         subtitle = "Impact vs Feasibility by Gender and Region",
         x = "Feasibility (Weighted Composite)", y = "Impact (Weighted Composite)") +
    theme_minimal() +
    facet_wrap(~ group, ncol = 2)  # Small multiples for each group
}

##### Fonio #######
matrices_list_fonio <- list(
  "Boukoumbe Women" = normalized_matrix_fonio_women_boukoumbe,
  "Boukoumbe Men" = normalized_matrix_fonio_men_boukoumbe,
  "Natitingou Women" = normalized_matrix_fonio_women_natitingou,
  "Natitingou Men" = normalized_matrix_fonio_men_natitingou
)

comparative_plot_fonio <- create_comparative_bubble_plot(matrices_list_fonio)
ggsave(plot = comparative_plot_fonio, 
       filename = "figures/comparative_plot_fonio.png", 
       width = 20, height = 15, units = "cm")

##### crincrin #######
matrices_list_crincrin <- list(
  "Boukoumbe Women" = normalized_matrix_crincrin_women_boukoumbe,
  "Boukoumbe Men" = normalized_matrix_crincrin_men_boukoumbe,
  "Natitingou Women" = normalized_matrix_crincrin_women_natitingou,
  "Natitingou Men" = normalized_matrix_crincrin_men_natitingou
)

comparative_plot_crincrin <- create_comparative_bubble_plot(matrices_list_crincrin)
ggsave(plot = comparative_plot_crincrin, 
       filename = "figures/comparative_bubble_plot_crincrin.png", 
       width = 20, height = 15, units = "cm")