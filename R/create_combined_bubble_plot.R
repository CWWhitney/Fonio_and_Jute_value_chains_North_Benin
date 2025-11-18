# Combined Bubble Plot with Quadrants and Gender Colors #######
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
  
  # Create gender column for coloring
  all_results <- all_results %>%
    mutate(gender = if_else(str_detect(group, "Women"), "Women", "Men"))
  
  # Calculate plot boundaries with padding
  xmin <- min(all_results$feasibility)
  xmax <- max(all_results$feasibility)
  ymin <- min(all_results$impact)
  ymax <- max(all_results$impact)
  x_range <- xmax - xmin
  y_range <- ymax - ymin
  
  # Add padding to boundaries
  xmin_plot <- xmin - 0.05 * x_range
  xmax_plot <- xmax + 0.05 * x_range
  ymin_plot <- ymin - 0.05 * y_range
  ymax_plot <- ymax + 0.05 * y_range
  
  # Calculate quadrant boundaries (using medians)
  x_median <- median(all_results$feasibility)
  y_median <- median(all_results$impact)
  
  # Create quadrant data for background rectangles with descriptive labels
  quadrants <- data.frame(
    xmin = c(xmin_plot, x_median, xmin_plot, x_median),
    xmax = c(x_median, xmax_plot, x_median, xmax_plot),
    ymin = c(y_median, y_median, ymin_plot, ymin_plot),
    ymax = c(ymax_plot, ymax_plot, y_median, y_median),
    quadrant_label = c("Hard but Impactful", "Easy & Impactful", "Hard & Limited", "Easy but Limited"),
    fill_color = c("#FFF2CC", "#E1F5FE", "#FCE4EC", "#E8F5E8")  # Light yellow, blue, pink, green
  )
  
  # Create the plot with quadrants and gender colors
  ggplot() +
    # Add quadrant backgrounds first
    geom_rect(data = quadrants, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = quadrant_label),
              alpha = 0.3) +
    # Add quadrant divider lines
    geom_vline(xintercept = x_median, linetype = "dashed", alpha = 0.5, color = "gray50") +
    geom_hline(yintercept = y_median, linetype = "dashed", alpha = 0.5, color = "gray50") +
    # Add points with gender colors
    geom_point(data = all_results, 
               aes(x = feasibility, y = impact, size = total_score, color = gender),
               alpha = 0.7) +
    # Add text labels
    geom_text_repel(data = all_results,
                    aes(x = feasibility, y = impact, label = intervention_clean), 
                    size = 2, max.overlaps = 20, segment.size = 0.1) +
    # Custom fill scale for quadrants with explicit labels
    scale_fill_manual(
      name = "Strategic Quadrants:",
      values = c(
        "Hard but Impactful" = "#FFF2CC",
        "Easy & Impactful" = "#E1F5FE", 
        "Hard & Limited" = "#FCE4EC",
        "Easy but Limited" = "#E8F5E8"
      ),
      guide = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(alpha = 0.5, size = 1)
      )
    ) +
    # Size scale for bubbles
    scale_size_continuous(range = c(2, 8), name = "Total Score") +
    # Gender colors - blue for men, specific pink for women
    scale_color_manual(values = c("Men" = "blue", "Women" = "#e377c2"), name = "Gender") +
    labs(title = "Strategic Investment Map: Cross-Group Comparison",
         subtitle = "Each point shows intervention performance by demographic group",
         x = "Feasibility", y = "Impact") +
    theme_minimal() +
    xlim(xmin_plot, xmax_plot) + 
    ylim(ymin_plot, ymax_plot) +
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.spacing.y = unit(0.2, "cm")
    )
}

## Fonio Boukoumbe ##########
matrices_list_fonio_boukoumbe <- list(
  "Boukoumbe Men" = normalized_matrix_fonio_men_boukoumbe,
  "Boukoumbe Women" = normalized_matrix_fonio_women_boukoumbe
)

combined_bubble_plot_fonio_boukoumbe <- create_combined_bubble_plot(matrices_list_fonio_boukoumbe)
ggsave(plot = combined_bubble_plot_fonio_boukoumbe, 
       filename = "figures/combined_bubble_plot_fonio_boukoumbe.png", 
       width = 15, height = 15, units = "cm")

## Fonio Natitingou ##########
matrices_list_fonio_natitingou <- list(
  "Natitingou Men" = normalized_matrix_fonio_men_natitingou,
  "Natitingou Women" = normalized_matrix_fonio_women_natitingou
)

combined_bubble_plot_fonio_natitingou <- create_combined_bubble_plot(matrices_list_fonio_natitingou)
ggsave(plot = combined_bubble_plot_fonio_natitingou, 
       filename = "figures/combined_bubble_plot_fonio_natitingou.png", 
       width = 15, height = 15, units = "cm")

## Crincrin Boukoumbe ##########
matrices_list_crincrin_boukoumbe <- list(
  "Boukoumbe Men" = normalized_matrix_crincrin_men_boukoumbe,
  "Boukoumbe Women" = normalized_matrix_crincrin_women_boukoumbe
)

combined_bubble_plot_crincrin_boukoumbe <- create_combined_bubble_plot(matrices_list_crincrin_boukoumbe)
ggsave(plot = combined_bubble_plot_crincrin_boukoumbe, 
       filename = "figures/combined_bubble_plot_crincrin_boukoumbe.png", 
       width = 15, height = 15, units = "cm")

## Crincrin Natitingou ##########
matrices_list_crincrin_natitingou <- list(
  "Natitingou Men" = normalized_matrix_crincrin_men_natitingou,
  "Natitingou Women" = normalized_matrix_crincrin_women_natitingou
)

combined_bubble_plot_crincrin_natitingou <- create_combined_bubble_plot(matrices_list_crincrin_natitingou)
ggsave(plot = combined_bubble_plot_crincrin_natitingou, 
       filename = "figures/combined_bubble_plot_crincrin_natitingou.png", 
       width = 15, height = 15, units = "cm")