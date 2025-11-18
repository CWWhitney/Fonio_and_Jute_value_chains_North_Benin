# Function to plot the data in a matrix


plot_heatmap <- function(heatmap_display_data, high_score_color = "green"){
  ggplot(heatmap_display_data, aes(x = intervention, y = label, fill = score_display)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = round(score_display, 1)), color = "black", size = 4) +
    scale_fill_gradient2(low = "white", high = high_score_color, 
                         midpoint = 5, name = "Score") +
    labs(title = "Intervention Impact Matrix",
         subtitle = "Scores show expected impact on each objective",
         x = "Interventions", y = "Objectives") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"),
          panel.grid = element_blank())
}