# Function to create plot with connected points and best fit line using US SWB  data
create_plot <- function(data, y_var, title, y_label) {
  ggplot(data, aes(x = Time, y = !!sym(y_var))) +
    geom_line() +  # Connect points with a line
    geom_point() +  # Add points
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add best fit line
    theme_minimal() +
    labs(title = title,
         x = "Year",
         y = y_label) +
    scale_x_continuous(breaks = unique(data$S020))
}