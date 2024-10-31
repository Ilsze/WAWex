#this file takes the processed data from swb_na_bird.R and plots with it
source("helper_swb_na_bird.R")
#dataset to be plotted
swb_bird_na <- readRDS("~/Documents/GitHub/WAWex/dat/swb_bird_na.rds")

##############################################################################
#####################   PLOTTING     ##########################################
###############################################################################
# Create plots of US mean life satisfaction beside US mean happiness, with best fit lines
plot_satisfaction <- create_plot(swb_bird_na, "mean_life_satisfaction", 
                                 "Mean Life Satisfaction in the US Over Time", 
                                 "Mean Life Satisfaction")
plot_happiness <- create_plot(swb_bird_na, "mean_happiness", 
                              "Mean Happiness in the US Over Time", 
                              "Mean Happiness")
# Display the plots
print(plot_satisfaction)
print(plot_happiness)
#put them side by side
us_LS_hap <- arrangeGrob(plot_satisfaction, plot_happiness, ncol = 2)
##save in output
ggsave(
  filename = "output/swb_bird_na/us_LS_hap.png",
  plot = us_LS_hap,
  width = 12,  # Width in inches
  height = 6,  # Height in inches
  dpi = 300    # Resolution
)

##################Plot human utils on the x axis and bird numbers on the y axis
# Get the final values for each line
final_year <- max(swb_bird_na$Time, na.rm = TRUE)
final_LSTot <- swb_bird_na$LSTot[swb_bird_na$Time == final_year]
final_birds <- swb_bird_na$N_med[swb_bird_na$Time == final_year]
final_LSTot_bird <- swb_bird_na$LSTot_bird[swb_bird_na$Time == final_year]

# Function to format numbers in billions with 1 significant figure
format_billions <- function(x) {
  billions <- x / 1e9  # Convert to billions
  rounded <- signif(billions, 2)  # Round to 1 significant figure
  paste0(rounded, " billion")  # Add "billion" text
}

# Create the plot with raw values and formatted end labels
# Create the plot with much larger text
plot_Time_LSTot_Nmed <- ggplot(swb_bird_na, aes(x = Time)) +
  # Add raw human utils line
  geom_line(aes(y = LSTot, color = "Aggregate Human Life Satisfaction (1-10)"), linewidth = 1.5) +  # Made lines thicker
  # Add raw bird population line
  geom_line(aes(y = N_med, color = "Bird Population"), linewidth = 1.5) +  # Made lines thicker
  # Add labels at the end of each line with larger text
  annotate("text", 
           x = final_year + 0.5,
           y = final_LSTot,
           label = format_billions(final_LSTot),
           color = "blue",
           hjust = 0,
           size = 8) +  # Much larger
  annotate("text", 
           x = final_year + 0.5,
           y = final_birds,
           label = format_billions(final_birds),
           color = "red",
           hjust = 0,
           size = 8) +  # Much larger
  # Custom colors
  scale_color_manual(
    name = "", 
    values = c("Aggregate Human Life Satisfaction (1-10)" = "blue", "Bird Population" = "red")
  ) +
  # Add log scale for y-axis
  scale_y_log10() +
  # Labels and theme
  labs(
    x = "Year",
    y = "Value",
    title = "Human Utils and Bird Population that Year Over Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # Much larger text sizes
    axis.title = element_text(size = 24),          # Axis titles
    axis.text = element_text(size = 20),           # Axis text
    plot.title = element_text(size = 28),          # Plot title
    legend.text = element_text(size = 20),         # Legend text
    legend.title = element_text(size = 24)         # Legend title
  ) +
  # Expand x-axis slightly to make room for labels
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))
# Save the plot with larger dimensions to accommodate bigger text
ggsave(
  filename = "output/swb_bird_na/time_LSTot_Nmed.png",
  plot = plot_Time_LSTot_Nmed,
  width = 15,    # Increased width
  height = 10,   # Increased height
  dpi = 300,
  bg = "white"
)

############## PLOT LSTOTs
plot_Time_LSTots <- ggplot(swb_bird_na, aes(x = Time)) +
  # Add raw human utils line
  geom_line(aes(y = LSTot, color = "Aggregate Human Life Satisfaction (1-10)"), linewidth = 1.5) +
  # Add raw bird population line
  geom_line(aes(y = LSTot_bird, color = "Bird Population"), linewidth = 1.5) +
  # Add labels at the end of each line with larger text
  annotate("text", 
           x = final_year + 0.5,
           y = final_LSTot,
           label = format_billions(final_LSTot),
           color = "blue",
           hjust = 0,
           size = 8) +
  annotate("text", 
           x = final_year + 0.5,
           y = final_LSTot_bird,
           label = format_billions(final_LSTot_bird),
           color = "red",
           hjust = 0,
           size = 8) +
  # Custom colors
  scale_color_manual(
    name = "", 
    values = c("Aggregate Human Life Satisfaction (1-10)" = "blue", "Bird Population" = "red")
  ) +
  # Add log scale for y-axis
  scale_y_log10() +
  # Labels and theme
  labs(
    x = "Year",
    y = "Value (log scale)",
    title = "Human Utils and Bird Population that Year Over Time"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 28),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 24)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15)))

ggsave(
  filename = "output/swb_bird_na/time_LSTots.png",
  plot = plot_Time_LSTots,
  width = 15,
  height = 10,
  dpi = 300,
  bg = "white"
)
