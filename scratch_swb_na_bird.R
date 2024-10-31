plot_Time_LSTots <- ggplot(swb_bird_na, aes(x = Time)) +
  # Add human utils line on primary axis
  geom_line(aes(y = LSTot, color = "Aggregate Human Life Satisfaction (1-10)"), linewidth = 1.5) +
  # Add bird population line on secondary axis
  geom_line(aes(y = LSTot_bird/(max(LSTot_bird)/max(LSTot)), 
                color = "Bird Population"), linewidth = 1.5) +
  # Add labels at the end of each line
  annotate("text", 
           x = final_year + 0.5,
           y = final_LSTot,
           label = format_billions(final_LSTot),
           color = "blue",
           hjust = 0,
           size = 8) +
  annotate("text", 
           x = final_year + 0.5,
           y = final_LSTot_bird/(max(LSTot_bird)/max(LSTot)),
           label = format_billions(final_LSTot_bird),
           color = "red",
           hjust = 0,
           size = 8) +
  # Custom colors
  scale_color_manual(
    name = "", 
    values = c("Aggregate Human Life Satisfaction (1-10)" = "blue", "Bird Population" = "red")
  ) +
  # Add secondary axis
  scale_y_continuous(
    name = "Human Utils",
    sec.axis = sec_axis(~.*(max(swb_bird_na$LSTot_bird)/max(swb_bird_na$LSTot)), 
                        name = "Bird Utils")
  ) +
  labs(
    x = "Year",
    title = "Aggreggate Human Life Satisfaction and Bird Population Over Time (US and Canada)"
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
    legend.title = element_text(size = 24),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red")
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