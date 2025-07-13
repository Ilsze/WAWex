create_four_panel_wr_score_range_plots

create_three_panel_nc_func_form_check(extended_data_for_net, output_dir)


create_four_panel_nc_score_range_plots <- function(data, 
                                                   output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating four-panel welfare score range plots...\n")
  
  # Ensure NC_utility column exists
  data <- ensure_nc_columns(data)
  
  # Prepare human NC_utility data
  human_nc_utility <- data %>%
    filter(Category == "Humans") %>%
    select(Year, NC_utility) %>%
    filter(!is.na(NC_utility))
  
  # Prepare farmed animal NC_apot data (aggregated and inverted)
  #Due to CE; change this later when not doing CE
  max_f_welfare_score <- 100
  farmed_NC_urange <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    group_by(Year) %>%
    summarise(NC_apot_total = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    mutate(NC_urange = max_f_welfare_score*NC_apot_total) %>%  #scale up due to range of welfare score
    mutate(NC_urange = -NC_urange) %>%  # Reflect across x-axis
    filter(!is.na(NC_urange))
  
  # Prepare wild animal NC_apot data (aggregated, scaled down, with BOTH positive and negative)
  max_w_welfare_score <- 0.001
  wild_NC_urange_base <- data %>%
    filter(Group == "Wild Animals") %>%
    group_by(Year) %>%
    summarise(NC_apot_total = sum(NC_apot, na.rm = TRUE), .groups = "drop") %>%
    mutate(NC_urange = max_w_welfare_score*NC_apot_total) %>%  #scale up due to range of welfare score
    filter(!is.na(NC_urange))
  
  # Create both positive and negative versions for wild animals
  wild_NC_urange_full <- bind_rows(
    wild_NC_urange_base %>% mutate(value_type = "positive"),
    wild_NC_urange_base %>% mutate(NC_urange = -NC_urange, value_type = "negative")
  )
  
  # Set theme
  original_theme <- theme_get()
  theme_set(theme_minimal(base_size = 12) +
              theme(
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 11, color = "grey30"),
                strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.ticks = element_line(color = "grey70"),
                axis.line = element_line(color = "grey70")
              ))
  
  # Color palette with updated farmed animal color
  main_colors <- c("Humans" = "#2E86AB", "Farmed Animals" = "#20B2AA", "Wild Animals" = "#8B4513")
  
  # Panel 1: Human Welfare Score (line only, no fill)
  p1 <- ggplot(human_nc_utility, aes(x = Year, y = NC_utility)) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Human Welfare Score",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 2: Farmed Animal Welfare Score (inverted with area)
  p2 <- ggplot(farmed_NC_urange, aes(x = Year, y = NC_urange)) +
    geom_area(alpha = 0.7, fill = main_colors["Farmed Animals"]) +
    geom_line(color = main_colors["Farmed Animals"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Farmed Animal Welfare - Possible Range",
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # Panel 3: Wild Animal Welfare Score (both positive and negative with areas)
  p3 <- ggplot(wild_NC_urange_full, aes(x = Year, y = NC_urange)) +
    geom_area(data = wild_NC_urange_full %>% filter(value_type == "positive"),
              alpha = 0.7, fill = main_colors["Wild Animals"]) +
    geom_area(data = wild_NC_urange_full %>% filter(value_type == "negative"),
              alpha = 0.7, fill = main_colors["Wild Animals"]) +
    geom_line(data = wild_NC_urange_full %>% filter(value_type == "positive"),
              color = main_colors["Wild Animals"], size = 1.2) +
    geom_line(data = wild_NC_urange_full %>% filter(value_type == "negative"),
              color = main_colors["Wild Animals"], size = 1.2) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Wild Animal Welfare - Possible Range",
         subtitle = paste0("Assuming average welfare score is 1/", scaled_by, " as intense as humans'"),
         y = "Welfare Score") +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  wild_NC_urange_base_neg <- wild_NC_urange_base %>%
    mutate(NC_urange = -NC_urange)
  
  # Panel 4: Combined comparison - need to combine all three with consistent naming
  combined_data <- bind_rows(
    human_nc_utility %>% 
      rename(value = NC_utility) %>%
      mutate(Group = "Humans"),
    farmed_NC_urange %>% 
      rename(value = NC_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_NC_urange_base %>%  # Use base (positive) for the combined view
      rename(value = NC_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_NC_urange_base_neg %>%  # Use base (negative) for the combined view
      rename(value = NC_urange) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split <- combined_data %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p4 <- ggplot(combined_data_split, aes(x = Year)) +
    # no fill for human values
    # negative line and fills for wild animals
    geom_area(data = combined_data_split %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    geom_area(data = combined_data_split %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals Neg"]) +
    # Add area fills for negative values (farmed)
    geom_area(data = combined_data_split %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    # Extend x-axis to make room for labels
    scale_x_continuous(limits = c(min(combined_data$Year), 
                                  max(combined_data$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Introducing Welfare Score",
         subtitle = "Comparative welfare scores across sentient beings",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")  # Remove legend since we have direct labels
  
  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Welfare Score Analysis: Introducing Utility Considerations",
      subtitle = "Exploring welfare scores across humans, farmed animals, and wildlife"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Save plots
  universal_ggsave(final_plot, "four_panel_welfare_score_range", output_dir,
                   pdf_width = 16, pdf_height = 10)
  
  # Individual panels
  universal_ggsave(p1, "welfare_score_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "welfare_score_farmed_inverted", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "welfare_score_wild_scaled_inverted", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "welfare_score_introducing", output_dir,
                   pdf_width = 10, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Four-panel welfare score range plots saved to:", output_dir, "\n")
}