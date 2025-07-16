#' Create three-panel nc_score_range fourth-panel-style plots with concave, 
#' linear, and convex functional forms for nc-based welfare potential
#' 
#' @param data extended_integrated_calc_tseries
#' @param output_dir Directory for saving visualizations
#' @param linear_plot p4 from create_four_panel_nc_score_range_plots - becomes middle panel
#' @return NULL (saves plots to files)
create_three_panel_nc_func_form_check <- function(data, output_dir = "visualizations", linear_plot) {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating three-panel neuron count functional form plot...\n")
  
  # Ensure NC_utility column exists
  data <- ensure_nc_columns(data)
  
  max_f_welfare_score <- 100
  max_w_welfare_score <- 0.001
  
  # Prepare human NC_utility data
  human_nc_utility <- data %>%
    filter(Category == "Humans") %>%
    select(Year, NC_utility) %>%
    filter(!is.na(NC_utility))
  
  # Prepare data for Farmed animals (concave + convex) - FIXED: removed negative sign from calculation
  farmed_NC_urange <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    mutate(
      # Apply transformations at row level, then multiply by welfare score
      NC_urange_conc = max_f_welfare_score * aliveatanytime * NC_pot_conc,
      NC_urange_conv = max_f_welfare_score * aliveatanytime * NC_pot_conv,
      NC_urange_linear = max_f_welfare_score * NC_utility  # for reference
    ) %>%
    group_by(Year) %>%
    summarise(
      # Sum the transformed values, then make negative
      NC_urange_conc = -sum(NC_urange_conc, na.rm = TRUE),
      NC_urange_conv = -sum(NC_urange_conv, na.rm = TRUE),
      NC_urange_linear = -sum(NC_urange_linear, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(NC_urange_conc) & !is.na(NC_urange_conv))
  
  # Prepare data for Wild animals (concave + convex)
  wild_NC_urange_base <- data %>%
    filter(Group == "Wild Animals") %>%
    mutate(
      # Apply transformations at row level, then multiply by welfare score
      NC_urange_conc = max_w_welfare_score * aliveatanytime * NC_pot_conc,
      NC_urange_conv = max_w_welfare_score * aliveatanytime * NC_pot_conv,
      NC_urange_linear = max_w_welfare_score * NC_utility  # for reference
    ) %>%
    group_by(Year) %>%
    summarise(
      # Sum the transformed values
      NC_urange_conc = sum(NC_urange_conc, na.rm = TRUE),
      NC_urange_conv = sum(NC_urange_conv, na.rm = TRUE),
      NC_urange_linear = sum(NC_urange_linear, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(NC_urange_conc) & !is.na(NC_urange_conv))
  
  # Create both positive and negative versions for wild animals
  wild_NC_urange_full_conc <- bind_rows(
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conc) %>%
      mutate(value_type = "positive"),
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conc) %>%
      mutate(NC_urange_conc = -NC_urange_conc, value_type = "negative")
  )
  
  wild_NC_urange_full_conv <- bind_rows(
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conv) %>%
      mutate(value_type = "positive"),
    wild_NC_urange_base %>% 
      select(Year, NC_urange_conv) %>%
      mutate(NC_urange_conv = -NC_urange_conv, value_type = "negative")
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
  
  # Color palette - FIXED: Added "Wild Animals Neg" mapping
  main_colors <- c("Humans" = "#2E86AB", 
                   "Farmed Animals" = "#20B2AA", 
                   "Wild Animals" = "#8B4513",
                   "Wild Animals Neg" = "#8B4513")  # Same color for negative
  
  # Panel 1: Combined comparison - CONCAVE
  combined_data_conc <- bind_rows(
    human_nc_utility %>% 
      rename(value = NC_utility) %>%
      mutate(Group = "Humans"),
    farmed_NC_urange %>% 
      rename(value = NC_urange_conc) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conc) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conc) %>%
      select(Year, value) %>%
      mutate(value = -value, Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split_conc <- combined_data_conc %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p1 <- ggplot(combined_data_split_conc, aes(x = Year)) +
    # Area fills for positive values (wild animals)
    geom_area(data = combined_data_split_conc %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    # Area fills for negative values (wild animals negative and farmed)
    geom_area(data = combined_data_split_conc %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals Neg"]) +
    geom_area(data = combined_data_split_conc %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data_conc,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data_conc %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    scale_x_continuous(limits = c(min(combined_data_conc$Year), 
                                  max(combined_data_conc$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Concave: sqrt(neuron count ratio)",
         subtitle = "Diminishing returns to scale",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
  
  # Panel 2: Linear (from input)
  p2 <- linear_plot + 
    labs(title = "Linear: neuron count ratio",
         subtitle = "Proportional returns to scale")
  
  # Panel 3: Combined comparison - CONVEX
  combined_data_conv <- bind_rows(
    human_nc_utility %>% 
      rename(value = NC_utility) %>%
      mutate(Group = "Humans"),
    farmed_NC_urange %>% 
      rename(value = NC_urange_conv) %>%
      select(Year, value) %>%
      mutate(Group = "Farmed Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conv) %>%
      select(Year, value) %>%
      mutate(Group = "Wild Animals"),
    wild_NC_urange_base %>%
      rename(value = NC_urange_conv) %>%
      select(Year, value) %>%
      mutate(value = -value, Group = "Wild Animals Neg")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals", "Wild Animals Neg")))
  
  # Create separate positive and negative data for proper area fills
  combined_data_split_conv <- combined_data_conv %>%
    mutate(
      positive_value = ifelse(value > 0, value, 0),
      negative_value = ifelse(value < 0, value, 0)
    )
  
  p3 <- ggplot(combined_data_split_conv, aes(x = Year)) +
    # Area fills for positive values (wild animals)
    geom_area(data = combined_data_split_conv %>% filter(Group == "Wild Animals"),
              aes(y = positive_value), 
              alpha = 0.4, fill = main_colors["Wild Animals"]) +
    # Area fills for negative values (wild animals negative and farmed)
    geom_area(data = combined_data_split_conv %>% filter(Group == "Wild Animals Neg"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Wild Animals Neg"]) +
    geom_area(data = combined_data_split_conv %>% filter(Group == "Farmed Animals"),
              aes(y = negative_value), 
              alpha = 0.4, fill = main_colors["Farmed Animals"]) +
    # Add lines for all groups
    geom_line(data = combined_data_conv,
              aes(y = value, color = Group), 
              size = 1.2) +
    # Add labels at the end of each line
    geom_text(data = combined_data_conv %>% 
                group_by(Group) %>% 
                filter(Year == max(Year)) %>% 
                ungroup(),
              aes(y = value, label = Group, color = Group), 
              hjust = -0.1, 
              size = 4, 
              check_overlap = TRUE) +
    geom_hline(yintercept = 0, color = "grey70", linetype = "dashed") +
    scale_color_manual(values = main_colors) +
    scale_x_continuous(limits = c(min(combined_data_conv$Year), 
                                  max(combined_data_conv$Year) + 8)) +
    scale_y_continuous(labels = label_number()) +
    labs(title = "Convex: (neuron count ratio)²",
         subtitle = "Increasing returns to scale",
         y = "Welfare Score",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "none")
  
  # Combine three plots with patchwork
  final_plot <- (p1 | p2 | p3) +
    plot_annotation(
      title = "Welfare Score Analysis: Different Transformations of Neuron Count Ratios",
      subtitle = "Exploring how welfare potential might scale with relative neuron counts"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "none")
  
  # Save plots
  universal_ggsave(final_plot, "three_panel_nc_func_form_check", output_dir,
                   pdf_width = 20, pdf_height = 7)
  
  # Individual panels
  universal_ggsave(p1, "welfare_score_range_conc_4th_style", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "welfare_score_range_conv_4th_style", output_dir,
                   pdf_width = 8, pdf_height = 6)
  
  # Restore theme
  theme_set(original_theme)
  
  cat("Three-panel welfare score range plots about neuron transformations saved to:", output_dir, "\n")
}