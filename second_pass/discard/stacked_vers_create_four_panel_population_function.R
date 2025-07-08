stacked version
create_four_panel_population_plots <- function(data, output_dir = "visualizations") {

  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("Creating four-panel population comparison plots...\n")

  # Prepare human population data (single category)
  human_pop <- data %>%
    filter(Category == "Humans") %>%
    select(Year, aliveatanytime) %>%
    filter(!is.na(aliveatanytime), aliveatanytime > 0)

  # Prepare farmed animal data by category for stacking
  farmed_pop <- data %>%
    filter(Group %in% c("Farmed Terrestrial Animals", "Farmed Aquatic Animals (Slaughtered)")) %>%
    select(Year, Category, aliveatanytime) %>%
    filter(!is.na(aliveatanytime), !is.na(Category)) %>%
    # Make sure Year is numeric
    mutate(Year = as.numeric(Year)) %>%
    # Complete all year-category combinations (fill missing with 0)
    complete(Year, Category, fill = list(aliveatanytime = 0)) %>%
    # Remove years where ALL categories are 0
    group_by(Year) %>%
    filter(sum(aliveatanytime, na.rm = TRUE) > 0) %>%
    ungroup()

  # Prepare wild animal data by category for stacking
  wild_pop <- data %>%
    filter(Group == "Wild Animals") %>%
    select(Year, Category, aliveatanytime) %>%
    filter(!is.na(aliveatanytime), !is.na(Category)) %>%
    mutate(Year = as.numeric(Year)) %>%
    complete(Year, Category, fill = list(aliveatanytime = 0)) %>%
    group_by(Year) %>%
    filter(sum(aliveatanytime, na.rm = TRUE) > 0) %>%
    ungroup()

  # Debug output
  cat("=== DATA SUMMARY ===\n")
  cat("Human data: ", nrow(human_pop), " rows\n")
  cat("Farmed data: ", nrow(farmed_pop), " rows\n")
  cat("Wild data: ", nrow(wild_pop), " rows\n")
  cat("Farmed categories: ", paste(unique(farmed_pop$Category), collapse = ", "), "\n")
  cat("Wild categories: ", paste(unique(wild_pop$Category), collapse = ", "), "\n")

  # Aggregated totals for the comparison panel
  farmed_total <- farmed_pop %>%
    group_by(Year) %>%
    summarise(aliveatanytime = sum(aliveatanytime, na.rm = TRUE), .groups = "drop") %>%
    filter(aliveatanytime > 0)

  wild_total <- wild_pop %>%
    group_by(Year) %>%
    summarise(aliveatanytime = sum(aliveatanytime, na.rm = TRUE), .groups = "drop") %>%
    filter(aliveatanytime > 0)

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

  # Color palettes
  main_colors <- c(Humans = "#2E86AB", Farmed = "#F24236", Wild = "#27AE60")

  # Panel 1: Human Population (simple area)
  p1 <- ggplot(human_pop, aes(x = Year, y = aliveatanytime)) +
    geom_area(alpha = 0.7, fill = main_colors["Humans"]) +
    geom_line(color = main_colors["Humans"], size = 1.2) +
    scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
    labs(title = "Human Population",
         y = "Population (Billions)") +
    theme(plot.title = element_text(size = 14, face = "bold"))

  # Panel 2: Farmed Animals (stacked area chart)
  p2 <- ggplot(farmed_pop, aes(x = Year, y = aliveatanytime, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B")) +
    labs(title = "Farmed Animal Population",
         y = "Population (Billions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 8)) +
    guides(fill = guide_legend(nrow = 3))

  # Panel 3: Wild Animals (stacked area chart)
  p3 <- ggplot(wild_pop, aes(x = Year, y = aliveatanytime, fill = Category)) +
    geom_area(position = "stack", alpha = 0.8) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = label_number(scale = 1e-12, suffix = "T")) +
    labs(title = "Wild Animal Population",
         y = "Population (Trillions)",
         fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 8)) +
    guides(fill = guide_legend(nrow = 2))

  # Panel 4: Combined comparison (log scale)
  combined_data <- bind_rows(
    human_pop %>% mutate(Group = "Humans"),
    farmed_total %>% mutate(Group = "Farmed Animals"),
    wild_total %>% mutate(Group = "Wild Animals")
  ) %>%
    mutate(Group = factor(Group, levels = c("Humans", "Farmed Animals", "Wild Animals")))

  p4 <- ggplot(combined_data, aes(x = Year, y = aliveatanytime,
                                  color = Group, fill = Group)) +
    geom_area(alpha = 0.4, position = "identity") +
    geom_line(size = 1.2) +
    scale_y_log10(labels = label_number()) +
    scale_color_manual(values = main_colors) +
    scale_fill_manual(values = main_colors) +
    labs(title = "Comparative Population Trends",
         subtitle = "Log scale reveals different growth patterns",
         y = "Population (Log Scale)",
         color = "", fill = "") +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "bottom")

  # Combine with patchwork
  final_plot <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "Global Population Dynamics: Humans, Farmed Animals, and Wildlife",
      subtitle = "A comparative analysis of population trends across sentient beings (1950-2025)",
      caption = "Data: Welfare Analysis Framework | Method: 32-82 Classification"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  # Save plots
  universal_ggsave(final_plot, "four_panel_population_comparison", output_dir,
                   pdf_width = 16, pdf_height = 10)

  # Individual panels
  universal_ggsave(p1, "population_humans_only", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p2, "population_farmed_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p3, "population_wild_stacked", output_dir,
                   pdf_width = 8, pdf_height = 6)
  universal_ggsave(p4, "population_comparison_log", output_dir,
                   pdf_width = 10, pdf_height = 6)

  # Restore theme
  theme_set(original_theme)

  cat("Four-panel population comparison plots saved to:", output_dir, "\n")
}