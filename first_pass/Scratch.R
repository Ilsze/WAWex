if("Wild Animals" %in% integrated_hf_data$Group) {
  # If humans already in the dataset, replace with new welfare data
  integrated_data <- integrated_hf_data %>%
    filter(Category != "Wild Animals") %>%
    bind_rows(wild_data_formatted)
} else {
  # If no humans in the dataset, simply append
  integrated_data <- bind_rows(integrated_hf_data, wild_data_formatted)
}