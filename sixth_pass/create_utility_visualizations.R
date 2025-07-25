

#' Create complete set of utility visualizations using factored functions
#' 
#' @param data The processed dataset
#' @param net_series The net series data (not used in current implementation)
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
create_utility_visualizations <- function(data, 
                                          net_series, 
                                          output_dir = "visualizations") {
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Creating utility visualizations...\n")
  
  
  
  #0. Create NC apot (aliveatanytime * NC_potential) plots
  create_nc_apot_plots(data, output_dir)
  
  # 1. Create NC utility plots
  create_nc_utility_plots(data, output_dir)
  
  # 2. Create WR utility plots  
  create_wr_utility_plots(data, output_dir)
  
  # 3. Prepare data for net series
  extended_data_for_net <- prepare_data_for_net_series(data, output_dir)
  
  # 4 Create population tables
  create_population_tables_n_wta_wfi_fbe(extended_data_for_net, output_dir)
  
  # 5. Create treemaps
  create_treemaps_n_wta_wfi_fbe(extended_data_for_net, output_dir)

  # 6a. Create four-panel population plot
  create_four_panel_population_plots_n(extended_data_for_net, output_dir)
  create_four_panel_population_plots(extended_data_for_net, output_dir)

  #6b. Create four-panel NC_tot plot
  create_four_panel_nc_tot_plots(extended_data_for_net, output_dir)

  #6c. Create four-panel NC_apot plot [doesn't need to be shown except in appendix]
  create_four_panel_nc_apot_plots(extended_data_for_net, output_dir)

  #6d. Create four-panel NC score range
  p4_from_four_panel_NC_score_range <-
    create_four_panel_nc_score_range_plots(extended_data_for_net, output_dir)

  #7a. Create three-panel NC functional form changes
  create_three_panel_nc_func_form_check(extended_data_for_net, output_dir, p4_from_four_panel_NC_score_range)

  #7b. Create four-panel WR score range
  create_four_panel_wr_score_range_plots(extended_data_for_net, output_dir)
  
  #6c. Something to do with different measures for human welare
  
  #Commenting out due to not needed for fifth pass
  # 8. Create NC net utility comparisons
  create_nc_net_utility_comparisons(extended_data_for_net, output_dir)

  # 9. Create WR net utility comparisons
  create_wr_net_utility_comparisons(extended_data_for_net, output_dir)

  # 10. Create NC net total series
  create_nc_net_tot_series(extended_data_for_net, output_dir)

  # 11. Create disaggregated plots with totals
  create_disaggregated_plots_with_totals(data, output_dir)
  
  cat("All utility visualizations completed successfully!\n")
}

#' Create utility visualizations from figures that have already been made
#' 
#' @param output_dir Directory for saving visualizations
#' @return NULL (saves plots to files)
combine_utility_visualizations <- function(output_dir = "visualizations") {
  # 1a. Create eight-panel population display
  create_six_panel_population_display(output_dir = output_dir, 
                                        save_dir = "fifth_pass/welfare_results/six_figure_displays")
}
