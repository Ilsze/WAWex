# The aim of this file is to create subsets for visualisation to streamline the 
# code. E.g, farmed animals only, non-numerous animals only. I hope to make 
# future generation of figures much easier - simply describe the subset, the 
# graph style (stacked or not), extension status, and the layout style (n-panel)
# to receive the desired output in a new folder. 
library(pacman)
p_load(tidyverse, dplyr, hrbrthemes, ggplot2, grid, gridExtra, gt, png, readr, magick, mgcv, tidyselect, 
       stringr, readxl, openxlsx, foreign, broom, knitr, data.table, dlm, 
       patchwork, scales, treemap)

