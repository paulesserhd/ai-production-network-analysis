################################################################################
# 00_setup.R
# Script to set up R environment by installing and loading necessary packages, 
# defining directory pathways and setting seed
################################################################################

rm(list = ls())

required_packages <- c(
  "igraph",
  "dplyr",
  "ggraph",
  "purrr", 
  "rnaturalearth", 
  "geosphere",
  "sf",
  "here", 
  "tidygraph", 
  "tidyr", 
  "patchwork", 
  "ggrepel", 
  "here", 
  "magick"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}



path_raw_hardware <- here("data", "raw", "hardware")
path_raw_cloud_service <- here("data", "raw", "cloud_services")
path_raw_data <- here("data", "raw", "data_services")
path_processed <- here("data", "processed")
path_external <- here("data", "external")
path_output_scatter <- here("output", "scatter_plots")
path_output_line <- here("output", "line_plots")
path_output_maps <- here("output", "maps")
path_output_degree <- here("output", "degree_in_out")
path_output_strength <- here("output", "strength_in_out")
path_output_cloud <- here("output", "cloud")
path_output_export_import <- here("output", "export_import")

options(stringsAsFactors = FALSE, scipen = 999)

set.seed(42)

################################################################################
