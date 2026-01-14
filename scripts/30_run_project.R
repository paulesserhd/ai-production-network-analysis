################################################################################
# 30_run_project.R
# Run project pipeline 
################################################################################

# Load environment
source("scripts/00_setup.R")

# Load and pre-process datasets
source("scripts/01_data_processing_hardware.R")
source("scripts/02_data_processing_data.R")
source("scripts/03_data_processing_cloudservices.R")

# Load network analysis and visualisation functions
source("scripts/11_network_visualisation_functions.R")
source("scripts/12_network_analysis_functions.R")

# Generate network plots
source("scripts/20_network_visualisation_call.R")

rm(list=ls())

################################################################################