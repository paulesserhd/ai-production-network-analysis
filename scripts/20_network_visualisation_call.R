################################################################################
# 20_network_visualisation_call.R
# Applying visualisation functions to prepared datasets
################################################################################

# Load environment and packages
source("scripts/00_setup.R")

hardware_trade_visual <- read.csv(file.path(path_processed, "hardware_trade_plot_2017_2023.csv"))
cloud_service_visual <- read.csv(file.path(path_processed, "cloud_service_regions_plot_2017_2023.csv"))
data_service_visual <- read.csv(file.path(path_processed, "dataservice_trade_plot_2017_2023.csv"))

hardware_trade_centrality <- read.csv(file.path(path_processed, "hardware_trade_centrality_2017_2023.csv"))
cloud_service_centrality <- read.csv(file.path(path_processed, "cloud_service_regions_centrality_2017_2023.csv"))
data_service_centrality <- read.csv(file.path(path_processed, "dataservice_trade_centrality_2017_2023.csv"))

# Load functions
source("scripts/11_network_visualisation_functions.R")

# Network plots
network_plot(hardware_trade_visual, "hardware_trade")
network_plot(cloud_service_visual, "cloud_regions")
network_plot(data_service_visual, "data_service")

# Network maps
network_map_trade(hardware_trade_visual, "hardware_trade")
network_map_regions(cloud_service_visual, "cloud_regions")
network_map_trade(data_service_visual, "data_service")

# Scatter plots
centrality_scatter_plot(hardware_trade_centrality, "hardware_trade")
centrality_scatter_plot(cloud_service_centrality, "cloud_regions")
centrality_scatter_plot(data_service_centrality, "data_service")

# Line plots
centrality_line_plot(hardware_trade_centrality, "hardware_trade")
centrality_line_plot(cloud_service_centrality, "cloud_regions")
centrality_line_plot(data_service_centrality, "data_service")

centrality_line_plot_norm(hardware_trade_centrality, "hardware_trade")
centrality_line_plot_norm(cloud_service_centrality, "cloud_regions")
centrality_line_plot_norm(data_service_centrality, "data_service")

# Comparison plot
centrality_across_networks_plot(hardware_trade_centrality, 
                                cloud_service_centrality, 
                                data_service_centrality)

################################################################################