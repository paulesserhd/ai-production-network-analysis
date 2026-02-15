################################################################################
# 20_network_visualisation_call.R
# Applying visualisation functions to prepared datasets
################################################################################

# Load environment and packages
source("scripts/00_setup.R")

hardware_trade_centrality <- read.csv(file.path(path_processed, "hardware_trade_centrality_2017_2023.csv"))
cloud_service_centrality <- read.csv(file.path(path_processed, "cloud_service_regions_centrality_2017_2023.csv"))
data_service_centrality <- read.csv(file.path(path_processed, "dataservice_trade_centrality_2017_2023.csv"))

# Load functions
source("scripts/11_network_visualisation_functions.R")

# Network maps
network_map_trade(hardware_trade_centrality, "hardware_trade")
network_map_regions(cloud_service_centrality, "cloud_regions")
network_map_trade(data_service_centrality, "data_service")

# Network map vs. US AI diffusion framework
ai_diffusion_network(hardware_trade_centrality)

# Scatter plot - degree (in) vs degree (out)
centrality_degree_in_out_plot(hardware_trade_centrality, "hardware_trade")
centrality_degree_in_out_plot(cloud_service_centrality, "cloud_regions")
centrality_degree_in_out_plot(data_service_centrality, "data_service")

centrality_degree_in_out_norm_plot(hardware_trade_centrality, "hardware_trade")
centrality_degree_in_out_norm_plot(cloud_service_centrality, "cloud_regions")
centrality_degree_in_out_norm_plot(data_service_centrality, "data_service")

# Scatter plot - strength (in) vs strength (out)
centrality_strength_in_out_plot(hardware_trade_centrality, "hardware_trade")
centrality_strength_in_out_plot(cloud_service_centrality, "cloud_regions")
centrality_strength_in_out_plot(data_service_centrality, "data_service")

centrality_strength_in_out_norm_plot(hardware_trade_centrality, "hardware_trade")
centrality_strength_in_out_norm_plot(cloud_service_centrality, "cloud_regions")
centrality_strength_in_out_norm_plot(data_service_centrality, "data_service")

# Line plot - betweenness over time
centrality_betweenness_line_plot(hardware_trade_centrality, "hardware_trade")
centrality_betweenness_line_plot(cloud_service_centrality, "cloud_regions")
centrality_betweenness_line_plot(data_service_centrality, "data_service")

# Line plot - strength over time
centrality_strength_line_plot(hardware_trade_centrality, "hardware_trade")
centrality_strength_line_plot(cloud_service_centrality, "cloud_regions")
centrality_strength_line_plot(data_service_centrality, "data_service")

# Line plot - degree over time
centrality_degree_line_plot(hardware_trade_centrality, "hardware_trade")
centrality_degree_line_plot(cloud_service_centrality, "cloud_regions")
centrality_degree_line_plot(data_service_centrality, "data_service")

# Export / Import Plots
exports(hardware_trade_centrality, "USA")
exports(hardware_trade_centrality, "CHN")
imports(hardware_trade_centrality, "USA")
imports(hardware_trade_centrality, "CHN")

################################################################################