################################################################################
# 11_network_visualisation_hardware.R
# Network visualisation function: network plot, network maps, line plots, scatter plots
################################################################################

network_plot <- function(data, data_name) {
  
  years <- 2017:2023
  
  for(yr in years) {
    # Filter data for the specified year
    data_year <- data %>%
      select(from, to, weight, year) %>%
      filter(year == yr)
    
    # Create graph object
    graph <- data_year %>%
      as_tbl_graph(directed = TRUE, 
                   from = from,
                   to = to) %>%
      activate(nodes) %>%
      mutate(strength = centrality_degree(mode = "all", weight = weight))
    
    # Plot graph
    ggraph(graph, layout = "fr") +
      geom_edge_link(aes(width = weight), alpha = 0.6) +
      geom_edge_link(aes(width = weight),
                     arrow = arrow(length = unit(2, "mm")),  
                     end_cap = circle(2, "mm"),              
                     alpha = 0.6,
                     color = "grey") +
      scale_edge_width(range = c(0.3, 2)) +
      geom_node_point(size = 3) +
      geom_node_point(aes(size = strength), color = "black") +
      scale_size(range = c(3, 6), limits = c(NA,NA)) +
      geom_node_text(aes(label = name), 
                     color = "black", 
                     repel = TRUE, 
                     size = 3, 
                     fontface= "bold", 
                     point.padding = unit(0.6, "lines"), 
                     box.padding   = unit(0.5, "lines"),  
                     segment.size  = 0.5) + 
      labs(
        title = paste0("Global ", data_name, " Network (", yr, ")"),
        size = "Strength Centrality", 
        width = "Resource Flow" 
      ) +
      theme_void() 
    
    ggsave(filename = paste0(data_name,"_", yr, ".png"), 
           path = path_output_network, 
           width = 10, 
           height = 8)
  }
}

network_map_trade <- function(data, data_name) {
  
  # Create world map object using the natural earth package
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  years <- 2017:2023
  
  for (yr in years) {
    
    # loading visualisation dataset and filter for year
    edges <- data %>% 
      filter(year == yr)
    
    # Identify active nodes and flag in world map
    active_nodes <- unique(c(edges$from, edges$to))
    
    world <- world %>%
      mutate(active = ifelse (iso_a3 %in% active_nodes, TRUE, FALSE))
    
    # Extract node coordinates & iso3 from world map
    nodes <- world %>%
      select(iso_a3, geometry) %>%
      st_centroid() %>% 
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
        ) %>%
      st_drop_geometry() %>%
      rename(name = iso_a3) %>%
      filter (name %in% active_nodes)  # keep only active nodes
    
    # Create graph object and compute centrality
    graph <- edges %>%
      as_tbl_graph(directed = TRUE, from = from, to = to) %>%
      activate(nodes) %>%
      mutate (betweenness_cent = centrality_betweenness(), 
              strength_cent = centrality_degree(mode = "all", weight = weight), 
              betweenness_norm = betweenness_cent / max(betweenness_cent, na.rm = TRUE), 
              strength_norm = strength_cent / max(strength_cent, na.rm = TRUE))
    
    # attach graph parameters to extracted nodes
    nodes <- nodes %>%
      left_join(
        graph %>% as_tibble() %>% select(name, 
                                         betweenness_cent, 
                                         strength_cent, 
                                         betweenness_norm, 
                                         strength_norm), 
        by = c("name"))
    
    # Merging coordinates with nodes based on iso3 codes
    edges_coords <- edges %>%
      left_join(nodes, by = c("from" = "name")) %>%
      rename(x_from = lon, y_from = lat) %>%
      left_join(nodes, by = c("to" = "name")) %>%
      rename(x_to = lon, y_to = lat)
    
    # Plot world map with trade network edges and nodes
    ggplot() +
      # load world map from rnaturalearth object
      geom_sf(data = world, aes(fill = active), color = "gray70") +
      scale_fill_manual(values = c("FALSE" = "gray95", "TRUE" = "lightblue"), guide = "none") +
      # project edges onto world map
      geom_curve(
        data = edges_coords,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = weight),
        curvature = 0.2, color = "gray50", alpha = 0.6,
        arrow = arrow(length = unit(2, "mm"), type = "closed")) +
      # weighted line width (trade value)
      scale_linewidth_continuous(
        range = c(0.2, 2),
        name  = "Trade value (million USD)") +
      # project nodes onto world map
      geom_point(
        data = nodes,
        aes(x = lon, y = lat,
            color = betweenness_norm, size = strength_norm)) +
      # strength centrality represented in node size
      scale_size_continuous(
        range = c(2, 5),
        name  = "Strength centrality") +
      # betweenness centrality represented in node color
      scale_color_gradient2(low = "darkslategray3", 
                            mid = "darkslategray4", 
                            high = "darkslategray",
                            midpoint = 0.5,
                            name = "Betweenness centrality") +
      # ensure node names aren't cut off when plotting
      geom_text_repel(
        data = nodes,
        aes(x = lon, y = lat, label = name),  
        size = 3,
        fontface = "bold") +
      coord_sf(clip = "off") +
      # customize layout & legend
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(10, 10, 30, 10),
        axis.title = element_blank(),      
        axis.text = element_blank(),       
        axis.ticks = element_blank()) +       
      guides(
        color = guide_colorbar(
          title = "Betweenness centrality",
          title.position = "top",
          title.hjust = 0.5),
        size = guide_legend(
          title = "Strength centrality",
          title.position = "top",
          title.hjust = 0.5),
        linewidth = guide_legend(
          title = "Trade value (million USD)",
          title.position = "top",
          title.hjust = 0.5, 
          nrow = 2,
          byrow = TRUE))
    
    ggsave(filename = paste0(data_name, "_network_map", yr, ".png"), 
           path = path_output_maps, 
           width = 10, 
           height = 8)
  }
}

network_map_regions <- function(data, data_name) {
  
  # Create world map object using the natural earth package
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  years <- 2017:2023
  
  # loading visualisation dataset and filter for year
  for (yr in years) {
    edges <- data %>% 
      filter(year == yr)
  
  # Identify active nodes and flag in world map  
    active_nodes <- unique(c(edges$from, edges$to))
    
    world <- world %>%
      mutate(active = ifelse (iso_a3 %in% active_nodes, TRUE, FALSE))
    
  # Extract node coordinates & iso3 from world map
    nodes <- world %>%
      select(iso_a3, geometry) %>%
      st_centroid() %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
      ) %>%
      st_drop_geometry() %>%
      rename(name = iso_a3) %>%
      filter (name %in% active_nodes)
    
  # Create graph object oand compute centrality
    graph <- edges %>%
      as_tbl_graph(directed = TRUE, from = from, to = to) %>%
      activate(nodes) %>%
      mutate (betweenness_cent = centrality_betweenness(), 
              strength_cent = centrality_degree(mode = "all", weight = weight))
    
  # Attach graph parameters to extracted nodes
    nodes <- nodes %>%
      left_join(
        graph %>% as_tibble() %>% select(name, betweenness_cent, strength_cent), 
        by = c("name"))
    
    nodes <- nodes %>%
      mutate(
        betweenness_norm = betweenness_cent / max(betweenness_cent, na.rm = TRUE), 
        strength_norm = strength_cent / max(strength_cent, na.rm = TRUE)
      )
    
  # Merging coordinates with nodes based on iso3 codes
    edges_coords <- edges %>%
      left_join(nodes, by = c("from" = "name")) %>%
      rename(x_from = lon, y_from = lat) %>%
      left_join(nodes, by = c("to" = "name")) %>%
      rename(x_to = lon, y_to = lat)
    
  # Plot world map with cloud service network edges and nodes
    ggplot() +
      # load world map from rnaturalearth object
      geom_sf(data = world, aes(fill = active), color = "gray70") +
      scale_fill_manual(values = c("FALSE" = "gray95", "TRUE" = "lightblue"), guide = "none") +# base map
      # project edges onto world map
      geom_curve(
        data = edges_coords,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = weight),
        curvature = 0.2, color = "gray50", alpha = 0.6,
        arrow = arrow(length = unit(2, "mm"), type = "closed")) +
      # weighted line width (number of hosted cloud regions)
       scale_linewidth_continuous(
        range = c(0.2, 2),
        name  = "Number of hosted cloud regions") +
      # project nodes onto world map
      geom_point(
        data = nodes,
        aes(x = lon, y = lat,
            color = betweenness_norm, size = strength_norm)) +
      # strength centrality represented in node size
      scale_size_continuous(
        range = c(2, 5),
        name  = "Strength centrality") +
      # betweenness centrality represented in node colour
      scale_color_gradient2(low = "darkslategray3", 
                            mid = "darkslategray4", 
                            high = "darkslategray",
                            midpoint = 0.5,
                            name = "Betweenness centrality") +
      # ensure node names aren't cut off when plotting
      geom_text_repel(
        data = nodes,
        aes(x = lon, y = lat, label = name),  
        size = 3,
        fontface = "bold") +
      coord_sf(clip = "off") +
      # customize layout and legend
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(10, 10, 30, 10),
        axis.title = element_blank(),      
        axis.text = element_blank(),      
        axis.ticks = element_blank()) +       
      guides(
        color = guide_colorbar(
          title = "Betweenness centrality",
          title.position = "top",
          title.hjust = 0.5),
        size = guide_legend(
          title = "Strength centrality",
          title.position = "top",
          title.hjust = 0.5),
        linewidth = guide_legend(
          title = "Number of hosted cloud regions",
          title.position = "top",
          title.hjust = 0.5, 
          nrow = 2,
          byrow = TRUE))
    
    ggsave(filename = paste0(data_name, "_network_map", yr, ".png"), 
           path = path_output_maps, 
           width = 10, 
           height = 8)
  }
}
  
centrality_line_plot <- function(data, data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(data)
  
  # Split centrality measures in rows 
  centrality_long <- centrality_results %>%
    pivot_longer(
      cols = -c(name, year),
      names_to = "measure",
      values_to = "value"
    )
  
  # Compute average centrality to identify top countries
  centrality_average <- centrality_long %>%
    group_by(name, measure) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  top_countries_between <- centrality_average %>%
    filter(measure == "betweenness") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_strength <- centrality_average %>%
    filter(measure == "strength") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Plot centrality measures over time for top countries
  
  centrality_plot_data <- centrality_long %>%
    filter(name %in% top_countries_strength$name | name %in% top_countries_between$name) %>%
    filter(measure == "betweenness" | measure == "strength")
    
  measure_labels <- c(
    "strength" = "Strength Centrality",
    "betweenness" = "Betweenness Centrality"
  )
  
  ggplot(centrality_plot_data,
         aes(x = year, y = value, color = name, group = name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ measure, scales = "free_y", labeller = labeller (measure = measure_labels)) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "year",
      y = "centrality value",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 1, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "bottom")
  
    ggsave(filename = paste0("line_plot_", data_name, ".png"), 
           path = path_output_line, 
           width = 10, 
           height = 6)
}

centrality_line_plot_norm <- function (data,data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(data)
  
  # Split centrality measures in rows 
  centrality_long <- centrality_results %>%
    pivot_longer(
      cols = -c(name, year),
      names_to = "measure",
      values_to = "value"
    )
  
  # Compute average centrality to identify top countries
  centrality_average <- centrality_long %>%
    group_by(name, measure) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  top_countries_between <- centrality_average %>%
    filter(measure == "betweenness") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_strength <- centrality_average %>%
    filter(measure == "strength") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Compute normalised centrality per year and filter for top countries
  centrality_plot_data <- centrality_results %>%
    group_by(year) %>% # normalising within year
    mutate(strength_norm = strength / max(strength, na.rm = TRUE),
           betweenness_norm = betweenness / max(betweenness, na.rm =TRUE)) %>%
    filter(name %in% top_countries_strength$name | name %in% top_countries_between$name) %>%
    select(name, year, strength_norm, betweenness_norm)
  
  # Prepare dataset for plotting
  centrality_plot_long <- centrality_plot_data %>%
    pivot_longer(
      cols = c(strength_norm, betweenness_norm),
      names_to = "measure",
      values_to = "value"
    )
  
  # Plot centrality measures over time for top countries
  measure_labels <- c(
    "strength_norm" = "Strength Centrality",
    "betweenness_norm" = "Betweenness Centrality"
  )
  
  ggplot(centrality_plot_long,
         aes(x = year, y = value, color = name, group = name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ measure, scales = "free_y", labeller = labeller (measure = measure_labels)) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "year",
      y = "centrality value",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 1, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "bottom")
  
  ggsave(filename = paste0("line_plot_norm_", data_name, ".png"), 
         path = path_output_line, 
         width = 10, 
         height = 6)
}

centrality_scatter_plot <- function(data, data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(data)
  
  # Split centrality measures in rows 
  centrality_long <- centrality_results %>%
    pivot_longer(
      cols = -c(name, year),
      names_to = "measure",
      values_to = "value"
    )
  
  # Compute average centrality to identify top countries
  centrality_average <- centrality_long %>%
    group_by(name, measure) %>%
    summarise(avg_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  top_countries_between <- centrality_average %>%
    filter(measure == "betweenness") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_strength <- centrality_average %>%
    filter(measure == "strength") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Prepare data for scatter plot: normalise centrality and filter for top countries
  centrality_plot_data <- centrality_results %>%
    filter(year == 2017 | year == 2020 | year == 2023) %>%
    group_by(year) %>% # normalising within year
    mutate(strength_norm = strength / max(strength, na.rm = TRUE),
           betweenness_norm = betweenness / max(betweenness, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(top5_strength = name %in% top_countries_strength$name,
           top5_betweenness = name %in% top_countries_between$name) %>%
    select(name, year, strength_norm, betweenness_norm, top5_strength, top5_betweenness)
  
  #Plot centrality measures in scatter plots  
  ggplot(centrality_plot_data,
         aes(x = strength_norm, y = betweenness_norm)) +
    geom_point(alpha = 0.7, size = 2) +
    # color top countries
    geom_point(
      data = centrality_plot_data %>% filter(top5_strength | top5_betweenness),
      aes(color = top5_strength | top5_betweenness),
      size = 2,
      show.legend = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = centrality_plot_data %>% filter(top5_strength | top5_betweenness),
      aes(label = name),
      size = 3,
      clip = "off"
    ) +
    facet_wrap(~ year, scales = "free") +
    labs(
      x = "strength centrality",
      y = "betweenness centrality", 
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      strip.text = element_text(face = "bold"), 
      panel.border = element_rect(color = "black", size = 0.5, fill = NA)
    )
  
  ggsave(filename = paste0("scatter_plot_", data_name, ".png"), 
         path = path_output_scatter, 
         width = 10, 
         height = 6)
}

centrality_across_networks_plot <- function(data_hardware, data_cloud, data_dataservice) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_hardware <- centrality_measures(data_hardware) %>%
    mutate(network = "hardware_trade")
  
  centrality_cloud <- centrality_measures(data_cloud) %>%
    mutate(network = "cloud_regions")
  
  centrality_dataservice <- centrality_measures(data_dataservice) %>%
    mutate(network = "data_service")
  
  centrality_all <- bind_rows(centrality_hardware, centrality_cloud, centrality_dataservice) %>%
    group_by(network) %>%
    mutate(strength_norm = strength / max(strength, na.rm = TRUE), 
           betweenness_norm = betweenness / max(betweenness, na.rm = TRUE)) %>%
    select(name, year, network, strength_norm, betweenness_norm)
  
   centrality_us_china <- centrality_all %>%
    filter(name == "USA" | name == "CHN")
  
  # Plot centrality measures per country and year across networks
  centrality_plot_long <- centrality_us_china %>%
    pivot_longer(
      cols = c(strength_norm, betweenness_norm),
      names_to = "measure",
      values_to = "value"
    )
  
  measure_labels <- c(
    "strength_norm" = "Strength Centrality",
    "betweenness_norm" = "Betweenness Centrality")
  
  network_labels <- c(
    "cloud_regions" = "Cloud Service Regions", 
    "data_service" = "Data Service Trade",
    "hardware_trade" = "Hardware Products Trade")

  ggplot(centrality_plot_long,
    aes(x = year, y = value, color = name, group = name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_grid(measure ~ network, scales = "free_y", 
      labeller = labeller(
        measure = measure_labels,
        network = network_labels)) +
    scale_color_manual(
          values = c("USA" = "#1f77b4", "CHN" = "#d62728"),
          labels = c("USA" = "United States", "CHN" = "China")) + 
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    labs(
      x = "Year",
      y = "Centrality value",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom", 
      panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
    )
  
  ggsave(filename = paste0("line_plot_compared_cross_network_centrality.png"), 
         path = path_output_comparison, 
         width = 10, 
         height = 6)  
  
}

################################################################################