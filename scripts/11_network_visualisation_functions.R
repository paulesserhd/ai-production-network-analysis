################################################################################
# 11_network_visualisation_hardware.R
# Network visualisation function: network plot, network maps, line plots, scatter plots
################################################################################

# Network Visualisation

network_map_trade <- function(data, data_name) {
  
  # Create world map object using the natural earth package
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Correct missing iso-codes for specific country cases
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    mutate(
      iso_a3 = case_when(
        admin == "France" ~ "FRA",
        admin == "Norway" ~ "NOR",
        admin == "Kosovo" ~ "XKX",
        admin %in% c("Taiwan", "Taiwan, Province of China") ~ "TWN",
        TRUE ~ iso_a3
      )
    )
  
  # set fixed edge width limits across the whole observation period to ensure cross-year comparability
  edge_limits <- range(data$weight, na.rm = TRUE)
  
  years <- 2017:2023
  
  for (yr in years) {
    
    # loading full network data for centrality measure computation
    edges_full <- data %>% 
      filter(year == yr)
    
    # loading visualisation dataset and filter for year
    edges_plot <- data %>%
      filter(year == yr) %>%
      mutate(total_trade_year = sum(weight)) %>%
      filter(weight >= 0.005 * total_trade_year)
    
    # Identify active nodes and flag in world map
    active_nodes <- unique(c(edges_plot$from, edges_plot$to))
    
    world_year <- world %>%
      mutate(active = ifelse (iso_a3 %in% active_nodes, TRUE, FALSE))
    
    # Extract node coordinates & iso3 from world map
    nodes <- world_year %>%
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
    graph_full <- edges_full %>%
      as_tbl_graph(directed = TRUE, from = from, to = to) %>%
      activate(nodes) %>%
      mutate (betweenness_cent = centrality_betweenness(), 
              strength_cent = centrality_degree(mode = "all", weight = weight))
    
    # Attach graph parameters to extracted nodes
    node_centrality <- graph_full %>%
      as_tibble() %>%
      select(name, betweenness_cent, strength_cent) %>%
      mutate(
        betweenness_norm = betweenness_cent / max(betweenness_cent, na.rm = TRUE), 
        strength_norm = strength_cent / max(strength_cent, na.rm = TRUE)
      )
    
    nodes <- nodes %>%
      left_join(node_centrality, by = c("name"))
    
    
    # Merging coordinates with nodes based on iso3 codes
    edges_coords <- edges_plot %>%
      left_join(nodes, by = c("from" = "name")) %>%
      rename(x_from = lon, y_from = lat) %>%
      left_join(nodes, by = c("to" = "name")) %>%
      rename(x_to = lon, y_to = lat)
    
     
  # Plot world map with trade network edges and nodes
    ggplot() +
      # load world map from rnaturalearth object
      geom_sf(data = world_year, aes(fill = active), color = "gray70") +
      scale_fill_manual(values = c("FALSE" = "gray95", "TRUE" = "lightblue"), guide = "none") +
      # project edges onto world map
      geom_curve(
        data = edges_coords,
        aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = weight),
        curvature = 0.2, color = "gray50", alpha = 0.6,
        arrow = arrow(length = unit(2, "mm"), type = "closed")) +
      # weighted line width (trade value)
      scale_linewidth_continuous(
        limits = edge_limits,
        range = c(0.2, 2),
        name  = "Trade balance (million USD)") +
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
                            limits = c(0, 1),
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
          title = "Trade balance (million USD)",
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
  
  # Correct missing iso-codes for specific country cases
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    mutate(
      iso_a3 = case_when(
        admin == "France" ~ "FRA",
        admin == "Norway" ~ "NOR",
        admin == "Kosovo" ~ "XKX",
        admin %in% c("Taiwan", "Taiwan, Province of China") ~ "TWN",
        TRUE ~ iso_a3
      )
    )
  
  # set fixed edge width limits across the whole observation period to ensure cross-year comparability
  edge_limits <- range(data$weight, na.rm = TRUE)
  
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
         limits = edge_limits, 
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

# Line Plots of Relative Degree Centrality

centrality_betweenness_line_plot <- function(data, data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(data) %>%
    group_by(year) %>%
    mutate(betweenness_norm = betweenness / max(betweenness, na.rm = TRUE)) %>%
    ungroup() %>%
    select(name, year, betweenness_norm)
  
  # Compute average centrality to identify top countries
  centrality_average <- centrality_results %>%
    group_by(name) %>%
    summarise(avg_betweenness = mean(betweenness_norm, na.rm = TRUE), .groups = "drop")
  
  top5_countries_between <- centrality_average %>%
    arrange(desc(avg_betweenness)) %>%
    head(5)
  
  top20_countries_between <- centrality_average %>%
    arrange(desc(avg_betweenness)) %>%
    head(20)
  
  # Plot centrality measures over time for top countries
  
  label_data <- centrality_results %>%
    filter(name %in% top5_countries_between$name) %>%
    group_by(name) %>%
    filter(year == max(year)) %>% 
    ungroup()
  
  centrality_plot_20 <- centrality_results %>%
    filter(name %in% top20_countries_between$name)
  
  centrality_plot_5 <- centrality_results %>%
    filter(name %in% top5_countries_between$name)
  
  ggplot(centrality_plot_20 %>% filter (!name %in% centrality_plot_5$name),
         aes(x = year, y = betweenness_norm, group = name)) +
    geom_line(linewidth = 0.5, color = "grey20", alpha = 0.2) +
    geom_point(size = 0.5, color = "grey20", alpha = 0.2) +
    geom_line(data = centrality_plot_5, 
             linewidth = 1, color = "black", 
             aes(linetype = name, group = name)) +
    geom_point(data = centrality_plot_5, 
               size = 1, color = "black", alpha = 0.7) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = name),
      nudge_x = 0.3,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3
    ) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "year",
      y = "betweenness centrality",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 0.5, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "none")
  
  ggsave(filename = paste0("betweenness_line_plot_", data_name, ".png"), 
         path = path_output_line, 
         width = 10, 
         height = 6)
}

centrality_degree_line_plot <- function(data, data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(data) %>%
    group_by(year) %>%
    mutate(degree_norm = degree / max(degree, na.rm = TRUE)) %>%
    ungroup() %>%
    select(name, year, degree_norm)
  
  # Compute average centrality to identify top countries
  centrality_average <- centrality_results %>%
    group_by(name) %>%
    summarise(avg_degree = mean(degree_norm, na.rm = TRUE), .groups = "drop")
  
  top5_countries_degree <- centrality_average %>%
    arrange(desc(avg_degree)) %>%
    head(5)
  
  top20_countries_degree <- centrality_average %>%
    arrange(desc(avg_degree)) %>%
    head(20)
  
  # Plot centrality measures over time for top countries
  
  label_data <- centrality_results %>%
    filter(name %in% top5_countries_degree$name) %>%
    group_by(name) %>%
    filter(year == max(year)) %>% 
    ungroup()
  
  centrality_plot_20 <- centrality_results %>%
    filter(name %in% top20_countries_degree$name)
  
  centrality_plot_5 <- centrality_results %>%
    filter(name %in% top5_countries_degree$name)
  
  ggplot(centrality_plot_20 %>% filter (!name %in% centrality_plot_5$name),
         aes(x = year, y = degree_norm, group = name)) +
    geom_line(linewidth = 0.5, color = "grey20", alpha = 0.2) +
    geom_point(size = 0.5, color = "grey20", alpha = 0.2) +
    geom_line(data = centrality_plot_5, 
              linewidth = 1, color = "black", 
              aes(linetype = name, group = name)) +
    geom_point(data = centrality_plot_5, 
               size = 1, color = "black", alpha = 0.7) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = name),
      nudge_x = 0.3,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3
    ) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "year",
      y = "degree centrality",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 0.5, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "none")
  
  ggsave(filename = paste0("degree_line_plot_", data_name, ".png"), 
         path = path_output_line, 
         width = 10, 
         height = 6)
}

centrality_strength_line_plot <- function(data, data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(data) %>%
    group_by(year) %>%
    mutate(strength_norm = strength / max(strength, na.rm = TRUE)) %>%
    ungroup() %>%
    select(name, year, strength_norm)
  
  # Compute average centrality to identify top countries
  centrality_average <- centrality_results %>%
    group_by(name) %>%
    summarise(avg_strength = mean(strength_norm, na.rm = TRUE), .groups = "drop")
  
  top5_countries_strength <- centrality_average %>%
    arrange(desc(avg_strength)) %>%
    head(5)
  
  top20_countries_strength <- centrality_average %>%
    arrange(desc(avg_strength)) %>%
    head(20)
  
  # Plot centrality measures over time for top countries
  
  label_data <- centrality_results %>%
    filter(name %in% top5_countries_strength$name) %>%
    group_by(name) %>%
    filter(year == max(year)) %>% 
    ungroup()
  
  centrality_plot_20 <- centrality_results %>%
    filter(name %in% top20_countries_strength$name)
  
  centrality_plot_5 <- centrality_results %>%
    filter(name %in% top5_countries_strength$name)
  
  ggplot(centrality_plot_20 %>% filter (!name %in% centrality_plot_5$name),
         aes(x = year, y = strength_norm, group = name)) +
    geom_line(linewidth = 0.5, color = "grey20", alpha = 0.2) +
    geom_point(size = 0.5, color = "grey20", alpha = 0.2) +
    geom_line(data = centrality_plot_5, 
              linewidth = 1, color = "black", 
              aes(linetype = name, group = name)) +
    geom_point(data = centrality_plot_5, 
               size = 1, color = "black", alpha = 0.7) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = name),
      nudge_x = 0.3,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3
    ) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "year",
      y = "strength centrality",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 0.5, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "none")
  
  ggsave(filename = paste0("strength_line_plot_", data_name, ".png"), 
         path = path_output_line, 
         width = 10, 
         height = 6)
}

# Scatter Plot for centrality dimension camparison

centrality_degree_in_out_plot <- function(data, data_name) {
  
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
  
  top_countries_degree_in <- centrality_average %>%
    filter(measure == "degree_in") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_degree_out <- centrality_average %>%
    filter(measure == "degree_out") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Prepare data for scatter plot: normalise centrality and filter for top countries
  centrality_plot_data <- centrality_results %>%
    filter(year == 2017 | year == 2020 | year == 2023) %>%
    mutate(top5_degree_in = name %in% top_countries_degree_in$name,
           top5_degree_out = name %in% top_countries_degree_out$name) %>%
    select(name, year, degree_in, degree_out, top5_degree_in, top5_degree_out)
  
  # Prepare for forced axis scales
  max_degree <- max(
    centrality_plot_data$degree_in,
    centrality_plot_data$degree_out,
    na.rm = TRUE
  )
  
  #Plot centrality measures in scatter plots  
  ggplot(centrality_plot_data,
         aes(x = degree_in, y = degree_out)) +
    geom_point(alpha = 0.2, size = 1.5) +
    # color top countries
    geom_point(
      data = centrality_plot_data %>% filter(top5_degree_in | top5_degree_out),
      aes(color = top5_degree_in | top5_degree_out),
      size = 2,
      show.legend = FALSE) +
    ggrepel::geom_text_repel(
      data = centrality_plot_data %>% filter(top5_degree_in | top5_degree_out),
      aes(label = name),
      size = 3,
      clip = "off") +
    facet_wrap(~ year, scales = "free") +
    labs(
      x = "degree centrality (in)",
      y = "degree centrality (out)") + 
    # force same axis scale
    scale_x_continuous(limits = c(0, max_degree)) +
    scale_y_continuous(limits = c(0, max_degree)) +
    # add reference line
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "grey50") +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      strip.text = element_text(face = "bold"), 
      panel.border = element_rect(color = "black", size = 0.5, fill = NA)
    )
  
  ggsave(filename = paste0("degree_comparison_", data_name, ".png"), 
         path = path_output_degree, 
         width = 10, 
         height = 6)
}

centrality_degree_in_out_norm_plot <- function(data, data_name) {
  
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
  
  top_countries_degree_in <- centrality_average %>%
    filter(measure == "degree_in") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_degree_out <- centrality_average %>%
    filter(measure == "degree_out") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Prepare data for scatter plot: normalise centrality and filter for top countries
  centrality_plot_data <- centrality_results %>%
    mutate(top5_degree_in = name %in% top_countries_degree_in$name,
           top5_degree_out = name %in% top_countries_degree_out$name, 
           degree_total = degree_in + degree_out,
           degree_in_norm = ifelse(degree_total > 0, degree_in / degree_total, NA),
           degree_out_norm = ifelse(degree_total > 0, degree_out / degree_total, NA)) %>%
    filter(year == 2017 | year == 2020 | year == 2023, 
           top5_degree_in == TRUE | top5_degree_out == TRUE) %>%
    select(name, year, degree_in_norm, degree_out_norm, top5_degree_in, top5_degree_out)
  
  #Plot centrality measures in scatter plots  
  ggplot(centrality_plot_data,
         aes(x = degree_in_norm, y = degree_out_norm)) +
    geom_point(aes(alpha = top5_degree_in | top5_degree_out),
               size = 2) +
    scale_alpha_manual(
      values = c('TRUE' = 1, 'FALSE' = 0.2), 
      guide = "none") +
    # color top countries
    geom_point(
      data = centrality_plot_data %>% filter(top5_degree_in | top5_degree_out),
      aes(color = top5_degree_in | top5_degree_out),
      size = 2,
      show.legend = FALSE) +
    ggrepel::geom_text_repel(
      data = centrality_plot_data %>% filter(top5_degree_in | top5_degree_out),
      aes(label = name),
      size = 3,
      clip = "off") +
    facet_wrap(~ year, scales = "free") +
    labs(
      x = "degree centrality (in)",
      y = "degree centrality (out)", 
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      strip.text = element_text(face = "bold"), 
      panel.border = element_rect(color = "black", size = 0.5, fill = NA)
    )
  
  ggsave(filename = paste0("degree_comparison_norm_", data_name, ".png"), 
         path = path_output_degree, 
         width = 10, 
         height = 6)
}

centrality_strength_in_out_plot <- function(data, data_name) {
  
  source("scripts/12_network_analysis_functions.R")
  
  centrality_results <- centrality_measures(hardware_trade_centrality)
  
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
  
  top_countries_strength_in <- centrality_average %>%
    filter(measure == "strength_in") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_strength_out <- centrality_average %>%
    filter(measure == "strength_out") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Prepare data for scatter plot: normalise centrality and filter for top countries
  centrality_plot_data <- centrality_results %>%
    filter(year == 2017 | year == 2020 | year == 2023) %>%
    mutate(top5_strength_in = name %in% top_countries_strength_in$name,
           top5_strength_out = name %in% top_countries_strength_out$name) %>%
    select(name, year, strength_in, strength_out, top5_strength_in, top5_strength_out)
  
  max_degree <- max(
    centrality_plot_data$strength_in,
    centrality_plot_data$strength_out,
    na.rm = TRUE
  )
  
  #Plot centrality measures in scatter plots  
  ggplot(centrality_plot_data,
         aes(x = strength_in, y = strength_out)) +
    geom_point(alpha = 0.2, size = 1.5) +
    # color top countries
    geom_point(
      data = centrality_plot_data %>% filter(top5_strength_in | top5_strength_out),
      aes(color = top5_strength_in | top5_strength_out),
      size = 2,
      show.legend = FALSE) +
    ggrepel::geom_text_repel(
      data = centrality_plot_data %>% filter(top5_strength_in | top5_strength_out),
      aes(label = name),
      size = 3,
      clip = "off") +
    facet_wrap(~ year, scales = "free") +
    labs(
      x = "strength centrality (in)",
      y = "strength centrality (out)") +
    # force same axis scale
    scale_x_continuous(limits = c(0, max_degree)) +
    scale_y_continuous(limits = c(0, max_degree)) +
    # add reference line
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "grey50") +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      strip.text = element_text(face = "bold"), 
      panel.border = element_rect(color = "black", size = 0.5, fill = NA)
    )
  
  ggsave(filename = paste0("strength_comparison", data_name, ".png"), 
         path = path_output_strength, 
         width = 10, 
         height = 6)
}

centrality_strength_in_out_norm_plot <- function(data, data_name) {
  
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
  
  top_countries_strength_in <- centrality_average %>%
    filter(measure == "strength_in") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  top_countries_strength_out <- centrality_average %>%
    filter(measure == "strength_out") %>%
    arrange(desc(avg_value)) %>%
    head(5)
  
  # Prepare data for scatter plot: normalise centrality and filter for top countries
  centrality_plot_data <- centrality_results %>%
    mutate(top5_strength_in = name %in% top_countries_strength_in$name,
           top5_strength_out = name %in% top_countries_strength_out$name, 
           strength_total = strength_in + strength_out,
           strength_in_norm = ifelse(strength_total > 0, strength_in / strength_total, NA),
           strength_out_norm = ifelse(strength_total > 0, strength_out / strength_total, NA)) %>%
    filter(year == 2017 | year == 2020 | year == 2023, 
           top5_strength_in == TRUE | top5_strength_out == TRUE) %>%
    select(name, year, strength_in_norm, strength_out_norm, top5_strength_in, top5_strength_out)
  
  #Plot centrality measures in scatter plots  
  ggplot(centrality_plot_data,
         aes(x = strength_in_norm, y = strength_out_norm)) +
    geom_point(aes(alpha = top5_strength_in | top5_strength_out),
               size = 2) +
    scale_alpha_manual(
      values = c('TRUE' = 1, 'FALSE' = 0.2), 
      guide = "none") +
    # color top countries
    geom_point(
      data = centrality_plot_data %>% filter(top5_strength_in | top5_strength_out),
      aes(color = top5_strength_in | top5_strength_out),
      size = 2,
      show.legend = FALSE) +
    ggrepel::geom_text_repel(
      data = centrality_plot_data %>% filter(top5_strength_in | top5_strength_out),
      aes(label = name),
      size = 3,
      clip = "off") +
    facet_wrap(~ year, scales = "free") +
    labs(
      x = "strength centrality (in)",
      y = "strength centrality (out)", 
    ) +
    theme_minimal() +
    theme(
      aspect.ratio = 1,
      strip.text = element_text(face = "bold"), 
      panel.border = element_rect(color = "black", size = 0.5, fill = NA)
    )
  
  ggsave(filename = paste0("strength_comparison_norm_", data_name, ".png"), 
         path = path_output_strength, 
         width = 10, 
         height = 6)
}

# Cloud Region Distribution Plot

cloud_region_relative_distribution <- function(data) {
  
  source("scripts/12_network_analysis_functions.R")
  
  # Identify country with most hosted cloud regions based on in-strength centrality
  centrality_cloud <- centrality_measures(cloud_service_visual)
  
  top10 <- centrality_cloud %>%
    filter(year==2023) %>%
    arrange(desc(strength_in)) %>%
    head(10)
  
  # Prepare data to visualise relative distribution of US and Chinese infrastructures
  plot_data <- cloud_service_visual %>%
    filter(to %in% top10$name,
           year >= 2017, year <= 2023) %>%
    group_by(to, from, year) %>%
    summarise(
      total_strength = max(weight, na.RM = TRUE), 
      .groups = "drop")
  
  # Descending order based on strength centrality
  host_order <- plot_data %>%
    filter(year == 2023) %>%
    group_by(to) %>%
    summarise(
      total_strength_2023 = sum(total_strength, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_strength_2023))
  
  plot_data <- plot_data %>%
    left_join(host_order, by = "to") %>%
    mutate(
      to = factor(to, levels = host_order$to)
    )
  
  # Fixed scale for y-axis
  y_max <- centrality_cloud %>%
    summarise(max(strength_in)) %>%
    pull()
  
  ggplot(plot_data,
         aes(x = factor(year),
             y = total_strength,
             fill = from)) +
    geom_col(position = "stack") +
    facet_wrap(~ to, nrow=2, ncol = 5) +
    scale_y_continuous(limits = c(0, y_max)) +
    scale_fill_manual(
      values = c("CHN" = "firebrick", "USA" = "steelblue")) +
    scale_x_discrete(
      breaks = c("2017", "2020", "2023")) +
    labs(
      x = "",
      y = "Number of hosted cloud regions",
      fill = "") +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.border = element_rect(color = "black", linewidth = 0.3, fill = NA))
  
  ggsave(filename = paste0("cloud_in_strength_comparison.png"), 
         path = path_output_cloud, 
         width = 10, 
         height = 6)
}

# US Exports China vs. World

exports <- function(data, country_iso){
  
  export <- data %>%
    filter(from == country_iso)
  
  top_partners_20 <- export %>%
    group_by(to) %>%
    summarise(avg_strength = mean(weight, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_strength)) %>%
    head(20)
  
  top_partners_5 <- export %>%
    group_by(to) %>%
    summarise(avg_strength = mean(weight, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_strength)) %>%
    head(5)
  
  label_data <- export %>%
    filter(to %in% top_partners_5$to) %>%
    group_by(to) %>%
    filter(year == max(year)) %>% 
    ungroup()
  
  export_top5 <- export %>%
    filter(to %in% top_partners_5$to)
  
  export_top20 <- export %>%
    filter(to %in% top_partners_20$to)
  
  ggplot(export_top20 %>% filter (!to %in% export_top5$to),
        aes(x = year, y = weight, group = to)) +
    geom_line(linewidth = 0.5, color = "grey20", alpha = 0.2) +
    geom_point(size = 0.5, color = "grey20", alpha = 0.2) +
    geom_line(data = export_top5, 
              linewidth = 1, color = "black", 
              aes(linetype = to, group = to)) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = to),
      nudge_x = 0.3,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3
    ) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "",
      y = "Net Exports (Mio. USD)",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 0.5, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "none")
  
  ggsave(filename = paste0("exports_", country_iso, ".png"), 
         path = path_output_export_import, 
         width = 10, 
         height = 6)
}

imports <- function(data, country_iso){
  
  import <- data %>%
    filter(to == country_iso)
  
  top_partners_20 <- import %>%
    group_by(from) %>%
    summarise(avg_strength = mean(weight, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_strength)) %>%
    head(20)
  
  top_partners_5 <- import %>%
    group_by(from) %>%
    summarise(avg_strength = mean(weight, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_strength)) %>%
    head(5)

  label_data <- import %>%
    filter(from %in% top_partners_5$from) %>%
    group_by(from) %>%
    filter(year == max(year)) %>% 
    ungroup()
  
  import_top5 <- import %>%
    filter(from %in% top_partners_5$from)
  
  import_top20 <- import %>%
    filter(from %in% top_partners_20$from)
  
  ggplot(import_top20 %>% filter (!from %in% import_top5$from),
         aes(x = year, y = weight, group = from)) +
    geom_line(linewidth = 0.5, color = "grey20", alpha = 0.2) +
    geom_point(size = 0.5, color = "grey20", alpha = 0.2) +
    geom_line(data = import_top5, 
              linewidth = 1, color = "black", 
              aes(linetype = from, group = from)) +
    geom_point(data = import_top5, 
               size = 1, color = "black", alpha = 0.7) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = from),
      nudge_x = 0.3,
      direction = "y",
      hjust = 0,
      segment.color = "grey50",
      size = 3
    ) +
    scale_x_continuous(breaks = c(2017, 2020, 2023)) +
    theme_minimal(base_size = 12) +
    labs(
      x = "",
      y = "Net imports (Mio. USD)",
      color = NULL,
    ) + 
    theme_minimal() +
    theme(aspect.ratio = 0.5, 
          strip.text = element_text(face = "bold"), 
          panel.border = element_rect(color = "black", size = 0.5, fill = NA), 
          legend.position = "none")
  
  ggsave(filename = paste0("imports_", country_iso, ".png"), 
         path = path_output_export_import, 
         width = 10, 
         height = 6)
}

# US AI diffusion network

ai_diffusion_network <- function(data) {
  
  trade_network_overall <- data %>%
    filter(year == 2023)
  
  trade_network_USA <- data %>%
    filter(year == 2023, from == "USA" | to == "USA") %>%
    mutate(total_trade_year = sum(weight),
           actor = "USA") %>%
    filter(weight >= 0.005 * total_trade_year)
  
  trade_network_CHN <- data %>% 
    filter(year == 2023, from == "CHN" | to == "CHN") %>%
    mutate(total_trade_year = sum(weight), 
           actor = "CHN") %>%
    filter(weight >= 0.005 * total_trade_year)
  
  trade_plot <- bind_rows(trade_network_USA, trade_network_CHN)
  
  # Identify active nodes and flag in world map
  active_nodes <- unique(c(trade_plot$from, trade_plot$to))
  
  # Define tiers based on AI diffusion framework
  tier1 <- data.frame(
    iso3 = c("USA", "AUS", "CAN", "NZL", "GBR", "BEL", "DNK", "FIN", "FRA", "DEU", "IRL", "ITA", "NLD", "NOR", "ESP", "SWE", "TWN", "JPN", "KOR"),
    stringsAsFactors = FALSE
  )
  
  tier3 <- data.frame(
    iso3 = c("AFG", "CHN", "IRN", "PRK", "RUS", "SYR", "VEN", "MMN", "BLR", "KHM", "CAF", "COD", "CUB", "ERI", "HTI", "IRQ", "LBN", "LBY", "NIC", "SOM", "SSD", "SDN", "ZWE")
  )
  
  # Create world map object using the natural earth package
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Correct missing iso-codes for specific country cases
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    mutate(
      iso_a3 = case_when(
        admin == "France" ~ "FRA",
        admin == "Norway" ~ "NOR",
        admin == "Kosovo" ~ "XKX",
        admin %in% c("Taiwan", "Taiwan, Province of China") ~ "TWN",
        TRUE ~ iso_a3), 
      tier = case_when(iso_a3 %in% tier1$iso3 ~ "Tier 1",
                       iso_a3 %in% tier3$iso3 ~ "Tier 3",
                       TRUE ~ "Tier 2"), 
      active = ifelse (iso_a3 %in% active_nodes, TRUE, FALSE))
  
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
  graph <- trade_overall %>%
    as_tbl_graph(directed = TRUE, from = from, to = to) %>%
    activate(nodes) %>%
    mutate(strength_cent = centrality_degree(mode = "all", weight = weight))
  
  node_centrality <- graph %>%
    as_tibble() %>%
    select(name, strength_cent) %>%
    mutate(strength_norm = strength_cent / max(strength_cent, na.rm = TRUE))
    
  nodes <- nodes %>%
    left_join(node_centrality, by = "name")
  
  # Merging coordinates with nodes based on iso3 codes
  edges_coords <- trade_plot %>%
    left_join(nodes, by = c("from" = "name")) %>%
    rename(x_from = lon, y_from = lat) %>%
    left_join(nodes, by = c("to" = "name")) %>%
    rename(x_to = lon, y_to = lat) %>%
    mutate(actor = factor(actor, levels = c("USA", "CHN")))
  
  ggplot() +
    # load world map from rnaturalearth object
    geom_sf(data = world, aes(fill = tier), color = "grey60", linewidth = 0.2, alpha = 0.7) +
    scale_fill_manual(values = c("Tier 1" = "darkgreen", "Tier 2" = "gold2", "Tier 3" = "firebrick4"), 
                      name = "AI diffusion framework") +
    # project edges onto world map
    geom_curve(
      data = edges_coords,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linewidth = weight, color = actor),
      curvature = 0.2, alpha = 0.9,
      arrow = arrow(length = unit(2, "mm"), type = "closed")) +
    # weighted line width (trade value)
    scale_linewidth_continuous(
      range = c(0.2, 2),
      name  = "Trade value (million USD)") +
    scale_color_manual(
      values = c("USA" = "steelblue", "CHN" = "darkred"),
      name = "Trade Network") +
    # project nodes onto world map
    geom_point(
      data = nodes,
      aes(x = lon, y = lat, size = strength_norm)) +
    # strength centrality represented in node size
    scale_size_continuous(
      range = c(2, 4),
      name  = "Strength centrality") +
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
      size = guide_legend(
        title = "Strength centrality",
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2, 
        byrow = TRUE),
      linewidth = guide_legend(
        title = "Trade balance (million USD)",
        title.position = "top",
        title.hjust = 0.5, 
        nrow = 2,
        byrow = TRUE), 
      fill = guide_legend(
        title = "AI diffusion tier",
        title.position = "top",
        title.hjust = 0.5,
        nrow = 3,
        byrow = TRUE
      ),
      color = guide_legend(
        title = "Trade network",
        title.position = "top",
        title.hjust = 0.5,
        nrow = 2,
        byrow = TRUE
      ),)
  
  ggsave(filename = paste0("diffusion_frame_us_china.png"), 
         path = path_output_maps, 
         width = 12, 
         height = 8)
}

################################################################################