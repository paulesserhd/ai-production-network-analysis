################################################################################
# 12_network_analysis_functions.R
# Network analysis function: centrality measures
################################################################################

centrality_measures <- function(data) { 
  
  years <- 2017:2023

  centrality_list <- list()
  
  for (yr in years) {
    
    data_year <- data %>%
      filter(year == yr, 
             from != to)
    
    graph_year <- data_year %>%
      as_tbl_graph(directed = TRUE, 
                   from = from, 
                   to = to, 
                   weight = weight)
    
    node_centrality <- graph_year %>%
      activate(nodes) %>%
      mutate(
        degree = centrality_degree(mode = "all"),
        degree_norm = degree / max(degree, na.rm = TRUE),
        degree_in = centrality_degree(mode = "in"),
        degree_in_norm = degree_in / max(degree_in, na.rm = TRUE),
        degree_out = centrality_degree(mode = "out"),
        degree_out_norm = degree_out / max(degree_out, na.rm = TRUE),
        strength = centrality_degree(mode = "all", weights = weight),
        strength_norm = strength / max(strength, na.rm = TRUE),
        strength_in = centrality_degree(mode = "in", weights = weight),
        strength_in_norm = strength_in / max(strength_in),
        strength_out = centrality_degree(mode = "out", weights = weight),
        strength_out_norm = strength_out / max(strength_out, na.rm = TRUE), 
        betweenness = centrality_betweenness(),
        betweenness_norm = betweenness / max(betweenness, na.rm = TRUE),
        year = yr,
      ) %>%
      as_tibble()
    
    centrality_list[[as.character(yr)]] <- node_centrality
  }
  
  centrality_results <- bind_rows(centrality_list)
  return(centrality_results)
}

##############################################################################