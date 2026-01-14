################################################################################
# 11_network_visualisation_functions.R
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
        strength = centrality_degree(mode = "all", weights = weight),
        betweenness = centrality_betweenness(),
      ) %>%
      as_tibble() %>%
      mutate(year = yr)
    
    centrality_list[[as.character(yr)]] <- node_centrality
  }
  
  centrality_results <- bind_rows(centrality_list)
  return(centrality_results)
}

##############################################################################