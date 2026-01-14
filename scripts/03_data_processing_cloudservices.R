################################################################################
# 03_data_processing_cloudservices.R
# Preprocess and transform data service dataset for network analysis & visualisation
################################################################################

### Clean environment
rm(list = ls())

### Load environment
source("scripts/00_setup.R")

### Load raw data
cloud_service_raw <- read.csv2(file.path(path_raw_cloud_service, "cloud_provider_availability_regions.csv"))

### Clean and process data

#Step 1: Remove NAs
cloud_service_filtered <- cloud_service_raw %>%
  filter(!is.na(launch_year))

# Step 2: Calculate number of new regions per country-pair per year

# Collapse to pair and year level 
cloud_service_processed <- cloud_service_filtered %>%
  mutate(pair = paste(provider_country_iso3, host_country_iso3, sep = "_")) %>%
  group_by(pair, launch_year) %>%
  reframe(num_regions = n(), .groups = "drop")

# Expand pairs to years with no newly launched cloud regions (num_regions = 0)
years <- seq(min(cloud_service_processed$launch_year), max(cloud_service_processed$launch_year))

cloud_service_edges <- cloud_service_processed %>%
  select(pair, launch_year, num_regions)

cloud_service_expanded <- cloud_service_edges %>%
  tidyr::complete(
    pair, 
    launch_year = years,
    fill = list(num_regions = 0)
  )

# Accumulate cloud regions and exclude years before 2017
cloud_service_acc <- cloud_service_expanded %>%
  arrange(pair, launch_year) %>%
  group_by(pair) %>%
  mutate(cumulative_regions = cumsum(num_regions)) %>%
  ungroup() %>%
  filter(launch_year >= 2017)

# Reattach metadata
pair_lookup <- cloud_service_filtered %>%
  mutate(pair = paste(provider_country_iso3, host_country_iso3, sep = "_")) %>%
  select(pair, 
         provider_country_iso3, 
         host_country_iso3, 
         provider_country_name, 
         host_country_name) %>%
  distinct()

cloud_service_final <- cloud_service_acc %>%
  left_join(pair_lookup, by = "pair") %>%
  rename(year = launch_year) %>%
  filter(cumulative_regions > 0, 
         provider_country_iso3 != host_country_iso3)

# Step 3: Transform dataset for visualisation
cloud_service_plot <- cloud_service_final %>%
  rename (from = provider_country_iso3, 
          to = host_country_iso3, 
          weight = cumulative_regions) %>%
  select(from, to, weight, year)

cloud_service_centrality <- cloud_service_final %>%
  rename (from = provider_country_iso3, 
          to = host_country_iso3, 
          weight = cumulative_regions)

### Save processed data
write.csv(cloud_service_plot, file.path(path_processed, "cloud_service_regions_plot_2017_2023.csv"))
write.csv(cloud_service_centrality, file.path(path_processed, "cloud_service_regions_centrality_2017_2023.csv"))

################################################################################
          