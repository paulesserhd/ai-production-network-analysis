################################################################################
# 01_data_processing_hardware.R
# Preprocess and transform compute hardware dataset for network analysis & visualistaion
################################################################################

rm(list = ls())

### Load environment
source("scripts/00_setup.R")

### Load raw data
hardware_trade_raw_2017 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2017_V202501.csv"))
hardware_trade_raw_2018 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2018_V202501.csv"))
hardware_trade_raw_2019 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2019_V202501.csv"))
hardware_trade_raw_2020 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2020_V202501.csv"))
hardware_trade_raw_2021 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2021_V202501.csv"))
hardware_trade_raw_2022 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2022_V202501.csv"))
hardware_trade_raw_2023 <- read.csv(file.path(path_raw_hardware, "BACI_HS17_Y2023_V202501.csv"))

### Combine datasets
hardware_trade_raw <- bind_rows(hardware_trade_raw_2017,
                                hardware_trade_raw_2018,
                                hardware_trade_raw_2019,
                                hardware_trade_raw_2020,
                                hardware_trade_raw_2021,
                                hardware_trade_raw_2022,
                                hardware_trade_raw_2023)

### Clean and process data

## Step 1: Rename rows
hardware_trade_raw <- hardware_trade_raw %>% rename(exporter_id = i, 
                                          importer_id = j, 
                                          product_id = k, 
                                          trade_value = v, 
                                          year = t) 

## Step 2: Filter for electric processing units based on HS code 854231
hardware_trade_filtered <- hardware_trade_raw %>%
  filter(product_id == "854231")

## Step 3: Add country names based on country codes
country_codes_hardware <- read.csv(file.path(path_external, "country_codes_V202501.csv"))

# process variable names and fix specific country names
country_codes_hardware <- country_codes_hardware %>% 
  mutate(country_name = 
           case_when(
             country_code == "344" ~ "Hong Kong", 
             country_code == "446" ~ "Macao", 
             country_code == "792" ~ "Turkey", 
             TRUE ~ country_name), 
         country_code = as.integer(country_code))

# merge country names into trade data
hardware_trade_named <- hardware_trade_filtered %>%
  left_join(country_codes_hardware, by = c("importer_id" = "country_code")) %>%
  rename(importer_name = country_name,
         importer_iso2 = country_iso2,
         importer_iso3 = country_iso3) %>%
  left_join(country_codes_hardware, by = c("exporter_id" = "country_code")) %>%
  rename(exporter_name = country_name,
         expoter_iso2 = country_iso2,
         exporter_iso3 = country_iso3) %>%
  select(year, 
         importer_id, 
         importer_name,
         importer_iso3, 
         exporter_id, 
         exporter_name, 
         exporter_iso3, 
         trade_value) %>%
  # remove special region codes (other Asia, Europe EFTA)
  filter(!(importer_iso3 == "S19"), 
         !(exporter_iso3 == "S19"), 
         !(importer_iso3 == "R20"), 
         !(exporter_iso3 == "R20"))

## Step 4: Calculate trade balance per country pair and year   
hardware_trade_bal <- hardware_trade_named %>%
  mutate(country1_iso3 = pmin(exporter_iso3, importer_iso3),
         country2_iso3 = pmax(exporter_iso3, importer_iso3),
         country1_name = ifelse(exporter_iso3 < importer_iso3, exporter_name, importer_name),
         country2_name = ifelse(exporter_iso3 < importer_iso3, importer_name, exporter_name),
         pair = paste(country1_iso3, country2_iso3, sep = "_")) %>%
  group_by(pair, year, country1_iso3, country1_name, country2_iso3, country2_name) %>%
  reframe(
    signed_trade = ifelse(exporter_iso3 < importer_iso3, trade_value, -trade_value),
    balance = sum(signed_trade, na.rm = TRUE)
  ) %>%
    ungroup () %>%
  mutate(
    surplus_country_iso3 = ifelse(balance > 0, country1_iso3, country2_iso3),
    surplus_country_name  = ifelse(balance > 0, country1_name, country2_name),
    deficit_country_iso3  = ifelse(balance > 0, country2_iso3, country1_iso3),
    deficit_country_name  = ifelse(balance > 0, country2_name, country1_name),
    trade_value = abs(balance)) %>%
  select(surplus_country_iso3, 
         surplus_country_name, 
         deficit_country_iso3, 
         deficit_country_name, 
         trade_value, 
         year) %>%
  mutate(trade_value = trade_value / 1000) # adjust to million USD

## Step 5: Exclude trade relationships below 0,5% treshold of total trade per year for plot dataset
hardware_trade_final <- hardware_trade_bal %>%
  group_by(year) %>%
  mutate(total_trade_year = sum(trade_value)) %>%
  ungroup() %>%
  filter(trade_value >= 0.005 * total_trade_year)

# Step 6: Transform dataset for network plot & centrality measures 
hardware_trade_plot <- hardware_trade_final %>%
  rename(from = surplus_country_iso3,
         to = deficit_country_iso3,
         weight = trade_value) %>%
  select(from, to, weight, year)

hardware_trade_centrality <- hardware_trade_bal %>%
  rename(from = surplus_country_iso3,
         to = deficit_country_iso3,
         weight = trade_value) %>%
  select(from, to, weight, year)

### Save processed data
write.csv(hardware_trade_plot, file.path(path_processed, "hardware_trade_plot_2017_2023.csv"), row.names = FALSE)
write.csv(hardware_trade_centrality, file.path(path_processed, "hardware_trade_centrality_2017_2023.csv"), row.names = FALSE)

################################################################################