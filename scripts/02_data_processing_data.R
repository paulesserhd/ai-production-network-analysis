################################################################################
# 02_data_processing_data.R
# Preprocess and transform data service dataset for network analysis and visualisation
################################################################################

### Clean environment
rm(list = ls())

### Load environment
source("scripts/00_setup.R")

### Load raw data
data_service_raw <- read.csv(file.path(path_raw_data, "OECD-WTO_BATIS_BPM6_December2025_bulk.csv"))

### Clean and process data

## Step 1: Filter for timeframe, years, service type & exclude NAs
data_service_prefiltered <- data_service_raw %>% filter(Year == "2017" |
                                                       Year == "2018" |
                                                      Year == "2019" |
                                                      Year == "2020" |
                                                      Year == "2021" |
                                                      Year == "2022" |
                                                      Year == "2023",
                                                    type_Reporter == "c", # select only country-reported trade flows 
                                                    !is.na(Reported_value), # exclude missing values
                                                    Reported_value > 0, # select only trade flows > 0
                                                    Item_code == "SI3", # select information services
                                                    Partner != "WL") # exclude trade partner "world"

## Step 2: Rename and filter rows
data_service_filtered <- data_service_prefiltered %>%
  rename(reporter_country_iso2 = Reporter,
         partner_country_iso2 = Partner,
         trade_value = Reported_value,
         year = Year, 
         flow = Flow) %>%
  select(reporter_country_iso2, 
         partner_country_iso2, 
         trade_value, 
         year, 
         flow) 

## Step 3: Add country names
country_codes_data <- read.csv2(file.path(path_external, "country_codes_data.csv"))

data_service_named <- data_service_filtered %>%
  left_join(country_codes_data, by = c("reporter_country_iso2" = "country_code")) %>%
  rename(reporter_country_name = country_name) %>%
  left_join(country_codes_data, by = c("partner_country_iso2" = "country_code")) %>%
  rename(partner_country_name = country_name) %>%
  mutate(reporter_country_iso2 = case_when(reporter_country_iso2 == "888" ~ "XK",
                                           TRUE ~ reporter_country_iso2),
         partner_country_iso2 = case_when(partner_country_iso2 == "888" ~ "XK",
                                          TRUE ~ partner_country_iso2)) %>%
  select(reporter_country_iso2,
         reporter_country_name,
         partner_country_iso2,
         partner_country_name,
         trade_value,
         year, 
         flow)

## Step 4: Calculate trade balance per country pair and year
data_service_bal <- data_service_named %>%
  mutate(country1_iso2 = pmin(reporter_country_iso2, partner_country_iso2),
         country2_iso2 = pmax(reporter_country_iso2, partner_country_iso2),
         country1_name = ifelse(reporter_country_iso2 == country1_iso2, reporter_country_name, partner_country_name),
         country2_name = ifelse(partner_country_iso2 == country2_iso2, partner_country_name, reporter_country_name), 
         pair = paste(country1_iso2, country2_iso2, sep = "_"), 
         signed_value = case_when(
           reporter_country_iso2 == country1_iso2 & flow == "X" ~ trade_value,
           reporter_country_iso2 == country1_iso2 & flow == "M" ~ -trade_value, 
           partner_country_iso2 == country2_iso2 & flow == "X" ~ -trade_value,
           partner_country_iso2 == country2_iso2 & flow == "M" ~ trade_value
         )) %>% 
  group_by(year, pair, country1_iso2, country1_name, country2_iso2, country2_name) %>%
  reframe(balance = sum(signed_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    surplus_country_iso2 = ifelse(balance > 0, country1_iso2, country2_iso2),
    surplus_country_name= ifelse(balance > 0, country1_name, country2_name),
    deficit_country_iso2 = ifelse(balance > 0, country2_iso2, country1_iso2),
    deficit_country_name = ifelse(balance > 0, country2_name, country1_name),
    trade_value = abs(balance)) %>%
  filter(trade_value > 0) %>%
  select(surplus_country_iso2, 
         surplus_country_name, 
         deficit_country_iso2, 
         deficit_country_name, 
         trade_value, 
         year)

## Step 5: Add iso3 codes
country_codes_iso3 <- read.csv(file.path(path_external, "country_codes_V202501.csv")) %>%
  select(country_iso2, country_iso3)

data_service_bal <- data_service_bal %>% 
  left_join(country_codes_iso3, by = c("surplus_country_iso2" = "country_iso2")) %>%
  rename(surplus_country_iso3 = country_iso3) %>%
  left_join(country_codes_iso3, by = c("deficit_country_iso2" = "country_iso2")) %>%
  rename(deficit_country_iso3 = country_iso3)

## Step 5: Exclude trade relationships below 0,5% treshold of total trade per year 
data_service_final <- data_service_bal %>%
  group_by(year) %>%
  mutate(total_trade_year = sum(trade_value)) %>%
  ungroup() %>%
  filter(trade_value >= 0.005 * total_trade_year)

#Step 6:Transform dataset for plotting
data_service_plot <- data_service_final %>%
  rename(from = surplus_country_iso3,
         to = deficit_country_iso3,
         weight = trade_value) %>%
  select(from, to, weight, year)

data_service_centrality <- data_service_bal %>%
  rename(from = surplus_country_iso3, 
         to = deficit_country_iso3, 
         weight = trade_value) %>%
  select(from, to, weight, year)

### Save processed data
write.csv(data_service_plot, file.path(path_processed, "dataservice_trade_plot_2017_2023.csv"), row.names = FALSE)
write.csv(data_service_centrality, file.path(path_processed, "dataservice_trade_centrality_2017_2023.csv"), row.names = FALSE)
############################################################