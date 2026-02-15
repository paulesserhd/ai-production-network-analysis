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
                                                    Partner != "WL", # exclude trade partner "world"
                                                    Partner != "ROW", # exlude trade partner "rest of world"
                                                    Flow == "X") # exclude mirrored export-import relationships
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
  mutate(reporter_country_iso2 = case_when(reporter_country_iso2 == "888" ~ "XK", # changing country code for Kosovo
                                           TRUE ~ reporter_country_iso2),
         partner_country_iso2 = case_when(partner_country_iso2 == "888" ~ "XK",
                                          TRUE ~ partner_country_iso2), 
         reporter_country_iso2 = case_when(reporter_country_iso2 == "PAL" ~ "PS", # changing country code for Palestine
                                           TRUE ~ reporter_country_iso2),
         partner_country_iso2 = case_when(partner_country_iso2 == "PAL" ~ "PS",
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
  group_by(year, reporter_country_iso2, partner_country_iso2) %>%
  summarise(
    exports = sum(trade_value, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    country1_iso2 = pmin(reporter_country_iso2, partner_country_iso2),
    country2_iso2 = pmax(reporter_country_iso2, partner_country_iso2),
    pair = paste(country1_iso2, country2_iso2, sep = "_"),
    signed_value = ifelse(
      reporter_country_iso2 == country1_iso2,
      exports,
      -exports)) %>%
  group_by(year, pair, country1_iso2, country2_iso2) %>%
  summarise(
    balance = sum(signed_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    surplus_country_iso2 = ifelse(balance > 0, country1_iso2, country2_iso2),
    deficit_country_iso2 = ifelse(balance > 0, country2_iso2, country1_iso2),
    trade_value = abs(balance)
  ) %>%
  filter(trade_value > 0)

## Step 5: Add iso3 codes
country_codes_iso3 <- read.csv(file.path(path_external, "country_codes_V202501.csv"))

country_codes_iso3 <- country_codes_iso3 %>%
  select(country_iso2, country_iso3) %>%
  distinct(country_iso2, country_iso3) %>%
  bind_rows(tibble(country_iso2 = "TW",
                   country_iso3 = "TWN"), # adding iso-translation for Taiwan
            tibble(country_iso2 = "XK", 
                   country_iso3 = "XKX"),
            tibble(country_iso2 = "PS", 
                   country_iso3 = "PSE")) # adding iso-translation for Palastine

data_service_bal <- data_service_bal %>% 
  left_join(country_codes_iso3,by = c("surplus_country_iso2" = "country_iso2")) %>%
  rename(surplus_country_iso3 = country_iso3) %>%
  left_join(country_codes_iso3,by = c("deficit_country_iso2" = "country_iso2")) %>%
  rename(deficit_country_iso3 = country_iso3)

#Step 6:Transform dataset for plotting
data_service_centrality <- data_service_bal %>%
  filter(trade_value > 0) %>%
  rename(from = surplus_country_iso3, 
         to = deficit_country_iso3, 
         weight = trade_value) %>%
  select(from, to, weight, year)

### Save processed data
write.csv(data_service_centrality, file.path(path_processed, "dataservice_trade_centrality_2017_2023.csv"), row.names = FALSE)
############################################################