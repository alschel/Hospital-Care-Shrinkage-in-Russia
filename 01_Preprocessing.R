# Hospital Care Shrinkage in Russia
# 01. Preprocessing: clean and aggregate by functional urban areas

library(tidyr)
library(dplyr)

# ================================
# 1. Rosstat data on hospital care
# ================================

# Read data on bed numbers, hospitals, places, and population
# Clean the data, keep only observations with full time serie, and convert to tidy format

# Койки круглосуточного стационара
beds <- read.csv2("data/raw/Beds.csv", stringsAsFactors = F)

beds %>% 
  select(FedDistrict:X2013) %>%
  filter(!is.na(X1991)) %>% 
  filter(!is.na(X2013)) %>% 
  gather(Year, Beds, starts_with("X")) %>% 
  mutate(Year = stringr::str_replace_all(Year, "X", "") %>% 
           as.integer()) %>%
  mutate(Region = stringr::str_replace_all(Region, "Респ. ", ""),
         Region = stringr::str_replace_all(Region, " край", ""),
         Region = stringr::str_replace_all(Region, " $", ""),
         City = stringr::str_replace_all(City, " $", "")) %>%
  filter(!is.na(Beds)) -> beds

# Больницы
# hospitals <- read.csv2("data/raw/Hospitals.csv", stringsAsFactors = F)
# hospitals %>%
#   select(FedDistrict:X2013) %>% 
#   gather(Year, Hospitals, starts_with("X")) %>% 
#   mutate(Year = stringr::str_replace_all(Year, "X", "") %>% 
#            as.integer()) %>%
#   mutate(Region = stringr::str_replace_all(Region, "Респ. ", ""),
#          Region = stringr::str_replace_all(Region, " край", ""),
#          Region = stringr::str_replace_all(Region, " $", ""),
#          City = stringr::str_replace_all(City, " $", "")) %>%
#   filter(!is.na(Hospitals)) -> hospitals

# Места денвного стационара
places <- readr::read_csv("data/raw/Places.csv", 
                          col_types = "ccciiiii") # c = character, i = integer
places %>%
  select(FedDistrict:`2013`) %>%
  gather(Year, Places, `2009`:`2013`) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(Region = stringr::str_replace_all(Region, "Респ. ", ""),
         Region = stringr::str_replace_all(Region, " край", ""),
         Region = stringr::str_replace_all(Region, " $", ""),
         City = stringr::str_replace_all(City, " $", "")) %>%
  filter(!is.na(Places)) -> places

# Численность населения
population <- read.csv2("data/raw/Population.csv", stringsAsFactors = F)
population %>%
  select(FedDistrict:X2013) %>% 
  gather(Year, Population, starts_with("X")) %>% 
  mutate(Year = stringr::str_replace_all(Year, "X", "") %>% 
           as.integer()) %>%
  mutate(Region = stringr::str_replace_all(Region, "Респ. ", ""),
         Region = stringr::str_replace_all(Region, " край", ""),
         Region = stringr::str_replace_all(Region, " $", ""),
         City = stringr::str_replace_all(City, " $", ""),
         Population = case_when(Population == 0 ~ NA_integer_,
                                Population != 0 ~ Population)) %>%
  filter(!is.na(Population)) -> population

# Combine data
left_join(population,
          beds, 
          by = c("FedDistrict", "Region", "City", "Year")) %>% 
  left_join(.,
            places, 
            by = c("FedDistrict", "Region", "City", "Year")) -> data

# Clean data
# data[data$City == "Ефремов" & data$Year == 2009, ]$Hospitals <- 5
data %>% 
  mutate(City = stringr::str_replace_all(City, " \\((.*?)\\)", "")) -> data

# Calculate beds + places
data %>% 
  mutate(BedsAndPlaces = Beds + Places) -> data

# =======================
# 2. Urban Agglomerations
# =======================
fua <- read.csv2("data/processed/DARIUSdatabaseCottineau2014EN.csv", dec = ".",  stringsAsFactors = F) %>% 
  select(ROKATO:X2010)

fua %>% 
  filter(country == "Russia") %>%
  gather(Year, Population, X1840:X2010) %>% 
  mutate(Year = stringr::str_replace_all(Year, "X", "") %>% 
           as.integer()) %>%
  mutate(City = stringr::str_replace_all(название, "ë", "е"),
         City = stringr::str_replace_all(City, " $", "")) %>% 
  select(-название) %>%
  mutate(Region_ru = stringr::str_replace_all(Region_ru, " $", "")) %>% 
  filter(Year == 2002) -> fua

# Filter the regions we need
fua %>% 
  filter(Region_ru %in% unique(data$Region)) -> fua

fua %>% 
  select(ROKATO, Region_ru, region, City, long, lat, AROKATO) -> fua

# ========================================================================================
# 3. Merge datasets, aggregate data by agglomerations and calculate variables of interests
# ========================================================================================

# 3.1. Merge datasets
data %>% 
  left_join(fua, by = c("Region" = "Region_ru", "City" = "City")) -> res_data

# Remove observations with no AROKATO
res_data %>% 
  filter(!is.na(AROKATO)) -> res_data

# Для агломераций, которые состоят из нескольких муниципалитетов, 
# выделим ядро (local unit with maximum population) и периферию
res_data %>% 
  group_by(AROKATO, Year) %>%
  mutate(Core_periphery_status = case_when(length(Population) == 1 ~ NA_character_,
                                           Population == max(Population) ~ "Core",
                                           Population != max(Population) ~ "Periphery") %>% 
           factor(levels = c("Core", "Periphery"))) %>%
  ungroup() -> res_data

# Итого, Всего у нас 710 единиц в 46 регионах, объединенные в 634 AROKATO, 
# из которых для 38 мы имеем отдельные данные для ядра и периферии
# Самая крупная AROKATO -  Московская (28 единиц)


# 3.2. Aggregate by FUA 
res_data %>% 
  group_by(AROKATO, Year) %>%
  arrange(desc(Population)) %>%   # arrange by population in descending order
  summarise(Core_city = City[1],  # pick the largest local unit
            long = long[1],
            lat = lat[1],
            Region = Region[1],
            region = region[1],
            Population = sum(Population), 
            Beds = sum(Beds),
            Places = sum(Places),
            BedsAndPlaces = sum(BedsAndPlaces)
            ) %>%
  ungroup() -> res_data_fua

# Remove fua of population less than 10000 people in 2013
res_data_fua %>%
  group_by(AROKATO) %>%
  arrange(desc(Year)) %>% 
  filter(Population[1] > 10000) -> 
  res_data_fua

# Calculate bed and bed+places density (per 10000 people)
res_data_fua %>% 
  mutate(BedDensity = Beds / Population * 10000,
         BedsAndPlacesDensity = BedsAndPlaces / Population *10000) -> res_data_fua

# Label fua according to the average size and annual population rate changes
res_data_fua %>%
  group_by(AROKATO) %>% 
  mutate(Mean_pop_91_13 = mean(Population, na.rm = T),
         Size_category_91_13 = case_when(Mean_pop_91_13 < 20000 ~ "<20",
                                         Mean_pop_91_13 < 50000 ~ "20-50",
                                         Mean_pop_91_13 < 200000 ~ "50-200",
                                         Mean_pop_91_13 >= 200000 ~ ">200") %>% 
           factor(levels = c("<20", "20-50", "50-200", ">200")),
         Mean_pop_09_13 = mean(Population[Year >= 2009], na.rm = T),
         Size_category_09_13 = case_when(Mean_pop_09_13 < 20000 ~ "<20",
                                         Mean_pop_09_13 < 50000 ~ "20-50",
                                         Mean_pop_09_13 < 200000 ~ "50-200",
                                         Mean_pop_09_13 > 200000 ~ ">200") %>%
         factor(levels = c("<20", "20-50", "50-200", ">200")),
         An_pop_ch = (Population - lag(Population, order_by = Year))/lag(Population, order_by = Year)*100,
         Mean_an_pop_ch_91_13 = mean(An_pop_ch, na.rm = T),
         Mean_an_pop_ch_09_13 = mean(An_pop_ch[Year >= 2009], na.rm = T),
         An_pop_ch_91_13_cat = case_when(Mean_an_pop_ch_91_13 < -0.1 ~ "Shrinking",
                                         Mean_an_pop_ch_91_13 <= 0.1 ~ "Stable",
                                         Mean_an_pop_ch_91_13 > 0.1 ~ "Growing") %>% 
           factor(levels = c("Shrinking", "Stable", "Growing")),
         An_pop_ch_09_13_cat = case_when(Mean_an_pop_ch_09_13 < -0.1 ~ "Shrinking",
                                         Mean_an_pop_ch_09_13 <= 0.1 ~ "Stable",
                                         Mean_an_pop_ch_09_13 > 0.1 ~ "Growing") %>%
         factor(levels = c("Shrinking", "Stable", "Growing"))
         ) -> res_data_fua

# Divide observations according to the bed number change rates
res_data_fua %>%
  group_by(AROKATO) %>%
  arrange(Year) %>%
  mutate(Beds_ch = (Beds[23] - Beds[1])/Beds[1]*100,
         BedDensity_ch = (BedDensity[23] - BedDensity[1])/BedDensity[1]*100) %>% 
  ungroup() %>%
  mutate(Beds_ch_cat = case_when(Beds_ch <= -50 ~ "Strong Decrease",
                                    Beds_ch <= -1 ~ "Decrease",
                                    Beds_ch > -1 ~ "Stable or Growing") %>%
                    factor(levels = c("Strong Decrease", "Decrease", "Stable or Growing")),
         BedDensity_ch_cat = case_when(BedDensity <= -50 ~ "Strong Decrease",
                                         BedDensity <= -1 ~ "Decrease",
                                         BedDensity > -1 ~ "Stable or Growing") %>%
           factor(levels = c("Strong Decrease", "Decrease", "Stable or Growing"))) -> res_data_fua

# Save to Rdata file
save(res_data, res_data_fua, file = "data/res_data.Rdata")
