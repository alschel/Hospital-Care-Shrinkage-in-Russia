# Preprocessing

library(tidyr)
library(dplyr)

# ================
# 1. Preprocessing
# ================

# 1.1. Read data on bed numbers, hospitals, and population
beds <- read.csv2("data/Beds.csv", stringsAsFactors = F)
# Clean the data, keep only observations with full time serie, and convert to tidy format
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

# hospitals <- read.csv2("data/Hospitals.csv", stringsAsFactors = F)
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

population <- read.csv2("data/Population.csv", stringsAsFactors = F)
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
left_join(beds, 
          # hospitals, 
          population,
          by = c("FedDistrict", "Region", "City", "Year")) -> data

# Clean data
# data[data$City == "Ефремов" & data$Year == 2009, ]$Hospitals <- 5
data %>% 
  mutate(City = stringr::str_replace_all(City, " \\((.*?)\\)", "")) -> data

# 1.2. Read data on FUA
fua <- read.csv2("data/DARIUSdatabaseCottineau2014EN.csv", dec = ".",  stringsAsFactors = F) %>% 
  select(ROKATO:X2010)

fua %>% 
  filter(country == "Russia") %>%  # filter Russia
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

# 1.3. Создаем итоговый датасет
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
           factor(levels = c("Core", "Periphery"))) %>% # 34 ядра и 71 периферии
  ungroup() -> res_data

# Всего у нас 574 единицы в 46 регионах, объединенные в 494 AROKATO, 
# из которых для 34 мы имеем отдельные данные для ядра и периферии
# Самые крупные это Московская (27 единиц) и Нижегородская (12 единиц) агломерации

# =========================
# 2. Data aggregated by FUA
# =========================

# Aggregate by FUA and label fua according the average size and annual population rate changes
res_data %>% 
  group_by(AROKATO, Year) %>%
  arrange(desc(Population)) %>%   # arrange by population in descending order
  summarise(Core_city = City[1],  # pick the largest local unit
            Region = Region[1],
            Population = sum(Population, na.rm = T), 
            Beds = sum(Beds, na.rm = T)
            # Hospitals = sum(Hospitals, na.rm = T)
            ) %>%
  ungroup() %>%
  mutate(Population = case_when(Population == 0 ~ NA_integer_,
                                Population != 0 ~ Population)) %>%
  group_by(AROKATO) %>% 
  mutate(Mean_pop_91_13 = mean(Population, na.rm = T),
         Size_category_91_13 = case_when(Mean_pop_91_13 < 20000 ~ "<20",
                                         Mean_pop_91_13 < 50000 ~ "20-50",
                                         Mean_pop_91_13 < 200000 ~ "50-200",
                                         Mean_pop_91_13 >= 200000 ~ ">200") %>% 
           factor(levels = c("<20", "20-50", "50-200", ">200")),
         # Mean_pop_97_13 = mean(Population[Year > 1996], na.rm = T),
         # Size_category_97_13 = case_when(Mean_pop_97_13 < 20000 ~ "<20",
         #                                 Mean_pop_97_13 < 50000 ~ "20-50",
         #                                 Mean_pop_97_13 < 200000 ~ "50-200",
         #                                 Mean_pop_97_13 > 200000 ~ ">200") %>% 
           # factor(levels = c("<20", "20-50", "50-200", ">200")),
         An_pop_ch = (Population - lag(Population, order_by = Year))/lag(Population, order_by = Year)*100,
         Mean_an_pop_ch_91_13 = mean(An_pop_ch, na.rm = T),
         # Mean_an_pop_ch_97_13 = mean(An_pop_ch[Year > 1996], na.rm = T),
         An_pop_ch_91_13_cat = case_when(Mean_an_pop_ch_91_13 < -0.1 ~ "Shrinking",
                                         Mean_an_pop_ch_91_13 <= 0.1 ~ "Stable",
                                         Mean_an_pop_ch_91_13 > 0.1 ~ "Growing") %>%
           factor(levels = c("Shrinking", "Stable", "Growing"))
         # An_pop_ch_97_13_cat = case_when(Mean_an_pop_ch_97_13 < -0.1 ~ "Shrinking",
         #                                 Mean_an_pop_ch_97_13 <= 0.1 ~ "Stable",
         #                                 Mean_an_pop_ch_97_13 > 0.1 ~ "Growing") %>% 
         # factor(levels = c("Shrinking", "Stable", "Growing"))
         ) -> res_data_fua


# Calculate beds per 10000 people number and divide observations according to the bed number change rates
res_data_fua %>%
  mutate(Beds_per10th = Beds/Population*10000) %>% 
  group_by(AROKATO) %>%
  arrange(Year) %>% 
  mutate(Beds_ch = (Beds[Year == 2013] - Beds[Year == 1991])/Beds[Year == 1991]*100,
         Beds_per10th_ch = (Beds_per10th[Year == 2013] - Beds_per10th[Year == 1991])/Beds_per10th[Year == 1991]*100) %>% 
  ungroup() %>%
  mutate(Beds_ch_cat = case_when(Beds_ch <= -50 ~ "Strong Decrease",
                                    Beds_ch <= -1 ~ "Decrease",
                                    Beds_ch > -1 ~ "Stable or Growing") %>%
                    factor(levels = c("Strong Decrease", "Decrease", "Stable or Growing")),
         Beds_per10th_ch_cat = case_when(Beds_per10th_ch <= -50 ~ "Strong Decrease",
                                         Beds_per10th_ch <= -1 ~ "Decrease",
                                         Beds_per10th_ch > -1 ~ "Stable or Growing") %>%
           factor(levels = c("Strong Decrease", "Decrease", "Stable or Growing"))) -> res_data_fua

# Save to Rdata file
save(res_data, res_data_fua, file = "data/res_data.Rdata")
