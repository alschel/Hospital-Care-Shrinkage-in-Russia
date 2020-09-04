# Exploratory Analysis: Tver and Leningrad

library(tidyr)
library(dplyr)
library(ggplot2)
library(colorblindr)

# ================
# 1. Preprocessing
# ================

# Beds
beds <- read.csv2("data/Beds.csv", stringsAsFactors = F) %>% 
  filter(Region %in% c("Тверская", "Ленинградская"))
beds %>%
  gather(Year, Beds, starts_with("X")) %>% 
  mutate(Year = stringr::str_replace_all(Year, "X", "") %>% 
           as.integer()) %>%
  mutate(Region = stringr::str_replace_all(Region, " $", ""),
         City = stringr::str_replace_all(City, " $", "")) %>%
  filter(!is.na(Beds)) -> beds

# Population
population <- read.csv2("data/Population.csv", stringsAsFactors = F) %>% 
  filter(Region %in% c("Тверская", "Ленинградская"))
population %>%
  gather(Year, Population, starts_with("X")) %>% 
  mutate(Year = stringr::str_replace_all(Year, "X", "") %>% 
           as.integer()) %>%
  mutate(Region = stringr::str_replace_all(Region, " $", ""),
         City = stringr::str_replace_all(City, " $", ""),
         Population = case_when(Population == 0 ~ NA_integer_,
                                Population != 0 ~ Population)) %>%
  filter(!is.na(Population)) -> population

# Combine datasets

# По Ленинградской области данные по кроватям данные доступны только с 1997 года,
# по Тверской - по неокторым городам с 1993 года. 
# Создадим столбец, который отделит столицу от других городов

# Тверская область
population %>% filter(Region == "Тверская", Year >= 1993) %>%
  left_join(beds %>% filter(Region == "Тверская", Year >= 1993), by = c("FedDistrict", "Region", "City", "Year")) %>% 
  mutate(Center = case_when(City == "Тверь" ~ "Tver",
                            T ~ "Other cities") %>% 
           factor(levels = c("Tver", "Other cities"))) -> 
  data_tver

# Ленинградская область
population %>% filter(Region == "Ленинградская", Year >= 1997) %>%
  left_join(beds %>% filter(Region == "Ленинградская", Year >= 1997), by = c("FedDistrict", "Region", "City", "Year")) %>% 
  mutate(Center = case_when(City == "Санкт-Петербург" ~ "St Petersburg",
                            T ~ "Other cities") %>% 
           factor(levels = c('St Petersburg', 'Other cities'))) -> 
  data_leningrad


# ================
# 2. Visualuzation
# ================

# Тверь: общие данные

year_labels <- 1993:2013
year_labels[seq(2,20,2)] <- ''

tver_general_plot <- data_tver %>% 
  group_by(Year) %>% 
  summarise(Population = sum(Population, na.rm = T),
            Beds = sum(Beds, na.rm = T),
            Beds_per10th = Beds/Population*10000) %>%
  ungroup() %>% 
  mutate(Population = Population/Population[Year == 1993]*100,
         Beds = Beds/Beds[Year == 1993]*100,
         Beds_per10th = Beds_per10th/Beds_per10th[Year == 1993]*100) %>% 
  gather(Var, Val, Population:Beds_per10th) %>% 
  ggplot(aes(x = Year, y = Val, col = Var))+
  geom_line(lwd = 1, aes(linetype = case_when(Var == "Beds_per10th" ~ "dotted",
                                              T ~ "solid") %>% factor(levels = c("solid", "dotted"))))+
  scale_color_OkabeIto(name = element_blank(), 
                       labels = c("Beds", "Beds per 10 000 people", "Population"))+
  scale_y_continuous(name = "%",
                     limits = c(55, 100), breaks = seq(55, 100, 5))+
  scale_x_continuous(name = element_blank(), limits = c(1993, 2013),
                     breaks = 1993:2013, labels = year_labels)+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.2, 0.25))+
  guides(linetype = F, color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid"))))

ggsave(plot = tver_general_plot, filename = "tver_general_plot.jpeg",
       device = "jpeg", path = "plots/Tver and Leningrad/",
       dpi = 300, width = 16, height = 9, units = "cm")

# Тверь: столица и другие города

tver_detailed_plot <- 
  data_tver %>% 
  group_by(Year, Center) %>% 
  summarise(Population = sum(Population, na.rm = T),
            Beds = sum(Beds, na.rm = T),
            Beds_per10th = Beds/Population*10000) %>%
  ungroup() %>% 
  mutate(Population = Population/Population[Year == 1993]*100,
         Beds = Beds/Beds[Year == 1993]*100,
         Beds_per10th = Beds_per10th/Beds_per10th[Year == 1993]*100) %>% 
  gather(Var, Val, Population:Beds_per10th) %>% 
  ggplot(aes(x = Year, y = Val, col = Var))+
  geom_line(lwd = 1, aes(linetype = case_when(Var == "Beds_per10th" ~ "dotted",
                                              T ~ "solid") %>% factor(levels = c("solid", "dotted"))))+
  scale_color_OkabeIto(name = element_blank(), 
                       labels = c("Beds", "Beds per 10 000 people", "Population"))+
  scale_y_continuous(name = "%",
                     limits = c(40, 105), breaks = seq(40, 105, 5))+
  scale_x_continuous(name = element_blank(), limits = c(1993, 2013),
                     breaks = 1993:2013, labels = year_labels)+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.13, 0.25))+
  guides(linetype = F, color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid")))) + 
  facet_wrap(.~Center)

ggsave(plot = tver_detailed_plot, filename = "tver_detailed_plot.jpeg",
       device = "jpeg", path = "plots/Tver and Leningrad/",
       dpi = 300, width = 23, height = 9, units = "cm")


# Ленинград: общие данные

year_labels_l <- 1997:2013
year_labels_l[seq(2,16,2)] <- ''

leningrad_general_plot <- data_leningrad %>% 
  group_by(Year) %>% 
  summarise(Population = sum(Population, na.rm = T),
            Beds = sum(Beds, na.rm = T),
            Beds_per10th = Beds/Population*10000) %>%
  ungroup() %>% 
  mutate(Population = Population/Population[Year == 1997]*100,
         Beds = Beds/Beds[Year == 1997]*100,
         Beds_per10th = Beds_per10th/Beds_per10th[Year == 1997]*100) %>% 
  gather(Var, Val, Population:Beds_per10th) %>% 
  ggplot(aes(x = Year, y = Val, col = Var))+
  geom_line(lwd = 1, aes(linetype = case_when(Var == "Beds_per10th" ~ "dotted",
                                              T ~ "solid") %>% factor(levels = c("solid", "dotted"))))+
  scale_color_OkabeIto(name = element_blank(), 
                       labels = c("Beds", "Beds per 10 000 people", "Population"))+
  scale_y_continuous(name = "%",
                     limits = c(85, 110), breaks = seq(85, 110, 5))+
  scale_x_continuous(name = element_blank(), limits = c(1997, 2013),
                     breaks = 1997:2013, labels = year_labels_l)+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.2, 0.25))+
  guides(linetype = F, color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid"))))

ggsave(plot = leningrad_general_plot, filename = "leningrad_general_plot.jpeg",
       device = "jpeg", path = "plots/Tver and Leningrad/",
       dpi = 300, width = 16, height = 9, units = "cm")


# Ленинград: столица и другие города

leningrad_detailed_plot <-
  data_leningrad %>% 
  group_by(Year, Center) %>% 
  summarise(Population = sum(Population, na.rm = T),
            Beds = sum(Beds, na.rm = T),
            Beds_per10th = Beds/Population*10000) %>%
  ungroup() %>% 
  mutate(Population = Population/Population[Year == 1997]*100,
         Beds = Beds/Beds[Year == 1997]*100,
         Beds_per10th = Beds_per10th/Beds_per10th[Year == 1997]*100) %>% 
  gather(Var, Val, Population:Beds_per10th) %>% 
  ggplot(aes(x = Year, y = Val, col = Var))+
  geom_line(lwd = 1, aes(linetype = case_when(Var == "Beds_per10th" ~ "dotted",
                                              T ~ "solid") %>% factor(levels = c("solid", "dotted"))))+
  scale_color_OkabeIto(name = element_blank(), 
                       labels = c("Beds", "Beds per 10 000 people", "Population"))+
  scale_y_continuous(name = "%",
                     limits = c(45, 125), breaks = seq(45, 125, 5))+
  scale_x_continuous(name = element_blank(), limits = c(1997, 2013),
                     breaks = 1997:2013, labels = year_labels_l)+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.13, 0.25))+
  guides(linetype = F, color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid")))) +
  facet_wrap(.~Center)

ggsave(plot = leningrad_detailed_plot, filename = "leningrad_detailed_plot.jpeg",
       device = "jpeg", path = "plots/Tver and Leningrad/",
       dpi = 300, width = 23, height = 9, units = "cm")

















# Population Leningradskaya oblast
data_leningrad %>%
  group_by(Year, Center) %>% 
  summarise(Population = sum(Population, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = Population/1000000, fill = Center))+
  geom_col()+
  scale_fill_discrete_qualitative(name = element_blank())+
  scale_y_continuous(name = "Urban population, mln",
                     limits = c(0, 6.5),
                     breaks = seq(0, 6.5, 0.5)
                     )+
  scale_x_continuous(name = element_blank(), limits = c(1996, 2014),
                     breaks = seq(1997, 2013, 1))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.85, 0.9))

population %>%
  filter(Region == "Тверская") %>%
  mutate(Center = case_when(City == "Тверь" ~ "Tver",
                            T ~ "Other cities") %>% 
           factor(levels = c("Tver", "Other cities"))) %>% 
  group_by(Year, Center) %>% 
  summarise(Population = sum(Population, na.rm = T)) %>%
  ungroup() %>% 
  ggplot(aes(x = Year, y = Population/1000000, fill = Center))+
  geom_col()+
  scale_fill_discrete_qualitative(name = element_blank())+
  scale_y_continuous(name = "Urban population, mln",
                     limits = c(0, 1.1),
                     breaks = seq(0, 1.1, 0.1)
  )+
  scale_x_continuous(name = element_blank(), limits = c(1990, 2014),
                     breaks = seq(1991, 2013, 2))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.85, 0.9))




ggsave(plot = total_pop, filename = "total_pop.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 15, height = 8, units = "cm")