# Exploratory Analysis

library(tidyr)
library(dplyr)
library(ggplot2)
library(colorblindr)


# Load data
load("data/res_data.Rdata")

# Population, mln people
total_pop <- res_data %>% 
  group_by(Year) %>% 
  summarise(Population = sum(Population, na.rm = T)) %>%
  ungroup() %>% 
  ggplot(aes(x = Year, y = Population/1000000))+
  geom_line()+
  scale_y_continuous(name = "Population, mln people",
                     limits = c(61, 66),
                     breaks = seq(61, 66, 1))+
  scale_x_continuous(name = element_blank(), limits = c(1991, 2013),
                     breaks = seq(1991, 2013, 2))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid.minor.y = element_blank())

ggsave(plot = total_pop, filename = "total_pop.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 15, height = 8, units = "cm")

# Beds, '000
total_beds <- res_data %>% 
  group_by(Year) %>% 
  summarise(Beds = sum(Beds, na.rm = T)) %>%
  ungroup() %>% 
  ggplot(aes(x = Year, y = Beds/1000))+
  geom_line()+
  scale_y_continuous(name = "Beds, '000",
                     limits = c(600, 1000), breaks = seq(600, 1000, 50))+
  scale_x_continuous(name = element_blank(), limits = c(1991, 2013),
                     breaks = seq(1991, 2013, 2))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid.minor.y = element_blank())

ggsave(plot = total_beds, filename = "total_beds.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 15, height = 8, units = "cm")

# Beds per 10000 people
total_beds_per_10th <- res_data %>% 
  group_by(Year) %>% 
  summarise(Population = sum(Population, na.rm = T),
            Beds = sum(Beds, na.rm = T),
            Beds_per_10th = Beds/Population*10000) %>%
  ungroup() %>% 
  ggplot(aes(x = Year, y = Beds_per_10th))+
  geom_line()+
  scale_y_continuous(name = "Beds per 10000 people",
                     limits = c(100, 150), seq(100,150,5))+
  scale_x_continuous(name = element_blank(), limits = c(1991, 2013),
                     breaks = seq(1991, 2013, 2))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid.minor.y = element_blank())

ggsave(plot = total_beds_per_10th, filename = "total_beds_per_10th.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 15, height = 8, units = "cm")

# # Hospitals
# total_hospitals <- res_data %>%
#   filter(Year > 1996) %>% 
#   group_by(Year) %>% 
#   summarise(Hospitals = sum(Hospitals, na.rm = T)) %>%
#   ungroup() %>% 
#   ggplot(aes(x = Year, y = Hospitals))+
#   geom_line()+
#   scale_y_continuous(name = "Hospitals",
#                      limits = c(2600, 3500), breaks = seq(2600, 3500, 100))+
#   scale_x_continuous(name = element_blank(), limits = c(1991, 2013),
#                      breaks = seq(1991, 2013, 2))+
#   theme_bw(base_family = "Arial", base_size = 12)+
#   theme(panel.grid.minor.y = element_blank())
# 
# ggsave(plot = total_hospitals, filename = "total_hospitals.jpeg",
#        device = "jpeg", path = "plots/",
#        dpi = 300, width = 15, height = 8, units = "cm")

# Relative values
total_rel <- res_data %>% 
  group_by(Year) %>% 
  summarise(Population = sum(Population, na.rm = T),
            Beds = sum(Beds, na.rm = T),
            Beds_per10th = Beds/Population*10000
            # Hospitals = sum(Hospitals, na.rm = T)
            ) %>%
  ungroup() %>% 
  mutate(Population = Population/Population[Year == 1991]*100,
         Beds = Beds/Beds[Year == 1991]*100,
         Beds_per10th = Beds_per10th/Beds_per10th[Year == 1991]*100
         # Hospitals = Hospitals/Hospitals[Year == 1997]*100
         ) %>% 
  gather(Var, Val, Population:Beds_per10th) %>% 
  ggplot(aes(x = Year, y = Val, col = Var))+
  geom_line(lwd = 1, aes(linetype = case_when(Var == "Beds_per10th" ~ "dotted",
                                              T ~ "solid") %>% factor(levels = c("solid", "dotted"))))+
  scale_color_OkabeIto(name = element_blank(), 
                       labels = c("Beds", "Beds per 10 000 people", "Population"))+
  scale_y_continuous(name = "%",
                     limits = c(70, 105), breaks = seq(70, 105, 5))+
  scale_x_continuous(name = element_blank(), limits = c(1991, 2013),
                     breaks = seq(1991, 2013, 2))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.18, 0.25))+
  guides(linetype = F, color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid"))))

ggsave(plot = total_rel, filename = "total_rel.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 18, height = 10, units = "cm")

# Annual population changes by fua by category
year_labs <- 1992:2013
year_labs[-seq(1,22,2)] <- ""

median_an_pop_ch <- res_data_fua %>%
  group_by(Year, Size_category_91_13) %>% 
  summarise(Median = median(An_pop_ch, na.rm = T),
            sd = sd(An_pop_ch, na.rm = T),
            Pop_change_lower = Median - sd,
            Pop_change_upper = Median + sd) %>% 
  ungroup() %>%
  ggplot(aes(x = Year))+
  # geom_ribbon(aes(ymin=Pop_change_lower,ymax=Pop_change_upper,fill=Size_category_91_13),lwd = 0,alpha=0.2)+
  geom_line(aes(y = Median, color = Size_category_91_13), lwd = 0.8)+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  scale_y_continuous(name = "Median annual population change, %",
                     limits = c(-3, 1.5),
                     breaks = seq(-3, 1.5, 0.5))+
  scale_x_continuous(name = element_blank(), limits = c(1992, 2013),
                     breaks = 1992:2013, labels = year_labs)+
  scale_color_OkabeIto(name = "Size, '000 people")+
  scale_fill_OkabeIto(name = "Size, '000 people")+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid.minor.y = element_blank(),
        legend.position = c(0.15,0.25))

# Export to jpeg 
ggsave(plot = median_an_pop_ch, filename = "median_an_pop_ch.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 18, height = 10, units = "cm")

# Сколько людей живет в городах разных категорий
res_data_fua %>% 
  group_by(Year, Size_category_91_13) %>% 
  summarize(Population = sum(Population, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(Pop_share = Population/sum(Population)*100) %>% View()

# Decrease in population numbers vs decrease in bed numbers
pop_change_vs_bed_change <- res_data_fua %>% 
  group_by(AROKATO) %>% 
  arrange(Year) %>% 
  mutate(Pop_ch = (Population[Year == 2013] - Population[Year == 1991])/Population[Year == 1991]*100) %>% 
  ungroup() %>%
  select(AROKATO, Beds_ch, Pop_ch, Size_category_91_13) %>% unique() %>% 
  ggplot(aes(x = Pop_ch, y = Beds_ch, col = Size_category_91_13))+
  geom_point(alpha = 1, pch = 21, stroke = 0.8)+
  # geom_smooth(method = "glm", se = F)+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_vline(aes(xintercept = 0), linetype = "dashed")+
  scale_x_continuous(name = "Population change, %")+
  scale_y_continuous(name = "Bed numbers change, %")+
  scale_color_OkabeIto(name = "Size, '000 people")+
  coord_equal()+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.8, 0.8))
  
ggsave(plot = pop_change_vs_bed_change, filename = "pop_change_vs_bed_change.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 16, height = 16, units = "cm")

# Decrease in population numbers vs decrease in bed numbers (W/B)
pop_change_vs_bed_change_wb <- res_data_fua %>% 
  group_by(AROKATO) %>% 
  arrange(Year) %>% 
  mutate(Pop_ch = (Population[Year == 2013] - Population[Year == 1991])/Population[Year == 1991]*100) %>% 
  ungroup() %>%
  select(AROKATO, Beds_ch, Pop_ch) %>% unique() %>% 
  ggplot(aes(x = Pop_ch, y = Beds_ch))+
  geom_point(alpha = 1, pch = 21, stroke = 0.8)+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_vline(aes(xintercept = 0), linetype = "dashed")+
  geom_smooth(method = "glm", col = "red", se = F)+
  scale_x_continuous(name = "Population change, %")+
  scale_y_continuous(name = "Bed numbers change, %")+
  coord_equal()+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank())

ggsave(plot = pop_change_vs_bed_change_wb, filename = "pop_change_vs_bed_change_wb.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 16, height = 16, units = "cm")

# Как много крователй потеряли города по категориям?
res_data_fua %>%
  group_by(Year, Size_category_91_13) %>% 
  summarise(Beds = sum(Beds, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Size_category_91_13) %>% 
  mutate(Beds_ch = Beds[Year == 2013] - Beds[Year == 1991],
         Beds_ch_rel = (Beds[Year == 2013] - Beds[Year == 1991])/Beds[Year == 1991]*100) %>% View()

# Center-peripery vs. bed numbers change
centre_periphery <- res_data %>%
  group_by(ROKATO) %>% 
  mutate(Beds_ch = (Beds[Year == 2013] - Beds[Year == 1991])/Beds[Year == 1991]*100) %>%
  ungroup() %>% 
  select(ROKATO,Beds_ch, Core_periphery_status) %>% unique() %>% 
  filter(!is.na(Core_periphery_status)) %>% 
  ggplot(aes(x = Core_periphery_status, y = Beds_ch))+
  geom_jitter()+
  geom_boxplot()+
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(name = "Bed numbers change, %", limits = c(-100, 120))+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(panel.grid = element_blank())

ggsave(plot = centre_periphery, filename = "centre_periphery.jpeg",
       device = "jpeg", path = "plots/",
       dpi = 300, width = 15, height = 15, units = "cm")
