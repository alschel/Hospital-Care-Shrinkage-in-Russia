


library(dplyr)
library(ggplot2)
library(DescTools)

load("data/res_data.Rdata")

# 1. City size vs change in bed numbers

table1 <- table(res_data_fua %>% 
                  select(AROKATO, Size_category_91_13, Beds_ch_cat) %>% unique() %>% 
                  select(Size_category_91_13, Beds_ch_cat))

chisq.test(table1) %>% .$expected

# table2 <- table(res_data_fua %>% 
#                   select(AROKATO, Size_category_91_13, Beds_per10th_ch_cat) %>% unique() %>% 
#                   select(Size_category_91_13, Beds_per10th_ch_cat))
# 
# chisq.test(table2) %>% .$expected

# 2. Cities population dynamics vs change in bed numbers
table2 <- table(res_data_fua %>% 
                  select(AROKATO, An_pop_ch_91_13_cat, Beds_ch_cat) %>% unique() %>% 
                  select(An_pop_ch_91_13_cat, Beds_ch_cat))

chisq.test(table2) %>% .$expected

# 3. Core vs periphery

library(coin)
u_test <- wilcox_test(Beds_ch~Core_periphery_status, 
            data=res_data %>%
              group_by(ROKATO) %>% 
              mutate(Beds_ch = (Beds[Year == 2013] - Beds[Year == 1991])/Beds[Year == 1991]*100) %>%
              ungroup() %>% 
              select(ROKATO,Beds_ch, Core_periphery_status) %>% unique() %>% 
              filter(!is.na(Core_periphery_status)), distribution="exact")

# 4. 

library(lme4)
res_data_fua %>%
  group_by(AROKATO) %>% 
  arrange(Year) %>% 
  mutate(Pop_ch = (Population[Year == 2013] - Population[Year == 1991])/Population[Year == 1991]*100) %>% 
  ungroup() %>%
  select(Region, AROKATO, Mean_pop_91_13, Beds_ch, Pop_ch) %>% unique() %>% 
  mutate(Pop_ch = scale(Pop_ch),
         Mean_pop_91_13 = scale(Mean_pop_91_13)) -> test_data

reg <- lmer(Beds_ch ~ Mean_pop_91_13 + Pop_ch + (1|Region), data = test_data, REML = T)
MuMIn::r.squaredGLMM(reg)
RMSE(x = predict(reg), ref = test_data$Beds_ch)
stargazer::stargazer(reg, type = "text")

resid(reg) %>% qqnorm()
