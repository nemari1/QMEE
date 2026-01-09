library(tidyverse)
library(ratdat)
library(readr)
surveys <- read_csv("C:/Users/noury/OneDrive/Desktop/Snail_Microfiber_Ingestion.csv")
colnames(surveys)
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(mean_Nylon_Fiber = mean(Nylon_Fiber, na.rm = T))
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(p_value = shapiro.test(Nylon_Fiber)$p.value)
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(mean_PET_Fiber = mean(PET_Fiber, na.rm = T))
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(p_value = shapiro.test(PET_Fiber)$p.value)
