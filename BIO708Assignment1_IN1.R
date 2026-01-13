library(tidyverse)
## library(ratdat)  ## BMB: don't need this
## library(readr)   ## BMB: included with tidyverse
## BMB: no absolute paths please!
## BMB: wrong file name ...
## surveys <- read_csv("Snail_Microfiber_Ingestion.csv")
surveys <- read_csv("Snail_Microfiber_Excretion.csv")
colnames(surveys)
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(mean_Nylon_Fiber = mean(Nylon_Fiber, na.rm = TRUE))
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(p_value = shapiro.test(Nylon_Fiber)$p.value)
surveys %>% 
  group_by(Treatment_Group) %>% 
  summarize(mean_PET_Fiber = mean(PET_Fiber, na.rm = TRUE))
surveys %>% 
  group_by(Treatment_Group) %>% 
    summarize(p_value = shapiro.test(PET_Fiber)$p.value)

## BMB: this does the same thing
surveys |>
    summarize(across(c(Nylon_Fiber, PET_Fiber),
                     .fns = list(mean = \(x) mean(x, na.rm = TRUE),
                                  p_val = \(x) shapiro.test(x)$p.value)),
              .by = Treatment_Group)

## BMB: please check/be more careful -- your code couldn't have run
## correctly because you gave the wrong file name ...
## mark: 1.9
