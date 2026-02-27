library(DHARMa)
library(tidyverse)
library(emmeans)
library(dplyr)

snail_data_long <- read_rds("C:Data/cleaned/snail_data_long.rds")

#Rearranged treatment groups to go from low to high
snail_data_long <- snail_data_long %>%
  mutate(Treatment_Group = factor
         (Treatment_Group,levels = c("Control", "Low", "Medium", "High")))

#I hypothesized that increased microfiber exposure will result in increased microfiber ingestion by snails because greater environmental availablity of pollutants will increase uptake. 

#GLM
g1 <- glm(Microfiber_Count~Treatment_Group * Microfiber_Type, data = snail_data_long, family=poisson(link="log"))
summary(g1)
#This GLM had a significant KS test, dispersion test and outlier test result when I did DHarma so I changed my model from a poisson to a negative binomial to help account for overdispersion.

#GLM2 
library(MASS)
g2 <- glm.nb(Microfiber_Count~Treatment_Group * Microfiber_Type, data = snail_data_long)


#diagnostic plot

plot(simulateResiduals(g2))
#This diagnostic plot is checking to see if my residuals are uniform and if there is significant dispersion. My plot shows insignificant results for the KS test and outlier test. Since my negative binomial model already accounted for overdispersion, I am not too concerned about the significant result on the dispersion test, therefore I believe this model is appropriately fitted.


#inferential plot
em_Treatment_Group <- emmeans(g2, ~ Treatment_Group*Microfiber_Type)
print(em_contr <- contrast(em_Treatment_Group, method = "eff", adjust = "none"))
plot(contrast(em_Treatment_Group, method = "consec")) + geom_vline(xintercept = 0, lty = 2)
#My inferential plot compares treatment groups for nylon and PET and can tell me if there are any statistically clear differences between groups. The horizontal blue line represents the conifdence intervals and if they cross and touch the dotted line, the difference is not statistically clear. This plot shows a statistically clear difference in microfiber ingestion (PET and nylon) between medium and high treatment groups, and control and low groups for nylon. This supports the idea of a dose-dependent relationship between microfiber exposure and ingestion. It is weird that microfiber ingestion seems to decrease for from the low to medium treatment groups, I am not sure why that is!
