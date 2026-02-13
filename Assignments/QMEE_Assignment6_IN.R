
library(DHARMa)
library(tidyverse)
library(effects)

#Hypothesis: It is hypothesized that increased microfiber exposure will result in increased microfiber ingestion by snails because greater environmnetal availablity of pollutants will increase uptake. 

snail_data_long <- read_rds("Data/cleaned/snail_data_long.rds")

#Rearranged treatment groups to go from low to high
snail_data_long <- snail_data_long %>%
  mutate(Treatment_Group = factor
         (Treatment_Group,levels = c("Control", "Low", "Medium", "High")))

#log transformed to fit the linear model better, I have zero values in my control so I added +1
snail_data_long$log_Microfiber_Count <- 
  log(snail_data_long$Microfiber_Count + 1)

#linear model
m1 <- lm(log_Microfiber_Count~Treatment_Group*Microfiber_Type, data=snail_data_long)

#diagnostic plot using DJARMa
library(DHARMa)
plot(simulateResiduals(m1))

#This plot shows if my residuals fit into my predicted model. After I used a log transformation, my residuals fit my model (seen by the triangles that match closely to the predicted straight red line with no extreme outliers). On the right, it says that the levene's test of variance was not significant (>0.05), and that my variance across and within treatment groups is similar, which means that my model seems to fit my data.

#predictive plot
library(emmeans)
em <- emmeans(m1, ~ Treatment_Group*Microfiber_Type)
plot(em)

#This plot calculates predicted values in different conditions, and can help with eyeballing differences between groups from the plot of means. In this plot, I can see that the predicted means for nylon and PET in my high treatment group are the highest, while my control groups are the lowest. This plot predicts that increasing exposure to microfibers increases ingestion.The predicted means for nylon are also higher than PET, which could suggest that fiber type affects microfiber ingestion.

#inferential plot
em_Treatment_Group <- emmeans(m1, ~ Treatment_Group)
print(em_contr <- contrast(em_Treatment_Group, method = "eff", adjust = "none"))
plot(em_contr) + geom_vline(xintercept = 0, lty = 2)

#This plot compares each treatment group to the overall mean. Since the confidence intervals in my high treatment group are past the dotted line, this indicates that my high treatment group is clearly higher than the overall mean. Since my control effect group is before the dotted line, this means that my control groups is clearly lower than the overall mean. My low and medium treatment groups touch the dotted line, and are therefore somewhat close to the overall mean. This plot shows how my treatment groups differ according to the mean across all of my groups, and helps me look at the magnitude of my responses.

#I am hoping that I have fixed the issue with the directories...it worked for me when I ran it on source. Please let me know if there are any issues with running my code.

