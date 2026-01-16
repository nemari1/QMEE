library(dplyr)
library(tidyr)
getwd()
snail_data <- read_csv("../data/raw/Snail_Microfiber_Ingestion.csv")
show_col_types = FALSE
summary(snail_data)

#characters should be factors in my dataset
snail_data <- snail_data |> mutate_if(is.character, as.factor)
head(snail_data)
print(snail_data
      |> group_by(Treatment_Group, Snail_Number, Nylon_Fiber, PET_Fiber)
      |> summarize(count = n())
)
print(snail_data
      |> count(Treatment_Group, Snail_Number, Nylon_Fiber, PET_Fiber, name="count")
)
print(snail_data
      |> count(Treatment_Group, Snail_Number, Nylon_Fiber, PET_Fiber, name="count")
      |> filter(count>1)
)
is.na(snail_data) 
#no NA values
long_snail <- snail_data |>
  pivot_longer(
    cols = c(Nylon_Fiber, PET_Fiber), names_to="Microfiber_Type", values_to="Microfiber_Count")
str(long_snail)
print(long_snail, width = Inf)
library(ggplot2)
print(ggplot(long_snail, aes(x=Treatment_Group, y=Microfiber_Count, fill = Microfiber_Type))
      + geom_col()
)
print(ggplot(long_snail, aes(x=Treatment_Group, y=Microfiber_Count)) + geom_boxplot())
#I am not sure if the outliers in the high concentration of my boxplot would be considered anomalies?
saveRDS(snail_data, file = "../data/cleaned/snail_data_clean.rds")
saveRDS(long_snail, file = "../data/cleaned/long_snail_clean.rds")
