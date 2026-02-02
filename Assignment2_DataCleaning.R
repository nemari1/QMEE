library(dplyr)
library(tidyr)
library(readr) ## need this for read_csv
## getwd()
## snail_data <- read_csv("data/raw/Snail_Microfiber_Ingestion.csv")

## BMB: this doesn't work. Need to spend more time figuring out how directories
##  etc. work. -- your repo structure doesn't look the same as whatever the structure
##  on your computer is, it has to match (you should be working *in* the repo directory)
snail_data <- read_csv("data/raw/Snail_Microfiber_Ingestion.csv", show_col_types = FALSE)
summary(snail_data)

#characters should be factors in my dataset
snail_data <- snail_data |> mutate_if(is.character, as.factor)
head(snail_data)

## BMB: is this the same as the next code chunk?
print(snail_data
      |> group_by(Treatment_Group, Snail_Number, Nylon_Fiber, PET_Fiber)
      |> summarize(count = n())
      )

## BMB: if you save this as an object (snail_count) you can repeat less code for the
##  next test
print(snail_count <- snail_data
      |> count(Treatment_Group, Snail_Number, Nylon_Fiber, PET_Fiber, name="count")
      )
filter(snail_count, count > 1)
## print(snail_data
##       |> count(Treatment_Group, Snail_Number, Nylon_Fiber, PET_Fiber, name="count")
##       |> filter(count>1)
## )

## BMB: automate
stopifnot(!any(is.na(snail_data) ))

#no NA values
long_snail <- snail_data |>
  pivot_longer(
    cols = c(Nylon_Fiber, PET_Fiber), names_to="Microfiber_Type", values_to="Microfiber_Count")
str(long_snail)
print(long_snail, width = Inf)  ## BMB: don't really need width = Inf since there are only four columns?
library(ggplot2)
print(ggplot(long_snail, aes(x=Treatment_Group, y=Microfiber_Count, fill = Microfiber_Type))
      + geom_col()
)
print(gg1 <- ggplot(long_snail, aes(x=Treatment_Group, y=Microfiber_Count)) + geom_boxplot())
#I am not sure if the outliers in the high concentration of my boxplot would be considered anomalies?
## BMB: probably not (although some suggestion 
gg1 + scale_y_log10() + stat_sum(alpha = 0.5)

## JD: You want to have the scripts put the rds files inside the repo directory, then you want to ignore the data directory.
## Using "../" from the top directory of your repo means your code is unlikely to be portable
## BMB: this is going to be a problem since this data structure isn't in the repo
saveRDS(snail_data, file = "data/cleaned/snail_data_clean.rds")
<<<<<<< HEAD
saveRDS(long_snail, file = "data/cleaned/snail_data_long.rds")

=======
saveRDS(long_snail, file = "data/cleaned/long_snail_clean.rds")
>>>>>>> 6eb3b5580dfb9ed4f11a6c7caaba17b51a834014
