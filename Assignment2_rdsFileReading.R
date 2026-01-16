long_snail_clean <- readRDS("../data/cleaned/long_snail_clean.rds")
library(ggplot2)
ggplot(long_snail_clean, aes(x = Treatment_Group, y = Microfiber_Count, fill = Microfiber_Type)) + geom_bar(stat = "identity", position = "dodge")

                                                                                                          