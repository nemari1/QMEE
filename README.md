# QMEE

## Assignment 2 - IN

I examined the structure of my data (using summary and head) and changed my Treatment_Group class from character to factor because it has a fixed number of categories (Control, Low, Medium, High). 
I also checked for any NA values and found none. I created 2 plots that show the amount of PET and nylon fibers in each treatment group and a boxplot of the total microfiber count across treatment groups. There were two outliers that seemed suspicious in my high treatment group but I looked back at my data and it does not seem like a typo so I was not sure if this is a problem that I should fix. I also saved two rds files, snail_data_clean has my cleaned data saved and long_snail_clean has my data saved using pivot_longer to make my data tidier.

My first script has cleaned versions of my raw data set where any problems were fixed. This file should be run from the R-Ecology-Workshop directory and can be found inside of the scripts folder in the project directory. It is titled "Assignment2_DataCleaning"

My second script was used to read my cleaned data that was saved as an rds file and explore/analyze the data in some way. I chose to explore it with a bar graph that analysed the microfiber count across treatment groups. This file should be run from the R-Ecology-Workshop directory and can be found inside of the scripts folder in the project directory. It is titled "Assignment2_rdsFileReading.

With my data, I would like to explore if microfiber concentrations in each treatment group affect the amount of microfibers that snails ingest and I would also like to look at the individual variability of snail microfiber ingestion to see if snails ingest more of one fiber type than others.
I would break my data into two different files, one for microfibers across treatment groups and a second for microfibers across individuals snails. I will check for NA values and check the ranges in my data to ensure that my values make sense. I will also look at the structure of my data to make sure that the R classes (characters, factors) are correct and that the columns have no spaces or typos that could cause problems with coding. I will plot this data to check for any outliers/anomalies or errors and mutate my data as needed. 
These files will be saved as rds files so that they can be read into new R scripts for plotting and calculations. I would create two scripts so that one script is for calculations, and another is for data visualization. 
These scripts can be replicated and reused for the other files.

## Assignment 1 - IN

Freshwater snails (*Planorbella pilsbryi*) (n=96) were exposed over 28 days at control, low (100 particles/L), medium (10,000 particles/L), and high (1,000,000 particles/L) concentrations of a mixture of polyethylene terephthalate (PET) and nylon microfibers. This dataset documents the amount of nylon and PET microfibers found in snails in each treatment group following chemical digestion. Some biological questions I hope to answer from this dataset are: 

Is there significant microplastic ingestion at environmentally relevant (low treatment group) concentrations? 

Is there a dose dependent relationship between microplastic exposure and microplastic ingestion? 

Are nylon and PET ingested at different rates? (BMB: careful about "is <X> different" questions ...)

Do individual snails in each treatment group ingest similar rates of microplastics?  (BMB: "are they similar" is better ...)