# QMEE

#Assignment 7 - IN

I hypothesized that increased microfiber exposure will result in increased microfiber ingestion by snails because greater environmental availablity of pollutants will increase uptake. 

My diagnostic plot is checking to see if my residuals are uniform and if there is significant dispersion. My plot shows insignificant results for the KS test and outlier test. Since my negative binomial model already accounted for overdispersion, I am not too concerned about the significant result on the dispersion test, therefore I believe this model is appropriately fitted.

#My inferential plot compares treatment groups for nylon and PET and can tell me if there are any statistically clear differences between groups. The horizontal blue line represents the conifdence intervals and if they cross and touch the dotted line, the difference is not statistically clear. This plot shows a statistically clear difference in microfiber ingestion (PET and nylon) between medium and high treatment groups, and control and low groups for nylon. This supports the idea of a dose-dependent relationship between microfiber exposure and ingestion. It is weird that microfiber ingestion seems to decrease for from the low to medium treatment groups, I am not sure why that is!


#Assignment 6 - IN

Hypothesis: It is hypothesized that increased microfiber exposure will result in increased microfiber ingestion by snails because greater environmnetal availablity of pollutants will increase uptake. 

My diagnostic plot shows if my residuals fit into my predicted model. After I used a log transformation, my residuals fit my model (seen by the triangles that match closely to the predicted straight red line with no extreme outliers). On the right, it says that the levene's test of variance was not significant (>0.05), and that my variance across and within treatment groups is similar, which means that my model seems to fit my data.

My predictive plot calculates predicted values in different conditions, and can help with eyeballing differences between groups from the plot of means. In this plot, I can see that the predicted means for nylon and PET in my high treatment group are the highest, while my control groups are the lowest. This plot predicts that increasing exposure to microfibers increases ingestion.The predicted means for nylon are also higher than PET, which could suggest that fiber type affects micrfoiber ingestion.

My Inferential plot compares each treatment group to the overall mean. Since the confidence intervals in my high treatment group are past the dotted line, this indicates that my high treatment group is clearly higher than the overall mean. Since my control effect group is before the dotted line, this means that my control groups is clearly lower than the overall mean. My low and medium treatment groups touch the dotted line, and are therefore somewhat close to the overall mean. This plot shows how my individual groups differ according to the mean across all of my groups, and helps me look at the magnitude of my responses.

# Assignment 5 comments (JD)

It's not OK to say “if it is normally distributed”, because it's not. You need to think about how you will determine if it is close enough, ideally without accepting the null. This is hard, you should talk to us about interpreting diagnostic plots. Given that you have a factor of 100 in your exposure, it's not likely that you will have anything like normality on a linear scale, and you should definitely be thinking about transformations.

You should never imply that a P value measures “magnitude of [a] difference”

It's generally better to use Hedges' g instead of Cohen's d. With more than one group, it's not exactly clear how Cohen's d applies. Do you mean you will compare each group separately for differences with “low” instead of control? Or something else?

Grade 1.8/3

#Assignment 3 - IN

For graph 1, I created a boxplot showing the microfiber ingestion count of nylon and PET across control low, medium and high treatment groups. I chose to put nylon and PET fiber counts beside each other so that I could observe the differences in ingestion of each fiber across different treatment groups. I chose a boxplot because I was curious to see the minimum, maximum, and median values at each treatment group as well as the interquartile range. The lines on the top and bottom show the range of my data while the dots show any outliers.
I chose to make a 2D plot with more subtle gridlines because Edward Tufte highlighted that overly decorated plots (3D, intense backgrounds) are distracting. I also included clear axis titles and a legend to make my plot easy to follow. 

Graph 2 is a violin plot of the microfiber count distribution in snails across control, low, medium, and high treatment groups. I chose to use a violin plot because it shows how the density of my data is distributed, with width increasing where my data was more dense. This allows me to see where my data is most concentrated. 
I chose this on the basis of Tufte's principles, which prioritize transparency and emphasize that data should not be oversimplified. My plot shows the individual data points as well as the density distribution, which allows me to see how my data is spread out and avoid making incorrect assumptions.

Graph 3 shows the amount of microfibers ingested across treatment groups. Each point represents individual data values, which help me see the distribution and variability of microfiber ingestion counts in different treatment groups. I chose to include a mean line across treatment groups, which shows a gradual increase in microfiber ingestion from control to high treatment groups.
Since the Cleveland Heirarchy states that graphs that have a position on a common scale are more easy to interpret, I created my plot to have a common y-axis (microfiber count).

## Assignment 2 - IN

I examined the structure of my data (using summary and head) and changed my `Treatment_Group` class from character to factor because it has a fixed number of categories (Control, Low, Medium, High). 
I also checked for any NA values and found none. I created 2 plots that show the amount of PET and nylon fibers in each treatment group and a boxplot of the total microfiber count across treatment groups. There were two outliers that seemed suspicious in my high treatment group but I looked back at my data and it does not seem like a typo so I was not sure if this is a problem that I should fix. I also saved two rds files, `snail_data_clean` has my cleaned data saved and `long_snail_clean` has my data saved using pivot_longer to make my data tidier.

BMB: I would call the second one `snail_data_clean_long` (or `snail_data_long`); consistency is super-helpful

My first script has cleaned versions of my raw data set where any problems were fixed. This file should be run from the R-Ecology-Workshop directory (BMB: do you mean the head directory of your repo? "R-Ecology-Workshop" doesn't exist on my computer ...) and can be found inside of the scripts folder in the project directory. It is titled "Assignment2_DataCleaning"

My second script was used to read my cleaned data that was saved as an rds file and explore/analyze the data in some way. I chose to explore it with a bar graph that analysed the microfiber count across treatment groups. This file should be run from the R-Ecology-Workshop directory and can be found inside of the scripts folder in the project directory. It is titled "Assignment2_rdsFileReading.

With my data, I would like to explore if microfiber concentrations in each treatment group affect the amount of microfibers that snails ingest and I would also like to look at the individual variability of snail microfiber ingestion to see if snails ingest more of one fiber type than others.
I would break my data into two different files, one for microfibers across treatment groups and a second for microfibers across individual snails. I will check for NA values and check the ranges in my data to ensure that my values make sense. I will also look at the structure of my data to make sure that the R classes (characters, factors) are correct and that the columns have no spaces or typos that could cause problems with coding. I will plot this data to check for any outliers/anomalies or errors and mutate my data as needed. 
These files will be saved as rds files so that they can be read into new R scripts for plotting and calculations. I would create two scripts so that one script is for calculations, and another is for data visualization. 
These scripts can be replicated and reused for the other files. (BMB: you might not need to reuse the code, if you have the data saved in a useful way ...)

## Assignment 1 - IN

Freshwater snails (*Planorbella pilsbryi*) (n=96) were exposed over 28 days at control, low (100 particles/L), medium (10,000 particles/L), and high (1,000,000 particles/L) concentrations of a mixture of polyethylene terephthalate (PET) and nylon microfibers. This dataset documents the amount of nylon and PET microfibers found in snails in each treatment group following chemical digestion. Some biological questions I hope to answer from this dataset are: 

Is there significant microplastic ingestion at environmentally relevant (low treatment group) concentrations? 

Is there a dose dependent relationship between microplastic exposure and microplastic ingestion? 

Are nylon and PET ingested at different rates? (BMB: careful about "is <X> different" questions ...)

Do individual snails in each treatment group ingest similar rates of microplastics?  (BMB: "are they similar" is better ...)
