#------------------------------------------------------------------------------# 
#               CA2 - Covid Data Analysis (Predictive Modeling)                #
#                             Siddharth Bangera                                #
#                        Professor : James Connolly                            #
#------------------------------------------------------------------------------#

# In this project, we will be using the provided Covid dataset to implement
# predictive modeling. The dataset that has been provided  has 84529 rows and 
# 59 columns. However, we will only be using those columns which makes sense
# to be predicted using one independent variable.
# All the code of this project will be found in CA2 repo given below:
# GitHub: https://github.com/bangerasiddharth/CA2.git
#------------------------------------------------------------------------------#
covid_data <- read.csv("covid.csv", na = "")                                    #Loading the covid.csv data into covid_data dataframe, replacing missing values with NA
head(covid_data, n = 15)                                                        #Displaying the first 15 sample records            
str(covid_data)                                                                 #Displaying the structure of records/verifying the data types of the fields
class(covid_data)                                                               #Ensuring that the class of covid_data is data.frame
ncol(covid_data)                                                                #59 - Number of columns                                                                                                                                       
nrow(covid_data)                                                                #84529 - number of rows
names(covid_data)                                                               #names of all the columns
library(VIM)                                                                    #Using the VIM package to visualize the missing values
missing_values <- aggr(covid_data, prop = FALSE, numbers = TRUE)                #aggregates and populates the missing values
summary(missing_values)     
#-------------------------------------------------------------------------------
#                 Data Preparation, Cleaning and Processing 
#
# We initially started with the aim of subsetting data just for the location
#'Ireland, however, since there were very few records, it made much sense to 
# do predictive modeling based on a larger subset.
# Here, were are basically filtering out only the records relevant to Europe.
# After extracting the Europe specific data, we replaced the NA values with 0
# for only those columns which will be required for prediction
#-------------------------------------------------------------------------------
#install.packages("sqldf")
library(sqldf)                                                                  #Library which enables to write sql queries
#covid_Europe <- subset(covid_data, continent %in% c("Europe") ,                #Alternative method to extract data based on Europe filter and selective columns
#                 select = c(2,3,4,5,6,8,9,17,18,20,22,24,26,27,32,34,35,36,
#                 37,38,44,45,46,47,48,49,50,51,52,53,54,55,56,58,59))
covid_Europe <- sqldf("select new_cases, new_deaths, reproduction_rate,         
                      icu_patients,hosp_patients,new_tests,total_tests,
                      positive_rate,total_vaccinations,people_fully_vaccinated,
                      new_vaccinations, stringency_index, population, 
                      population_density, aged_65_older, gdp_per_capita, 
                      aged_70_older, extreme_poverty, diabetes_prevalence, 
                      female_smokers, male_smokers, median_age, 
                      handwashing_facilities, human_development_index 
                      from covid_data where continent = 'Europe'")              #Selecting only the required columns and applying filter to extract records only relevant to Europe continent

nrow(covid_Europe)                                                              #20280 records extracted out of 84529
ncol(covid_Europe)                                                              #24 columns selected out of 59
dim(covid_Europe)                                                               #shows the dimension, rows vs cols
names(covid_Europe)                                                             #displays the names of the 24 columns
names(which(sapply(covid_Europe, anyNA)))                                       #Displays the number of columns that contain NA values, almost every column contains it                                      
#install.packages("VIM")                                                        #Installing the VIM package
library(VIM)                                                                    #Using the VIM package to visualize the missing values
missing_values <- aggr(covid_Europe, prop = FALSE, numbers = TRUE)              #aggregates and populates the missing values
summary(missing_values)                                                         #summarizes and gives the number of missing values per column

covid_Europe[,c("new_cases", "new_deaths", 
                "people_fully_vaccinated", "reproduction_rate", "icu_patients", 
                "hosp_patients", "new_tests", "total_tests", "aged_70_older", 
                "stringency_index", "female_smokers", "male_smokers", 
                "handwashing_facilities", "population", "positive_rate", "total_vaccinations", 
                "new_vaccinations", "population_density", "median_age", "aged_65_older",
                "gdp_per_capita", "extreme_poverty", "diabetes_prevalence","human_development_index")][is.na(covid_Europe[,c("new_cases", "new_deaths", 
                                                                                                                             "people_fully_vaccinated", "reproduction_rate", "icu_patients", 
                                                                                                                             "hosp_patients", "new_tests", "total_tests", "aged_70_older", 
                                                                                                                             "stringency_index", "female_smokers", "male_smokers", 
                                                                                                                             "handwashing_facilities", "population", "positive_rate", "total_vaccinations", 
                                                                                                                             "new_vaccinations", "population_density", "median_age", "aged_65_older",
                                                                                                                             "gdp_per_capita", "extreme_poverty", "diabetes_prevalence","human_development_index")])] <- 0
#In the above step, we analyzed and concluded that few columns having NA values can be replaced with 0
#Replacing NA values with 0 helps us for better prediction of the dataset
missing_values <- aggr(covid_Europe, prop = FALSE, numbers = TRUE)              #populating the missing values
summary(missing_values)                                                         #summarizing the missing values
names(which(sapply(covid_Europe, anyNA)))                                       #As evident, all the NA values have been replaced with 0.
#-------------------------------------------------------------------------------
#                            Checking for Correlation
#-------------------------------------------------------------------------------
library(psych)                                                                  #Used to get the linear correlation amongst the variables
par(mar=rep(2, 4))
# pairs.panels(covid_Europe,
#              smooth = TRUE,                                                     # If TRUE, draws loess smooths
#              scale = FALSE,                                                     # If TRUE, scales the correlation text font
#              density = TRUE,                                                    # If TRUE, adds density plots and histograms
#              ellipses = TRUE,                                                   # If TRUE, draws ellipses
#              method = "spearman",                                               # Correlation method (also "pearson" or "kendall")
#              pch = 21,                                                          # psych symbol
#              lm = FALSE,                                                        # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
#              cor = TRUE,                                                        # If TRUE, reports correlations
#              jiggle = FALSE,                                                    # If TRUE, data points are jittered
#              factor = 2,                                                        # Jittering factor
#              hist.col = 4,                                                      # Histograms color
#              stars = TRUE,                                                      # If TRUE, adds significance level with stars
#              ci = TRUE)                                                         # If TRUE, adds confidence intervals

scatter.smooth(x = covid_Europe$new_cases,                                      #new cases, is the independent variable(predictor), which will form the x-axis
               y = covid_Europe$new_deaths,                                     #new_deaths is the dependent variable(response), which will for the y-axis
               main = "new_cases ~ new_deaths",                                 #using scatter plot, we are concluding whether there exists a relationship between x and y
               xlab = "new_cases",
               ylab = "new_deaths")                                             #0.6876309
cor(covid_Europe$new_cases, covid_Europe$new_deaths)                            #Using the cor() function to find correlation value, a values closer to 1 indicates a strong correlation

scatter.smooth(x = covid_Europe$new_cases,                                      #new cases - independent variable - x axis
               y = covid_Europe$reproduction_rate,                              #reproduction_rate - dependent variable - x axis
               main = "new_cases ~ reproduction_rate",                          #using scatter plot, we are concluding whether there exists a relationship between x and y
               xlab = "new_cases",
               ylab = "reproduction_rate")                                      
cor(covid_Europe$new_cases, covid_Europe$reproduction_rate)                     #0.09860146

scatter.smooth(x = covid_Europe$new_cases,                                      #new cases - independent variable - x axis
               y = covid_Europe$new_tests,                                      #new_tests - dependent variable - x axis
               main = "new_cases ~ new_tests",
               xlab = "new_cases",
               ylab = "new_tests")
cor(covid_Europe$new_cases, covid_Europe$new_tests)                             #0.4565849

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$total_tests,
               main = "new_cases ~ total_tests",
               xlab = "new_cases",
               ylab = "total_tests")
cor(covid_Europe$new_cases, covid_Europe$total_tests)                           #0.380741 

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$positive_rate,
               main = "new_cases ~ positive_rate",
               xlab = "new_cases",
               ylab = "positive_rate")
cor(covid_Europe$new_cases, covid_Europe$positive_rate)                         #0.2487843

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$total_vaccinations,
               main = "new_cases ~ total_vaccinations",
               xlab = "new_cases",
               ylab = "total_vaccinations")
cor(covid_Europe$new_cases, covid_Europe$total_vaccinations)                    #0.2399132

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$new_vaccinations,
               main = "new_cases ~ new_vaccinations",
               xlab = "new_cases",
               ylab = "new_vaccinations")
cor(covid_Europe$new_cases, covid_Europe$new_vaccinations)                      #0.3284449 

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$stringency_index,
               main = "new_cases ~ stringency_index",
               xlab = "new_cases",
               ylab = "stringency_index")
cor(covid_Europe$new_cases, covid_Europe$stringency_index)                      #0.2233905

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$population,
               main = "new_cases ~ population",
               xlab = "new_cases",
               ylab = "population")
cor(covid_Europe$new_cases, covid_Europe$population)                            #0.4914605

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$population_density,
               main = "new_cases ~ population_density",
               xlab = "new_cases",
               ylab = "population_density")
cor(covid_Europe$new_cases, covid_Europe$population_density)                    #-0.05760811

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$median_age,
               main = "new_cases ~ median_age",
               xlab = "new_cases",
               ylab = "median_age")
cor(covid_Europe$new_cases, covid_Europe$median_age)                            #0.1721878

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$aged_65_older,
               main = "new_cases ~ aged_65_older",
               xlab = "new_cases",
               ylab = "aged_65_older")
cor(covid_Europe$new_cases, covid_Europe$aged_65_older)                         #0.1908851

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$aged_70_older,
               main = "new_cases ~ aged_70_older",
               xlab = "new_cases",
               ylab = "aged_70_older")
cor(covid_Europe$new_cases, covid_Europe$aged_70_older)                         #0.1992033

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$gdp_per_capita,
               main = "new_cases ~ gdp_per_capita",
               xlab = "new_cases",
               ylab = "gdp_per_capita")
cor(covid_Europe$new_cases, covid_Europe$gdp_per_capita)                        #0.06705892

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$extreme_poverty,
               main = "new_cases ~ extreme_poverty",
               xlab = "new_cases",
               ylab = "extreme_poverty")
cor(covid_Europe$new_cases, covid_Europe$extreme_poverty)                       #-0.01016553

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$diabetes_prevalence,
               main = "new_cases ~ diabetes_prevalence",
               xlab = "new_cases",
               ylab = "diabetes_prevalence")
cor(covid_Europe$new_cases, covid_Europe$diabetes_prevalence)                   #-0.005461203

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$female_smokers,
               main = "new_cases ~ female_smokers",
               xlab = "new_cases",
               ylab = "female_smokers")
cor(covid_Europe$new_cases, covid_Europe$female_smokers)                        #0.1429622

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$male_smokers,
               main = "new_cases ~ male_smokers",
               xlab = "new_cases",
               ylab = "male_smokers")
cor(covid_Europe$new_cases, covid_Europe$male_smokers)                          #0.1223959

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$handwashing_facilities,
               main = "new_cases ~ handwashing_facilities",
               xlab = "new_cases",
               ylab = "handwashing_facilities")
cor(covid_Europe$new_cases, covid_Europe$handwashing_facilities)                #-0.05648708

scatter.smooth(x = covid_Europe$new_cases,
               y = covid_Europe$human_development_index,
               main = "new_cases ~ human_development_index",
               xlab = "new_cases",
               ylab = "human_development_index")
cor(covid_Europe$new_cases, covid_Europe$human_development_index)               #0.1324068

paste("Correlation for new_cases and new_deaths: ", cor(covid_Europe$new_cases, covid_Europe$new_deaths))
paste("Correlation for new_cases and reproduction_rate: ", cor(covid_Europe$new_cases, covid_Europe$reproduction_rate))
paste("Correlation for new_cases and icu_patients: ", cor(covid_Europe$new_cases, covid_Europe$icu_patients))
paste("Correlation for new_cases and new_tests: ", cor(covid_Europe$new_cases, covid_Europe$new_tests))
paste("Correlation for new_cases and total_tests: ", cor(covid_Europe$new_cases, covid_Europe$total_tests))
paste("Correlation for new_cases and positive_rate: ", cor(covid_Europe$new_cases, covid_Europe$positive_rate))
paste("Correlation for new_cases and total_vaccinations: ", cor(covid_Europe$new_cases, covid_Europe$total_vaccinations))
paste("Correlation for new_cases and people_fully_vaccinated: ", cor(covid_Europe$new_cases, covid_Europe$people_fully_vaccinated))
paste("Correlation for new_cases and new_vaccinations: ", cor(covid_Europe$new_cases, covid_Europe$new_vaccinations))
paste("Correlation for new_cases and stringency_index: ", cor(covid_Europe$new_cases, covid_Europe$stringency_index))
paste("Correlation for new_cases and population: ", cor(covid_Europe$new_cases, covid_Europe$population))
paste("Correlation for new_cases and population_density: ", cor(covid_Europe$new_cases, covid_Europe$population_density))
paste("Correlation for new_cases and median_age: ", cor(covid_Europe$new_cases, covid_Europe$median_age))
paste("Correlation for new_cases and aged_65_older: ", cor(covid_Europe$new_cases, covid_Europe$aged_65_older))
paste("Correlation for new_cases and aged_70_older: ", cor(covid_Europe$new_cases, covid_Europe$aged_70_older))
paste("Correlation for new_cases and gdp_per_capita: ", cor(covid_Europe$new_cases, covid_Europe$gdp_per_capita))
paste("Correlation for new_cases and extreme_poverty: ", cor(covid_Europe$new_cases, covid_Europe$extreme_poverty))
paste("Correlation for new_cases and diabetes_prevalence: ", cor(covid_Europe$new_cases, covid_Europe$diabetes_prevalence))
paste("Correlation for new_cases and female_smokers: ", cor(covid_Europe$new_cases, covid_Europe$female_smokers))
paste("Correlation for new_cases and male_smokers: ", cor(covid_Europe$new_cases, covid_Europe$male_smokers))
paste("Correlation for new_cases and handwashing_facilities: ", cor(covid_Europe$new_cases, covid_Europe$handwashing_facilities))
paste("Correlation for new_cases and human_development_index: ", cor(covid_Europe$new_cases, covid_Europe$human_development_index))

# Predictor Variable	Correlation
# 
# new_deaths	        0.687630882
# reproduction_rate	0.098601455
# icu_patients	        0.598058405
# new_tests	        0.456584891
# total_tests	        0.380740955
# positive_rate	        0.248784336
# total_vaccinations	0.23991319
# people_fully_vaccinated0.257664029
# new_vaccinations	0.32844493
# stringency_index	0.223390462
# population	        0.491460458
# population_density	-0.057608114
# median_age	        0.17218783
# aged_65_older	        0.190885068
# aged_70_older	        0.199203325
# gdp_per_capita	0.067058919
# extreme_poverty	-0.010165526
# diabetes_prevalence	-0.005461203
# female_smokers	0.14296223
# male_smokers	        0.122395861
# handwashing_facilities-0.056487077
# human_development_index0.132406833

covid_Europe <- subset(covid_Europe, select = -c(population_density,            #As evident from the above table, we see that population_density,
                                                 extreme_poverty,             #extreme_poverty and diabetes_prevalence has got very low correlation.
                                                 diabetes_prevalence,         #Hence these columns can be filtered out
))
#-------------------------------------------------------------------------------
#                       Plotting, removing Outliers

# Outliers are basically few data points which are scattered/positioned away
# from the cluster of data points.
# These outliers can dramatically change the prediction, hence one needs to 
# analyze and decide whether an outlier can be removed.
# Here we are checking the outliers for key variables using boxplots, then 
# we are setting a upper and lower threshold as a criteria to filter out the
# outliers.
# Any point that lies outside 1.5 times the inter quartile range is considered 
# to be an outlier.
# outlier = 1.5 * IQR
# We will also be calculating skewness.
# moderately skewed = -1 to -0.5 to 1
# highly skewed = < -1 or > 1
# -0.5 to 0.5 = approximately symmetrical

# We will be checking outliers only for the following fields since the show
# better correlation values

# new_deaths
# reproduction_rate
# icu_patients
# new_tests
# positive_rate
# new_vaccinations
# stringency_index
# median_age
# aged_65_older
# gdp_per_capita
# female_smokers
# male_smokers
# human_development_index
# population
#-------------------------------------------------------------------------------
#install.packages("e1071")
library(e1071)                                                                  #library userof statistical functions
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))                                                            #graph shown in 1 row by 2 cols
attach(covid_Europe)                                                            #attaching the dataframe for easier variable access

boxplot(new_cases,
        main = "new_cases",
        sub = paste("Outlier rows: ",
                    boxplot.stats(new_cases)$out))                              #plotting boxplot to find the outliers

covid_Europe <- subset(covid_Europe,                                            
                       covid_Europe$new_cases > 0 & covid_Europe$new_cases < 50000     #removing outliers
)

plot(density(covid_Europe$new_cases),
     main = "Density plot : new_cases",
     ylab = "Frequency", xlab = "new_cases",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$new_cases), 2)))
#
boxplot(reproduction_rate,
        main = "reproduction_rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(reproduction_rate)$out)) 

covid_Europe <- subset(covid_Europe,
                       covid_Europe$reproduction_rate > 0 & covid_Europe$reproduction_rate < 3.0
)

plot(density(covid_Europe$reproduction_rate),
     main = "Density plot : reproduction_rate",
     ylab = "Frequency", xlab = "reproduction_rate",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$reproduction_rate), 2)))
#
boxplot(icu_patients,
        main = "icu_patients",
        sub = paste("Outlier rows: ",
                    boxplot.stats(icu_patients)$out)) 

covid_Europe <- subset(covid_Europe,
                       covid_Europe$icu_patients > 0 & covid_Europe$icu_patients < 5500
)

plot(density(covid_Europe$icu_patients),
     main = "Density plot : icu_patients",
     ylab = "Frequency", xlab = "icu_patients",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$icu_patients), 2)))
#
boxplot(new_tests,
        main = "new_tests",
        sub = paste("Outlier rows: ",
                    boxplot.stats(new_tests)$out)) 

covid_Europe <- subset(covid_Europe,
                       covid_Europe$new_tests > 0 & covid_Europe$new_tests < 80000
)

plot(density(covid_Europe$new_tests),
     main = "Density plot : new_tests",
     ylab = "Frequency", xlab = "new_tests",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$new_tests), 2)))
#
boxplot(positive_rate,
        main = "positive_rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(positive_rate)$out))

covid_Europe <- subset(covid_Europe,
                       covid_Europe$positive_rate > 0  & covid_Europe$positive_rate < 0.4
)

plot(density(covid_Europe$positive_rate),
     main = "Density plot : positive_rate",
     ylab = "Frequency", xlab = "positive_rate",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$positive_rate), 2)))
#
boxplot(new_vaccinations,
        main = "new_vaccinations",
        sub = paste("Outlier rows: ",
                    boxplot.stats(new_vaccinations)$out)) 

covid_Europe <- subset(covid_Europe,
                       covid_Europe$new_vaccinations > 0 & covid_Europe$new_vaccinations < 50000
)

plot(density(covid_Europe$new_vaccinations),
     main = "Density plot : new_vaccinations",
     ylab = "Frequency", xlab = "new_vaccinations",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$new_vaccinations), 2)))
#
boxplot(stringency_index,
        main = "stringency_index",
        sub = paste("Outlier rows: ",
                    boxplot.stats(stringency_index)$out)) 

plot(density(covid_Europe$stringency_index),
     main = "Density plot : stringency_index",
     ylab = "Frequency", xlab = "stringency_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$stringency_index), 2)))
#
boxplot(median_age,
        main = "median_age",
        sub = paste("Outlier rows: ",
                    boxplot.stats(median_age)$out)) 

covid_Europe <- subset(covid_Europe,
                       covid_Europe$median_age > 10
)

plot(density(covid_Europe$median_age),
     main = "Density plot : median_age",
     ylab = "Frequency", xlab = "median_age",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$median_age), 2)))
#
boxplot(aged_65_older,
        main = "aged_65_older",
        sub = paste("Outlier rows: ",
                    boxplot.stats(aged_65_older)$out)) 	

covid_Europe <- subset(covid_Europe,
                       covid_Europe$aged_65_older > 5
)

plot(density(covid_Europe$aged_65_older),
     main = "Density plot : aged_65_older",
     ylab = "Frequency", xlab = "aged_65_older",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$aged_65_older), 2)))
#
boxplot(gdp_per_capita,
        main = "gdp_per_capita",
        sub = paste("Outlier rows: ",
                    boxplot.stats(gdp_per_capita)$out))

covid_Europe <- subset(covid_Europe,
                       covid_Europe$gdp_per_capita > 0 & covid_Europe$gdp_per_capita < 60000
)

plot(density(covid_Europe$gdp_per_capita),
     main = "Density plot : gdp_per_capita",
     ylab = "Frequency", xlab = "gdp_per_capita",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$gdp_per_capita), 2)))
#
boxplot(female_smokers,
        main = "female_smokers",
        sub = paste("Outlier rows: ",
                    boxplot.stats(female_smokers)$out))

covid_Europe <- subset(covid_Europe,
                       covid_Europe$female_smokers > 10 & covid_Europe$female_smokers < 40
)

plot(density(covid_Europe$female_smokers),
     main = "Density plot : female_smokers",
     ylab = "Frequency", xlab = "female_smokers",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$female_smokers), 2)))
#
boxplot(male_smokers,
        main = "male_smokers",
        sub = paste("Outlier rows: ",
                    boxplot.stats(male_smokers)$out))

covid_Europe <- subset(covid_Europe,
                       covid_Europe$male_smokers > 20 & covid_Europe$male_smokers < 50
)

plot(density(covid_Europe$male_smokers),
     main = "Density plot : male_smokers",
     ylab = "Frequency", xlab = "male_smokers",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$male_smokers), 2)))
#
boxplot(human_development_index,
        main = "human_development_index",
        sub = paste("Outlier rows: ",
                    boxplot.stats(human_development_index)$out))

covid_Europe <- subset(covid_Europe,
                       covid_Europe$human_development_index > 0.6
)

plot(density(covid_Europe$human_development_index),
     main = "Density plot : human_development_index",
     ylab = "Frequency", xlab = "human_development_index",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$human_development_index), 2)))
#
boxplot(population,
        main = "population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(population)$out))

covid_Europe <- subset(covid_Europe,
                       covid_Europe$population > 20 & covid_Europe$population < 11589616
)

plot(density(covid_Europe$population),
     main = "Density plot : population",
     ylab = "Frequency", xlab = "population",
     sub = paste("Skewness : ", round(e1071::skewness(covid_Europe$population), 2)))

paste("Skewness for new_deaths: ", round(e1071::skewness(covid_Europe$new_deaths), 2))
paste("Skewness for reproduction_rate: ", round(e1071::skewness(covid_Europe$reproduction_rate), 2))
paste("Skewness for icu_patients : ", round(e1071::skewness(covid_Europe$icu_patients),2))
paste("Skewness for new_tests)) : ", round(e1071::skewness(covid_Europe$new_tests),2))
paste("Skewness for positive_rate)) : ", round(e1071::skewness(covid_Europe$positive_rate), 2))
paste("Skewness for new_vaccinations: ", round(e1071::skewness(covid_Europe$new_vaccinations), 2))
paste("Skewness for hosp_patients: ", round(e1071::skewness(covid_Europe$stringency_index), 2))
paste("Skewness for median_age : ", round(e1071::skewness(covid_Europe$median_age),2))
paste("Skewness for aged_65_older)) : ", round(e1071::skewness(covid_Europe$aged_65_older),2))
paste("Skewness for gdp_per_capita)) : ", round(e1071::skewness(covid_Europe$gdp_per_capita), 2))
paste("Skewness for female_smokers: ", round(e1071::skewness(covid_Europe$female_smokers), 2))
paste("Skewness for male_smokers : ", round(e1071::skewness(covid_Europe$male_smokers),2))
paste("Skewness for human_development_index)) : ", round(e1071::skewness(covid_Europe$human_development_index),2))
paste("Skewness for population)) : ", round(e1071::skewness(covid_Europe$population), 2))

# Preditor variable		Skewness
# 
# new_deaths		        1.43
# reproduction_rate		-0.4
# icu_patients		        1.42
# new_tests		        1.2
# positive_rate		        0.1
# new_vaccinations		1.43
# stringency_index		-0.06
# median_age		        0.31
# aged_65_older	        	0.19
# gdp_per_capita		-0.15
# female_smokers		0.28
# male_smokers		        0.09
# human_development_index	-0.65
# population		        0.01

detach(covid_Europe)
#-------------------------------------------------------------------------------
#                           Checking for Normality
#
# We will be using histograms, Q-Q-Norm plots for checking the normality
#-------------------------------------------------------------------------------
attach(covid_Europe)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

hist(new_deaths, 
     main = "Normalility proportion of new_deaths", 
     xlab = "new_deaths",
     col = "#66a3ff")
qqnorm(new_deaths, col="#66a3ff")
qqline(new_deaths, col = "#ff8080")

hist(reproduction_rate, 
     main = "Normalility proportion of reproduction_rate", 
     xlab = "reproduction_rate",
     col = "#66a3ff")
qqnorm(reproduction_rate, col="#66a3ff")
qqline(reproduction_rate, col = "#ff8080")

hist(icu_patients, 
     main = "Normalility proportion of icu_patients", 
     xlab = "icu_patients",
     col = "#66a3ff")
qqnorm(icu_patients, col="#66a3ff")
qqline(icu_patients, col = "#ff8080")

hist(new_tests, 
     main = "Normalility proportion of new_tests", 
     xlab = "new_tests",
     col = "#66a3ff")
qqnorm(new_tests, col="#66a3ff")
qqline(new_tests, col = "#ff8080")

hist(positive_rate, 
     main = "Normalility proportion of positive_rate", 
     xlab = "positive_rate",
     col = "#66a3ff")
qqnorm(positive_rate, col="#66a3ff")
qqline(positive_rate, col = "#ff8080")

hist(new_vaccinations, 
     main = "Normalility proportion of new_vaccinations", 
     xlab = "new_vaccinations",
     col = "#66a3ff")
qqnorm(new_vaccinations, col="#66a3ff")
qqline(new_vaccinations, col = "#ff8080")

hist(stringency_index, 
     main = "Normalility proportion of stringency_index", 
     xlab = "stringency_index",
     col = "#66a3ff")
qqnorm(stringency_index, col="#66a3ff")
qqline(stringency_index, col = "#ff8080")

hist(median_age, 
     main = "Normalility proportion of median_age", 
     xlab = "median_age",
     col = "#66a3ff")
qqnorm(median_age, col="#66a3ff")
qqline(median_age, col = "#ff8080")

hist(aged_65_older, 
     main = "Normalility proportion of aged_65_older", 
     xlab = "aged_65_older",
     col = "#66a3ff")
qqnorm(aged_65_older, col="#66a3ff")
qqline(aged_65_older, col = "#ff8080")

hist(gdp_per_capita, 
     main = "Normalility proportion of gdp_per_capita", 
     xlab = "gdp_per_capita",
     col = "#66a3ff")
qqnorm(gdp_per_capita, col="#66a3ff")
qqline(gdp_per_capita, col = "#ff8080")

hist(female_smokers, 
     main = "Normalility proportion of female_smokers", 
     xlab = "female_smokers",
     col = "#66a3ff")
qqnorm(female_smokers, col="#66a3ff")
qqline(female_smokers, col = "#ff8080")

hist(male_smokers, 
     main = "Normalility proportion of female_smokers", 
     xlab = "male_smokers",
     col = "#66a3ff")
qqnorm(male_smokers, col="#66a3ff")
qqline(male_smokers, col = "#ff8080")

hist(human_development_index, 
     main = "Normalility proportion of human_development_index", 
     xlab = "human_development_index",
     col = "#66a3ff")
qqnorm(human_development_index, col="#66a3ff")
qqline(human_development_index, col = "#ff8080")

hist(population, 
     main = "Normalility proportion of population", 
     xlab = "population",
     col = "#66a3ff")
qqnorm(population, col="#66a3ff")
qqline(population, col = "#ff8080")
#-------------------------------------------------------------------------------
#                               REGRESSION MODEL
#
# 70% of data will be used for training set
# 30% will be used for training set
#-------------------------------------------------------------------------------
set.seed(1)
row_count <- nrow(covid_Europe)                                                 #storing the count of rows in row_count dataframe
sample_records <- sample(1:row_count, 
                         size = round(0.7 * row_count), 
                         replace = FALSE)
training_data <- covid_Europe[sample_records, ]                                 #70% data
testing_data <- covid_Europe[-sample_records, ]                                 #30% data

fit <- lm(new_cases ~ new_deaths +
            reproduction_rate +
            icu_patients +
            new_tests +
            positive_rate +
            new_vaccinations +
            stringency_index +
            median_age +
            aged_65_older +
            gdp_per_capita +
            female_smokers +
            male_smokers +
            human_development_index 
          , data=training_data)                                                 # the training data is stored in the fit variable

summary(fit)                                                                    #displaying summary of the model

#####################################OUTPUT#####################################
# Call:
#         lm(formula = new_cases ~ new_deaths + reproduction_rate + icu_patients + 
#                    new_tests + positive_rate + new_vaccinations + stringency_index + 
#                    median_age + aged_65_older + gdp_per_capita + female_smokers + 
#                    male_smokers + human_development_index, data = training_data)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -5295.5  -528.3    58.8   597.0  6420.8 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -5.046e+05  1.136e+05  -4.440 1.23e-05 ***
#         new_deaths               3.387e+01  2.669e+00  12.692  < 2e-16 ***    #*** signifies that they have a high correlation between the variables
#         reproduction_rate        4.953e+03  5.258e+02   9.420  < 2e-16 ***
#         icu_patients            -3.605e-01  5.756e-01  -0.626 0.531565    
# new_tests                2.514e-02  8.235e-03   3.053 0.002451 ** 
#         positive_rate            6.848e+03  2.256e+03   3.035 0.002595 ** 
#         new_vaccinations         8.309e-03  9.834e-03   0.845 0.398769    
# stringency_index        -5.649e+00  1.442e+01  -0.392 0.695464    
# median_age               3.182e+03  7.787e+02   4.086 5.51e-05 ***
#         aged_65_older            3.188e+03  8.201e+02   3.888 0.000122 ***
#         gdp_per_capita          -6.272e-01  1.504e-01  -4.171 3.87e-05 ***
#         female_smokers           3.045e+02  1.126e+02   2.703 0.007225 ** 
#         male_smokers             7.010e+02  1.399e+02   5.011 8.82e-07 ***
#         human_development_index  3.193e+05  6.948e+04   4.596 6.15e-06 ***
#         ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1320 on 331 degrees of freedom
# Multiple R-squared:  0.8617,	Adjusted R-squared:  0.8563 
# F-statistic: 158.7 on 13 and 331 DF,  p-value: < 2.2e-16
################################################################################

confint(fit)                                                                    # Analyzing the confidence interval of the model

#####################################OUTPUT#####################################
# 2.5 %        97.5 %
# (Intercept)              -7.281319e+05 -2.810218e+05
# new_deaths               2.862284e+01  3.912304e+01
# reproduction_rate        3.918745e+03  5.987425e+03
# icu_patients            -1.492834e+00  7.718380e-01
# new_tests                8.940832e-03  4.134133e-02
# positive_rate            2.409554e+03  1.128654e+04
# new_vaccinations        -1.103563e-02  2.765267e-02
# stringency_index        -3.401368e+01  2.271518e+01
# median_age               1.650120e+03  4.713744e+03
# aged_65_older            1.574859e+03  4.801288e+03
# gdp_per_capita          -9.229735e-01 -3.314076e-01
# female_smokers           8.289787e+01  5.260915e+02
# male_smokers             4.257978e+02  9.761031e+02
# human_development_index  1.826306e+05  4.560009e+05
################################################################################

#install.packages("ggplot2")   
library(ggplot2)
library(car)
qqPlot(fit, labels=row.names(location),                                         #Plots empirical quantiles of studentized residuals from a linear model against theoretical quantiles of a comparison distribution
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot")                                                         

training_data[4145,]                                                            #There are two outliers which are identified, which will be analyzed and decided whether to keep them or remove them
training_data[4138,]                                                            #training the outlier data and analyzing whether it will affect or not

fitted(fit)[4145]                                                               #fitting the outlier data to a model
fitted(fit)[4138]

covid_Europe <- subset(covid_Europe,
                       covid_Europe$people_fully_vaccinated != "NA"
                       & covid_Europe$icu_patients  != "NA"
                       & covid_Europe$human_development_index  != "NA")        #The outliers can be easily removed since they wont affect the entire model

set.seed(1)
row_count <- nrow(covid_Europe)
sample_records <- sample(1:row_count, 
                         size = round(0.7 * row_count), 
                         replace = FALSE)
training_data <- covid_Europe[sample_records, ]
testing_data <- covid_Europe[-sample_records, ]

fit <- lm(new_cases ~ new_deaths +
            reproduction_rate +
            icu_patients +
            new_tests +
            new_vaccinations +
            stringency_index +
            median_age +
            aged_65_older +
            gdp_per_capita +
            female_smokers +
            male_smokers +
            human_development_index , 
          data=training_data)
outlierTest(fit)

student_fit <- rstudent(fit)                                                    #histogram of the studentized residuals and superimposes a normal curve, kernel-density curve, and rug plot
hist(student_fit,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")
rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE,col="#66a3ff", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="#ff8080", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("#66a3ff","#ff8080"), cex=.7)

library(car)                                                                    #importing the car library
crPlots(fit)                                                                    #Used to visualize linear relationship between the dependent and independent variable through a linear line
cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)                #using cooks distance formula on trained data
plot(fit, which = 4, cook.levels = cutoff)                                      #plotting the graph
abline(h = cutoff, lty = 2, col = "red")                                        #cutoff line which indicates that the data above the line can be ignored
avPlots(fit, ask=FALSE)                                                         #showing regression coefficient of the predictor variables
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")             #The influence plot clearly depicts the outliers that are close to each other
ncvTest(fit)                                                                    #check for Homoscedasticity, generates the result for the hypothesis of constant error variance with a fitted model data
#####################################OUTPUT######################################The score test is non significant
# Variance formula: ~ fitted.values                                             #p-value < 0.05, then the error variance value may change (Homoscedasticity)
# Chisquare = 315.4378, Df = 1, p = < 2.22e-16                                  #p-value > 0.05, then the error variance value may not change (Heteroscedasticity)
################################################################################

spreadLevelPlot(fit)                                                            #Suggested power transformation:  0.5404465
#shows the scatter plot of the standardized residuals versus the fitted model values & draws the line which best fits the data
#install.packages("gvlma")
library(gvlma)                                                                  #determines global validation of linear model assumptions
#install.packages("mice")
library(mice)                                                                   #importing mice library
gvmodel <- gvlma(fit)                                                           
summary(gvmodel)
#####################################OUTPUT#####################################
# Value   p-value                   Decision
# Global Stat                 734.4273 0.000e+00  Assumptions NOT satisfied!
#         Skewness            27.1734 1.860e-07   Assumptions NOT satisfied!
#         Kurtosis            603.1889 0.000e+00  Assumptions NOT satisfied!
#         Link Function       103.9062 0.000e+00  Assumptions NOT satisfied!
#         Heteroscedasticity  0.1587 6.904e-01    Assumptions acceptable.
################################################################################
vif(fit)                                                                        #checking for Multicollinearity
sqrt(vif(fit)) > 2
#####################################OUTPUT######################################many variables has multicollinearity since the value is greater than 2
# new_deaths       reproduction_rate            icu_patients               new_tests        new_vaccinations        stringency_index 
# 4.428581                1.738257               11.270347                5.218601                1.982900                6.075218 
# median_age           aged_65_older          gdp_per_capita          female_smokers            male_smokers human_development_index 
# 153.239582              109.238925              154.623385               58.118080              178.215218             1080.215666 

# new_deaths       reproduction_rate            icu_patients               new_tests        new_vaccinations        stringency_index 
# TRUE                   FALSE                    TRUE                    TRUE                   FALSE                    TRUE 
# median_age           aged_65_older          gdp_per_capita          female_smokers            male_smokers human_development_index 
# TRUE                    TRUE                    TRUE                    TRUE                    TRUE                    TRUE 
################################################################################
summary(powerTransform(training_data$new_cases))
sqrt_transform_new_cases <- sqrt(training_data$new_cases)
training_data$new_cases_sqrt <- sqrt_transform_new_cases
training_data$new_cases_sqrt

fit_model_test1 <- lm(new_cases ~ new_deaths +
                        reproduction_rate +
                        icu_patients +
                        new_tests +
                        new_vaccinations +
                        stringency_index +
                        median_age +
                        aged_65_older +
                        gdp_per_capita +
                        female_smokers +
                        male_smokers +
                        human_development_index, 
                      data=training_data)
fit_model_test2 <- lm(new_cases_sqrt ~ new_deaths +
                        reproduction_rate +
                        icu_patients +
                        new_tests +
                        new_vaccinations +
                        stringency_index +
                        median_age +
                        aged_65_older +
                        gdp_per_capita +
                        female_smokers +
                        male_smokers +
                        human_development_index, 
                      data=training_data)
AIC(fit_model_test1,fit_model_test2)
spreadLevelPlot(fit_model_test1)
spreadLevelPlot(fit_model_test2)
#####################################OUTPUT#####################################
# fit_model_test1 14 5960.010
# fit_model_test2 14 2495.249

# Suggested power transformation:  0.5404465
# Suggested power transformation:  0.5277926 
################################################################################
library(MASS)
fit_model_test3 <- lm(new_cases ~ new_deaths +                                  #Evaluating the model using STEPWISE Regression - Backward
                        reproduction_rate +
                        icu_patients +
                        new_tests +
                        new_vaccinations +
                        stringency_index +
                        median_age +
                        aged_65_older +
                        gdp_per_capita +
                        female_smokers +
                        male_smokers +
                        human_development_index, data=training_data)
stepAIC(fit_model_test3, direction="backward")

library(leaps)
leaps <-regsubsets(new_cases ~ new_deaths +                                     #The leap plot shows the best correlation of the variables with the score of R-squared and Adjusted R-squared values on the y-axis.
                     reproduction_rate +
                     icu_patients +
                     new_tests +
                     new_vaccinations +
                     stringency_index +
                     median_age +
                     aged_65_older +
                     gdp_per_capita +
                     female_smokers +
                     male_smokers +
                     human_development_index, data=training_data, nbest=4)
plot(leaps, scale="adjr2")                                                      # bottom row has adjusted R-squared = 0.13.


library(MASS)
fit_model_test3 <- lm(new_cases_sqrt ~ new_deaths +
                        reproduction_rate +
                        icu_patients +
                        new_tests +
                        new_vaccinations +
                        stringency_index +
                        median_age +
                        aged_65_older +
                        gdp_per_capita +
                        female_smokers +
                        male_smokers +
                        human_development_index, data=training_data)
stepAIC(fit_model_test3, direction="backward")


library(leaps)
leaps <-regsubsets(new_cases_sqrt ~ new_deaths +
                     reproduction_rate +
                     icu_patients +
                     new_tests +
                     new_vaccinations +
                     stringency_index +
                     median_age +
                     aged_65_older +
                     gdp_per_capita +
                     female_smokers +
                     male_smokers +
                     human_development_index, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

fit_model <- lm(new_cases ~ new_deaths +                                        #Examining predicted accuracy
                  reproduction_rate +
                  icu_patients +
                  new_tests +
                  new_vaccinations +
                  stringency_index +
                  median_age +
                  aged_65_older +
                  gdp_per_capita +
                  female_smokers +
                  male_smokers +
                  human_development_index, data=training_data)

fit_model_sqrt <- lm(new_cases_sqrt ~ new_deaths +
                       reproduction_rate +
                       icu_patients +
                       new_tests +
                       new_vaccinations +
                       stringency_index +
                       median_age +
                       aged_65_older +
                       gdp_per_capita +
                       female_smokers +
                       male_smokers +
                       human_development_index, data=training_data)

predicted_new_cases <- predict(fit_model, testing_data)
predicted_new_cases_sqrt <- predict(fit_model_sqrt, testing_data)
converted_new_cases_sqrt <- predicted_new_cases_sqrt ^2
converted_new_cases_sqrt
actuals_predictions <- data.frame(cbind(actuals = testing_data$new_cases, predicted = predicted_new_cases))
head(actuals_predictions)
actuals_predictions_sqrt <- data.frame(cbind(actuals = testing_data$new_cases, predicted = converted_new_cases_sqrt))
head(actuals_predictions_sqrt)
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
#####################################OUTPUT#####################################
#         actuals        predicted
# actuals   1.0000000     0.9469763                                             #model has 94% of correlation accuracy
# predicted 0.9469763     1.0000000
################################################################################
correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy
#####################################OUTPUT#####################################
#            actuals  predicted
# actuals   1.000000  0.957062                                                  #model has 95% of correlation accuracy
# predicted 0.957062  1.000000
################################################################################
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
min_max_accuracy                                                                #0.4160489
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) / apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy                                                                #0.700202
sigma(fit_model)/ mean(testing_data$new_cases)                                  #0.4153718 - Residual Standard Error (RSE), or sigma
sigma(fit_model_sqrt)/ mean(testing_data$new_cases)                             #0.002739518 - 
detach (covid_Europe)
################################################################################
attach (covid_Europe)
summary(covid_Europe)
summary(covid_Europe$male_smokers)
df1 <- data.frame(new_deaths = c(10),
                  reproduction_rate = c(1),
                  icu_patients = c(70),
                  new_tests = c(5400),
                  new_vaccinations = c(2000),
                  stringency_index = c(55),
                  median_age = c(45),
                  aged_65_older = c(20),
                  gdp_per_capita = c(28000),
                  female_smokers = c(20),
                  male_smokers= c(22),
                  human_development_index = c(0.8920))
predicted_new_cases <- predict(fit_model, df1)
predicted_new_cases

df2 <- data.frame(,
                  new_tests = c(5400),
                  new_vaccinations = c(2000),
                  
                  gdp_per_capita = c(28000)
)
predicted_new_cases <- predict(fit_model, df2)
predicted_new_cases
predicted_new_cases <- predict(fit_model_sqrt, df2)
predicted_new_cases
