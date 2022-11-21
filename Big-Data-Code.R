install.packages("readxl")
install.packages("parallel")
install.packages("foreach")
install.packages("doParallel")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("microbenchmark")
install.packages("tidyverse")
library(tidyverse)
library(microbenchmark)
library(ggpubr)
library(parallel)
library(doParallel)
library(ggplot2)
library(dplyr)

setwd("/Users/User/Big-Data-Project/Big-Data-Project/Dataset")

#Sequential Processing
df <-
  list.files(path = "/Users/User/Big-Data-Project/Big-Data-Project/Dataset", pattern = "*.csv") %>%
  map_df(~read_csv(.))
df

f<- function(i){
  list.files(path = "/Users/User/Big-Data-Project/Big-Data-Project/Dataset", pattern = "*.csv") %>%
    map_df(~read_csv(.))
}

mbm_seq <- microbenchmark(lapply(1:1,f),times = 1L)

boxplot(mbm_seq,xlab = "Sequential Processing",
        ylab = "Runtime (seconds)")


#Parallel processing
numCores <- detectCores()

cl <- makeCluster(numCores)

clusterEvalQ(cl, {
  library(tidyverse)
  library(microbenchmark)
  library(ggpubr)
  library(parallel)
  library(doParallel)
  library(ggplot2)
  library(dplyr)
})

mbm_par <- microbenchmark(parLapply(cl, 1:1, f),times = 1L)

stopCluster(cl)

boxplot(mbm_par,xlab = "Parallel Processing",
        ylab = "Runtime (seconds)")




#Sequential Processing
#Runtime of code

#Import file
seq_dec_osward <- read.csv("Dec_osward_grocery.csv")
seq_jan_osward <- read.csv("Jan_osward_grocery.csv")
seq_feb_osward <- read.csv("Feb_osward_grocery.csv")

#Save specific column
seq_dec_osward_cleaned <- seq_dec_osward[,c("area_id","num_transactions","male", "female","age_18_64")]
seq_jan_osward_cleaned <- seq_jan_osward[,c("area_id","num_transactions","male", "female","age_18_64")]
seq_feb_osward_cleaned <- seq_feb_osward[,c("area_id","num_transactions","male", "female","age_18_64")]


#Descriptive Analysis

#Mean
#mean of number of transactions 
dec_trans_mean <- mean(seq_dec_osward_cleaned$num_transactions)
jan_trans_mean <- mean(seq_jan_osward_cleaned$num_transactions)
feb_trans_mean <- mean(seq_feb_osward_cleaned$num_transactions)

#mean of male 
dec_male_mean <- mean(seq_dec_osward_cleaned$male)
jan_male_mean <- mean(seq_jan_osward_cleaned$male)
feb_male_mean <- mean(seq_feb_osward_cleaned$male)

#mean of female 
dec_female_mean <- mean(seq_dec_osward_cleaned$female)
jan_female_mean <- mean(seq_jan_osward_cleaned$female)
feb_female_mean <- mean(seq_feb_osward_cleaned$female)

#mean of age_18_64
dec_age_18_64_mean <- mean(seq_dec_osward_cleaned$age_18_64)
jan_age_18_64_mean <- mean(seq_jan_osward_cleaned$age_18_64)
feb_age_18_64_mean <- mean(seq_feb_osward_cleaned$age_18_64)

#Median
#median of number of transactions
dec_trans_median <- median(seq_dec_osward_cleaned$num_transactions)
jan_trans_median <- median(seq_jan_osward_cleaned$num_transactions)
feb_trans_median <- median(seq_feb_osward_cleaned$num_transactions)

#median of male 
dec_male_median <- median(seq_dec_osward_cleaned$male)
jan_male_median <- median(seq_jan_osward_cleaned$male)
feb_male_median <- median(seq_feb_osward_cleaned$male)

#median of female 
dec_female_median <- median(seq_dec_osward_cleaned$female)
jan_female_median <- median(seq_jan_osward_cleaned$female)
feb_female_median <- median(seq_feb_osward_cleaned$female)

#median of age_18_64
dec_age_18_64_median <- median(seq_dec_osward_cleaned$age_18_64)
jan_age_18_64_median <- median(seq_jan_osward_cleaned$age_18_64)
feb_age_18_64_median <- median(seq_feb_osward_cleaned$age_18_64)

#Max
#max of transaction
dec_trans_max <- max(seq_dec_osward_cleaned$num_transactions)
jan_trans_max <- max(seq_jan_osward_cleaned$num_transactions)
feb_trans_max <- max(seq_feb_osward_cleaned$num_transactions)

#max of male
dec_male_max <- max(seq_dec_osward_cleaned$male)
jan_male_max <- max(seq_jan_osward_cleaned$male)
feb_male_max <- max(seq_feb_osward_cleaned$male)

#max of female
dec_female_max <- max(seq_dec_osward_cleaned$female)
jan_female_max <- max(seq_jan_osward_cleaned$female)
feb_female_max <- max(seq_feb_osward_cleaned$female)

#max of age_18_64
dec_age_18_64_max <- max(seq_dec_osward_cleaned$age_18_64)
jan_age_18_64_max <- max(seq_jan_osward_cleaned$age_18_64)
feb_age_18_64_max <- max(seq_feb_osward_cleaned$age_18_64)

#Min
#min of transaction
dec_trans_min <- min(seq_dec_osward_cleaned$num_transactions)
jan_trans_min <- min(seq_jan_osward_cleaned$num_transactions)
feb_trans_min <- min(seq_feb_osward_cleaned$num_transactions)

#min of male
dec_male_min <- min(seq_dec_osward_cleaned$male)
jan_male_min <- min(seq_jan_osward_cleaned$male)
feb_male_min <- min(seq_feb_osward_cleaned$male)

#min of female
dec_female_min <- min(seq_dec_osward_cleaned$female)
jan_female_min <- min(seq_jan_osward_cleaned$female)
feb_female_min <- min(seq_feb_osward_cleaned$female)

#min of age_18_64
dec_age_18_64_min <- min(seq_dec_osward_cleaned$age_18_64)
jan_age_18_64_min <- min(seq_jan_osward_cleaned$age_18_64)
feb_age_18_64_min <- min(seq_feb_osward_cleaned$age_18_64)


#Standard Deviation
dec_trans_sd <- sd(seq_dec_osward_cleaned$num_transactions)
jan_trans_sd <- sd(seq_jan_osward_cleaned$num_transactions)
feb_trans_sd <- sd(seq_feb_osward_cleaned$num_transactions)

dec_male_sd <- sd(seq_dec_osward_cleaned$male)
jan_male_sd <- sd(seq_jan_osward_cleaned$male)
feb_male_sd <- sd(seq_feb_osward_cleaned$male)

dec_female_sd <- sd(seq_dec_osward_cleaned$female)
jan_female_sd <- sd(seq_jan_osward_cleaned$female)
feb_female_sd <- sd(seq_feb_osward_cleaned$female)

dec_age_18_64_sd <- sd(seq_dec_osward_cleaned$age_18_64)
jan_age_18_64_sd <- sd(seq_jan_osward_cleaned$age_18_64)
feb_age_18_64_sd <- sd(seq_feb_osward_cleaned$age_18_64)


#Store mean value into vector
trans_mean_vector <- c(dec_trans_mean,jan_trans_mean,feb_trans_mean)
male_mean_vector <- c(dec_male_mean,jan_male_mean,feb_male_mean)
female_mean_vector <- c(dec_female_mean,jan_female_mean,feb_female_mean)
age_18_64_mean_vector <- c(dec_age_18_64_mean,jan_age_18_64_mean,feb_age_18_64_mean)

#Store median value into vector
trans_median_vector <- c(dec_trans_median,jan_trans_median,feb_trans_median)
male_median_vector <- c(dec_male_median,jan_male_median,feb_male_median)
female_median_vector <- c(dec_female_median,jan_female_median,feb_female_median)
age_18_64_median_vector <- c(dec_age_18_64_median,jan_age_18_64_median,feb_age_18_64_median)

#Store max value into vector
trans_max_vector <- c(dec_trans_max,jan_trans_max,feb_trans_max)
male_max_vector <- c(dec_male_max,jan_male_max,feb_male_max)
female_max_vector <- c(dec_female_max,jan_female_max,feb_female_max)
age_18_64_max_vector <- c(dec_age_18_64_max,jan_age_18_64_max,feb_age_18_64_max)

#Store min value into vector
trans_min_vector <- c(dec_trans_min,jan_trans_min,feb_trans_min)
male_min_vector <- c(dec_male_min,jan_male_min,feb_male_min)
female_min_vector <- c(dec_female_min,jan_female_min,feb_female_min)
age_18_64_min_vector <- c(dec_age_18_64_min,jan_age_18_64_min,feb_age_18_64_min)

#Store standard deviation value into vector
trans_sd_vector <- c(dec_trans_sd,jan_trans_sd,feb_trans_sd)
male_sd_vector <- c(dec_male_sd,jan_male_sd,feb_male_sd)
female_sd_vector <- c(dec_female_sd,jan_female_sd,feb_female_sd)
age_18_64_sd_vector <- c(dec_age_18_64_sd,jan_age_18_64_sd,feb_age_18_64_sd)

#Store vector into data frame for visualization purpose
trans_mean_df <- data.frame(trans_mean_vector,male_mean_vector,female_mean_vector,age_18_64_mean_vector)
trans_median_df <- data.frame(trans_median_vector,male_median_vector,female_median_vector,age_18_64_median_vector)
trans_max_df <- data.frame(trans_max_vector,male_max_vector,female_max_vector,age_18_64_max_vector)
trans_min_df <- data.frame(trans_min_vector,male_min_vector,female_min_vector,age_18_64_min_vector)
trans_sd_df <- data.frame(trans_sd_vector,male_sd_vector,female_sd_vector,age_18_64_sd_vector)

View(trans_mean_df)

#Visualization of mean
#box plot
ggboxplot(trans_mean_df, x= "trans_mean_vector",y = "male_mean_vector", xlab="Number of transactions", ylab="Mean of male", width = 0.5)

ggboxplot(trans_mean_df, x= "trans_mean_vector",y = "female_mean_vector", xlab="Number of transactions", ylab="Mean of female",width = 0.5)

ggboxplot(trans_mean_df, x= "trans_mean_vector",y = "age_18_64_mean_vector", xlab="Number of transactions", ylab="Mean of age_18_64",width = 0.5)

#Visualization of median
#box plot
ggboxplot(trans_median_df, x= "trans_median_vector",y = "male_median_vector", xlab="Number of transactions", ylab="Median of male", width = 0.5)

ggboxplot(trans_median_df, x= "trans_median_vector",y = "female_median_vector", xlab="Number of transactions", ylab="Median of female",width = 0.5)

ggboxplot(trans_median_df, x= "trans_median_vector",y = "age_18_64_median_vector", xlab="Number of transactions", ylab="Median of age_18_64",width = 0.5)

#Visualization of max
#box plot
ggboxplot(trans_max_df, x= "trans_max_vector",y = "male_max_vector", xlab="Number of transactions", ylab="Max of male", width = 0.5)

ggboxplot(trans_max_df, x= "trans_max_vector",y = "female_max_vector", xlab="Number of transactions", ylab="Max of female",width = 0.5)

ggboxplot(trans_max_df, x= "trans_max_vector",y = "age_18_64_max_vector", xlab="Number of transactions", ylab="Max of age_18_64",width = 0.5)

#Visualization of min
#box plot
ggboxplot(trans_min_df, x= "trans_min_vector",y = "male_min_vector", xlab="Number of transactions", ylab="Min of male", width = 0.5)

ggboxplot(trans_min_df, x= "trans_min_vector",y = "female_min_vector", xlab="Number of transactions", ylab="Min of female",width = 0.5)

ggboxplot(trans_min_df, x= "trans_min_vector",y = "age_18_64_min_vector", xlab="Number of transactions", ylab="Min of age_18_64",width = 0.5)


#Visualization of standard deviation
#box plot
ggboxplot(trans_sd_df, x= "trans_sd_vector",y = "male_sd_vector", xlab="Number of transactions", ylab="Standard deviation of male", width = 0.5)

ggboxplot(trans_sd_df, x= "trans_sd_vector",y = "female_sd_vector", xlab="Number of transactions", ylab="Standard deviation of female",width = 0.5)

ggboxplot(trans_sd_df, x= "trans_sd_vector",y = "age_18_64_sd_vector", xlab="Number of transactions", ylab="Standard deviation of age_18_64",width = 0.5)

#Correlation test
#Dec 

cor.test(seq_dec_osward_cleaned$num_transactions, seq_dec_osward_cleaned$male)
cor.test(seq_dec_osward_cleaned$num_transactions, seq_dec_osward_cleaned$female)
cor.test(seq_dec_osward_cleaned$num_transactions, seq_dec_osward_cleaned$age_18_64)

#Jan
cor.test(seq_jan_osward_cleaned$num_transactions, seq_jan_osward_cleaned$male)
cor.test(seq_jan_osward_cleaned$num_transactions, seq_jan_osward_cleaned$female)
cor.test(seq_jan_osward_cleaned$num_transactions, seq_jan_osward_cleaned$age_18_64)

#Feb
cor.test(seq_feb_osward_cleaned$num_transactions, seq_feb_osward_cleaned$male)
cor.test(seq_feb_osward_cleaned$num_transactions, seq_feb_osward_cleaned$female)
cor.test(seq_feb_osward_cleaned$num_transactions, seq_feb_osward_cleaned$age_18_64)


#Hypothesis testing (T-test)
dataFrame <-
    list.files(path = "/Users/User/Big-Data-Project/Big-Data-Project", pattern = "*.csv") %>%
    map_df(~read_csv(.))

View(dataFrame)


# Create a linear regression model
install.packages("caTools")
install.packages("partykit")
library(partykit)
library(caTools)
dataFrameCleaned <- data.frame(dataFrame$num_transactions,dataFrame$male,dataFrame$female)

sample_data = sample.split(dataFrameCleaned, SplitRatio = 0.8)
train_data <- subset(dataFrameCleaned, sample_data == TRUE)
test_data <- subset(dataFrameCleaned, sample_data == FALSE)




model <- lm(dataFrame.num_transactions ~ dataFrame.male + dataFrame.female,dataFrameCleaned)
# Print regression model
print(model)
print(summary(model))


View(dataFrameCleaned)

