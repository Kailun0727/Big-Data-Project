install.packages("readxl")
install.packages("parallel")
install.packages("foreach")
install.packages("doParallel")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
library(ggpubr)
library(readxl)
library(parallel)
library(foreach)
library(doParallel)
library(ggplot2)
library(dplyr)

#Sequential Processing
#Runtime of code
seq_start_time <- Sys.time()

#Import file
seq_dec_osward <- read.csv("Dec_osward_grocery.csv")
seq_jan_osward <- read.csv("Jan_osward_grocery.csv")
seq_feb_osward <- read.csv("Feb_osward_grocery.csv")

#Save specific column
seq_dec_osward_cleaned <- seq_dec_osward[,c("area_id","num_transactions","male", "female")]
seq_jan_osward_cleaned <- seq_jan_osward[,c("area_id","num_transactions","male", "female")]
seq_feb_osward_cleaned <- seq_feb_osward[,c("area_id","num_transactions","male", "female")]

seq_end_time <- Sys.time()

seq_runtime <- seq_end_time - seq_start_time


#Parallel Processing
parallel<-detectCores()
registerDoParallel(parallel)  
n.cores <- parallel::detectCores() - 1

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

foreach::getDoParRegistered()
foreach::getDoParWorkers()

foreach (i=1:1) %dopar% {

  #Runtime of code
  par_start_time <- Sys.time()
  
  #Import file
  par_dec_osward <- read.csv("Dec_osward_grocery.csv")
  par_jan_osward <- read.csv("Jan_osward_grocery.csv")
  par_feb_osward <- read.csv("Feb_osward_grocery.csv")
  
  #Save specific column
  par_dec_osward_cleaned <- par_dec_osward[,c("area_id","num_transactions","male", "female")]
  par_jan_osward_cleaned <- par_jan_osward[,c("area_id","num_transactions","male", "female")]
  par_feb_osward_cleaned <- par_feb_osward[,c("area_id","num_transactions","male", "female")]
  
  par_end_time <- Sys.time()
  
  par_runtime <- par_end_time - par_start_time
}


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


#Store value into vector
dec_trans_mean_vector <- c(dec_trans_mean,jan_trans_mean,feb_trans_mean)
dec_male_mean_vector <- c(dec_male_mean,jan_male_mean,feb_male_mean)
dec_female_mean_vector <- c(dec_female_mean,jan_female_mean,feb_female_mean)


#Store vector into data frame for visualization purpose
trans_mean_df <- data.frame(trans_mean_vector,male_mean_vector,female_mean_vector)

View(trans_mean_df)

#Visualization (num_transactions, male)
#box plot
ggboxplot(trans_mean_df, x= "trans_mean_vector",y = "male_mean_vector", xlab="Number of transactions", ylab="Mean of male", width = 0.5)

ggboxplot(trans_mean_df, x= "trans_mean_vector",y = "female_mean_vector", xlab="Number of transactions", ylab="Mean of female",width = 0.5)


