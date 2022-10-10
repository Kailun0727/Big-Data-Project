install.packages("readxl")
install.packages("parallel")
install.packages("foreach")
install.packages("doParallel")
library(readxl)
library(parallel)
library(foreach)
library(doParallel)

seq_load_dataset  <- function(i) {
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
  
  seq_end_time - seq_start_time
}

par_load_dataset  <- function(i) {
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
  
  par_end_time - par_start_time
}

#Sequential Processing
seq_load_dataset()


#Parallel Processing
registerDoParallel(8)  
foreach (i=1:1) %dopar% {
  par_load_dataset()
}

