source("util.R")

## 'pollutantMean' calculates the mean of a pollutant (sulfate or nitrate) 
## across a specified list of monitors. The function 'pollutantmean' takes 
## three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor 
## ID numbers, 'pollutantMean' reads that monitors' particulate matter data from 
## the directory specified in the 'directory' argument and returns the mean of the 
## pollutant across all of the monitors, ignoring any missing values coded as NA
##
##
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
##
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
##
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
##
## Returns the non-rounded mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
## 
pollutantmean <- function(directory, pollutant, id = 1:332) {
  data <- data.frame();
  data <- populateDataFrame(data, directory, id)
  
  # obtaining the desired monitors from the data frame
  neededMonitors <- subset(data, ID %in% id);
  
  # calculatin the mean values of the desired column/pollutant while removing
  # all unwanted values (e.g. NA)
  pollutantMean <- mean(neededMonitors[[pollutant]], na.rm = TRUE);
  
  # returning the mean value
  pollutantMean;
}