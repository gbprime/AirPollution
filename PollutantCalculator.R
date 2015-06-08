## Builds and returns a data frame with the proper information contained in it.
## data: is the data frame object we are returning
## directory: string representing the dir we need to iterate through
## id: vector containging the desired monitors
populateDataFrame <- function(data, directory, id) {
  slash <- "/"
  fileExt <- ".csv"
  newID <- ""
  
  # Need to build my own path since I don't want to consume an entire list
  # of files in the directory.
  for (counter in seq_along(id)) {
    if (id[counter] <= 9) {
      newID <- paste("00", id[counter], sep="")
    } 
    else if (id[counter] >= 10 & id[counter] <= 99) {
      newID <- paste("0", id[counter], sep="")
    } 
    else {
      newID <- id[counter]
    }
    
    fileInfo <- paste(".", slash, directory, slash, newID, fileExt, sep = "")
    
    # Only bind to data frame if the monitor is in the list.
    # This reduces time and space complexities
    data <- rbind(data, read.csv(fileInfo, comment.char = ""))
  }
  # return the data
  data
}

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