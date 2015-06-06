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
pollutantMean <- function(directory, pollutant, id = 1:332) {
  
  slash <- "/"
  fileExt <- ".csv"
  newID <- ""
  if (id <= 9) {
    newID <- paste("00", id, sep="")
  }
  
  print(newID)
  
  fileInfo <- paste(".", slash, directory, slash, newID, fileExt, sep = "")
  print(fileInfo)
  
  pollutantData <- read.csv(file=fileInfo, header=TRUE, sep=",")
  ##print(pollutantData)
  
  numberOfRows = nrow(pollutantData)
  
  switch(pollutant,
         sulfate = {
           
         },
         nitrate = {
           
         },
         {
           print("Wrong Parameter for pollutant " + pollutant)
         }
         )
  
  
}

pollutantmean2 <- function(directory, pollutant, id = 1:332) {
  data <- data.frame();
  files <- list.files(directory, full.names = TRUE);
  
  for (index in files) {
    data <- rbind(data, read.csv(index, comment.char = ""))
  }
  
  neededMonitors <- subset(data, ID %in% id);
  pollutantMean <- mean(neededMonitors[[pollutant]], na.rm = TRUE);
  pollutantMean;
}

