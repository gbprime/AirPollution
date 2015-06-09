## Builds and returns a data frame with the proper information contained in it.
## This function only reads/loads data from the monitors that are needed.
##
## data: is the data frame object we are returning
## directory: string representing the dir we need to iterate through
## id: vector containging the desired monitors
populateDataFrame <- function(data, directory, id) {
  # variables used to build a new path based on the items needed
  # as described by the id vector
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