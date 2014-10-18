complete <- function(directory, id = 1:332, naRemove=TRUE) {
  fileName <- paste("c:/a511400",sprintf("%s/%03d.csv",directory,id),sep='/')
  compData <- data.frame('id' = numeric(), 'nobs' = numeric())
  #n<-numeric()
  for(i in 1:length(id))
  {
    csvReader <- read.csv(fileName[i], header=TRUE)
##    compData[i,] <- list(id[i],min(colSums(!is.na(csvReader))))
    compData[i,] <-list(id[i],nrow(csvReader[complete.cases(csvReader),]))
  } 
  print (compData)
  
}
