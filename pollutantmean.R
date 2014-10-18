pollutantmean <- function(directory, pollutant, id = 1:332, naRemove=TRUE) {
  fileName <- paste("c:/a511400",sprintf("%s/%03d.csv",directory,id),sep='/')
  fileName
  meanPoll <- numeric()
  for(i in 1:length(id))
  {
    csvReader <- read.csv(fileName[i], header=TRUE)
    meanPoll <- c(meanPoll,csvReader[,pollutant])
  }
 round(mean (meanPoll,na.rm=naRemove),digits=3)

}
