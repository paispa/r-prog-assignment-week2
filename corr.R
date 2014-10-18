corr <- function(directory, threshold=0) {
#  compData <- data.frame('sulp' = numeric(), 'nitr' = numeric())
  compData <- numeric()
  j <-1
  for(i in 1:332)
  {
    fileName <- paste("c:/a511400",sprintf("%s/%03d.csv",directory,i),sep='/')
    csvReader <- read.csv(fileName)
    if (nrow(csvReader[complete.cases(csvReader),]) > threshold)
    {
      corData <- cor(csvReader$sulfate[complete.cases(csvReader)],csvReader$nitrate[complete.cases(csvReader)])
      compData <- c(compData,corData)
    }
  }
  compData
}