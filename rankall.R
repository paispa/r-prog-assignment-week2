rankall <- function(outcome, num = "best") {
  ## Read outcome data
  setwd("C:\\A511400\\Git-Hub\\data")
  outcomeDf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeDf[,11]<-sapply(outcomeDf[,11],function(data) if(data=="Not Available"){NA} else {as.numeric(data)})
  outcomeDf[,17]<-sapply(outcomeDf[,17],function(data) if(data=="Not Available"){NA} else {as.numeric(data)})
  outcomeDf[,23]<-sapply(outcomeDf[,23],function(data) if(data=="Not Available"){NA} else {as.numeric(data)})
  outnameFact <- factor(c("heart attack", "heart failure", "pneumonia"))
  outstFact <- factor(unique(outcomeDf[,7]))
  ## Check that outcome are valid
  if(missing(outcome) | !(outcome %in% outnameFact))
  {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outbestDf<-outcomeDf[,c(2,7,11,17,23)]
  colnames(outbestDf)<-c("Hospital.Name","state","heart attack","heart failure","pneumonia")
  
  j<-data.frame()
  for(i in levels(outstFact))
  {
    stDf<-outbestDf[outbestDf$state==i & !is.na(outbestDf[,outcome]),c("Hospital.Name",outcome,"state")]
    t<-stDf[order(stDf[,outcome],stDf$Hospital.Name),]
    if (num=="best")
    { 
      n<-1
    }
    else 
    {
      if (num=="worst")
      {
        n<-nrow(t)
      }
      else
      {
        n<-as.numeric(num)
      }
    }
    j<-rbind(j,data.frame(hospital=t[n, 1],state=i))
  }
  row.names(j)<-j$state
  j
}
