best <- function(state, outcome) {
  ## Read outcome data
  setwd("C:\\A511400\\Git-Hub\\data")
  outcomeDf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeDf[,11]<-sapply(outcomeDf[,11],function(data) if(data=="Not Available"){NA} else {as.numeric(data)})
  outcomeDf[,17]<-sapply(outcomeDf[,17],function(data) if(data=="Not Available"){NA} else {as.numeric(data)})
  outcomeDf[,23]<-sapply(outcomeDf[,23],function(data) if(data=="Not Available"){NA} else {as.numeric(data)})
  outnameFact <- factor(c("heart attack", "heart failure", "pneumonia"))
  outstFact <- factor(unique(outcomeDf[,7]))
  ## Check that state and outcome are valid
  if(missing(state) | !(state %in% outstFact))
  {
    stop("invalid state")
  }
  if(missing(outcome) | !(outcome %in% outnameFact))
  {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outbestDf<-outcomeDf[which(complete.cases(outcomeDf[,c(2,7,11,17,23)])),c(2,7,11,17,23)]
  colnames(outbestDf)<-c("Hospital.Name","state","heart attack","heart failure","pneumonia")
  
  stDf<-outbestDf[outbestDf$state==state,c("Hospital.Name",outcome)]
  t<-stDf[order(stDf[,outcome]),]
  t[1,1]
}
