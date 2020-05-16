rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  nbC <- 0
  if (outcome == "heart attack"){nbC <- 11}
  if (outcome == "heart failure"){nbC <- 17}
  if (outcome == "pneumonia"){nbC <- 23}
  if (nbC == 0) {stop("invalid outcome")}
  S <- subset(data, data[,7] == state)
  if(nrow(S) == 0) {stop("invalid state")}
  newS <- data.frame("Hospital.name" = as.character(S[,2]), 
                     "Rate" = as.numeric(as.character(S[,nbC])))
  newS <- na.omit(newS)
  sorted <- newS[order(newS[,2], newS[,1]), ]
  sorted$Rank <- c(1:nrow(sorted))
  if (num == "best") {return(sorted[1,1])}
  if (num == "worst") {return(sorted[nrow(sorted),1])}
  if (num > nrow(sorted)) {return("NA")}
  if (is.numeric(num)) {return(sorted[num,1])}
  answer
}
