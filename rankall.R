rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  nbC <- 0
  if (outcome == "heart attack"){nbC <- 11}
  if (outcome == "heart failure"){nbC <- 17}
  if (outcome == "pneumonia"){nbC <- 23}
  if (nbC == 0) {stop("invalid outcome")}
  ## For each state, find the hospital of the given rank
  states <- sort(unique(data[,7]))
  df <- data.frame(Hospital=character(), State=character(), stringsAsFactors=FALSE)
  colnames(df)<-c("Hospital", "State")
  for (st in states) {
    S <- subset(data, data[,7] == st)
    newS <- data.frame("Hospital" = as.character(S[,2]), 
                       "Rate" = as.numeric(as.character(S[,nbC])))
    newS <- na.omit(newS)
    sorted <- newS[order(newS[,2], newS[,1]),]
    #print(head(sorted))
    if (num == "best") {
      df[nrow(df)+1,] <- c(as.character(sorted[1,1]), st)
      next}
    if (num == "worst") {
      df[nrow(df)+1,] <- c(as.character(sorted[nrow(newS),1]), st)
      next
    }
    if(nrow(sorted) < num) {
      df[nrow(df)+1,] <- c("NA", st)
      next
    }
    if (is.numeric(num)) {
      df[nrow(df)+1,] <- c(as.character(sorted[num,1]), st)
    }
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df
  }

