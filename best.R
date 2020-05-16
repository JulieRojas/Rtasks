best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  nbC <- 0
  if (outcome == "heart attack"){nbC <- 11}
  if (outcome == "heart failure"){nbC <- 17}
  if (outcome == "pneumonia"){nbC <- 23}
  if (nbC == 0) {stop("invalid outcome")}
  #print(nbC)
  ## Return hospital name in that state with lowest 30-day death
  ## rate 
  S <- subset(data, data[,7] == state)
  if(nrow(S) == 0) {stop("invalid state")}
  S[,nbC] <- as.numeric(as.character(S[,nbC]))
  #print(class(S[,nbC]))
  minimum <- min(S[,nbC], na.rm = TRUE)
  #print(minimum)
  best <- subset(S, S[,nbC] == minimum)
  #print("best")
  #print(best[,2])
  if (nrow(best) == 1) { hosp <- best[,2]}
  if (nrow(best) >1) {
    name <- as.vector(best[,2])
    #print("name")
    #print(name)
    sorted <- sort(name)
    # print(sorted)
    hosp  <- sorted[1]
  }
  hosp
}