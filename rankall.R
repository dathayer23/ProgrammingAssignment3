source("best.R")


indexFromNum <- function(num, max) {
  if (num == "best") {  return(1) }
  else if (num == "worst") { return(max) }
  else if (num > max) {
    index <- 1
    is.na(index) <- c(1) 
    return(index)
  }
  as.numeric(num)
}

getHospitalByRankForState <- function(num, state, outcomes){
  statedata <- dataForState(state, outcomes)
  ranks <- rankingsForData(statedata)
  statedata[[2]][ranks]  
}

getHospitalAtRanking <- function(num, state, rankedData) {
  index <- indexFromNum(num, length(rankedData))
  if(is.na(index)) {
    return (c("NA", state))
  }
  c(rankedData[index],state)
}

getHospitalAtRankForState <-  function(num, state, outcomes) {
  rankedData <- getHospitalByRankForState(num, state, outcomes)
  getHospitalAtRanking(num, state, rankedData)
}
  


rankall <- function(outcome, num = "best") {
  ## read outcome data
  hospitalOutcomes <- readOutcomeData()
  
  ## Check that state and outcome are valid
  if (!isValidOutcome(outcome)) stop ("invalid outcome")
  
  outcomes <- outComeData(outcome, hospitalOutcomes)
  ## For each state find the hospital of the given rank
  outp <- sapply(state.abb, function(st) getHospitalAtRankForState(num, st, outcomes))
  dim(outp) <- c(2,52)
  outp <- t(outp)
  colnames(outp) <- c("hospital", "state")
  data.frame(outp)
  ## return the dataframt with the hospital names and the 
  ## (abbreviated) state name
  
}
