source("best.R")


rankingsForData <- function(statedata) {
  
}


getRankings <- function(state, outcome, hospitalOutcomes) {
      outcomeData <- outComeData(outcome, hospitalOutcomes)
      dataforstate <- dataForState(state, outcomeData)
      rankings <- rankingsForData(dataforstate)
}

indexFromNum <- function(num, max) {
  
}

rankhospital <- function(state, outcome, num = "best") {
     
     data(state)
  
     ## Check that the state and outcome are valid
     if (!isStateAbb(state)) stop("invalid state")
     if (!isValidOutcome(outcome)) stop ("invalid outcome")
     
     ## read outcome data
     hospitalOutcomes <- readOutcomeData()
     rankings <- getRankings(state, outcome, hospitalOutcomes)
     index <- indexFromNum(num, length(rankings))
     if (is.na(index))
     {
       return NA
     }
     rankings[index]
     rankings[index]
     ## Return hospital name in that state with the given rank
     ## for 30-day mortality
}