source("best.R")


rankingsForData <- function(statedata) {
     order(statedata[[1]], statedata[[2]])  
}


getRankings <- function(state, outcome, hospitalOutcomes) {
      outcomeData <- outComeData(outcome, hospitalOutcomes)
      dataforstate <- dataForState(state, outcomeData)
      rankings <- rankingsForData(dataforstate)
      dataforstate[[2]][rankings]
}

indexFromNum <- function(num, max) {
      if (num == "best") {  return(1) }
      else if (num == "worst") { return(max) }
      else if (num > max) {
           index <- 1
           is.na(index) <- c(1) 
           return(index)
      }
      else {
          return(num)
      }      
}

rankhospital <- function(state, outcome, num = "best") {     
     ##Read Outcome Data
     hospitalOutcomes <- readOutcomeData()
  
     ## Check that the state and outcome are valid
     if (!isStateAbb(state)) stop("invalid state")
     if (!isValidOutcome(outcome)) stop ("invalid outcome")
     
     ## read outcome data
     rankings <- getRankings(state, outcome, hospitalOutcomes)
     index <- indexFromNum(num, length(rankings))
     if (is.na(index))
     {       
           return(index)
     }
     
     ## Return hospital name in that state with the given rank
     ## for 30-day mortality
     rankings[index]
}

