## file: rankall.R
## author: David Thayer
## date : 11/28/2014
## return a data frame containing a column for a hospital name and a 
## column for a state with some specified ranking and a specific
## outcome.  Ranking of hospital data is by 30 day mortality for 
## the given outcome.

source("best.R")

## returns index for given ranking specification
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

## given a collection of outcomes and a state return a list of state and outcome for that state
##
dataForState <- function(state, outComedata) {
  flags <- outComedata[[1]] == state
  list(outComedata[[2]][flags], outComedata[[3]][flags])    
}

## given the data for a particular outcome return hospitals 
## for a state ranked by 30 day mortality
getHospitalByRankForState <- function(state, outcomes){
      statedata <- dataForState(state, outcomes)
      ranks <- rankingsForData(statedata)
      statedata[[2]][ranks]  
}

## takes a ranking specification a state and the data for a particular outcome
## returns one of the following cases paired with the state
## case 1 rank is greater than number of hospitals in state => NA
## case 2 hospital has no data for this outcome             => None
## case 3 return  hospital name with requested ranking      => HospitalName
getHospitalAtRanking <- function(num, state, rankedData) {
      index <- indexFromNum(num, length(rankedData))
      
      ## if index is beyond length of hospital list for state 
      ## mark result as NA
      if(is.na(index)) {
          return (c("NA", state))
      }
      
      r <- rankedData[index]
      
      ## if data does not exist for this hospital 
      ## mark it fir exclusion from reported results
      if (is.na(r)) {
         return(c("None", state))
      }
      c(r,state)
}

## given the hospital data for a state return the hospital at a particular ranking
getHospitalAtRankForState <-  function(num, state, outcomes) {
      rankedData <- getHospitalByRankForState(state, outcomes)
      getHospitalAtRanking(num, state, rankedData)
}
  
## converts a list of 2 vectors with first vector giving the hospital name 
## and the second vector giving the state into a dataframe with the first
## column being the hospital name and the second column giving the state

formatOutput <- function(outp) {
      ## if result is marked for non-reporting exclude from output
      flags <- (outp[1,] != "None")
      outp <- outp[,flags]  
  
      ## create a matrix
      dim(outp) <- c(2,length(outp)/2)
      
      ## transpose matrix to get columns of data instead of rows of data
      outp <- t(outp)
      
      ## add column names
      colnames(outp) <- c("hospital", "state")
      
      ## return data.frame
      data.frame(outp)
}

## given a specified outcome and a ranking value return 
## the ranked hospital for each state including DC and VI
rankall <- function(outcome, num = "best") {
      ## read outcome data
      hospitalOutcomes <- readOutcomeData()
  
      ## Check that state and outcome are valid
      if (!isValidOutcome(outcome)) stop ("invalid outcome")
  
      outcomes <- outComeData(outcome, hospitalOutcomes)
      ## For each state find the hospital of the given rank
      outp <- sapply(state.abb, function(st) getHospitalAtRankForState(num, st, outcomes))
      
      ## return the dataframe with the hospital names and the 
      ## (abbreviated) state name
      formatOutput(outp)      
}
