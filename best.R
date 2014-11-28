## file: best.R
## author: David Thayer
## date : 11/28/2014
## Read in data for hospital mortality for the US
## define a function that for a given state and outcome 
## returns the best mortality rate (minimum 30 day mortality)
## for hospitals in that state.  If there are ties report the 
## hospital that is first in alphabetical order


## read in data and create list of required outcomes
data(state)
state.abb <- sort(c(state.abb, "DC", "VI"))

outcomes <- c('heart attack','heart failure','pneumonia')
heartAttackMortality <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
heartFailureMortality <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
pneumoniaMortality <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'

## Read Hospital Outcome data from file
## Set required numerical columns to numbers
readOutcomeData <- function()
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  data[, heartAttackMortality] <- as.numeric(data[,heartAttackMortality])
  data[, heartFailureMortality] <- as.numeric(data[,heartFailureMortality])
  data[, pneumoniaMortality] <- as.numeric(data[,pneumoniaMortality])
  c( cleanData(data,heartAttackMortality), 
     cleanData(data, heartFailureMortality), 
     cleanData(data, pneumoniaMortality) )
}

# combine the state value with the selected outcome value
# and clean out the pairs where the outrcome is NA
cleanData <- function(data, outcome){
   d <- data[, c('State', outcome, 'Hospital.Name')]  
   flags <- !is.na(d[,2])
   d <- d[flags,]    
}

## return true if item is a member of collection
##
isMember <- function (item, collection) {
     flags <- collection == item
     sum(flags > 0)
}

## return true if outcome is member of outcomes
## otherwise return error message
isValidOutcome <- function(outcome) { isMember(outcome, outcomes) }


## return true if state is a valid state abbreviation
## otherwise return error message
isStateAbb <- function (st) { isMember(st, state.abb) }


## seperate out data for desired outcome from hospitalOutcomes
##
outComeData <- function(outcome, hospitalOutcomes) {
      
      if (outcome == "heart attack"){
        data <-  c(hospitalOutcomes[1], hospitalOutcomes[2], hospitalOutcomes[3])
      }
      else if(outcome == "heart failure") {
        data <- c(hospitalOutcomes[4], hospitalOutcomes[5], hospitalOutcomes[6])        
      }
      else if (outcome == "pneumonia") {
        data <- c(hospitalOutcomes[7], hospitalOutcomes[8], hospitalOutcomes[9])
      }
      else {
        stop("Unknown outcomes requested ", outcome)
      } 
      
      data
}

## given a collection of outcomes and a state return a list of state and outcome for that state
##
dataForState <- function(state, outComedata) {
      flags <- outComedata[[1]] == state
      list(outComedata[[2]][flags], outComedata[[3]][flags])    
}

## return a vector of index positions for the minimum values of the outcome
indicesOfBest <- function(data) {
     mi <- min(data)
     flags <- (data == mi)
     (1:length(data))[flags]                 
}

## for a stae and an outcome return the list of hospital names 
## in that state that have the best outcome for the specified outcome
getBest <- function(state, outcome, hospitalOutcomes) {
    outcomeData <- outComeData(outcome, hospitalOutcomes)
    dataforstate <- dataForState(state, outcomeData)
    best <- indicesOfBest(dataforstate[[1]])
    dataforstate[[2]][best]
}

## given a valid state and outcome return best hospital for state 
## with that outcome and return rank
best <- function(state, outcome)
{    
     ##Read Outcome Data
     hospitalOutcomes <- readOutcomeData()
  
     ## Check that state and outcome are valid
     if (!isStateAbb(state)) stop("invalid state")
     if (!isValidOutcome(outcome)) stop ("invalid outcome")
     
     ## get best entries
     best <- getBest(state, outcome, hospitalOutcomes)
     best <- sort(best)
     
     ## Return hospital name in that state with lowest 30-day death rate
     best[1]
}

