best <- function(state, outcome) {
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  

  ## conform inputs
  state <- toupper(state)
  outcome <- tolower(outcome)
  
  ## check that state and outcome are valid
  ## HCAHPS Measures – State.csv has a row for every state. Pull the state code from this file...
  
  
  ##--------------
  ##validate state
  ##--------------
  
  ## valid states
  validStates <- state.abb
  
  ## filter validStates to the input "state" for comparison/validation
  State <- validStates[grep(state,validStates)]
  
  ## override null State if there isn't a match
  if(is.na(State[1])){
    State <- c("XX")
  }
  
  if(!(State == state)){
    stop("invalid state")
    
  }
  
  ##--------------
  ##validate outcome
  ##--------------
  
  ## valid outcomes
  validOutcomes <- data.frame(fields = c(11,17,23),outcome = c("heart attack", "heart failure", "pneumonia"))
    
#c("heart attack", "heart failure", "pneumonia") 
  
# not sure if i should use a character vector or a data.frame
# data.frame(outcome = c("heart attack", "heart failure", "pneumonia"))
  
  ## filter validOutcomes to the input "outcome" for comparison/validation
  Outcome <- validOutcomes[grep(outcome,validOutcomes$outcome),]
#validOutcomes[grep(outcome,validOutcomes)]
  
  ## override null Outcome if there isn't a match
  if(is.na(Outcome[1,1])){
    Outcome[1,1] = "XX"
    Outcome$outcome = "XX"
    print(is.na(Outcome[1,1]))
  }
  
  ## subset outcomes using the passed-in variable
  if(!(Outcome[1,2] == outcome)){
    ## invalid outcome
    stop("invalid outcome")
  }
  Outcome
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## clean up
  rm(outcomeData)
  
}
