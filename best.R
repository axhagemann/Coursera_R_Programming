
# Date        :   26.10.2015
# Topic       :   Coursera R-Programming Assignment Week 4 Part 1 - best

# Description :   Get the hospital with the best mortality rate depending on the state.
#             :   Use the STOP function to stop the execution if the state / outcome is invalid


# Relevant columns of the input data
# state is colum 7
# hospital name is colum 2
# hospital 30 day mortality rate from heart attack is colum 11
# hospital 30 day mortality rate from heart failure is colum 17
# hospital 30 day mortality rate from pneumonia is colum 23

# driver function
# input is state and condition
best <- function(state, condition) {
  
  # set working directory
  setwd("~/Coursera/r-prog/week4")
  
  # read data, note that data is read in the character class
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # get best hospital
  getHospital(data, condition, state)
}

# function to get best hospital
# input data, condition and state
getHospital <- function(data, condition, state) {
  
  states <- unique(data[, 7])
  stateStatus <- checkState(states, state )

  if (stateStatus == FALSE) {
    # as specified use stop function when the state is invalid
    stop("invalid state")
  }

  if (condition == "heart attack") {colMed <- 11}
  else {
      if (condition == "heart failure") {colMed <- 17} 
      else {if (condition == "pneumonia") {colMed <- 23}
            else {
              stop("invalid outcome")
            }
      }
      
  }
  
  cols <- c(2,7,colMed)
  
  # only get the hospitals for the state with the data and remove Not available
  data_Filtered <- subset(data[ , cols], State == state & data[ , colMed] != "Not Available" )
  
  # convert the mortality column to numeric
  data_Filtered[, 3] <- sapply(data_Filtered[, 3], as.numeric)
  
  # sort the results, first by rate then alphabetically
  data_Sorted <- data_Filtered[ order(data_Filtered[,3], data_Filtered[,1]), ]
  
  # only return the top hospital
  return(data_Sorted[1, 1])

}

# function to check if the provided state is valid, i.e. occurs in the provided data set
# return boolean TRUE/FALSE depending on whether it is a valid state
checkState <- function(states, inputState) {
  ## check if the provided state is actually valid
  if (inputState %in% states) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

## Tests
#best("MD", "heart attack") # result should be "JOHNS HOPKINS HOSPITAL, THE"
#best("TX", "heart attack") # result should be "CYPRESS FAIRBANKS MEDICAL CENTER"
#best("TX", "heart failure") # result should be "FORT DUNCAN MEDICAL CENTER"
#best("MD", "pneumonia") # result should be "GREATER BALTIMORE MEDICAL CENTER"
#best("BB", "heart attack") # result should be error
#best("NY", "hert attack") # result should be error


