# Write a function called best that take two arguments: the 2-character abbreviated 
# name of a state and an outcome name. The function reads the outcome-of-care-measures.csv 
# file and returns a character vector with the name of the hospital that has the 
# best (i.e. lowest) 30-day mortality for the specified outcome in that state. 
# The hospital name is the name provided in the Hospital.Name variable. The outcomes 
# can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that 
# do not have data on a particular outcome should be excluded from the set of 
# hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, then 
# the hospital names should be sorted in alphabetical order and the first hospital 
# in that set should be chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best, 
# then hospital “b” should be returned).

# The function should check the validity of its arguments. If an invalid state 
# value is passed to best, the function should throw an error via the stop 
# function with the exact message “invalid state”. If an invalid outcome value 
# is passed to best, the function should throw an error via the stop function 
# with the exact message “invalid outcome”.


best <- function(state, outcome) {
     
     ## Read outcome data
     outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     if(!(toupper(state) %in% state.abb)){
          return("invalid state")
     }
     if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
          return("invalid outcome")
     }
     
     ## Return hospital name in that state with lowest 30-day death
     if(tolower(outcome) == "heart attack"){
          tempholder <- 11
     } else if(tolower(outcome) == "heart failure"){
          tempholder <- 17
     } else {
          tempholder <- 23
     }
     
     # Limit data: no NA values and selected state only.
     outcomeData <- outcomeData[!(is.na(as.numeric(outcomeData[,tempholder]))) & (outcomeData[,7] == toupper(state)),
                                c(2,tempholder)]
     
     # Return result
     # min = alphabetically, the first hospital
     # summary...[[1]] = minimum value
     min(outcomeData[,1][as.numeric(outcomeData[,2]) == summary(as.numeric(outcomeData[,2]))[[1]]])
     
}


# Write a function called rankhospital that takes three arguments: the 2-character 
# abbreviated name of a state (state), an outcome (outcome), and the ranking of 
# a hospital in that state for that outcome (num).

# The function reads the outcome-of-care-measures.csv file and returns a character 
# vector with the name of the hospital that has the ranking specified by the num 
# argument. For example, the call rankhospital("MD", "heart failure", 5) would 
# return a character vector containing the name of the hospital with the 5th lowest 
# 30-day death rate for heart failure. The num argument can take values “best”, 
# “worst”, or an integer indicating the ranking (smaller numbers are better). 
# If the number given by num is larger than the number of hospitals in that state, 
# then the function should return NA. Hospitals that do not have data on a 
# particular outcome should be excluded from the set of hospitals when deciding 
# the rankings.

# Handling ties. It may occur that multiple hospitals have the same 30-day 
# mortality rate for a given cause of death. In those cases ties should be broken 
# by using the hospital name. For example, in Texas (“TX”), the hospitals with 
# lowest 30-day mortality rate for heart failure are shown here.

rankhospital <- function(state, outcome, num = "best") {
     ## Read outcome data
     outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     if(!(toupper(state) %in% state.abb)){
          return("invalid state")
     }
     if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
          return("invalid outcome")
     }
     
     ## Return hospital name in that state with lowest 30-day death
     if(tolower(outcome) == "heart attack"){
          tempholder <- 11
     } else if(tolower(outcome) == "heart failure"){
          tempholder <- 17
     } else {
          tempholder <- 23
     }
     
     ## Return hospital name in that state with the given rank
     # Limit data: no NA values and selected state only.
     outcomeData <- outcomeData[!(is.na(as.numeric(outcomeData[,tempholder]))) & (outcomeData[,7] == toupper(state)),
                                c(2,tempholder)]
     
     #Sort Results
     outcomeData <- outcomeData[order(as.numeric(outcomeData[,2]),outcomeData[,1]),]
     
     if(num == "best"){
          num <- 1
     } else if(num == "worst"){
          num = nrow(outcomeData)
     }
     
     # Return Result
     outcomeData[,1][num]
}


# Write a function called rankall that takes two arguments: an outcome name (outcome) 
# and a hospital ranking (num). The function reads the outcome-of-care-measures.csv 
# file and returns a 2-column data frame containing the hospital in each state 
# that has the ranking specified in num. For example the function call 
# rankall("heart attack", "best") would return a data frame containing the names 
# of the hospitals that are the best in their respective states for 30-day heart 
# attack death rates. The function should return a value for every state (some 
# may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 
# 2-character abbreviation for the state name. Hospitals that do not have data on 
# a particular outcome should be excluded from the set of hospitals when deciding 
# the rankings.

# Handling ties. The rankall function should handle ties in the 30-day mortality 
# rates in the same way that the rankhospital function handles ties. 

rankall <- function(outcome, num = "best") {
     
     ## Read outcome data
     outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     if(!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))){
          return("invalid outcome")
     }
     
     ## Return hospital name in that state with lowest 30-day death
     if(tolower(outcome) == "heart attack"){
          tempholder <- 11
     } else if(tolower(outcome) == "heart failure"){
          tempholder <- 17
     } else {
          tempholder <- 23
     }
     
     
     # start a blank data frame
     ResultFrame <- data.frame()
     
     ## For each state, find the hospital of the given rank
     for(i in state.abb){
          outcomeData2 <- outcomeData[!(is.na(as.numeric(outcomeData[,tempholder]))) & (outcomeData[,7] == toupper(i)),
                                      c(2,tempholder)]
          
          #Sort Results
          outcomeData2 <- outcomeData2[order(as.numeric(outcomeData2[,2]),outcomeData2[,1]),]
          
          if(num == "best"){
               num2 <- 1
          } else if(num == "worst"){
               num2 = nrow(outcomeData2)
          } else{
               num2 <- num
          }
          
          # Append to results frame
          ResultFrame <- rbind(ResultFrame, data.frame(hospital = outcomeData2[,1][num2], state = i))
     }
     
     # return results
     ResultFrame
}