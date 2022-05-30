## Assignment Instructions:
## Write a function named 'pollutantmean' that calculates the mean of a pollutant 
## (sulfate or nitrate) across a specified list of monitors. The function 
## 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
## particulate matter data from the directory specified in the 'directory' 
## argument and returns the mean of the pollutant across all of the monitors, 
## ignoring any missing values coded as NA. 

pollutantmean <- function(directory, pollutant, id = 1:332){
     
     #Default the counters to find the mean
     sumTotal <- 0 #value of all SUMS scross csv files
     totalDays <- 0 #Number of days counted
     
     #Loop running through the csv files
     for(index in id){
          
          #Format the id to read the correct file
          formattedID <- if(index < 10){
               paste("00", index, sep = "")
          } else if(index < 100){
               paste("0", index, sep = "")
          } else{
               index
          }
          
          #Read the csv file
          currentFile <- read.csv(paste(directory, "/",formattedID,".csv", sep = ""))
          
          # Add the sum of the column to the running total
          sumTotal <- sumTotal + sum(currentFile[pollutant], na.rm = TRUE)
          # Add the count of days to the running total
          totalDays <- totalDays + length(currentFile[pollutant][!is.na(currentFile[pollutant])])
          
     }
     
     # Return the final mean
     sumTotal / totalDays
     
}


## Write a function that reads a directory full of files and reports the number 
## of completely observed cases in each data file. The function should return a 
## data frame where the first column is the name of the file and the second 
## column is the number of complete cases. 

complete <- function(directory, id = 1:332){
     
     dataset <- data.frame()
     
     #Loop running through the csv files
     for(index in id){
          
          #Format the id to read the correct file
          formattedID <- if(index < 10){
               paste("00", index, sep = "")
          } else if(index < 100){
               paste("0", index, sep = "")
          } else{
               index
          }
          
          #Read the csv file
          currentFile <- read.csv(paste(directory, "/",formattedID,".csv", sep = ""))
          
          #Append new data to output dataset
          dataset <- rbind(dataset, 
                           data.frame(id = index,
                                      nobs = length(currentFile$ID[!is.na(currentFile$nitrate) & !is.na(currentFile$sulfate)])))
          
     }
     
     dataset
     
}


## Write a function that takes a directory of data files and a threshold for 
## complete cases and calculates the correlation between sulfate and nitrate for 
## monitor locations where the number of completely observed cases (on all variables) 
## is greater than the threshold. The function should return a vector of correlations 
## for the monitors that meet the threshold requirement. If no monitors meet the 
## threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshhold = 0){
     
     #default number of files
     id <- 1
     formattedID <- "001"
     
     # Create an empty vector to append to.
     correlations <- numeric()
     
     #Loop running through the csv files
     while(file.exists(paste(directory, "/",formattedID,".csv", sep = ""))){
          
          #Read the csv file
          currentFile <- read.csv(paste(directory, "/",formattedID,".csv", sep = ""))
          
          # Find the number of complete cases
          NumCompleteCases <- length(currentFile$sulfate[!is.na(currentFile$nitrate) & !is.na(currentFile$sulfate)])
          
          # Does the number of complete cases meet the threshhold? If so, add it to the list of correlations
          if(NumCompleteCases > threshhold){
               correlations <- c(correlations,
                                 cor(currentFile$sulfate[!is.na(currentFile$nitrate) & !is.na(currentFile$sulfate)],
                                     currentFile$nitrate[!is.na(currentFile$nitrate) & !is.na(currentFile$sulfate)]))
          }
          
          # Prep the next fileID
          id <- id + 1
          #Format the id to read the correct file
          formattedID <- if(id < 10){
               paste("00", id, sep = "")
          } else if(id < 100){
               paste("0", id, sep = "")
          } else{
               id
          }
     }
     #Return result
     correlations
}