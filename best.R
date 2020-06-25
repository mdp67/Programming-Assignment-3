best <- function(state = "TX", outcome = "heart attack") {
    
    ## Read outcome data
    outcomeDF <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    outcomeDF <- outcomeDF[ ,c(2,7,11,17,23)]
    
    ## Check that state and outcome are valid
    
    #create a vector of the state column
    vectOfstateCol <- outcomeDF$State
    #check that state is in a the available selections
    if (!(state %in% vectOfstateCol)) {
        stop("invalid state" )
    }
    #create a vector of the names of each column because the outcome must be in there, the first ten are not outcomes
    colnames(outcomeDF) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
    columnNames <- colnames(outcomeDF)
    if (!(outcome %in% columnNames)) {
        stop("invalid outcome")
    }
    
 
    columnsToSort <- outcomeDF[ , c("Hospital.Name", "State", outcome)]
    
    columnsToSort[ columnsToSort == "Not Available"] <- NA
    
    columnsToSort <- na.omit(columnsToSort)
    
    #columnsToSort[outcome] <- as.numeric(columnsToSort[outcome])
    columnsToSort[outcome] <- as.numeric(as.character(columnsToSort[outcome]))
    
    
    
    library(plyr)
    ## Return hospital name in that state with lowest 30-day death ## rate
    #arrangedData <- arrange(outcomeDF, outcome)
    #outcomeDF
    #splitByState <- split(outcomeDF, outcomeDF$State)
    #selectedStateDF <- splitByState[state]
    #library(plyr)
    #outcomeDF
    
    #Arrange the data by outcome
    #vectOfstateCol
    columnsToSort
}

#testing <- best(state = "TX", outcome = "fuck")
#testing <- best(outcome = "fuck")
testing <- best()
