
rankall <- function(outcome, num = "best") { 

    ## Read outcome data
    #stringsAsFactors = F keeps the Not Available from driving values to factors
    #na.strings tells R which factors to consider NA
    outcomeDF <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
    
    
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    }
    
    columnToUse <- 11
    if (outcome %in% "heart failure") {
        columnToUse <- 17
    } else if (outcome %in% "pneumonia") {
        columnToUse <- 23
    }
    
    print(columnToUse)
    dfToSort <- outcomeDF[ ,c(2,7, columnToUse)]
    names(dfToSort) <- c("Hospital.Name", "State", outcome)
    dfToSort <- na.omit(dfToSort)
    
    #assume it's best (first selection), if it's worst, pick the last row.
    rowToSelect <- 1
    if (num %in% "worst") {
        rowToSelect <- nrow(dfOfOneState)
    }
    # if num is a numeric value, set that as the row to select
    if (is.numeric(num)) {
        rowToSelect <- num
    }
    
    #order() is ordering the rows of dfToSort the blank (could be from 1-3) selects what columns will be included
    dfOrdered <- dfToSort[order(dfToSort[[outcome]], dfToSort$Hospital.Name),    ]
    
    listSplitByState <- split(dfOrdered, dfOrdered$State)
    
    selectedDF <- data.frame(character, character)
    for (dfState in listSplitByState) {
        selectedRow <- dfState[rowToSelect, 1:2]
        row.names(selectedRow) <- selectedRow[2]
        print(selectedRow)
        rbind(selectedDF, selectedRow)
    }
    selectedDF
    
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
}