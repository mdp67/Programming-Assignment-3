
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
    
    #get all rows but only the hospital, state, and outcome of interest (so you can omit NA only from column of interest)
    dfToSort <- outcomeDF[ ,c(2,7, columnToUse)]
    #name the columns appropriately
    names(dfToSort) <- c("Hospital.Name", "State", outcome)
    #omit NA's
    dfToSort <- na.omit(dfToSort)
    

    #order() is ordering the rows of dfToSort the blank (could be from 1-3) selects what columns will be included
    dfOrdered <- dfToSort[order(dfToSort[[outcome]], dfToSort$Hospital.Name),    ]
    
    #split the data frame into a list of dataframes, the name of the list members is the state'n name
    listSplitByState <- split(dfOrdered, dfOrdered$State)
    #print(names(listSplitByState)) ** lists all the states
    
    #create a data frame to store the selected rows
    selectedDF <- data.frame(character, character)
    
    for (dfState in listSplitByState) {
        #need to do select the correct row every time, because if it's the worst, need to count rows in this data frame
        #could only do the "worst check outside of the loop, but more confusing
        #assume it's best (first selection), if it's worst, pick the last row.
        
        ################### selecting rows could pull out in function ########
        rowToSelect <- 1
        if (num %in% "worst") {
            rowToSelect <- nrow(dfState)
        }
        # if num is a numeric value, set that as the row to select
        if (is.numeric(num)) {
            rowToSelect <- num
        }
        ########################
        
        #get the name of the state from the first row of the data frame,
        #need to do in case there is a row ouside of the range and both state and hostpital are NA
        stateName <- dfState[[1,2]]
        #select the correct row and both columns
        selectedRow <- dfState[rowToSelect, 1:2]
        #add the state name to the correct row
        selectedRow[1,2] <- stateName
        #name the row
        row.names(selectedRow) <- selectedRow[2]
        #bind the row to the data frame
        selectedDF <- rbind(selectedDF, selectedRow)
    }
    selectedDF
    
    # this wont work, but it wwould be interesting to try to have the anonomous function do everyting above
    #dfComplete <- lapply(listSplitByState, function(df) df[rowToSelect, 1:2])
       
}