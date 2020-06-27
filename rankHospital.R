rankhospital <- function(state = "TX", outcome = "heart attack", num = "best") {
    
    ## Read outcome data
    #stringsAsFactors = F keeps the Not Available from driving values to factors
    #na.strings tells R which factors to consider NA
    outcomeDF <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
    
    #2 = names, 7 = state, 11, 17, 23 are the three factors we are ranking on h-attack, h-failure, pneumonia. Only pull those
    outcomeDF <- outcomeDF[ ,c(2,7,11,17,23)]
    
    ## Check that state and outcome are valid
    
    #create a vector of the state column
    vectOfstateCol <- outcomeDF$State
    #check that state is in a the available selections
    if (!(state %in% vectOfstateCol)) {
        stop("invalid state" )
    }
    #create names for the columns that are relevent
    namesForColumns <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
    colnames(outcomeDF) <- namesForColumns
    if (!(outcome %in% namesForColumns)) {
        stop("invalid outcome")
    }
    
    #keep all the rows, name and state, but only keep the outcome column of concern in the argument, now that it's verified as good
    dfToSort <- outcomeDF[ , c("Hospital.Name", "State", outcome)]
    
    #get rid of all the rows that don't have values for the outcome in question
    dfToSort <- na.omit(dfToSort)
    
    #order the rows on outcome first, then the name
    #originally worked dfToSort[3] dfToSort[1].  the [[]] gives an obect [] gives a list
    orderedDF <- dfToSort[order(dfToSort[[outcome]], dfToSort$Hospital.Name), ]
    
    #this split creates a list of, lists, of data frames - took me forever to figure out using dim and str
    listByState <- split(orderedDF, orderedDF$State)
    
    #top level list is the states, so select the state of interest, the [[]] pulls the data frame out of the list
    #if only using [] it returns a list of one data frame x unkown rows, 3 columns
    dfOfOneState <- listByState[[state]]
    
    #assume it's best (first selection), if it's worst, pick the last row.
    rowToSelect <- 1
    if (num %in% "worst") {
        rowToSelect <- nrow(dfOfOneState)
    }
    # if num is a numeric value, set that as the row to select
    if (is.numeric(num)) {
        rowToSelect <- num
    }

    #return the row selected
    dfOfOneState[rowToSelect, 1]
}

testing <- rankhospital()
head(testing)

