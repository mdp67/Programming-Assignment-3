rankhospital <- function(state = "TX", outcome = "heart attack", num = "best") {
    
    ## Read outcome data
    outcomeDF <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
    outcomeDF <- outcomeDF[ ,c(2,7,11,17,23)]
    
    ## Check that state and outcome are valid
    
    #create a vector of the state column
    vectOfstateCol <- outcomeDF$State
    #check that state is in a the available selections
    if (!(state %in% vectOfstateCol)) {
        stop("invalid state" )
    }
    #create a vector of the names of each column because the outcome must be in there, the first ten are not outcomes
    namesForColumns <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
    colnames(outcomeDF) <- namesForColumns
    if (!(outcome %in% namesForColumns)) {
        stop("invalid outcome")
    }
    

    
    dfToSort <- outcomeDF[ , c("Hospital.Name", "State", outcome)]
    dfToSort <- na.omit(dfToSort)
    
    
    ## 
    orderedDF <- dfToSort[order(dfToSort[[outcome]], dfToSort$Hospital.Name), ]
    listByState <- split(orderedDF, orderedDF$State)
    listOfOneState <- listByState[[state]]
    
    #ranked_hospitals <- lapply(listOfOneState, function(x) x[ , 1])
    
    #hospital <- ranked_hospitals[[num]]
    # vectorOfHospitals <- vector(mode = "list", length = )
    
    dfOrderedName <- listOfOneState[1]
    numberOfRows <- nrow(dfOrderedName)
    print(dfOrderedName[numberOfRows, 1])
    
    # index <- 1
    # for (i in seq_along(listOfOneState)) {
    #     
    #     index <- index +1
    #     print(i)
    #     print(listOfOneState[i])
    #     print(class(listOfOneState[i]))
    #     print(dim(listOfOneState[i]))
    #     print(nrow(listOfOneState[i]))
    #     print(listOfOneState[i][4, 1])
    # }
    #print(ranked_hospitals)

}