best <- function(state, outcomeName)
{
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (is.element(state,outcome$State)) {
        data <- outcome[outcome$State == state,]
        data[,c(11,17,23)] <- suppressWarnings(sapply(data[,c(11,17,23)],as.numeric))
        
        if (grepl('heart attack', outcomeName)) {
            minIdx <- which(data[,11]==min(data[,11],na.rm=T))
        }
        else if (grepl('heart failure', outcomeName)) {
            minIdx <- which(data[,17]==min(data[,17],na.rm=T))
        }
        else if (grepl('pneumonia', outcomeName)) {
            minIdx <- which(data[,23]==min(data[,23],na.rm=T))
        }
        else {
            stop("invalid outcome")
        }
    }
    else
        stop("invalid state")
    
    bestHospital <- sort(data[minIdx,2])
    bestHospital[1]
}