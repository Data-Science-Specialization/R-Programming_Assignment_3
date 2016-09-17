rankhospital <- function(state, outcomeName, num = "best")
{
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (is.element(state,outcome$State)) {
        data <- outcome[outcome$State == state,]
        data[,c(11,17,23)] <- suppressWarnings(sapply(data[,c(11,17,23)],as.numeric))
        
        if (grepl('heart attack', outcomeName)) {
            rankedHospitals <- data[order(data[,11],data[,2]),]
            rankedHospitals <- rankedHospitals[!is.na(rankedHospitals[,11]),2]
        }
        else if (grepl('heart failure', outcomeName)) {
            rankedHospitals <- data[order(data[,17],data[,2]),]
            rankedHospitals <- rankedHospitals[!is.na(rankedHospitals[,11]),2]
        }
        else if (grepl('pneumonia', outcomeName)) {
            rankedHospitals <- data[order(data[,23],data[,2]),]
            rankedHospitals <- rankedHospitals[!is.na(rankedHospitals[,11]),2]
        }
        else {
            stop("invalid outcome")
        }
    }
    else {
        stop("invalid state")
    }
    
    if (num == 'best')
        rankedHospitals[1]
    else if (num == 'worst')
        rankedHospitals[length(rankedHospitals)]
    else
        rankedHospitals[num]
}