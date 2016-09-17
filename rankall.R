rankall <- function(outcomeName, num = "best")
{
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    outcome[,c(11,17,23)] <- suppressWarnings(sapply(outcome[,c(11,17,23)],as.numeric))
    
    if (grepl('heart attack', outcomeName)) {
        rankedHospitals <- outcome[order(outcome[,7],outcome[,11],outcome[,2]),]
        rankedHospitals <- rankedHospitals[!is.na(rankedHospitals[,11]),]
    }
    else if (grepl('heart failure', outcomeName)) {
        rankedHospitals <- outcome[order(outcome[,7],outcome[,17],outcome[,2]),]
        rankedHospitals <- rankedHospitals[!is.na(rankedHospitals[,17]),]
    }
    else if (grepl('pneumonia', outcomeName)) {
        rankedHospitals <- outcome[order(outcome[,7],outcome[,23],outcome[,2]),]
        rankedHospitals <- rankedHospitals[!is.na(rankedHospitals[,23]),]
    }
    else {
        stop("invalid outcome")
    }
    
    if (num == 'best')
        hospital <- tapply(rankedHospitals[,2],rankedHospitals[,7],function (x) x[1])
    else if (num == 'worst')
        hospital <- tapply(rankedHospitals[,2],rankedHospitals[,7],function (x) x[length(x)])
    else
        hospital <- tapply(rankedHospitals[,2],rankedHospitals[,7],function (x) x[num])
            
    
    state <- tapply(rankedHospitals[,7],rankedHospitals[,7],function (x) x[1])
    data.frame(hospital,state)
}