rankhospital <- function(state, outcome, num = "best") {
    
    ##taRiX library to avoid coercion warning
    
    library(taRifx)
    
    ## Read Outcome data
    
    data_measure <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data_measure[, 11] <- destring(data_measure[, 11])
    data_measure[, 17] <- destring(data_measure[, 17])
    data_measure[, 23] <- destring(data_measure[, 23])
    
    ## Checking the state and outcome are valid
    
    if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia") {
        stop("invalid outcome")
    } else {
        count_state <- state == data_measure[, 7]
        if (sum(count_state) == 0) {
            stop("invalid state")
        }
    }
    
    if (num == "best") {
        num <- 1
    }
    
    ## Return hospital name with lowest mortality
    
    newdata <- data_measure[ which(data_measure$State == state), ]
    
    if (outcome == "heart attack") {
        orderdata <- newdata[order(newdata[, 11], newdata$Hospital.Name, na.last = NA),]
        ln <- length(orderdata$Hospital.Name)
    } else if (outcome == "heart failure") {
        orderdata <- newdata[order(newdata[, 17], newdata$Hospital.Name, na.last = NA),]
        ln <- length(orderdata$Hospital.Name)
    } else {
        orderdata <- newdata[order(newdata[, 23], newdata$Hospital.Name, na.last = NA),]
        ln <- length(orderdata$Hospital.Name)
    }
    
    
    if (num == "worst") {
        num <- ln
    }
    
    if (num > ln) {
        return(NA)
    }
    
    return(orderdata$Hospital.Name[num])
    
    
}

