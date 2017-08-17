rankall <- function(outcome, num = "best") {
    
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
    }
    
    num_check <- 0
    
    if (num == "best") {
        num <- 1
    }
    
    
    if (num == "worst") {
        num_check <- 1
    }
    
    ## Return hospital name with lowest mortality
    
   # newdata <- data_measure[ which(data_measure$State == state), ]
    
    if (outcome == "heart attack") {
        orderdata <- data_measure[order(data_measure$State, data_measure[, 11], data_measure$Hospital.Name, na.last = NA),]
    } else if (outcome == "heart failure") {
        orderdata <- data_measure[order(data_measure$State, data_measure[, 17], data_measure$Hospital.Name, na.last = NA),]
    } else {
        orderdata <- data_measure[order(data_measure$State, data_measure[, 23], data_measure$Hospital.Name, na.last = NA),]
    }
    
    
    data_factor <- factor(orderdata$State)
    state <- levels(data_factor)
    hospital <- 1:length(state)
    
    j <- 1
    
    for (i in state) {
        
        newdata <- orderdata[ which(orderdata$State == i), ]
        
        ln <- length(newdata$Hospital.Name)
        
        if (num_check == 1) {
            num <- ln
        } 
        
        if (num > ln) {
            hospital[j] <- NA
        }
        else {
            hospital[j] <- newdata$Hospital.Name[num]
        }
        j=j+1
    }
    
    #newdata <- data_measure[ which(data_measure$State == "TN")
    
    df = data.frame(hospital, state)
    
    #return(ord$Hospital.Name[num])
    return(df)
    
}

