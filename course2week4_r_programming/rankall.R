rankall <- function(outcome, num = "best") {

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that outcome is valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% outcomes) {
        stop("invalid outcome")
    } else {
        if (outcome == "heart attack") {
            outcome.ind <- 11
        }else if (outcome == "heart failure") {
            outcome.ind <- 17
        } else {
            outcome.ind <- 23
        }
    }
    #   [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    #   [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    #   [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    data <- data[, c(2, 7, outcome.ind)]
    names(data) <- c("hospital", "state", "rate")
    data[, 3] <- suppressWarnings(as.numeric(data[, 3], stringsAsFactors = FALSE))
    #drop NAs - in each case all states are in the data set
    data <- data[which(!is.na(data[, 3])),]

    ## For each state, find the hospital of the given rank
    states <- split(data, data$state)

    retrieve <- function(x, num) {
        x <- x[with(x, order(x[,3], x[,1])),]
        if (num == "best") {
            return (x[1,c(1, 2)])
        }else if (num == "worst"){
            num = nrow(x)
            return (x[num,c(1, 2)])
        }else if (is.numeric(num)){
            if (num > nrow(x)) {
                return(NA)
            }else if (num == 0) {
                stop("invalid num value")
            }else {
                return(x[num,c(1, 2)])
            }
        }else {
            stop("invalid num value")
        }
        return (x[num,c(1, 2)])
    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data <- lapply(states, retrieve, num = num )
    data <- do.call(rbind.data.frame, data)
    data[, 2] <- rownames(data)
    return(data)
}


