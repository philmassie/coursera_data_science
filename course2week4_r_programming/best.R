best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if (!state %in% unique(data$State)) {
    stop("invalid state")
  }
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
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data.sub <- data[which(data$State==state), c(2, outcome.ind)]
  data.sub[, 2] <- suppressWarnings(as.numeric(data.sub[, 2], stringsAsFactors = FALSE))
  return(min(data.sub[which(data.sub[,2] == min(data.sub[,2], na.rm = TRUE)), 1]))
}
