setwd("D:/My Folders/R/2016/coursera/mod_2_week_4_programming/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
#### 1
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

source("best.R")

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

#====================================================================================================
source("rankhospital.R")

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("MN", "heart attack", "5000blue")

#====================================================================================================
source("rankall.R")

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

Quiz

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)


r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
