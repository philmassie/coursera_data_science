# setwd("D:/My Folders/R/2016/coursera/mod_3_week_4_programming/")
library(dplyr)
library(reshape2)

# download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data/getdata_projectfiles_UCI HAR Dataset.zip")

## Load all the required data
# Test
test_subj <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
test_x <- read.table("data/UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("data/UCI HAR Dataset/test/y_test.txt")

# Train
train_subj <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
train_x <- read.table("data/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("data/UCI HAR Dataset/train/y_train.txt")

# activity lables
activity.lables <- read.table("data/UCI HAR Dataset/activity_labels.txt")

# colnames
c.names <- read.table("data/UCI HAR Dataset/features.txt")

## Combine datasets
subj.comb <- rbind(test_subj, train_subj)
x.comb <- rbind(test_x, train_x)
activ.comb <- rbind(test_y, train_y)
activ.comb <- left_join(activ.comb, activity.lables, by = "V1") # associate descriptive activity lables here
data.full <- cbind(subj.comb, activ.comb, x.comb)

## assign descriptive column names
c.names.full <- c("subject", "activityid", "activityname", as.character(c.names[,2]))
colnames(data.full) <- c.names.full

## subset means and std devs
# based on features_info.txt, filtering for mean() or std() should work
# this does exclude the frequency means, imnot sure if those are important forthis exercise?
index <- grep("mean\\()|std\\()", c.names.full)
index <- c(1,3, index)
data_sub <- data.full[,index]

head(data_sub)
names(data_sub)

# ## make the col names a bit more R friendly, 
# # maintain legibility with underscores
# # drop parentheses
# stemp <- gsub("-", "_", colnames(data_sub))
# colnames(data_sub) <- gsub("[()]", "", stemp)


## Genereate tidy data set
data_long <- melt(data_sub, id = c("subject", "activityname"))
data_wide_means <- dcast(data_long, subject + activityname ~ variable, fun.aggregate = mean, na.rm = TRUE)

tidy_data <- melt(data_wide_means, id = c("subject", "activityname"))
head(tidy_data)
write.table(tidy_data, "tidy_data.txt",  row.name=FALSE)

## Not part of the exercise
## test that melt > dcast returned correct mean values with some old fashioned loops
data_test <- list()
n <- 1
for (i in unique(data_sub$subject)){
    stemp <- data_sub[data_sub$subject == i, ]
    for (j in unique(stemp$activityname)){
        stemp2 <- stemp[stemp$activityname == j,]
        mean.temp <- colMeans(stemp2[,3:(ncol(stemp2))])
        
        str <- c(i, j, as.vector(mean.temp))
        data_test[[n]] <- str
        n <- n + 1
        }
}

data_test <- data.frame(matrix(unlist(data_test), nrow=180, byrow=T),stringsAsFactors=FALSE)
colnames(data_test) <- colnames(tidy_data)
data_test$subject <- as.numeric(data_test$subject)
data_test <- data_test[order(data_test$subject, data_test$activityname),]
head(data_test)
head(tidy_data)

# the test
r <-sample(180, 1)
c <-sample(68, 1)
data_test[r,c] == data_wide_means[r,c]
