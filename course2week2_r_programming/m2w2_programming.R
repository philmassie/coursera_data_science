setwd("D:/My Folders/R/2016/coursera/mod_2_week_2_programming/")

source("pollutantmean.R")

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

source("complete.R")

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 295)
complete("specdata")

source("corr.R")

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)

# Quiz
# ====
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

# 7
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


cc[which(cc$nobs == 135),]

a <- c(608, 885, 684, 510, 765, 171, 244, 745, 624, 216)

a <- c(711, 135, 74, 445, 178, 73, 49, 0, 687, 237)

a <- c(643, 99, 703, 673 ,59 ,366,277, 644 ,318, 594)

a <- c(524, 577 ,276 ,487, 3 ,592, 5, 148, 645, 435)

a <- c(270, 310, 27,692 ,307, 681, 631, 455, 690, 440)

for(i in a) {
  print(cc[which(cc$nobs == i),])
  
  }



cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
