corr <- function(directory, threshold = 0) {
  
  stemp <- complete(directory)
  stemp <- subset(stemp, stemp$nobs >= threshold)
  corrs <- vector()
  
  if (nrow(stemp) > 0) {
    csv.fnames <- paste0(directory, "/", dir(directory))
    data.subset <- do.call(rbind,lapply(csv.fnames[stemp$id],read.csv)) 
    
    index <- complete.cases(data.subset[["sulfate"]], data.subset[["nitrate"]])
    data.subset <- data.subset[index,]
    head(data.subset)
    
    n <- 1
    
    for (i in unique(data.subset$ID)) {
      stemp <- subset(data.subset, data.subset$ID == i)
      corrs[n] <- cor(stemp$sulfate, stemp$nitrate, method = "pearson")
      # res <- rbind(res, c(i, nrows))
      n <- n + 1
    }
  }
  return(corrs)
}