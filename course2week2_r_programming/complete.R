complete <- function(directory, id = 1:332) {
  
  csv.fnames <- paste0(directory, "/", dir(directory))
  data <- do.call(rbind,lapply(csv.fnames[id],read.csv))
  index <- complete.cases(data[["sulfate"]], data[["nitrate"]])
  data.subset <- data[index,]

  res <- list()
  ids <- vector()
  nobs <- vector()
  n <- 1
  for (i in unique(data$ID)) {
    ids[n] <- i
    nobs[n] <- nrow(subset(data.subset, data.subset$ID == i))
    n <- n + 1
  }

  res <- data.frame(id = ids, nobs = nobs)

  return(res)
}