pollutantmean <- function(directory, pollutant, id = 1:332) {
  csv.fnames <- paste0(directory, "/", dir(directory))
  data.subset <- do.call(rbind,lapply(csv.fnames[id],read.csv))
  mean(data.subset[[pollutant]], na.rm=TRUE)
}