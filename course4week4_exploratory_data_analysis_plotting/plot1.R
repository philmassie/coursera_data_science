setwd("D:/My Folders/R/2016/coursera/mod_4_week_4/")

scc <- readRDS("summarySCC_PM25.rds", refhook = NULL)
nei <- readRDS("Source_Classification_Code.rds", refhook = NULL)

# 1. Total emissions over time - USA
# calculate emissions sums
emiss_yr <- with(scc, tapply(Emissions, year, sum))

png("plot1.png", width = 1024, height = 768)
plot(as.numeric(names(emiss_yr)), emiss_yr, 
     pch = 16,
     xlab = "Year", ylab = "Total PM2.5 emission (tons)",
     xaxt = "n")
axis(1, at = as.numeric(names(emiss_yr)))
title(main = "Total PM2.5 Emissions in the United States\n(1999 - 2008)")
fit <- lm(emiss_yr ~ as.numeric(names(emiss_yr)))
abline(fit, lwd=1, col="blue")
dev.off()