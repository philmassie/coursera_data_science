setwd("D:/My Folders/R/2016/coursera/mod_4_week_4/")

scc <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds", refhook = NULL)
nei <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds", refhook = NULL)

head(scc)
head(nei)


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

# 2. Total emissions over time - Baltimore
scc_baltimore <- subset(scc, scc$fips == "24510")
emiss_yr <- with(scc_baltimore, tapply(Emissions, year, sum))
png("plot2.png", width = 1024, height = 768)
plot(as.numeric(names(emiss_yr)), emiss_yr, 
     pch = 16,
     xlab = "Year", ylab = "Total PM2.5 emission (tons)",
     xaxt = "n")
axis(1, at = as.numeric(names(emiss_yr)))
title(main = "Total PM2.5 Emissions in Baltimore City\n(1999 - 2008)")
fit <- lm(emiss_yr ~ as.numeric(names(emiss_yr)))
abline(fit, lwd=1, col="blue")
dev.off()

# 3. 
library(ggplot2)
library(reshape2)
library(plyr)
library(RColorBrewer)
scc_baltimore <- subset(scc, scc$fips == "24510")
emiss_yr_typ <- with(scc_baltimore, tapply(Emissions, list(year, type), sum))
emiss_yr_typ <- melt(emiss_yr_typ, varnames = c("year", "type"), value.name = "emissions")

# Some fun with colours
# wrk out slopes for coloring the linear regression lines
# later, map them to green for decreased and red for increased
l.regressions <- ddply(emiss_yr_typ,
                       .(type),
                       summarize,
                       intercept=lm(emissions~year)$coef[1],
                       slope=lm(emissions~year)$coef[2])
l.regressions$colref <- as.numeric(lapply(l.regressions$slope, function(x) if (x < 0) {return(1)} else {return(0)}))


# join the data for plotting
emiss_yr_typ <- join(emiss_yr_typ, l.regressions)

p <- ggplot(emiss_yr_typ, aes(x=year, y=emissions, col=colref))
p <- p + geom_point(size = 2)
p <- p + geom_smooth(method = "lm", se = FALSE)
p <- p + scale_colour_gradientn(colours=brewer.pal(4, "RdYlGn"))
p <- p + facet_grid(type ~ ., scales = "free")
p <- p + theme(legend.position = "none")
p <- p + ggtitle("Total PM2.5 emissions for Baltimore\n (split by source type)")
p <- p + labs(x="Year",y="PM2.5 emissions (tons)") 
ggsave(filename = 'plot3.png', plot = p, type="cairo-png")
p


# 4. I was a little unclear as to the most correct column to use to find coal sources.
# I chose EI.Sector and filtered for [Cc]oal
library(dplyr)
nei$SCC <- as.character(nei$SCC)
data_full <- left_join(scc, nei, by = "SCC")
index <- grep("[Cc]oal", data_full$EI.Sector)
data_coal <- data_full[index,]


emiss_yr <- with(data_coal, tapply(Emissions, year, sum))
png("plot4.png", width = 1024, height = 768)
plot(as.numeric(names(emiss_yr)), emiss_yr, 
     pch = 16,
     xlab = "Year", ylab = "Total PM2.5 emission (tons)",
     xaxt = "n")
axis(1, at = as.numeric(names(emiss_yr)))
title(main = "Total PM2.5 Coal Combustion Related Emissions in the United States\n(1999 - 2008)")
fit <- lm(emiss_yr ~ as.numeric(names(emiss_yr)))
abline(fit, lwd=1, col="blue")
dev.off()

# 5.  I was a little unclear as to the most correct column to use to find motor vehicle sources. 
# I chose EI.Sector and filtered for [Vv]ehicle
library(dplyr)

nei$SCC <- as.character(nei$SCC)
data_full <- left_join(scc, nei, by = "SCC")
index <- grep("[Vv]ehicle", data_full$EI.Sector)
data_vehicle <- data_full[index,]

scc_baltimore <- subset(data_vehicle, data_vehicle$fips == "24510")
emiss_yr <- with(scc_baltimore, tapply(Emissions, year, sum))

png("plot5.png", width = 1024, height = 768)
plot(as.numeric(names(emiss_yr)), emiss_yr, 
     pch = 16,
     xlab = "Year", ylab = "Total PM2.5 emission (tons)",
     xaxt = "n")
axis(1, at = as.numeric(names(emiss_yr)))
title(main = "Total PM2.5 Motor Vehicle Emissions in Baltimore City\n(1999 - 2008)")
fit <- lm(emiss_yr ~ as.numeric(names(emiss_yr)))
abline(fit, lwd=1, col="blue")
dev.off()



# 6.  Comparing Baltimore and LA County
library(dplyr)
library(reshape2)
library(ggplot2)
nei$SCC <- as.character(nei$SCC)
data_full <- left_join(scc, nei, by = "SCC")
index <- grep("[Vv]ehicle", data_full$EI.Sector)
data_vehicle <- data_full[index,]

scc_balti_lac <- subset(data_vehicle, data_vehicle$fips == "24510" | data_vehicle$fips == "06037")

scc_balti_lac <- with(scc_balti_lac, tapply(Emissions, list(year, fips), sum))
scc_balti_lac <- melt(scc_balti_lac, varnames = c("year", "county"), value.name = "emissions")

scc_balti_lac[(scc_balti_lac[,2] == 6037), 2] <- "Los Angeles County"
scc_balti_lac[(scc_balti_lac[,2] == 24510), 2] <- "Baltimore City"


p <- ggplot(scc_balti_lac, aes(x=year, y=emissions))
p <- p + geom_point(size = 2)
p <- p + geom_smooth(method = "lm", se = FALSE)
p <- p + facet_grid(county ~ ., scales = "free")
p <- p + theme(legend.position = "none")
p <- p + ggtitle("Total PM2.5 motor vehicle emissions\n (1999 - 2008, split by county)")
p <- p + labs(x="Year",y="PM2.5 emissions (tons)") 
p <- p + scale_x_continuous(breaks =unique(scc_balti_lac$year), labels=as.character(unique(scc_balti_lac$year)))
p
ggsave(filename = 'plot6.png', plot = p, type="cairo-png")
