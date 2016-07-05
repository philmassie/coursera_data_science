setwd("D:/My Folders/R/2016/coursera/mod_4_week_4/")

scc <- readRDS("summarySCC_PM25.rds", refhook = NULL)
nei <- readRDS("Source_Classification_Code.rds", refhook = NULL)

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

# Some fun with colours
# wrk out slopes for coloring the linear regression lines
# later, map them to green for decreased and red for increased
library(plyr)
library(RColorBrewer)
l.regressions <- ddply(scc_balti_lac,
                       .(county),
                       summarize,
                       intercept=lm(emissions~year)$coef[1],
                       slope=lm(emissions~year)$coef[2])
l.regressions$colref <- as.numeric(lapply(l.regressions$slope, function(x) if (x < 0) {return(1)} else {return(0)}))


# join the data for plotting
scc_balti_lac <- join(scc_balti_lac, l.regressions)



p <- ggplot(scc_balti_lac, aes(x=year, y=emissions, col=colref))
p <- p + geom_point(size = 2)
p <- p + geom_smooth(method = "lm", se = FALSE)
p <- p + scale_colour_gradientn(colours=brewer.pal(4, "RdYlGn"))
p <- p + facet_grid(county ~ ., scales = "free")
p <- p + theme(legend.position = "none")
p <- p + ggtitle("Total PM2.5 motor vehicle emissions\n (1999 - 2008, split by county)")
p <- p + labs(x="Year",y="PM2.5 emissions (tons)") 
p <- p + scale_x_continuous(breaks =unique(scc_balti_lac$year), labels=as.character(unique(scc_balti_lac$year)))
p
ggsave(filename = 'plot6.png', plot = p, dpi = 96, type="cairo-png")
