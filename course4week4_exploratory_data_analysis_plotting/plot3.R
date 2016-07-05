setwd("D:/My Folders/R/2016/coursera/mod_4_week_4/")

scc <- readRDS("summarySCC_PM25.rds", refhook = NULL)
nei <- readRDS("Source_Classification_Code.rds", refhook = NULL)

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
ggsave(filename = 'plot3.png', plot = p, dpi = 96, type="cairo-png")
p
