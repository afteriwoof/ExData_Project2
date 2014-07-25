## Load libraries
library("ggplot2")

## Read in data
NEI<-readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC<-readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Subset out only coal combustion pollutant.
coals <- SCC$SCC[grep("comb.*coal",SCC$EI.Sector,ignore.case=TRUE)]
sub_c <- !is.na(match(NEI$SCC,coals))
NEI <- NEI[sub_c, ]

## Initialise variables
inds <- unique(NEI$year)
totalpm <- numeric(length(inds))

## Loop over the years, to sum pollutants for each.
for(i in 1:length(inds)){
	## All pollutants are PM2.5 so can just sum over the full vector.
	totalpm[i] <- sum(NEI[NEI$year==inds[i],]$Emissions)
}

## Plot graph
plot(inds,totalpm,xlab="Year",ylab=expression(paste("Total PM"[2.5]," (tons)")),main="Total Coal Combustion-related Emissions in the United States",pch=19)
model <- lm(totalpm~inds)
abline(model,lwd=2,col="blue")
legend("topright",legend="Data",pch=19)

## Copy to png file
dev.copy(png, "plot4.png")
dev.off()


