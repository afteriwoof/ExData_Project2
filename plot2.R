
## Read in data
NEI<-readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC<-readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Subset out only Baltimore City, Maryland.
MD <- NEI[NEI$fips=="24510",]

## Initialise variables
inds <- unique(MD$year)
totalpm <- numeric(length(inds))

## Loop over the years, to sum pollutants for each.
for(i in 1:length(inds)){
	## All pollutants are PM2.5 so can just sum over the full vector.
	totalpm[i] <- sum(MD[MD$year==inds[i],]$Emissions)
}

## Plot graph
plot(inds,totalpm,xlab="Year",ylab=expression(paste("Total PM"[2.5]," (tons)")),main="Total Emissions in Baltimore City, Maryland",pch=19)
model <- lm(totalpm~inds)
abline(model,lwd=2,col="blue")
legend("topright",legend="Data",pch=19)

## Copy to png file
dev.copy(png, "plot2.png")
dev.off()


