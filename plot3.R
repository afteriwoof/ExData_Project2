## Load libraries
library("ggplot2")

## Read in data
NEI<-readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC<-readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Subset out only Baltimore City, Maryland.
MD <- NEI[NEI$fips=="24510",]

## Initialise variables
inds_year <- unique(MD$year)
inds_type <- unique(MD$type)
totalpm <- data.frame(year=rep(inds_year,each=4),type=inds_type,pm=numeric(length(inds_year)*length(inds_type)))

## Loop over the years, to sum pollutants for each.
for(i in 1:length(inds_year)){
	## All pollutants are PM2.5 so can just sum over the full vector.
	for(j in 1:length(inds_type)){
		totalpm$pm[totalpm$year==inds_year[i] & totalpm$type==inds_type[j]] <- sum(MD[MD$year==inds_year[i] & MD$type==inds_type[j],]$Emissions)
	}
}

## Plot graph using qplot
#qplot(year,pm,data=totalpm,col=type,geom=c("point","smooth"),method="lm")

## Plot graph using ggplot
g <- ggplot(totalpm,aes(year,pm,col=type))

p <- g + geom_point(size=3) + geom_smooth(method="lm",se=F,size=1) +labs(title="Total Pollution in Baltimore City, Maryland",x="Year",y=expression(paste("Total PM"[2.5]," (tons)")))

print(p)

ggsave(file="plot3.png", plot=p, width=7, height=5)




