## Load libraries
library("ggplot2")

## Read in data
NEI<-readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC<-readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Subset Baltimore City, Maryland.
BC <- NEI[NEI$fips=="24510", ]
## Subset Loa Angeles County, California.
LA <- NEI[NEI$fips=="06037", ]

## Subset out only coal combustion pollutant.
vehicles <- SCC$SCC[grep("Vehicles",SCC$EI.Sector,ignore.case=TRUE)]
sub_v <- !is.na(match(BC$SCC,vehicles))
BC <- BC[sub_v, ]
sub_v <- !is.na(match(LA$SCC,vehicles))
LA <- LA[sub_v, ]

## Initialise variables
inds_year <- unique(NEI$year)
totalpm <- data.frame(city=rep(c("Baltimore City","Los Angeles County"),each=length(inds_year)),year=rep(inds_year),pm=numeric(length(inds_year)*2))

## Loop over the years, to sum pollutants for each.
for(i in 1:length(inds_year)){
	totalpm$pm[totalpm$city=="Baltimore City"&totalpm$year==inds_year[i]] <- sum(BC[BC$year==inds_year[i],]$Emissions)
	totalpm$pm[totalpm$city=="Los Angeles County"&totalpm$year==inds_year[i]] <- sum(LA[LA$year==inds_year[i],]$Emissions)
}

## Plot graph
g <- ggplot(totalpm, aes(year,pm,col=city))
p <- g + geom_point(size=3) + geom_smooth(method="lm",se=F,size=1) +labs(title="Total Motor Vehicle Emissions Pollution",x="Year",y=expression(paste("Total PM"[2.5]," (tons)")))
print(p)
ggsave(file="plot6.png",plot=p,width=7,height=5)



