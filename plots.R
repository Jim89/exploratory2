dir<-"/home/jim/R/exploratory_analysis/project2"
setwd(dir)

require(dplyr,quietly=T)
require(lubridate,quietly=T)

url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url,"data.zip",method="curl",quiet=T)
unzip("data.zip",overwrite=TRUE)

data<-readRDS("summarySCC_PM25.rds")
    
summary<-summarize(group_by(select(data,Emissions,year),year),total=sum(Emissions))
barplot(summary$total,
        col="red",
        xlab="Year",
        ylab=expression('Total Emissions (tons) PM'[2.5]),
        names.arg=summary$year
        )
