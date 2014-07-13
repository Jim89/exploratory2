# Exploratory Data Analysis - Course Project 2
# Create plots
# Authour: Jim Leach
# Data: 2014/07/13

dir<-"/home/jim/R/exploratory_analysis/project2/exploratory2"
setwd(dir)
################################################################################
# Load required packages
################################################################################
require(dplyr,quietly=T)
require(lubridate,quietly=T)

################################################################################
# Check if data exists, if not download and unzip. Then read in the data
################################################################################
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists("./data/summarySCC_PM25.rds")|!file.exists("./data/Source_Classification_Code.rds"))
  {
  download.file(url,"./data/data.zip",method="curl",quiet=T)
  unzip("./data/data.zip",overwrite=TRUE,exdir="./data")
  }
data<-readRDS("./data/summarySCC_PM25.rds")
sources<-readRDS("./data/Source_Classification_Code.rds")    

################################################################################
# Plot 1 - total PM2.5 Emissions Per Year - all regions
################################################################################
all_regions_summary<-summarize(group_by(select(data,Emissions,year),year),total=sum(Emissions))
options(scipen=10)
png("./plots/plot1.png")
barplot(all_regions_summary$total,
        col="darkblue",
        xlab="Year",
        ylab=expression('Tons PM'[2.5]),
        main=expression("Total US Emissions PM"[2.5]),
        names.arg=all_regions_summary$year
        )
dev.off()

################################################################################
# Plot 2 - total PM2.5 Emissions Per Year - Baltimore, MD only
################################################################################
baltimore <-filter(data,data$fips=="24510")
balt_summary<-summarize(group_by(select(baltimore,Emissions,year),year),total=sum(Emissions))
rm(baltimore)
png("./plots/plot2.png")
barplot(balt_summary$total,
        col="darkblue",
        xlab="Year",
        ylab=expression('Tons PM'[2.5]),
        main=expression("Total Baltimore Emissions PM"[2.5]),
        names.arg=balt_summary$year        
)
dev.off()
