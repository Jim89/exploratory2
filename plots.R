# Exploratory Data Analysis - Course Project 2
# Create plots
# Authour: Jim Leach
# Data: 2014/07/13

dir<-"/home/jim/R/exploratory_analysis/project2"
setwd(dir)
################################################################################
# Step 1: Load required packages
################################################################################
require(dplyr,quietly=T)
require(lubridate,quietly=T)

################################################################################
# Step 2: Check if data exists, if not download and unzip. Then read in the data
################################################################################
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

if(!file.exists(c("summarySCC_PM25.rds","Source_Classification_Code.rds")))
  {
  download.file(url,"data.zip",method="curl",quiet=T)
  unzip("data.zip",overwrite=TRUE)
  }

data<-readRDS("summarySCC_PM25.rds")
sources<-readRDS("Source_Classification_Code.rds")
    

################################################################################
# Step 3: Plot 1 - total PM2.5 Emissions Per Year
################################################################################
summary<-summarize(group_by(select(data,Emissions,year),year),total=sum(Emissions))
options(scipen=10)
png("plot1.png")
barplot(summary$total,
        col="darkblue",
        xlab="Year",
        ylab=expression('Total Emissions (tons) PM'[2.5]),
        names.arg=summary$year
        )
dev.off()