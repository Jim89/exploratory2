# Exploratory Data Analysis - Course Project 2
# Create plots
# Authour: Jim Leach
# Data: 2014/07/13

dir<-"/home/jim/R/exploratory_analysis/project2/exploratory2"
setwd(dir)
################################################################################
# Check if required are packages available, if not then install and load 
################################################################################
# 1. dplyr
if(require(dplyr,quietly=T)){
  print("loading dlplr")
} else {
  print("trying to install dplyr")
  install.packages("dplyr")
  if(require(dplyr,quietly=T)){
    } else {
    stop("could not install dplyr")
  }
}
# 2. lubridate
if(require(lubridate,quietly=T)){
  print("loading lubridate")
} else {
  print("trying to install lubridate")
  install.packages("lubridate")
  if(require(lubridate,quietly=T)){
  } else {
    stop("could not install lubridate")
  }
}
# 3. sqldf
if(require(sqldf,quietly=T)){
  print("loading sqldf")
} else {
  print("trying to install sqldf")
  install.packages("sqldf")
  if(require(sqldf,quietly=T)){
  } else {
    stop("could not install sqldf")
  }
}
# 4. ggplot2
if(require(ggplot2,quietly=T)){
  print("loading ggplot2")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  if(require(ggplot2,quietly=T)){
  } else {
    stop("could not install ggplot2")
  }
}
# 5. gridExtra
if(require(gridExtra,quietly=T)){
  print("loading gridExtra")
} else {
  print("trying to install gridExtra")
  install.packages("gridExtra")
  if(require(gridExtra,quietly=T)){
  } else {
    stop("could not install gridExtra")
  }
}

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
joined<-sqldf("select * from data as a left join sources as b on a.SCC = b.SCC")


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

################################################################################
# Plot 3 - Emissions for Baltimore by Type and Year 
################################################################################
summary_year_and_type<-data.frame(summarize(group_by(select(filter(data,fips=="24510"),Emissions,year,type),year,type),total=sum(Emissions)))
summary_year_and_type$year<-as.character(summary_year_and_type$year)
png("./plots/plot3.png",height=480,width=600,unit="px")
(ggplot(summary_year_and_type,aes(x=year,y=total))
+geom_bar(stat="identity",aes(fill=type))
+facet_grid(.~type)
+labs(x="Year",y=expression("Tons PM"[2.5]),title=expression("Total, by Type, Baltimore Emissions PM"[2.5]))
+scale_fill_manual(values=c("dodgerblue4", "firebrick", "darkgreen","gold2"))
)
dev.off()

################################################################################
# Plot 4 - Coal Combustion Emissions - US Wide
################################################################################
combustion_index<-grep("[Cc]ombustion",joined$SCC_Level_One)
combust<-joined[combustion_index,]
coal_index<-grep("[Cc]oal",combust$SCC_Level_Three)
coal<-combust[coal_index,]
coal_summary<-summarize(group_by(select(coal,year,Emissions),year),total=sum(Emissions))
png("./plots/plot4.png",height=480,width=800,unit="px")
(ggplot(coal_summary,aes(x=year,y=total))
+geom_line(color="steelblue",size=1.5)
+labs(x="Year",y=expression("Tons PM"[2.5]),title=expression("Total US Emissions from Coal Combustion Related Sources PM"[2.5]))
)
dev.off()

################################################################################
# Plot 5 - Motor Vehicle Emissions in Baltimore
################################################################################
baltimore_mobile<-filter(joined,joined$fips=="24510" & joined$SCC_Level_One=="Mobile Sources")
vehicle_index<-grep("Vehicle",baltimore_mobile$SCC_Level_Two)
balt_vehicles<-baltimore_mobile[vehicle_index,]
vehicle_summary<-summarize(group_by(select(balt_vehicles,year,Emissions),year),total=sum(Emissions))
png("./plots/plot5.png",height=480,width=600)
(ggplot(vehicle_summary,aes(x=as.character(year),y=total))
+geom_bar(stat="identity",fill="dodgerblue4")
+theme_light()
+labs(x="Year",y=expression("Tons PM"[2.5]),title=expression("Baltimore Emissions from Vehicle Sources PM"[2.5]))
)
dev.off()



################################################################################
# Plot 5 - Motor Vehicle Emissions in Baltimore
################################################################################
la_balt<-filter(joined,(joined$fips=="24510" | joined$fips=="06037") & joined$SCC_Level_One=="Mobile Sources")
la_balt$Location<-NULL
la_balt$Location<-ifelse(la_balt$fips=="24510","Baltimore City","Los Angeles")
la_balt_summary<-summarize(group_by(select(la_balt,Emissions,year,Location),Location,year),total=sum(Emissions))

plot5a<-
(ggplot(la_balt_summary,aes(x=as.character(year),y=total,fill=Location))
+geom_bar(stat="identity")
+scale_fill_manual(values=c("firebrick","dodgerblue4"))
+labs(x="Year",y=expression("Tons PM"[2.5]),title=expression("Bar Chart of Emissions from Vehicle Sources PM"[2.5]))
+theme_minimal()
+theme(legend.position="bottom")
)
plot5b<-
(
ggplot(la_balt_summary,aes(x=year,y=total,color=Location))
+geom_line()
+scale_colour_manual(values=c("firebrick","dodgerblue4"))
+labs(x="Year",y=expression("Tons PM"[2.5]),title=expression("Line Chart of Emissions from Vehicle Sources PM"[2.5]))
+theme_minimal()
+theme(legend.position="bottom")
)
png("./plots/plot6.png",height=480,width=960)
grid.arrange(plot5a,plot5b,ncol=2)
dev.off()





