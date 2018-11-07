####### USGS_NWIS_oahu_data.r ###############
#
# Purpose
#     : Get information of each gage (stream gages Ohau here)
#
# Created by yfhuang on 2017/10/14
#############################################

#clean the working space
rm(list = ls(all = TRUE))
ls(all = TRUE)

### Environment Setting --------

# directories
setwd("C:/Users/Yu-Fen Huang/GoogleDrive_yfhuang/R_Script/R_workplace/USGS_gage")

directory.data<-"./data/"
directory.result<-"./result/"
directory.figure<-"./figures/"

# Conversion
m2ft<-3.28084
cms2cfs<-35.3146662127

# Library
library(dataRetrieval)
library(ggplot2)
library(RColorBrewer)

### Choose the gage ----------------
# Read the chosen gage data we want (get the site_no)
USGS.NWIS.Oahu<-read.csv(paste(directory.data,"USGS_NWIS_Oahu.txt",sep = ""))
USGS.NWIS.Oahu.ST<-USGS.NWIS.Oahu[USGS.NWIS.Oahu$CATEGORY=="ST",]
# str(USGS.NWIS.Oahu.ST)
siteNumbers <- USGS.NWIS.Oahu.ST$SITENO

# Daily discharge

available.daily.usgs<-whatNWISdata(siteNumbers = as.character(siteNumbers), service = c("dv"),
                                   parameterCd = c("00060"), statCd = "00003")
length(unique(available.daily.usgs$site_no)) #Check if there is site repeat

year_begin<-as.numeric(format(as.Date( available.daily.usgs$begin_date, format="%d/%m/%Y"),"%Y"))
year_end<-as.numeric(format(as.Date( available.daily.usgs$end_date, format="%d/%m/%Y"),"%Y"))
available.daily.usgs$len_year<-year_end-year_begin

png(filename = paste(directory.figure,"data_range_daily_oahu.png",sep = ""),
    width = 1200, height = 1600, res = 200)
ggplot(available.daily.usgs, aes(site_no, -len_year, fill=site_no)) +
  geom_bar(stat="identity") + coord_flip() + theme(legend.position="none") +
  labs(title="Avaliable USGS daily data in Oahu", x="USGS Site Number", y = "Year")+
  scale_y_continuous(breaks=seq(0,-max(available.daily.usgs$len_year),-7),
                     label=as.character(seq(max(year_end),min(year_begin),-7)))
dev.off()

# Subdaily discharge
available.subdaily.usgs<-whatNWISdata(siteNumbers = as.character(siteNumbers),
                                      service = "uv", parameterCd = c("00060"), 
                                      statCd = "00003")
length(unique(available.subdaily.usgs$site_no)) #Check if there is site repeat
year_begin<-as.numeric(format(as.Date(available.subdaily.usgs$begin_date, format="%d/%m/%Y"),"%Y"))
year_end<-as.numeric(format(as.Date(available.subdaily.usgs$end_date, format="%d/%m/%Y"),"%Y"))
available.subdaily.usgs$len_year<-year_end-year_begin

png(filename = paste(directory.figure,"data_range_subdaily_oahu.png",sep = ""),
    width = 1200, height = 1600, pointsize = 1, res = 200)
ggplot(available.subdaily.usgs, aes(site_no, -len_year, fill=site_no)) +
  geom_bar(stat="identity") + coord_flip() + theme(legend.position="none") +
  labs(title="Avaliable USGS subdaily data in Oahu", x="USGS Site Number", y = "Year")+
  scale_y_continuous(breaks=seq(0,-max(available.subdaily.usgs$len_year),-10),
                     label=as.character(seq(max(year_end),min(year_begin),-10)))
dev.off()
