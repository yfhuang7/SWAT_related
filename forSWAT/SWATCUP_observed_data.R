### Create SWATCUP calibration observed data
#
#  The gage observed data for calibration in SWATCUP
#
#  Format:
#     
#     1	FLOW_OUT_1_2005	57.455
#     2	FLOW_OUT_2_2005	75
#     3	FLOW_OUT_3_2005	61.123
#     ......
#     ......
#     2191	FLOW_OUT_2191_2010	17.297 
#     (in here, better to rename it FLOW_OUT_365_2010. It does not matter but this looks more natural, the flow of 365th day of 2010)
#
#  Created by yfhuang 20170916
#
#==============================

### Environment Setting ====================
#clean the working space
rm(list = ls(all = T))
ls(all = T)

# Working directory
setwd("G:/My Drive/R_Script/R_workplace/USGS_gage")

dir.data <- "./data/"
dir.result <- "./result/"
filename <- "USGS_AlaWai.csv"  # USGS Site info file

### Parameter Setting =======================

# Conversion
m2ft<-3.28084
cms2cfs<-35.3146662127

# The similartion start and end year, years of warmup
year_start<-1999
year_end<-2012#2012
year_warmup<-5
year<-year_start+year_warmup

# Site Number from my USGS gage filename
site.info <- read.csv(paste(dir.data,filename, sep = ""), stringsAsFactors = F)
SiteNumbers <- as.character(site.info$site_no)

### Read and re-write data ==================

# Input USGS data
for(i in 1:length(SiteNumbers)){
  input<-read.csv(paste("./result/USGS_daily_",SiteNumbers[i],".csv",sep = ""))
  input$Date<-as.Date(format(input$Date),"%Y-%m-%d")
  input<-data.frame(DATE=input$Date,DISCHARGE_cms=input$X_00060_00003/cms2cfs)
  
  idata<-data.frame(DATE=seq(as.Date(paste(year,"/01/01",sep = '')),
                             as.Date(paste(year_end,"/12/31",sep = '')),1),
                    YEAR=NA,DAY=NA,ID=NA,FLOW_OUT=NA)
  idata$YEAR<-as.numeric(format(idata$DATE, format = "%Y"))
  idata$DAY<-as.numeric(strftime(idata$DATE, format = "%j"))
  idata$ID<-seq(1,nrow(idata),1)
  idata$FLOW_OUT<-paste("FLOW_OUT",idata$DAY,idata$YEAR,sep = "_")
  
  combine<-merge(idata,input,by='DATE')
  which(is.na(combine$DISCHARGE_cms))  #Check if we have NA in the discharge
  
  output<-combine[,4:6]
   write.table(output,file = paste("./SWATCUP_observed/FLOW_OUT_",site.info[site.info$site_no==SiteNumbers[i],"SUBBASIN"],'_',nrow(output),".txt",sep = ""),
               sep ="\t",quote = FALSE, row.names = FALSE, col.names = FALSE)
}

