#### download_certain_gage.R ###############################
#
# Purpose:
#   - Dowanload certain USGS peakflow gages
#   - Clean the data (remove NAs and duplicated data within a year)
#   - Convert the discharge unit into mm (devided by watershed)
#
# Input:
#   - The information of chosen gages 
#
# created by yfhuang 20180129
#####################################################

#clean the working space
rm(list = ls(all = T))
ls(all = T)

### Environment Setting (NEED MANUAL INPUT)--------
# Library (Please install if you don't have one)
#install.packages(c('curl',printr','reshape2'))
#devtools::install_github("mccreigh/rwrfhydro")
library(dataRetrieval)
library(lubridate)
library(dplyr)

# Working directory
setwd("C:/Users/Yu-Fen Huang/Dropbox/Cooperation/PeakflowTrend_Hawaii/")
dir.data <- '/Data/'
dir.fig <- '/Figure/'
dir.result <- '/Result/'

island <- "BigIsland"
# Conversion
m2ft<-3.28084
cms2cfs<-35.3146662127
m2mi<-0.000621371

m2mm<-1000
d2s<-86400

### Read the Information of Chosen Gage  ------------

## read 'NWIS_pk_Maui_pick.csv' from ./Data
gages <- read.csv(paste("./",island,dir.data,"NWIS_pk_",island,"_pick.csv",sep = ""), stringsAsFactors = F)

## Basic set-up for the dataRetrieval
siteNumbers <- as.character(gages$site_no)

parameterCd <- "00060"  #Discharge (cfs)
dataType <- "pk"  #Peakflow

startDate <- as.Date("1969-10-01")
endDate <- as.Date("2005-09-30")
startYear <- year(startDate)
endYear <- year(endDate)

### Download Each Gage ------------------

# ## Dowanload Peak Flow
# for(i in 1:length(siteNumbers)){
#   pk.input <- readNWISpeak(siteNumbers[i], startDate = startDate, endDate = endDate)
#   write.csv(pk.input,file=paste("./",island,dir.data,"Peakflow/USGS_pk_",siteNumbers[i],".csv",sep = ""),
#             row.names = F)
# }

## Download the watershed area
site_info <- readNWISsite(siteNumbers)
site_info$drain_area_m2 <- site_info$drain_area_va/m2mi**2

### Read Peak Flow and Put Them Together

pk.table <- data.frame(matrix(ncol=length(siteNumbers)+1, nrow = endYear-startYear))
colnames(pk.table) = c("water_year", siteNumbers)
pk.table$water_year <- seq(startYear+1, endYear)


year.table <- data.frame(year = seq(startYear+1, endYear))
for(i in 1:length(siteNumbers)){
  pk.site <- read.csv(paste("./",island,dir.data,"Peakflow/USGS_pk_",siteNumbers[i],".csv",sep = ""), 
                      stringsAsFactors = F)
  pk.site$peak_dt <- as.Date(pk.site$peak_dt)
  pk.site$year <- year(pk.site$peak_dt)
  pk.site$water_year <- year(pk.site$peak_dt)
  pk.site$water_year[which(month(pk.site$peak_dt)>9)] <- 
    year(pk.site$peak_dt)[which(month(pk.site$peak_dt)>9)]+1
  
 
  # Clean the duplicate data and na within a year, leave the max
  pk.site.clean <- pk.site[order(pk.site$water_year, pk.site$gage_ht), ]
  pk.site.clean <- na.omit(pk.site.clean[!duplicated(pk.site.clean$water_year), 
                c("site_no","water_year","peak_dt","peak_va","gage_ht")])
  pk.site.clean <- merge(year.table, pk.site.clean, 
                         by.x = "year", by.y = "water_year", all.x = T)
  
  # Add converted discharge from cfs to mm
  pk.site.clean$discharge_mm <- pk.site.clean$peak_va/cms2cfs/site_info$drain_area_m2[i]*m2mm*d2s
  
  # Write the peak flow data in the pk.table
  pk.table[siteNumbers[i]] <- pk.site.clean$peak_va/cms2cfs # cfs2cms

  # Export individual cleaned-up gage file
  write.csv(pk.site.clean,file=paste("./",island,dir.data,"USGS_pk_clean_",
                                   siteNumbers[i],".csv",sep = ""), row.names = F)
}

# Check and delete if there is column with all NA
pk.table <- pk.table[,colSums(is.na(pk.table))<nrow(pk.table)]

  # Export pk.table
write.csv(pk.table,file=paste("./",island,dir.data,"USGS_pk_table_cms.csv",sep = ""), row.names = F)

### Basic Statistics for all the gages ------------------
## Read peak flow table
pk.table <- read.csv(paste("./",island,dir.data, "USGS_pk_table_cms.csv", sep = ""), 
           stringsAsFactors = F)
pk.summary <- summary(pk.table)
write.csv(pk.summary, file=paste("./",island, dir.result,"pk_table_cms_summary.csv",sep = ""), 
          row.names = F)

## boxplot of all the peak flow
png(paste("./",island, dir.fig,"boxplot_pk_all_gages.png",sep = ""),
    width=1000, height=800, units="px",pointsize=26,bg="white")
par(mar=c(7,5,1,1))
boxplot(pk.table[,-1],las=2, main = paste("NWIS Peak Flow of Each Gage in", island),
        ylab = "peak flow (cms)")
dev.off()

### Convert pk.table unit into mm -------------------
site_info <- site_info[site_info$site_no %in% gsub('X',"",colnames(pk.table)[2:ncol(pk.table)]),]
colnames(pk.table)[2:ncol(pk.table)] == paste("X",site_info$site_no,sep = "")  # Check the site no is in the same order
pk.table.unit<-data.frame(year=pk.table[,1])
for (i in 2:ncol(pk.table)-1){
  pk.table.unit[,i+1] <- pk.table[,i+1]/site_info$drain_area_m2[i]*m2mm*d2s
}
colnames(pk.table.unit) = c("water_year", colnames(pk.table)[2:ncol(pk.table)])
write.csv(pk.table.unit,file=paste("./",island,dir.data,"USGS_pk_table_mm.csv",sep = ""), row.names = F)


## trend of peak flow

pk.trend.linear <- data.frame(trend.coef.1 = NA, trend.coef.2 = NA)
pk.ts <- ts(pk.table)
# Linear model
for (i in 2:ncol(pk.table)){
  model.linear <- lm(pk.table[,i] ~ c(1970:(1969+length(pk.ts[,i]))))
  model.linear.f <- lm(pk.table[,i] ~ c(1:+length(pk.ts[,i])))
  png(paste("./",island,dir.fig,"trend_linear_",colnames(pk.table[i]),".png",sep = ""),
      width=1000, height=800, units="px",pointsize=26,bg="white")
  {plot(pk.table[,1],pk.table[,i], type = "p",
        xlab = NA, ylab = "peak flow (cms)")
    abline(coef = model.linear$coefficients, col = "red")
    legend("topright",paste("y =",model.linear.f$coefficients[1], ifelse(sign(model.linear.f$coefficients[2])==1, " + ", " - "), 
                            abs(model.linear.f$coefficients[2]), " x ", sep = " "))}
  dev.off()
  pk.trend.linear[i-1,1:2] <- model.linear.f$coefficients[1:2]
}
rownames(pk.trend.linear) <- colnames(pk.table[2:ncol(pk.table)])

write.csv(pk.trend.linear, file=paste("./",island, dir.result,"pk_trend_linear_coef.csv",sep = ""))

