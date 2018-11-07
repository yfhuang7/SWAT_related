#### data_completion.R ###############################
#
# Purpose:
#    Check the data completion of USGS pk gages
#
# created by yfhuang 20181195
#####################################################

#clean the working space
rm(list = ls(all = T))
ls(all = T)

### Environment Setting (NEED MANUAL INPUT)--------
# Library (Please install if you don't have one)
library(lubridate)
library(reshape)
library(ggplot2)

# Working directory
setwd("C:/Users/Yu-Fen Huang/Dropbox/Cooperation/PeakflowTrend_Hawaii/USGS_gage")
dir.data <- './Data/'
dir.fig <- './Figure/'
dir.result <- './Result/'
dbPath <-  "./All_USGS_StreamGage/"# database path


### Main content ======================

## Basic Setting

# State
state.target <- "HI"
# Area of interest
lat.min <- 18.71
lat.max <- 22.3386
lon.min <- -160.3922
lon.max <- -154.6271

parameterCd <- "00060"  # 00060: Discharge (cfs), 00045: rainfall (in)
dataType <- "pk"  #Peakflow: pk; daily data: dv

startDate <- as.Date("1910-10-01")
endDate <- as.Date(Sys.Date())

# Conversion
m2ft<-3.28084
cms2cfs<-35.3146662127
m2mi<-0.000621371

m2mm<-1000
d2s<-86400

### Read the Information of Chosen Gage  ------------
## read 'NWIS_pk_Maui_pick.csv' from ./Data
gages <- read.csv(paste(dbPath, "NWIS_",dataType,".csv", sep = ""), stringsAsFactors = F)

## Basic set-up for the dataRetrieval
siteNumbers <- as.character(gages$site_no)

### Download Each Gage ------------------

# ## Dowanload USGS gage data by dataType
# for(i in 1:length(siteNumbers)){
#   input <- readNWISpeak(siteNumbers[i], startDate = startDate, endDate = endDate)
#   write.csv(input,file=paste(dir.data,"USGS_",dataType,"_",siteNumbers[i],".csv",sep = ""),
#             row.names = F)
# }

## Download the watershed area and convert the unit to m2
site_info <- readNWISsite(siteNumbers)
site_info$drain_area_m2 <- site_info$drain_area_va/(m2mi**2)

### Clean data for each gage ---------------------

## Clean up duplicate peakflow data in the same water year

pk.table <- data.frame(matrix(ncol=length(siteNumbers)+1, nrow = year(endDate)-year(startDate)))
colnames(pk.table) = c("water_year", siteNumbers)
pk.table$water_year <- (year(startDate)+1):year(endDate)

year.table <- data.frame(year = (year(startDate)+1):year(endDate))

for(i in 1:length(siteNumbers)){
  pk.site <- read.csv(paste(dir.data,"USGS_",dataType,"_",siteNumbers[i],".csv",sep = ""), 
                    stringsAsFactors = F)
  pk.site$peak_dt <- as.Date(pk.site$peak_dt)
  pk.site$year <- year(pk.site$peak_dt)
  pk.site$water_year <- year(pk.site$peak_dt)
  pk.site$water_year[which(month(pk.site$peak_dt)>9)] <- 
    year(pk.site$peak_dt)[which(month(pk.site$peak_dt)>9)]+1
  
  # Clean the duplicate data and na within a year, save the max
  pk.site.clean <- pk.site[order(pk.site$water_year, pk.site$gage_ht), ]
  pk.site.clean <- na.omit(pk.site.clean[!duplicated(pk.site.clean$water_year), 
                                         c("site_no","water_year","peak_dt","peak_va","gage_ht")])
  pk.site.clean <- merge(year.table, pk.site.clean, 
                         by.x = "year", by.y = "water_year", all.x = T)
  
  # Add converted discharge from cfs to cms
  pk.site.clean$discharge_cms <- pk.site.clean$peak_va/cms2cfs
  # Add standadized discharge from cfs to mm
  pk.site.clean$discharge_mm <- pk.site.clean$peak_va/cms2cfs/site_info$drain_area_m2[i]*m2mm*d2s
  
  # Write the peak flow data in the pk.table
  pk.table[siteNumbers[i]] <- pk.site.clean$peak_va/cms2cfs # cfs2cms
  
  # Export individual cleaned-up gage file
  write.csv(pk.site.clean,file=paste(dir.data,"USGS_",dataType,"_clean_",
                                     siteNumbers[i],".csv",sep = ""), row.names = F)
}


### Summarize all the gages ----------------------------

## by discharge
allGage <- data.frame(matrix(ncol=length(siteNumbers)+1, nrow = year(endDate)-year(startDate)))
colnames(allGage) = c("water_year", siteNumbers)
allGage$water_year <- (year(startDate)+1):year(endDate)

for(i in 1:length(siteNumbers)){
  idata <- read.csv(file=paste(dir.data,"USGS_",dataType,"_clean_",
                               siteNumbers[i],".csv",sep = ""), stringsAsFactors = F)
  idata$peak_dt <- as.Date(idata$peak_dt)
  allGage[siteNumbers[i]] <- idata$discharge_cms
}

write.csv(allGage, paste(dir.result,"allGage_",dataType,"_discharge_cms.csv", sep = ""), row.names = F)


## by date
allGage <- data.frame(matrix(ncol=length(siteNumbers)+1, nrow = year(endDate)-year(startDate)))
colnames(allGage) = c("water_year", siteNumbers)
allGage$water_year <- (year(startDate)+1):year(endDate)

for(i in 1:length(siteNumbers)){
  idata <- read.csv(file=paste(dir.data,"USGS_",dataType,"_clean_",
                               siteNumbers[i],".csv",sep = ""), stringsAsFactors = F)
  idata$peak_dt <- as.Date(idata$peak_dt)
  allGage[siteNumbers[i]] <- idata$peak_dt
}

write.csv(allGage, paste(dir.result,"allGage_",dataType,"_data.csv", sep = ""), row.names = F)

### Convert to data tag --------------------------------
allGage[,c(2:ncol(allGage))] <- lapply(allGage[,c(2:ncol(allGage))], as.character)
allGage[!is.na(allGage)] <- "Y"
allGage$water_year <- (year(startDate)+1):year(endDate)
allGage <- allGage[,colSums(is.na(allGage))<nrow(allGage)]

allGage.melt <- melt(allGage, id = "water_year")

### Plot -------------------------------------------------

## allGage
ggplot(allGage.melt, aes(x=water_year, y=as.factor(variable), fill=value)) +
  #redrawing tiles to remove cross lines from legend
  geom_tile(colour="white",size=0.25)+
  #remove axis labels, add title
  labs(x="",y="",title="Data Completion") + 
  scale_x_discrete(name = "Water Year", expand=c(0,0),
                   breaks=c((year(startDate)+1):year(endDate)),
                   labels = c((year(startDate)+1):year(endDate))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # scale_y_discrete(expand =c(0,0),
  #                  breaks=
  #                  )
  scale_fill_manual(name = "Data avalibility",values="black", na.value = "grey90",
                    labels = "Yes")

ggsave(filename=paste(dir.fig,"allGage_",dataType,".png",sep = ""),
       dpi = 300, width = 16, height = 12, units = "in")


## by island
island <- c("Kauai","Oahu","Molokai","Maui","BigIsland")
for(n in 1:length(island)){
  info.island <- read.csv(paste(dbPath,"NWIS_",dataType,"_",island[n],".csv",sep = ""))
  info.island$site_no <- as.character(info.island$site_no)
  island.no <- which(colnames(allGage) %in% info.island$site_no)
  allGage.island <- allGage[,c(1,island.no)]
  
  allGage.island.melt <- melt(allGage.island, id = "water_year")
  
  ggplot(allGage.island.melt, aes(x=as.factor(water_year), y=as.factor(variable), fill=value)) +
    #redrawing tiles to remove cross lines from legend
    geom_tile(colour="white",size=0.25)+
    #remove axis labels, add title
    labs(y="",title=paste("Peakflow Data Completion in", island[n])) + 
    scale_x_discrete(name = "Water Year",
                     breaks= (year(startDate)+1):year(endDate),
                     labels = as.character(c((year(startDate)+1):year(endDate)))) +
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
    # scale_y_discrete(expand =c(0,0),
    #                  breaks=
    #                  )
    scale_fill_manual(name = "Data avalibility",values="black", na.value = "grey90",
                      labels = "Yes")
  
  ggsave(filename=paste(dir.fig,"allGage_",dataType,"_",island[n],".png",sep = ""),
         dpi = 300, width = 16, height = 12, units = "in")

}
