#### download_region.R ###############################
#
# Purpose:
#   1. Download all the Hawaii USGS stream gages information
#   2. Separate data based on island
#   3. Choose the gages based on the parameter that we want
#
# created by yfhuang 20180116
#####################################################

#clean the working space
rm(list = ls(all = T))
ls(all = T)

### Environment Setting (NEED MANUAL INPUT)--------
# Library (Please install if you don't have one)
#install.packages(c('curl',printr','reshape2'))
#devtools::install_github("mccreigh/rwrfhydro")
library(dataRetrieval)

# Working directory
setwd("C:/Users/Yu-Fen Huang/Dropbox/Cooperation/PeakflowTrend_Hawaii/USGS_gage")
dir_data <- "./All_USGS_StreamGage/"
# dir_data <- "./All_USGS_rainGage"
### Main content ======================

## Basic Setting
island <- 'Kauai'   # Note Hawaii needs to turn on the lat lon threshold
parameterCd <- "00045"  # 00060: Discharge (cfs), 00045: rainfall (in)
dataType <- "dv"  #Peakflow: pk; daily data: dv
dbPath <- paste('../',island,'/',sep = "")

startDate <- as.Date("1970-01-01")
endDate <- as.Date("2005-12-31")

# ## Get the list of gage information ----------------------
# ## (just need to run the first time, or if you want updated data)
# NWIS.meta<-whatNWISdata(stateCd="HI")
# write.csv(NWIS.meta, paste(dir_data,"NWIS_meta.csv",sep = ""))
# write.table(NWIS.meta, paste(dir_data,"NWIS_meta.txt", sep = ""))
# ## ------------------------------------------------------------

NWIS.meta <- read.csv(paste(dir_data,"NWIS_meta.csv",sep = ""))

# # via parameterCd
NWIS.par <-NWIS.meta[NWIS.meta$parm_cd == as.numeric(parameterCd),]
NWIS.par.island <- NWIS.par[grep(NWIS.par$station_nm, pattern=island, ignore.case=TRUE),]
write.csv(NWIS.par.island, paste(dbPath,"NWIS_",parameterCd,"_",island,".csv",sep = ""))
# 
# # via dataType: Peakflow
# NWIS.pk <- NWIS.meta[grep(NWIS.meta$data_type_cd, pattern = dataType),]
# NWIS.pk.island <- NWIS.pk[grep(NWIS.pk$station_nm, pattern=island, ignore.case=TRUE),]
# write.csv(NWIS.pk.island, paste(dbPath,"NWIS_",dataType,"_",island,".csv",sep = ""))


## Only for BigIsland
NWIS.par <-NWIS.meta[NWIS.meta$parm_cd == as.numeric(parameterCd),]
NWIS.par.island <- NWIS.par[NWIS.par$dec_lat_va<20.35,]
NWIS.par.island <- NWIS.par.island[!is.na(NWIS.par.island$site_no),]
write.csv(NWIS.par.island, paste(dbPath,"NWIS_",parameterCd,"_",island,".csv",sep = ""))

NWIS.pk <- NWIS.meta[grep(NWIS.meta$data_type_cd, pattern = dataType),]
NWIS.pk.island <- NWIS.pk[NWIS.pk$dec_lat_va<20.35,]
NWIS.pk.island <- NWIS.pk.island[!is.na(NWIS.pk.island$site_no),]
write.csv(NWIS.pk.island, paste(dbPath,"NWIS_",dataType,"_",island,".csv",sep = ""))


## Find the peak flow gage within the time we want ---------

# Peakflow
NWIS.pk.island$begin_date<-as.Date(as.character(NWIS.pk.island$begin_date))
NWIS.pk.island$end_date<-as.Date(as.character(NWIS.pk.island$end_date))

NWIS.pick<-
  NWIS.pk.island[NWIS.pk.island$begin_date<=startDate & NWIS.pk.island$end_date>=endDate,]
write.csv(NWIS.pick, paste(dbPath,"NWIS_",dataType,"_",island,"_pick.csv",sep = ""))
#length(NWIS.pk.island[NWIS.pk.island$count_nu>35,])

# ParameterCd
NWIS.par.island$begin_date<-as.Date(as.character(NWIS.par.island$begin_date))
NWIS.par.island$end_date<-as.Date(as.character(NWIS.par.island$end_date))

NWIS.pick<-
  NWIS.par.island[NWIS.par.island$begin_date<=startDate & NWIS.par.island$end_date>=endDate,]

write.csv(NWIS.pick, paste(dbPath,"NWIS_",parameterCd,"_",island,"_pick.csv",sep = ""))


