#### download_region.R ###############################
#
# Purpose:
#   Download certain state USGS stream gages information
#
# created by yfhuang 20180116
# edited for data completion use by yfhuang 20181105
#####################################################

#clean the working space
rm(list = ls(all = T))
ls(all = T)

### Environment Setting (NEED MANUAL INPUT)--------
# Library (Please install if you don't have one)
library(dataRetrieval)

# Working directory
setwd("C:/Users/Yu-Fen Huang/Dropbox/Cooperation/PeakflowTrend_Hawaii/USGS_gage")
dir.data <- './Data/'
dir.fig <- './Figure/'
dir.result <- './Result/'
# dir_data <- "./All_USGS_rainGage"
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
dbPath <-  "./All_USGS_StreamGage/"# database path

startDate <- as.Date("1900-01-01")
endDate <- as.Date(Sys.Date())

## Get the list of gage information ----------------------
## (just need to run the first time, or if you want updated data)
NWIS.meta<-whatNWISdata(stateCd=state.target)
write.csv(NWIS.meta, paste(dbPath,"NWIS_meta.csv",sep = ""))
write.table(NWIS.meta, paste(dbPath,"NWIS_meta.txt", sep = ""))
## ------------------------------------------------------------

NWIS.meta <- read.csv(paste(dbPath,"NWIS_meta.csv",sep = ""))


## by the parameter
NWIS.par <- NWIS.meta[NWIS.meta$parm_cd == as.numeric(parameterCd),]
NWIS.par <- NWIS.par[!is.na(NWIS.par$X),]
write.csv(NWIS.par, paste(dbPath,"NWIS_",parameterCd,".csv",sep = ""), row.names = FALSE)

## by the type
NWIS.datatype <- NWIS.meta[grep(NWIS.meta$data_type_cd, pattern = dataType),]
NWIS.datatype <- NWIS.datatype[!is.na(NWIS.datatype$X),]
write.csv(NWIS.datatype, paste(dbPath,"NWIS_",dataType,".csv",sep = ""), row.names = FALSE)

