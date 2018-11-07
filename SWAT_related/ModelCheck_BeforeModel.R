### Check the USGS gage, make sure there is no tidal effect before modeling

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
year_start<-2010
year_end<-2010#2012
year_warmup<-5
year<-year_start+year_warmup



# Site Number from my USGS gage filename
site.info <- read.csv(paste(dir.data,filename, sep = ""), stringsAsFactors = F)
SiteNumbers <- as.character(site.info$site_no)


## Plot --------------------------------


for(i in 6:length(SiteNumbers)){

i=10
  input<-read.csv(paste("./result/subdaily_",SiteNumbers[i],".csv",sep = ""), stringsAsFactors = F)
  input$dateTime<-as.POSIXct(input$dateTime)
  input<-data.frame(DATE=input$dateTime,DISCHARGE_cms=input$X_00060_00000/cms2cfs)
  
  start.nm <- which(input$DATE==paste(year_start,"-12-01 00:00:00",sep = ''))
  end.nm <- which(input$DATE==paste(year_end,"-12-18 00:00:00",sep = ''))
  
  jpeg(paste(dir.result, "SWAT_",SiteNumbers[i],".jpg",sep = ""), width = 1600, height = 1200,
       units = "px", pointsize = 30, quality = 75)
  plot(input[start.nm:end.nm,], main = paste(SiteNumbers[i]), ylim = c(0,2))
  dev.off()

}



