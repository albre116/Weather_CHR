library(devtools)
library(rnoaa)
library(ggmap)
options(noaakey = "QDqOpowUVBqxuNYLygxnxXXjxFOqaJuy") ###Mark Albrecht NOAA Key



#####find weather stations meeting the recording requirements######
address<-c("Bakersfield CA","NEW YORK")###to be geocoded through googlemaps
geocode_result<-geocode(address, output="all")###geocode through googlemaps

extents_box<-list()
station_daily<-list()
station_normals<-list()
station_both_data<-list()
i=1
for (i in 1:length(geocode_result)){
extents_box[[i]]<-unlist(geocode_result[i][[1]]$results[[1]]$geometry$bounds)[c(3,4,1,2)]
station_daily[[i]]<-noaa_stations(extent=extents_box[[i]],datasetid=c('GHCND'),limit=1000)
station_normals[[i]]<-noaa_stations(extent=extents_box[[i]],datasetid=c("NORMAL_DLY"),limit=1000)
idx<-station_daily[[i]]$data$id %in% station_normals[[i]]$data$id
station_both_data[[i]]<-station_daily[[i]]$data[idx,]
}

###select best weather station (for now with highest data coverage)###
i=1
chosen<-list()
for (i in 1:length(station_both_data)){
  chosen[[i]]<-station_both_data[[i]][which.max(station_both_data[[i]]$datacoverage),]
}


i=1
DAILY<-data.frame();DAILY_NORMAL<-data.frame()
for (i in 1:length(chosen)){
STOP=FALSE
startdate<-as.Date('2009-01-01')
enddate<-Sys.Date()
while (STOP==FALSE){###advance data collection 6 months
print(startdate)
next_date<-strsplit(as.character(startdate),"-")
if(next_date[[1]][2]=="01"){next_date[[1]][2]="07"}else{
  next_date[[1]][2]="01"
  next_date[[1]][1]=as.character(as.numeric(next_date[[1]][1])+1)
}
next_date<-as.Date(paste(next_date[[1]],collapse="-"))

if(next_date>=enddate){
  next_date=enddate
  STOP=TRUE
}

TMP<-noaa(datasetid='GHCND', stationid =chosen[[i]]$id, startdate = as.character(startdate),
                 enddate = as.character(next_date),datatypeid=c("TMAX","TMIN","PRCP","SNOW"),limit=1000)
DAILY<-rbind(DAILY,TMP$data)
startdate=next_date
}

TMP<-noaa(datasetid='NORMAL_DLY', stationid =chosen[[i]]$id, startdate = '2010-01-01',
                        enddate = '2011-01-01',limit=1000,datatypeid='DLY-TAVG-NORMAL')
DAILY_NORMAL<-rbind(DAILY_NORMAL,TMP$data)
}

DAILY
DAILY_NORMAL

