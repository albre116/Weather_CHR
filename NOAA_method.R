library(devtools)
library(rnoaa)
library(ggmap)
library(ggplot2)
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



#DAILY
#DAILY_NORMAL

####construct averages using tapply
AVG_DATA<-DAILY[DAILY$datatype %in% c("TMIN","TMAX"),]
id<-as.factor(paste(AVG_DATA$station,AVG_DATA$date,sep="~"))
AVG<-tapply(AVG_DATA$value,id,mean)
NAMES<-as.data.frame(strsplit(names(AVG),"~"))
NAMES<-data.frame(t(NAMES))
rownames(AVG)<-NULL
rownames(NAMES)<-NULL


####combine all data into 1 stacked data frame
AVG<-data.frame(AVG)
#colnames(AVG)<-colnames(DAILY)[2]
#colnames(NAMES)<-colnames(DAILY)[c(1,4)]
AVG<-data.frame("station"=NAMES[1],"value"=AVG,"datatype"="TAVG","date"=NAMES[2],stringsAsFactors=FALSE)
AVG$date<-as.character(AVG$date)
ALL<-rbind(DAILY[,1:4],AVG,DAILY_NORMAL[,1:4])
ALL$date<-as.Date(ALL$date)
summary(ALL)
###Convert Measurements to Degrees, not Degrees * 10
idx<-ALL$datatype %in% c("TAVG","TMAX","TMIN","DLY-TAVG-NORMAL")
ALL$value[idx]<-ALL$value[idx]/10 ###get rid of extra 10 multiplier
####Convert Celsius measures to Farenheight
idx<-ALL$datatype %in% c("TAVG","TMAX","TMIN")
ALL$value[idx]<-ALL$value[idx]*(9/5)+32###C to F conversion



theGraph <- ggplot(ALL,aes(x=date, y=value)) + 
  geom_point(size = 1) + facet_wrap(~station+datatype, scales = "free") + 
  xlab("Date") + ylab(NULL) + ggtitle("Raw Data")
print(theGraph)








