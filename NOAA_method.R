install.packages("devtools")
library(devtools)
install_github("rnoaa", "ropensci")
library(rnoaa)
options(noaakey = "QDqOpowUVBqxuNYLygxnxXXjxFOqaJuy") ###Mark Albrecht NOAA Key


orig_zips<-as.character(c(55317))
dest_zips<-as.character(c("00070"))

location<-paste("ZIP:",c(orig_zips,dest_zips),sep="")
noaa_datasets(locationid =location,limit=1000)
noaa_datacats(datasetid="GHCND")

###here is the code to extract the daily normals
avg_temp<-noaa(datasetid='NORMAL_DLY', locationid =location, startdate = '2010-01-01',
     enddate = '2011-01-01',limit=1000,datatypeid='DLY-TAVG-NORMAL')
noaa(datasetid='GHCND', locationid ='ZIP:55317', startdate = '2010-01-01',
               enddate = '2011-01-01',limit=1000)
