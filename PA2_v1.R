## Getting the Data
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl,destfile="weather.csv",method="curl")
weather <- read.csv("weather.csv",header=TRUE)
# load useful packages
library(dplyr)
#pare down the file to the relevant variables
weatherdat<-weather[,c("BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","CROPDMG","REFNUM")]
attach(weatherdat)
weatherdat$health<-FATALITIES+INJURIES
weatherdat$damage<-PROPDMG+CROPDMG
#create table of health impacts by type
badhealth<-group_by(weatherdat,EVTYPE)
badhealth<-summarise(badhealth,healthsum=sum(health))
qbadhealth<-quantile(badhealth$healthsum,seq(0,1,by=0.01))
badhealth$impgroups<-cut(badhealth$healthsum,unique(qbadhealth),include.lowest=TRUE)
#find the top ten events with the maximum health impact
# histogram of healthimpacts to see distribution
hist(log(badhealth$healthsum))
table(badhealth$EVTYPE,badhealth$impgroups)
table(badhealth$impgroups)
hlevels<-levels(badhealth$impgroups)
topten<-filter(badhealth,badhealth$impgroups==hlevels[16])
# calculate events with greatest damages
#create table of damages by type
bigdamages<-group_by(weatherdat,EVTYPE)
bigdamages<-summarise(bigdamages,damagesum=sum(damage))
qdamages<-quantile(bigdamages$damagesum,seq(0,1,by=0.01))
bigdamages$impgroups<-cut(bigdamages$damagesum,unique(qdamages),include.lowest=TRUE)
#find top twenty events causing damages
hist(log(bigdamages$damagesum))
table(bigdamages$impgroups)
hlevels<-levels(bigdamages$impgroups)
dtopten<-filter(bigdamages,bigdamages$impgroups==hlevels[36])
#produce information about the environment
sessionInfo()



