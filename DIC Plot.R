library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)
library(imputeTS)

#calculate DIC Loads
#get stream data (april 1 - November 15)
setwd('~/Dropbox/LaMe GLM Calibration/Greedy/')
dorn<-read.csv('Mendota_dorn.csv')
sixmile<-read.csv('Mendota_sixmile.csv')
pb<-read.csv('Mendota_pheasant.csv')
yahara<-read.csv('Mendota_yahara.csv')
outflow<-read.csv('Mendota_outflow.csv')

#shorten stream data to April 16 - November 11
dorn_short<-dorn[16:225,]
sixmile_short<-sixmile[16:225,]
pb_short<-pb[16:225,]
yahara_short<-yahara[16:225,]
outflow_short<-outflow[16:225,]

#sum ALL inflow DIC (all in mmol/m3)
total_dic<-dorn_short$CAR_dic+sixmile_short$CAR_dic+pb_short$CAR_dic+yahara_short$CAR_dic
interp_dic<-na.interpolation(total_dic,option='linear')
plot(as.Date(dorn_short$Time),interp_dic,type='l')

#manipulating flow vars
dorn_flow<-dorn_short$FLOW*86400 #m3/day
sixmile_flow<-sixmile_short$FLOW*86400
pb_flow<-pb_short$FLOW*86400
yahara_flow<-yahara_short$FLOW*86400
all_flow<-dorn_flow+sixmile_flow+pb_flow+yahara_flow

lake.area = 3961 * 10000

#calculating load
dic_load<-(interp_dic*all_flow)/lake.area #mmol/m2
dic.load<-c(na.interpolation(dic_load,option='linear'))
plot(as.Date(dorn_short$Time),dic.load,type='l')

#calculating export
outflow_flow<-outflow_short$FLOW*86400 #m^3

setwd('~/Dropbox/Mendota Summer 16/R/')
lake_dic<-read.csv("weekly_dic_stream.csv")
lake.dic.mg.L<-c(na.interpolation(lake_dic$DIC,option='spline'))
lake.dic.mmol.m3<-lake.dic.mg.L *1000/12  
  
dic_export<-(outflow_flow*lake.dic.mmol.m3)/lake.area
dic.export<-c(na.interpolation(dic_export,option='linear'))  

quartz()
par(mar=c(3,4,1,4),mgp=c(2,0.5,0),tck=-0.02,bg='white')
plot(dic.load,dic.export,pch=16,cex=1.25,ylab=expression(DIC~Export~(mmol~m^-2)),xlab=expression(DIC~Load~(mmol~m^-2)),ylim=c(0,500),xlim=c(0,500))  
dic.mod<-lm(dic.export~dic.load)
abline(dic.mod,lwd=1.5)
abline(0,1,col='red',lwd=1.5)



