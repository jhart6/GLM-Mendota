#Script to compare log CH4 residuals against all other potential explanatory vars
#4/1/17
#JAH

#working directory for observed data
setwd("~/Dropbox/Mendota Summer 16/R/")

#working directory for modeled data
library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)
library(imputeTS)
SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
setwd(SimDir)
SimFile = paste(SimDir,'output.nc',sep = '') 

#working directory for observed data
setwd("~/Dropbox/Mendota Summer 16/R/")

####COMPARE CH4 RESIDUALS TO THERMOCLINE DEPTH, OBS & MODELED####
weekly_thermo<-read.csv('weekly_thermocline_gas.csv')
t<-weekly_thermo$Thermocline

#interpolate weekly to daily
interp_spline_t<-c(na.interpolation(t,option = 'spline'))
plot(as.Date(weekly_thermo$Date),interp_spline_t,type='l')

#plot interp obs data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(weekly_thermo$Date),interp_spline_t,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (Thermo~Depth~(m)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('Thermo Depth','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))



####COMPARE CH4 RESIDUALS TO DO, OBS & MODELED####
###potential for correlation###
weekly_do<-read.csv('weekly_do_gas.csv')
do<-weekly_do$DO

#interpolate weekly to daily
interp_spline_do<-c(na.interpolation(do,option = 'spline'))
plot(as.Date(weekly_do$DateTime),interp_spline_do,type='l')

#extract modeled data at 3m
daily_mod_do<-get_var(SimFile,var_name='DO',reference='surface',z_out=3)
daily_mod_do_short<-daily_mod_do[47:200,]

#plot interp obs data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(weekly_do$DateTime),interp_spline_do,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (DO~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('Obs DO @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))

#plot sim data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(residual$Date),daily_mod_do_short$DO_3,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (DO~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('mod DO @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))




####COMPARE CH4 RESIDUALS TO DOC, BOTH OBS AND MODELED####
#load obs DOC data
weekly_doc<-read.csv("weekly_doc_gas.csv")
d<-weekly_doc$DOC

#interpolate weekly doc to daily
interp_spline_d<-c(na.interpolation(d,option = 'spline'))
plot(as.Date(weekly_doc$DATETIME),interp_spline_d,type='l')

#extract modeled DOC at 3m
daily_mod_DOC<-get_var(SimFile, var_name = 'DOC',reference = 'surface',z_out=3)
daily_mod_DOC_short<-daily_mod_DOC[47:200,]

#plot interp obs doc with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(weekly_doc$DATETIME),interp_spline_d,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (DOC~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('DOC @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))

#plot sim doc with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(residual$Date),daily_mod_DOC_short$DOC_3,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (DOC~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('mod DOC @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))




####COMPARE CH4 RESIDUALS TO TN, OBS & MODELED####
weekly_tn<-read.csv("weekly_tn_gas.csv")
tn<-weekly_tn$TN

#interpolate weekly to daily
interp_spline_tn<-c(na.interpolation(tn,option = 'spline'))
plot(as.Date(weekly_tn$DateTime),interp_spline_tn,type='l')

#extract modeled data at 3m
daily_mod_tn<-get_var(SimFile,var_name='TotN2',reference='surface',z_out=3)
daily_mod_tn_short<-daily_mod_tn[47:200,]

#plot interp obs data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(weekly_tn$DateTime),interp_spline_tn,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (TN~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('Obs TN @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))

#plot sim data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(residual$Date),daily_mod_tn_short$TotN2_3,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (TN~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('mod TN @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))




####COMPARE CH4 RESIDUALS TO TP, OBS & MODELED####
weekly_tp<-read.csv("weekly_tp_gas.csv")
tp<-weekly_tp$TP

#interpolate weekly to daily
interp_spline_tp<-c(na.interpolation(tp,option = 'spline'))
plot(as.Date(weekly_tp$DateTime),interp_spline_tp,type='l')

#extract modeled data at 3m
daily_mod_tp<-get_var(SimFile,var_name='TotP2',reference='surface',z_out=3)
daily_mod_tp_short<-daily_mod_tp[47:200,]

#plot interp obs data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(weekly_tp$DateTime),interp_spline_tp,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (TP~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('Obs TP @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))

#plot sim data with log CH4 residuals
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(residual$Date),daily_mod_tp_short$TotP2_3,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (TP~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
legend('topleft',c('mod TP @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))

