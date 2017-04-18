#Discussion Plots
#4/3/17
#JAH

library(imputeTS)
library(glmtools)

####Panel ONE####
#surface CH4 against surface CO2

#CH4 data
setwd("~/Dropbox/Masters Writing/Flux")
ch4 <- read.csv('surface_ch4.csv')
daily_ch4<-c(na.interpolation(ch4$CH4,option = 'spline'))
plot(as.Date(ch4$DATETIME),daily_ch4,type='l',main = 'Spline')

#CO2 data
co2 <- read.csv('surface_co2.csv')
daily_co2 <- c(na.interpolation(co2$CO2,option = 'spline'))
plot(as.Date(co2$DATETIME),daily_co2, type = 'l', main = 'Spline')

#plot
xlab=expression(Log~(Surface~CO[2])~(mmol~m^-3))
ylab=expression(Log~(Surface~CH[4])~(mmol~m^-3))
plot(log10(co2$CO2),log10(ch4$CH4),pch=16,xlab=xlab,ylab=ylab)
mod<-lm(log10(ch4$CH4)~log10(co2$CO2))
abline(mod)
summary(mod)



####Panel TWO####
#gas flux data (April 16 - November 11)
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
flux<-read.csv('flux.data.csv')
date<-as.Date(flux$datetime)
co2viaco2<-flux$co2.flux.read.obs #co2 flux
doflux<-flux$do.flux.read.obs #nep

#hypolimnetic POC data
setwd('~/Dropbox/Mendota Summer 16/R/')
poc<-read.csv('POC@20.csv') #April 16 - November 11
hypo_poc_weekly<-poc$POC
hypo_poc_daily<-c(na.interpolation(hypo_poc_weekly,option='spline'))
plot(date,hypo_poc_daily,type='l')
hypo_poc_daily_mmol<-(hypo_poc_daily*1000)/12 #poc in hypo

#get stream data (april 1 - November 15)
setwd('~/Dropbox/LaMe GLM Calibration/Greedy/')
dorn<-read.csv('Mendota_dorn.csv')
sixmile<-read.csv('Mendota_sixmile.csv')
pb<-read.csv('Mendota_pheasant.csv')
yahara<-read.csv('Mendota_yahara.csv')

#shorten stream data to April 16 - November 11
dorn_short<-dorn[16:225,]
sixmile_short<-sixmile[16:225,]
pb_short<-pb[16:225,]
yahara_short<-yahara[16:225,]

#calculate toc for that short time period
dorn_toc<-dorn_short$OGM_poc+dorn_short$OGM_docr
sixmile_toc<-sixmile_short$OGM_poc+sixmile_short$OGM_docr
pb_toc<-pb_short$OGM_poc+pb_short$OGM_docr
yahara_toc<-yahara_short$OGM_poc+yahara_short$OGM_docr

#sum ALL inflow TOC
total_poc<-dorn_short$OGM_poc+sixmile_short$OGM_poc+pb_short$OGM_poc+yahara_short$OGM_poc
total_doc<-dorn_short$OGM_docr+sixmile_short$OGM_docr+pb_short$OGM_docr+yahara_short$OGM_docr
total_toc<-dorn_toc+sixmile_toc+pb_toc+yahara_toc #inflow toc
plot(as.Date(date),total_toc,type='l')

#create mega inflow data frame
mystery<-data.frame(date,co2viaco2,doflux,total_toc,total_poc,total_doc)

#fill in NA's from errors in Kread
interp_co2<-na.interpolation(mystery$co2viaco2,option='linear')
interp_nep<-na.interpolation(mystery$doflux,option='linear')
interp_toc<-na.interpolation(mystery$total_toc,option='linear')
interp_poc<-na.interpolation(mystery$total_poc,option='linear')
interp_doc<-na.interpolation(mystery$total_doc,option='linear')

mysteryCO2<-data.frame(mystery$date,interp_co2,interp_nep,interp_toc,interp_poc,interp_doc)
colnames(mysteryCO2)<-c('Date','interp_co2','interp_nep','interp_toc','interp_poc','interp_doc')

cum_co2<-cumsum(mysteryCO2$interp_co2)
cum_nep<-cumsum(mysteryCO2$interp_nep)
cum_poc<-cumsum(mysteryCO2$interp_poc) #stream poc
cum_doc<-cumsum(mysteryCO2$interp_doc) #stream doc

#manipulating flow vars
dorn_flow<-dorn_short$FLOW*86400 #m3/day
sixmile_flow<-sixmile_short$FLOW*86400
pb_flow<-pb_short$FLOW*86400
yahara_flow<-yahara_short$FLOW*86400
all_flow<-dorn_flow+sixmile_flow+pb_flow+yahara_flow

lake.area = 3961 * 10000

#calculating load
toc_load<-(interp_toc*all_flow)/lake.area
toc_load_interp<-c(na.interpolation(toc_load,option='linear'))
cum_toc<-cumsum(toc_load_interp)

cumsums<-data.frame(mysteryCO2$Date,cum_co2,cum_nep,cum_toc,cum_poc,cum_doc,hypo_poc_daily_mmol)

#cumulative sum plot
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(date,cum_co2,type='l',col='blue',ylim=c(-12000,35000),xlab='Date',ylab=expression(Cumulative~Sum))
lines(date,cum_nep,col='red')
lines(date,cum_toc,col='brown')
lines(date,hypo_poc_daily_mmol,col='purple')
legend('topleft',c('Cumulative CO2 Flux via CO2 (mmol/m2/day)','Cumulative CO2 Flux via DO (mmol/m2/day)','Cumulative TOC Inflow (mmol/m3)','Hypolimnetic POC (not cumulative; mg/L)'),lty=c(1,1,1,1),col=c('blue','red','brown','purple'))

#cumulative plot with polygons (no hypolimnetic POC)
poly_x <- c(date,rev(date))
nep_y <- c(cum_nep,rep(0,length(cum_nep)))
toc_y <- c(cum_toc,rep(0,length(cum_toc)))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
xlab = 'Date'
ylab = expression(Cumulative~Flux~(mmol~m^-2~d^-1))
plot(poly_x,nep_y,type = 'n',xlab=xlab,ylab=ylab,ylim=c(-500,10500))
polygon(poly_x,nep_y,col='red')
par(new=TRUE)
polygon(poly_x,toc_y,col='blue')
lines(date,cum_co2,type='l',lwd=3)
abline(0,0,col='darkgrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~CO[2]~Flux~via~CO[2]),'Cumulative DO Flux','Cumulative TOC Inflow'),lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,15,15),col=c('black','red','blue'))

sed_oxy<-get_var(SimFile,var_name = 'OXY_sed_oxy')
sed_oxy_num<-sed_oxy$OXY_sed_oxy
cum_sed_oxy<-cumsum(sed_oxy_num)

#time series plot
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(mysteryCO2$Date,mysteryCO2$interp_co2,type='l',col='blue',ylim=c(-250,3700),xlab='Date',ylab=expression(Time~Series))
# lines(mysteryCO2$Date,mysteryCO2$interp_nep,col='red')
# lines(mysteryCO2$Date,mysteryCO2$interp_toc,col='brown')
# lines(mysteryCO2$Date,hypo_poc_daily_mmol_short,col='purple')
# legend('topleft',c('TS CO2 Flux via CO2 (mmol/m2/day)','TS CO2 Flux via DO (mmol/m2/day)','TS TOC Inflow (mmol/m3)','Hypolimnetic POC (not cumulative; mg/L)'),lty=c(1,1,1,1),col=c('blue','red','brown','purple'))
# 

####All Panels Together####
#option 1
quartz()
par(mar=c(3,4,1,3),mgp=c(1.5,0.5,0),tck=-0.02)
#par(mfrow=c(2,1))
par(fig=c(0,1,0.6,1))
xlab=expression(Log~(Surface~CO[2])~(mmol~m^-3))
ylab=expression(Log~(Surface~CH[4])~(mmol~m^-3))
plot(log10(co2$CO2),log10(ch4$CH4),pch=16,xlab=xlab,ylab=ylab,cex.lab=0.8,cex.axis=0.8)
mod<-lm(log10(ch4$CH4)~log10(co2$CO2))
abline(mod)
summary(mod)

par(new=TRUE)
par(fig=c(0,1,0.2,0.6))
poly_x <- c(date,rev(date))
nep_y <- c(cum_nep,rep(0,length(cum_nep)))
toc_y <- c(cum_toc,rep(0,length(cum_toc)))
xlab = 'Date'
ylab = expression(mmol~m^-2~d^-1)
plot(poly_x,nep_y,type = 'n',xlab=xlab,ylab=ylab,ylim=c(-500,35000),cex.axis=0.8,cex.lab=0.8)
polygon(poly_x,nep_y,col='dodgerblue3')
par(new=TRUE)
polygon(poly_x,toc_y,col='lightsalmon4')
lines(date,cum_co2,type='l',lwd=3)
abline(0,0,col='slategrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~CO[2]~Flux~via~CO[2]),'Cumulative DO Flux','Cumulative TOC Inflow'),lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,15,15),col=c('black','dodgerblue3','lightsalmon4'),cex=0.8)

par(new=TRUE)
par(fig=c(0,1,0,0.2))
plot(as.Date(flux$datetime),flux$ch4.flux.read.obs,type='l',xlab=xlab,ylab=expression(mmol~m^-2~d^-1),cex.lab=0.8,cex.axis=0.8,col='firebrick',lwd=1.5,ylim=c(-3,18))
abline(0,0,col='slategrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Observed~CH[4]~Flux)),lty=1,lwd=2,col='firebrick',cex=0.8)

#option 2
quartz()
par(mfrow=c(2,1),mar=c(0,4,1,3),mgp=c(1.5,0.5,0),tck=-0.02)
poly_x <- c(date,rev(date))
nep_y <- c(cum_nep,rep(0,length(cum_nep)))
toc_y <- c(cum_toc,rep(0,length(cum_toc)))
xlab = 'Date'
ylab = expression(mmol~m^-2~d^-1)
plot(poly_x,nep_y,type = 'n',xlab=NA,ylab=ylab,ylim=c(-500,35000),cex.axis=0.8,cex.lab=0.8,xaxt='n')
polygon(poly_x,nep_y,col='dodgerblue3')
par(new=TRUE)
polygon(poly_x,toc_y,col='lightsalmon4')
lines(date,cum_co2,type='l',lwd=3)
abline(0,0,col='slategrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~CO[2]~Flux~via~CO[2]),'Cumulative DO Flux','Cumulative TOC Inflow'),lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,15,15),col=c('black','dodgerblue3','lightsalmon4'),cex=0.8)
par(mar=c(3,4,0,3))
plot(as.Date(flux$datetime),flux$ch4.flux.read.obs,type='l',xlab=xlab,ylab=expression(mmol~m^-2~d^-1),cex.lab=0.8,cex.axis=0.8,col='firebrick',lwd=1.5,ylim=c(-3,18))
abline(0,0,col='slategrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Observed~CH[4]~Flux)),lty=1,lwd=2,col='firebrick',cex=0.8)

#option 3
quartz()
par(mar=c(3,4,1,3),mgp=c(1.5,0.5,0),tck=-0.02)
par(fig=c(0,1,0.25,1))
poly_x <- c(date,rev(date))
nep_y <- c(cum_nep,rep(0,length(cum_nep)))
toc_y <- c(cum_toc,rep(0,length(cum_toc)))
xlab = 'Date'
ylab = expression(mmol~m^-2~d^-1)
plot(poly_x,nep_y,type = 'n',xlab=xlab,ylab=ylab,ylim=c(-500,10500),cex.axis=0.8,cex.lab=0.8)
polygon(poly_x,nep_y,col='dodgerblue3')
par(new=TRUE)
polygon(poly_x,toc_y,col='lightsalmon4')
lines(date,cum_co2,type='l',lwd=3)
abline(0,0,col='slategrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~CO[2]~Flux~via~CO[2]),'Cumulative DO Flux','Cumulative TOC Inflow'),lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,15,15),col=c('black','dodgerblue3','lightsalmon4'),cex=0.8)

par(new=TRUE)
par(fig=c(0,1,0,0.25))
plot(as.Date(flux$datetime),flux$ch4.flux.read.obs,type='l',xlab=xlab,ylab=expression(mmol~m^-2~d^-1),cex.lab=0.8,cex.axis=0.8,col='firebrick',lwd=1.5,ylim=c(-3,10))
abline(0,0,col='slategrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Observed~CH[4]~Flux)),lty=1,lwd=2,col='firebrick',cex=0.8)





