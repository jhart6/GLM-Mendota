#Supersaturation 1:1 Figure
#source the Gas Flux script before running this script
source("~/Dropbox/GitHub Repos/GLM-Mendota/Gas Flux.R")

#start by running Gas Atm Sat script to estimate equilibrium saturation of CO2 and CH4
#from temperature and pressure
source("~/Dropbox/GitHub Repos/GLM-Mendota/Gas Atm Sat.R")

####Calculating CO2 atm saturation####
obs_henry_co2 <- Kh_Plummer(temp_daily+273.15)
obs_CO2sat_atm <- getSaturation(LakeKh = obs_henry_co2, AtmP = Pressure, gas = 'CO2') #in uM

#####Get Observed Data#####
#needs to be interpolated to the daily time step
setwd("~/Dropbox/Masters Writing/Flux")

co2 <- read.csv('surface_co2.csv')
daily_co2 <- c(na.interpolation(co2$CO2,option = 'spline'))
plot(as.Date(co2$DATETIME),daily_co2, type = 'l', main = 'Spline')

co2.flux.read.obs = (k.co2.read*(daily_co2 - obs_CO2sat_atm))

co2.supersat<-daily_co2-obs_CO2sat_atm
plot(as.Date(co2$DATETIME),co2.supersat,type='l')

#OBS DO Flux
setwd("~/Dropbox/Masters Writing/Flux/")
library(imputeTS)
do <-read.csv('surface_do.csv')
daily_do <- c(na.interpolation(do$DO,option = 'spline'))
daily_do_mmol_m3<-daily_do*1000/32

temp<-read.csv('surface_temp.csv')
temp_daily <-c(na.interpolation(temp$TEMP, option = 'spline'))
plot(as.Date(temp$datetime),temp_daily,type = 'l')

do.sat.obs<-o2.at.sat.base(temp=temp_daily) #mg/L
do.sat.obs.mmol.m3 <- do.sat.obs *1000/32

#flux in mmol m-2 day-1
do.flux.read.obs = (k.do.read*(daily_do_mmol_m3 - do.sat.obs.mmol.m3))

#calculate supersaturation
do.supersat<-daily_do_mmol_m3 - do.sat.obs.mmol.m3
plot(as.Date(temp$datetime),do.supersat,type='l')


####Create plots####
#supersaturation plot
quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot(do.supersat,co2.supersat,type='n',xlim=c(-100,100),ylim=c(-100,100),ylab=expression((CO[2][Obs]-CO[2][Eq])),xlab=expression((DO[Obs]-DO[Eq])))
abline(h=0,col='red',lty=2,lwd=2)
abline(v=0,col='red',lty=2,lwd=2)
points(do.supersat,co2.supersat,pch=16)


#flux comparison plot
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
flux<-read.csv('flux.data.csv')
quartz()
plot(do.flux.read.obs,co2.flux.read.obs.do,pch=16,xlim=c(-200,200),ylim=c(-200,200))
abline(0,-1,col='red')

xlab=expression(Observed~DO~Flux~(mmol~m^-2~day^-1))
ylab.do=expression(Biological~CO[2]~Flux~(mmol~m^-2~day^-1))
ylab.obs=expression(Observed~CO[2]~Flux~(mmol~m^-2~day^-1))

quartz()
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot(do.flux.read.obs,co2.flux.read.obs.do,pch=16,xlim=c(-200,300),ylim=c(-200,300),xlab=xlab,ylab=ylab.do)
abline(0,-1,col='red',lwd=2)
plot(do.flux.read.obs,co2.flux.read.obs,pch=16,ylim=c(-200,300),xlim=c(-200,300),xlab=xlab,ylab=ylab.obs)
abline(0,-1,lwd=2,col='red')



