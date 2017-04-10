#Supersaturation 1:1 Figure
#source the Gas Flux script before running this script

#start by running Gas Atm Sat script to estimate equilibrium saturation of CO2 and CH4
#from temperature and pressure

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
plot(do.flux.read.obs,co2.flux.read.obs,pch=16)
abline(0,-1,lty=2,col='red')

plot(do.supersat,co2.supersat,pch=16)
abline(0,-1,lty=2,col='red')
