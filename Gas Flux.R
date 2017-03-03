#CO2 Flux 
#2 March 2017
#JAH

library(glmtools)
library(GLMr)
library(rLakeAnalyzer)
library(LakeMetabolizer)
library(dplyr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)

###############################################################################
#####################ESTIMATE CO2 FLUX FROM DO IN GLM##########################
setwd("~/Dropbox/Masters Writing/Flux/")

#import hourly met data
met<-read_csv('NLDAS2_Mendota_2010_2016_cell_5.csv')
head(met)

#subset met data for sim time period
#everything starts 4/16/16, goes through 11/11/16, still hourly
sim_met <- met[55129:60157,]

metHourly <- sim_met %>%
  select(time,ShortWave,LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow) %>% #selecting for only the columns we want
  mutate(date = as.Date(time)) %>% #add 'sampledate' column
  group_by(date) %>% #group by these columns
  summarise_each(funs(mean(.,na.rm=T))) %>% #applying the function 'mean' to every single column
  ungroup() %>%
  select(-date) #getting rid of those two columns
View(metHourly) #now daily met data to match simulation output

#get Sim data: water temp and DO @ 1m
SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 

wtr_temp_1<-get_var(SimFile, var_name = 'temp',reference = 'surface',z_out = 1)
colnames(wtr_temp_1)<-c('DateTime','Temp')
do<-get_var(SimFile, var_name = 'OXY_oxy', reference = 'surface', z_out = 1)
colnames(do)<-c('DateTime','DO')

#calculating O2 sat at 1m from water temp, assuming equilibrium
do.obs<-do$DO
do.sat.mg.L<-o2.at.sat.base(temp = wtr_temp_1$Temp)
do.sat <- do.sat.mg.L*1000/32

#calculating z_mix
wtr_temp_profile <- get_var(SimFile, var_name = 'temp')
z.mix <- ts.thermo.depth(wtr = wtr_temp_profile, na.rm = TRUE)[,2]

####K.Cole####
#calculating K from Cole & Caraco 1998
U10<-wind.scale.base(metHourly$WindSpeed,2)
k600.cole <- k.cole.base(U10)

####K.Vachon####
lake.area = 3961 * 10000
k600.vachon <- k.vachon.base(U10, lake.area)

####K.Read####
#calculating K from Read et al. 2012
wnd.z = 10
Kd = 2
lat = 43.1097
lake.area = 3961 * 10000
atm.press = 1013.25
datetime <- as.POSIXct(strptime(metHourly$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
wtr <- wtr_temp_1$Temp
#use z.mix
air.temp <- metHourly$AirTemp
#use U10
rh <- metHourly$RelHum
sw <- metHourly$ShortWave
lwnet <- calc.lw.net.base(dateTime=datetime,sw=sw,Ts = wtr,lat = lat,atm.press = atm.press,airT = air.temp, RH = rh)
k600.read <- k.read.base(wnd.z, Kd, lat, lake.area, atm.press, datetime, Ts = wtr, z.aml = z.mix, airT = air.temp, U10, RH = rh, sw, lwnet)

####DO Flux####
do.flux.cole = (k600.cole*(do.obs-do.sat))
do.flux.vachon = (k600.vachon*(do.obs-do.sat))
do.flux.read = (k600.read*(do.obs-do.sat))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,do.flux.cole,type = 'l', ylab = expression(DO~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-450,250))
lines(datetime, do.flux.vachon, type ='l',col = 'red')
lines(datetime, do.flux.read, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0, lty =2, col = 'red')

####Convert to CO2 Flux####
#mg of CO2 per L per day
co2.flux.cole = do.flux.cole * (44/32) * -1
co2.flux.vachon = do.flux.vachon * (44/32) * -1
co2.flux.read = do.flux.read * (44/32) * -1

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,co2.flux.cole,type = 'l',ylim=c(-250,550), ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date))
lines(datetime, co2.flux.vachon, type ='l',col = 'red')
lines(datetime, co2.flux.read, type = 'l', col = 'blue')
legend('topleft',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0, lty=2, col='red')


###############################################################################
#####################ESTIMATE CO2 FLUX FROM OBS CO2############################

#start by running Gas Atm Sat script to estimate equilibrium saturation of CO2 and CH4
#from temperature and pressure

####Calculating CO2 atm saturation####
henry_co2 <- Kh_Plummer(wtr+273.15)
CO2sat_atm <- getSaturation(LakeKh = henry_co2, AtmP = Pressure, gas = 'CO2') #in uM

####Calculating CH4 atm saturation####
henry_ch4 <- getKh(temperature = (wtr+273.15),gas = "CH4")
CH4sat_atm <- getSaturation(LakeKh = henry_ch4, AtmP = Pressure, gas = 'CH4') #in uM

#####Get Observed Data#####
#read in observed CO2 and CH4 data
#needs to be interpolated to the daily time step
library(imputeTS)
setwd("~/Dropbox/Masters Writing/Flux")

ch4 <- read.csv('surface_ch4.csv')
daily_ch4<-c(na.interpolation(ch4$CH4,option = 'spline'))
plot(as.Date(ch4$DATETIME),daily_ch4,type='l',main = 'Spline')

co2 <- read.csv('surface_co2.csv')
daily_co2 <- c(na.interpolation(co2$CO2,option = 'spline'))
plot(as.Date(co2$DATETIME),daily_co2, type = 'l', main = 'Spline')

co2.flux.cole = (k600.cole*(daily_co2 - CO2sat_atm))
co2.flux.vachon = (k600.vachon*(daily_co2 - CO2sat_atm))
co2.flux.read = (k600.read*(daily_co2 - CO2sat_atm))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,co2.flux.cole,type = 'l', ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim=c(-100,2100))
lines(datetime, co2.flux.vachon, type ='l',col = 'red')
lines(datetime, co2.flux.read, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0,lty=2,col='red')

###############################################################################
########################ESTIMATE CH4 FLUX FROM OBS DATA########################

#using observed CH4 data#
ch4.flux.cole = (k600.cole*(daily_ch4 - CH4sat_atm))
ch4.flux.vachon = (k600.vachon*(daily_ch4 - CH4sat_atm))
ch4.flux.read = (k600.read*(daily_ch4 - CH4sat_atm))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,ch4.flux.cole,type = 'l', ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-3,27))
lines(datetime, ch4.flux.vachon, type ='l',col = 'red')
lines(datetime, ch4.flux.read, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0,lty=2,col='red')


###############################################################################
########################ESTIMATE CH4 FLUX FROM MOD DATA########################

SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 

sim_ch4 <- get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out =1)
ch4.mod.obs <- sim_ch4$CAR_ch4_1

ch4.flux.cole = (k600.cole*(ch4.mod.obs - CH4sat_atm))
ch4.flux.vachon = (k600.vachon*(ch4.mod.obs - CH4sat_atm))
ch4.flux.read = (k600.read*(ch4.mod.obs - CH4sat_atm))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,ch4.flux.cole,type = 'l', ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(0,20))
lines(datetime, ch4.flux.vachon, type ='l',col = 'red')
lines(datetime, ch4.flux.read, type = 'l', col = 'blue')
legend('topleft',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0,lty=2,col='red')

###############################################################################
#################PLOT READ FLUXES THROUGH TIME ON SAME PLOT####################
ch4.flux.read = (k600.read*(ch4.mod.obs - CH4sat_atm))
co2.flux.read = do.flux.read * (44/32) * -1

quartz()
par(mar=c(3,4.5,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime, co2.flux.read, type = 'l', lwd = 2,ylab = NA,xlab=expression(Date))
mtext(side = 2, line = 2.5, expression(mmol~CO[2]~m^-2~day^-1))
par(new = TRUE)
plot(datetime, ch4.flux.read, col = 'red', axes = FALSE, xlab = NA, ylab = NA, type = 'l')
axis(side = 4)
mtext(side = 4, line = 2.5, expression(mmol~CH[4]~m^-2~day^-1), col = 'red')
