#Gas Flux 
#2 March 2017
#JAH

library(glmtools)
library(GLMr)
library(rLakeAnalyzer)
library(LakeMetabolizer)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(flux)

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

#calculating z_mix
# SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
# setwd(SimDir) #setwd
# SimFile = paste(SimDir,'output.nc',sep = '')
SimDir = '~/Dropbox/Mendota Simulations/2017DecJulia/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output_2017_Dec_14.nc',sep = '') 
wtr_temp_1<-get_var(SimFile, var_name = 'temp',reference = 'surface',z_out = 1)
colnames(wtr_temp_1)<-c('DateTime','Temp')
wtr_temp_profile <- get_var(SimFile, var_name = 'temp')
z.mix <- ts.thermo.depth(wtr = wtr_temp_profile, na.rm = TRUE)[,2]

####K.Cole####
#calculating K from Cole & Caraco 1998
U10<-wind.scale.base(metHourly$WindSpeed,2)
k600.cole <- k.cole.base(U10)

####K.Vachon####
#calculating K from Vachon and Prairie 2013
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

####Convert all K600 to Kgas####
#DO
k.do.cole <- k600.2.kGAS.base(k600.cole,temperature = wtr_temp_1$Temp,gas = 'O2')
k.do.vachon <- k600.2.kGAS.base(k600.vachon, temperature = wtr_temp_1$Temp, gas = 'O2')
k.do.read <- k600.2.kGAS.base(k600.read, temperature = wtr_temp_1$Temp, gas = 'O2')

#CO2
k.co2.cole <- k600.2.kGAS.base(k600.cole,temperature = wtr_temp_1$Temp,gas = 'CO2')
k.co2.vachon <- k600.2.kGAS.base(k600.vachon, temperature = wtr_temp_1$Temp, gas = 'CO2')
k.co2.read <- k600.2.kGAS.base(k600.read, temperature = wtr_temp_1$Temp, gas = 'CO2')

#CH4
k.ch4.cole <- k600.2.kGAS.base(k600.cole,temperature = wtr_temp_1$Temp,gas = 'CH4')
k.ch4.vachon <- k600.2.kGAS.base(k600.vachon, temperature = wtr_temp_1$Temp, gas = 'CH4')
k.ch4.read <- k600.2.kGAS.base(k600.read, temperature = wtr_temp_1$Temp, gas = 'CH4')

###############################################################################
#####################ESTIMATE DO FLUX FROM MOD DO##############################
#get Sim data: water temp and DO @ 1m
# SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
# setwd(SimDir) #setwd
# SimFile = paste(SimDir,'output.nc',sep = '') 

SimDir = '~/Dropbox/Mendota Simulations/2017DecJulia/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output_2017_Dec_14.nc',sep = '') 

do<-get_var(SimFile, var_name = 'OXY_oxy', reference = 'surface', z_out = 1)
colnames(do)<-c('DateTime','DO')

#calculating O2 sat at 1m from water temp, assuming equilibrium
do.obs<-do$DO
do.sat.mg.L<-o2.at.sat.base(temp = wtr_temp_1$Temp)
do.sat <- do.sat.mg.L*1000/32

#flux in mmol m-2 day-1
do.flux.cole.mod = (k.do.cole*(do.obs-do.sat))
do.flux.vachon.mod = (k.do.vachon*(do.obs-do.sat))
do.flux.read.mod = (k.do.read*(do.obs-do.sat))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,do.flux.cole.mod,type = 'l', ylab = expression(DO~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-450,300),main = 'Modeled DO Flux')
lines(datetime, do.flux.vachon.mod, type ='l',col = 'red')
lines(datetime, do.flux.read.mod, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0, lty =2, col = 'red')

auc(time(do.flux.read.mod),do.flux.read.mod)
mean(do.flux.read.mod,na.rm=TRUE)
sum(do.flux.read.mod,na.rm=TRUE)

###############################################################################
#####################ESTIMATE DO FLUX FROM OBS DO##############################
setwd("~/Dropbox/Masters Writing/Flux/")
library(imputeTS)
do <-read.csv('surface_do.csv')
daily_do <- c(na.interpolation(do$DO,option = 'spline'))
plot(as.Date(do$DATETIME),daily_do,type = 'l', main = 'Spline')
daily_do_mmol_m3<-daily_do*1000/32

temp<-read.csv('surface_temp.csv')
temp_daily <-c(na.interpolation(temp$TEMP, option = 'spline'))
plot(as.Date(temp$datetime),temp_daily,type = 'l')

do.sat.obs<-o2.at.sat.base(temp=temp_daily) #mg/L
do.sat.obs.mmol.m3 <- do.sat.obs *1000/32

#flux in mmol m-2 day-1
do.flux.cole.obs = (k.do.cole*(daily_do_mmol_m3 - do.sat.obs.mmol.m3))
do.flux.vachon.obs = (k.do.vachon*(daily_do_mmol_m3 - do.sat.obs.mmol.m3))
do.flux.read.obs = (k.do.read*(daily_do_mmol_m3 - do.sat.obs.mmol.m3))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,do.flux.cole.obs,type = 'l', ylab = expression(DO~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim=c(-150,300),main="Observed DO Flux")
lines(datetime, do.flux.vachon.obs, type ='l',col = 'red')
lines(datetime, do.flux.read.obs, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0, lty =2, col = 'red')

auc(time(do.flux.read.obs),do.flux.read.obs)
mean(do.flux.read.obs,na.rm=TRUE)
sum(do.flux.read.obs,na.rm=TRUE)

###############################################################################
#####################ESTIMATE CO2 FLUX FROM OBS DO#############################
#flux in mmol m-2 day-1
co2.flux.cole.obs.do <- do.flux.cole.obs *-1 * (44/32)
co2.flux.vachon.obs.do <- do.flux.vachon.obs * -1 * (44/32)
co2.flux.read.obs.do <- do.flux.read.obs * -1 * (44/32)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,co2.flux.cole.obs.do,type = 'l', ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim=c(-350,225),main="Observed CO2 Flux from Observed DO")
lines(datetime, co2.flux.vachon.obs.do, type ='l',col = 'red')
lines(datetime, co2.flux.read.obs.do, type = 'l', col = 'blue')
legend('topleft',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0, lty =2, col = 'red')

auc(time(co2.flux.read.obs.do),co2.flux.read.obs.do)
mean(co2.flux.read.obs.do,na.rm=TRUE)
sum(co2.flux.read.obs.do,na.rm=TRUE)

###############################################################################
#####################ESTIMATE CO2 FLUX FROM MOD DO#############################

#flux in mmol m-2 day-1
co2.flux.cole.mod = do.flux.cole.mod * (44/32) * -1
co2.flux.vachon.mod = do.flux.vachon.mod * (44/32) * -1
co2.flux.read.mod = do.flux.read.mod * (44/32) * -1

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,co2.flux.cole.mod,type = 'l',ylim=c(-400,550), ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),main="Modeled CO2 Flux")
lines(datetime, co2.flux.vachon.mod, type ='l',col = 'red')
lines(datetime, co2.flux.read.mod, type = 'l', col = 'blue')
legend('topleft',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0, lty=2, col='red')

auc(time(co2.flux.read.mod),co2.flux.read.mod)
mean(co2.flux.read.mod,na.rm=TRUE)
sum(co2.flux.read.mod,na.rm=TRUE)


###############################################################################
#####################ESTIMATE CO2 FLUX FROM OBS CO2############################

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

co2.flux.cole.obs = (k.co2.cole*(daily_co2 - obs_CO2sat_atm))
co2.flux.vachon.obs = (k.co2.vachon*(daily_co2 - obs_CO2sat_atm))
co2.flux.read.obs = (k.co2.read*(daily_co2 - obs_CO2sat_atm))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,co2.flux.cole.obs,type = 'l', ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),ylim=c(-100,500),xlab = expression(Date),main="Observed CO2 Flux from Observed CO2")
lines(datetime, co2.flux.vachon.obs, type ='l',col = 'red')
lines(datetime, co2.flux.read.obs, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0,lty=2,col='red')

auc(time(co2.flux.read.obs),co2.flux.read.obs)
mean(co2.flux.read.obs,na.rm=TRUE)
sum(co2.flux.read.obs,na.rm=TRUE)

###############################################################################
########################ESTIMATE CH4 FLUX FROM OBS DATA########################

####Calculating CH4 atm saturation####
obs_henry_ch4 <- getKh(temperature = (temp_daily+273.15),gas = "CH4")
obs_CH4sat_atm <- getSaturation(LakeKh = obs_henry_ch4, AtmP = Pressure, gas = 'CH4') #in uM

#####Get Observed Data#####
ch4 <- read.csv('surface_ch4.csv')
daily_ch4<-c(na.interpolation(ch4$CH4,option = 'spline'))
plot(as.Date(ch4$DATETIME),daily_ch4,type='l',main = 'Spline')

#using observed CH4 data#
ch4.flux.cole.obs = (k.ch4.cole*(daily_ch4 - obs_CH4sat_atm))
ch4.flux.vachon.obs = (k.ch4.vachon*(daily_ch4 - obs_CH4sat_atm))
ch4.flux.read.obs = (k.ch4.read*(daily_ch4 - obs_CH4sat_atm))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,ch4.flux.cole.obs,type = 'l', ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-3,8),main="Observed CH4 Flux from Observed CH4")
lines(datetime, ch4.flux.vachon.obs, type ='l',col = 'red')
lines(datetime, ch4.flux.read.obs, type = 'l', col = 'blue')
legend('topright',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0,lty=2,col='red')

auc(time(ch4.flux.read.obs),ch4.flux.read.obs)
mean(ch4.flux.read.obs,na.rm=TRUE)
sum(ch4.flux.read.obs,na.rm=TRUE)

###############################################################################
########################ESTIMATE CH4 FLUX FROM MOD DATA########################

# SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
# setwd(SimDir) #setwd
# SimFile = paste(SimDir,'output.nc',sep = '') 

SimDir = '~/Dropbox/Mendota Simulations/2017DecJulia/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output_2017_Dec_14.nc',sep = '') 

sim_ch4 <- get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out =1)
ch4.mod.obs <- sim_ch4$CAR_ch4_1

####Calculating CH4 atm saturation####
henry_ch4 <- getKh(temperature = (wtr+273.15),gas = "CH4")
CH4sat_atm <- getSaturation(LakeKh = henry_ch4, AtmP = Pressure, gas = 'CH4') #in uM

ch4.flux.cole.mod = (k.ch4.cole*(ch4.mod.obs - CH4sat_atm))
ch4.flux.vachon.mod = (k.ch4.vachon*(ch4.mod.obs - CH4sat_atm))
ch4.flux.read.mod = (k.ch4.read*(ch4.mod.obs - CH4sat_atm))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,ch4.flux.cole.mod,type = 'l', ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(0,17),main="Modeled CH4 Flux")
lines(datetime, ch4.flux.vachon.mod, type ='l',col = 'red')
lines(datetime, ch4.flux.read.mod, type = 'l', col = 'blue')
legend('topleft',c('k600.Cole','k600.Vachon','k600.Read'),col = c('black','red','blue'),lty = c(1,1,1))
abline(0,0,lty=2,col='red')

auc(time(ch4.flux.read.mod),ch4.flux.read.mod)
mean(ch4.flux.read.mod,na.rm=TRUE)
sum(ch4.flux.read.mod,na.rm=TRUE)

###############################################################################
##########################EXPORT DATA FOR POSTERITY############################

# flux.data<-data.frame(datetime,do.flux.cole.obs,do.flux.vachon.obs,do.flux.read.obs,do.flux.cole.mod,do.flux.vachon.mod,do.flux.read.mod)
# flux.data<-cbind(flux.data,co2.flux.cole.obs,co2.flux.vachon.obs,co2.flux.read.obs,co2.flux.cole.mod,co2.flux.vachon.mod,co2.flux.read.mod)
# flux.data<-cbind(flux.data,co2.flux.cole.obs.do,co2.flux.vachon.obs.do,co2.flux.read.obs.do)
# flux.data<-cbind(flux.data,ch4.flux.cole.obs,ch4.flux.vachon.obs,ch4.flux.read.obs,ch4.flux.cole.mod,ch4.flux.vachon.mod,ch4.flux.read.mod)
# setwd('~/Dropbox/GitHub Repos/GLM-Mendota/Data')
# write.csv(flux.data,file = 'flux.data.csv',row.names = FALSE)

flux.data<-data.frame(datetime,do.flux.cole.obs,do.flux.vachon.obs,do.flux.read.obs,do.flux.cole.mod,do.flux.vachon.mod,do.flux.read.mod)
flux.data<-cbind(flux.data,co2.flux.cole.obs,co2.flux.vachon.obs,co2.flux.read.obs,co2.flux.cole.mod,co2.flux.vachon.mod,co2.flux.read.mod)
flux.data<-cbind(flux.data,co2.flux.cole.obs.do,co2.flux.vachon.obs.do,co2.flux.read.obs.do)
flux.data<-cbind(flux.data,ch4.flux.cole.obs,ch4.flux.vachon.obs,ch4.flux.read.obs,ch4.flux.cole.mod,ch4.flux.vachon.mod,ch4.flux.read.mod)
setwd('~/Dropbox/GitHub Repos/GLM-Mendota/Data')
write.csv(flux.data,file = 'flux.data.PaulCalibration.csv',row.names = FALSE)
