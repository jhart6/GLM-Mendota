###CH4 Flux Distributions
#8 March 2017
#JAH

library(GLMr)
library(glmtools)
library(LakeMetabolizer)
library(dplyr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)

#########################################################################
###########################CRANKED UP CH4################################

#import flux data
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
net.forcing<-read.csv('flux.data.csv')

#sum observed CH4 flux over entire summer
obs.ch4.flux<-net.forcing$ch4.flux.read.obs
obs.sum=sum(obs.ch4.flux,na.rm=TRUE) #379 mmol CH4 m-3

#establish GLM directory 
SimDir = '~/Dropbox/LaMe GLM Calibration/Methane Bump/' 
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '')

run_glm()

#to calculate CH4 flux from model
setwd("~/Dropbox/Masters Writing/Flux/")

#import hourly met data
met<-read_csv('NLDAS2_Mendota_2010_2016_cell_5.csv')
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

####start from here to calculate flux after first time####
wtr_temp_profile <- get_var(SimFile, var_name = 'temp')
z.mix <- ts.thermo.depth(wtr = wtr_temp_profile, na.rm = TRUE)[,2]

#calculating K from Read et al. 2012
wnd.z = 10
Kd = 2
lat = 43.1097
lake.area = 3961 * 10000
atm.press = 1013.25
datetime <- as.POSIXct(strptime(metHourly$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
wtr_temp_1<-get_var(SimFile, var_name = 'temp',reference = 'surface',z_out = 1)
colnames(wtr_temp_1)<-c('DateTime','Temp')
wtr <- wtr_temp_1$Temp
#use z.mix
air.temp <- metHourly$AirTemp
U10<-wind.scale.base(metHourly$WindSpeed,2)
rh <- metHourly$RelHum
sw <- metHourly$ShortWave
lwnet <- calc.lw.net.base(dateTime=datetime,sw=sw,Ts = wtr,lat = lat,atm.press = atm.press,airT = air.temp, RH = rh)
k600.read <- k.read.base(wnd.z, Kd, lat, lake.area, atm.press, datetime, Ts = wtr, z.aml = z.mix, airT = air.temp, U10, RH = rh, sw, lwnet)

#calculate KgasCH4 from K600
k.ch4.read <- k600.2.kGAS.base(k600.read, temperature = wtr_temp_1$Temp, gas = 'CH4')

#calculate atm saturation of CH4
henry_ch4 <- getKh(temperature = (wtr+273.15),gas = "CH4")
CH4sat_atm <- getSaturation(LakeKh = henry_ch4, AtmP = Pressure, gas = 'CH4') #in uM

#get modeled CH4
CH4mod.frame <- get_var(SimFile, var_name = 'CAR_ch4',reference='surface', z_out =1)
CH4mod <- CH4mod.frame$CAR_ch4_1

#calculate flux in mmol m-2 d-1
cranked.ch4.flux <- k.ch4.read*(CH4mod - CH4sat_atm)
cranked.sum = sum(cranked.ch4.flux,na.rm=TRUE) #375 mmol CH4 m-3


#########################################################################
###########################GATHER ALL DATA###############################
datetime <- as.POSIXct(strptime(metHourly$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
cranked.ch4.flux <- k.ch4.read*(CH4mod - CH4sat_atm)
obs.ch4.flux<-net.forcing$ch4.flux.read.obs
mod.ch4.flux <- net.forcing$ch4.flux.read.mod

xlab = expression(Date)
ylab = expression(CH[4]~Flux~(mmol~C~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(datetime),mod.ch4.flux,type='l',lwd=2,ylim=c(-3,33),xlab=xlab,ylab=ylab)
lines(as.Date(datetime),cranked.ch4.flux,type='l',lwd=2,col='red')
lines(as.Date(datetime),obs.ch4.flux,type='l',lwd=2,col='purple')
legend('topleft',c(expression(Observed~CH[4]~Flux),expression(Orig~Modeled~CH[4]~Flux),expression(Cranked~Modeled~CH[4]~Flux)),lwd=c(2,2,2),lty=c(1,1,1),col=c('purple','black','red'))
abline(0,0,lty=2,lwd=2,col='blue')

#########################################################################
###########################PLOT HISTOGRAMS###############################
quartz()
par(mfrow=c(3,1))
hist(obs.ch4.flux,freq=FALSE,breaks=20)
hist(cranked.ch4.flux,freq=FALSE,breaks=20)
hist(mod.ch4.flux,freq=FALSE,breaks=20)

#Continuous, Right Skewed - Gamma, Exponential, Lognormal
#GAMMA
gammaNLL<-function(p,x){
  shape=p[1]
  rate=p[2]
  NLL=-sum(dgamma(x,shape=shape,rate=rate,log=TRUE))
  return(NLL)
}
guess=c(1,0.33)
mfit=optim(par=guess,fn=gammaNLL,x=obs.ch4.flux)
mfit
gammaAIC=c(2*mfit$value+2*length(mfit$par))
gammaAIC

#EXPONENTIAL
expNLL<-function(p,x){
  rate=p[1]
  NLL=-sum(dexp(x,rate=p[1],log=TRUE))
  return(NLL)
}
guess2=c(0.5)
mfit2=optim(par=guess2,fn=expNLL,x=obs.ch4.flux)
mfit2
expAIC=c(2*mfit2$value+2*length(mfit2$par))
expAIC

#LOGNORMAL
lnormNLL<-function(p,x){
  meanlog=p[1]
  sdlog=p[2]
  NLL=-sum(dlnorm(x,meanlog=meanlog,sdlog=sdlog,log=TRUE))
  return(NLL)
}
guess3=c(1,1)
mfit3=optim(par=guess3,fn=lnormNLL,x=obs.ch4.flux)
mfit3
lnormAIC=c(2*mfit3$value+2*length(mfit3$par))
lnormAIC



