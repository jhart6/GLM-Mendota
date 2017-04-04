#Discussion Plots
#4/3/17
#JAH

library(imputeTS)

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
#gas flux data
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
flux<-read.csv('flux.data.csv')
date<-as.Date(flux$datetime)
co2viaco2<-flux$co2.flux.read.obs #co2 flux
co2viado<-flux$co2.flux.read.obs.do #nep

setwd('~/Dropbox/Mendota Summer 16/R/')
poc<-read.csv('POC@20.csv')
hypo_poc_weekly<-poc$POC
hypo_poc_daily<-c(na.interpolation(hypo_poc_weekly,option='spline'))
plot(date,hypo_poc_daily,type='l')
hypo_poc_daily_mmol<-(hypo_poc_daily*1000)/12 #poc in hypo
hypo_poc_daily_mmol_short<-hypo_poc_daily_mmol[1:200]

setwd('~/Dropbox/LaMe GLM Calibration/Greedy/')
dorn<-read.csv('Mendota_dorn.csv')
sixmile<-read.csv('Mendota_sixmile.csv')
pb<-read.csv('Mendota_pheasant.csv')
yahara<-read.csv('Mendota_yahara.csv')

dorn_short<-dorn[16:225,]
sixmile_short<-sixmile[16:225,]
pb_short<-pb[16:225,]
yahara_short<-yahara[16:225,]

dorn_toc<-dorn_short$OGM_poc+dorn_short$OGM_docr
sixmile_toc<-sixmile_short$OGM_poc+sixmile_short$OGM_docr
pb_toc<-pb_short$OGM_poc+pb_short$OGM_docr
yahara_toc<-yahara_short$OGM_poc+yahara_short$OGM_docr

total_poc<-dorn_short$OGM_poc+sixmile_short$OGM_poc+pb_short$OGM_poc+yahara_short$OGM_poc
total_doc<-dorn_short$OGM_docr+sixmile_short$OGM_docr+pb_short$OGM_docr+yahara_short$OGM_docr
total_toc<-dorn_toc+sixmile_toc+pb_toc+yahara_toc #inflow toc
plot(as.Date(date),total_toc,type='l')

mystery<-data.frame(date,co2viaco2,co2viado,total_toc,total_poc,total_doc)
mystery_short<-mystery[1:200,]

interp_co2<-na.interpolation(mystery_short$co2viaco2,option='linear')
interp_nep<-na.interpolation(mystery_short$co2viado,option='linear')
interp_toc<-na.interpolation(mystery_short$total_toc,option='linear')
interp_poc<-na.interpolation(mystery_short$total_poc,option='linear')
interp_doc<-na.interpolation(mystery_short$total_doc,option='linear')

mysteryCO2<-data.frame(mystery_short$date,interp_co2,interp_nep,interp_toc,interp_poc,interp_doc)
colnames(mysteryCO2)<-c('Date','interp_co2','interp_nep','interp_toc','interp_poc','interp_doc')

cum_co2<-cumsum(mysteryCO2$interp_co2)
cum_nep<-cumsum(mysteryCO2$interp_nep)
cum_poc<-cumsum(mysteryCO2$interp_poc) #stream poc
cum_doc<-cumsum(mysteryCO2$interp_doc) #stream doc

#manipulating flow vars
dorn_flow<-dorn$FLOW*86400 #m3/day
dorn_flow_short<-dorn_flow[16:215]
sixmile_flow<-sixmile$FLOW*86400
sixmile_flow_short<-sixmile_flow[16:215]
pb_flow<-pb$FLOW*86400
pb_flow_short<-pb_flow[16:215]
yahara_flow<-yahara$FLOW*86400
yahara_flow_short<-yahara_flow[16:215]
all_flow<-dorn_flow_short+sixmile_flow_short+pb_flow_short+yahara_flow_short

lake.area = 3961 * 10000

#calculating load
toc_load<-(interp_toc*all_flow)/lake.area
toc_load_interp<-c(na.interpolation(toc_load,option='linear'))
cum_toc<-cumsum(toc_load_interp)

cumsums<-data.frame(mysteryCO2$Date,cum_co2,cum_nep,cum_toc,cum_poc,cum_doc,hypo_poc_daily_mmol_short)

#cumulative sum plot
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(cumsums$mysteryCO2.Date,cum_co2,type='l',col='blue',ylim=c(-12000,35000),xlab='Date',ylab=expression(Cumulative~Sum))
lines(cumsums$mysteryCO2.Date,cum_nep,col='red')
lines(cumsums$mysteryCO2.Date,cum_toc,col='brown')
lines(cumsums$mysteryCO2.Date,hypo_poc_daily_mmol_short,col='purple')
legend('topleft',c('Cumulative CO2 Flux via CO2 (mmol/m2/day)','Cumulative CO2 Flux via DO (mmol/m2/day)','Cumulative TOC Inflow (mmol/m3)','Hypolimnetic POC (not cumulative; mg/L)'),lty=c(1,1,1,1),col=c('blue','red','brown','purple'))

#time series plot
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(mysteryCO2$Date,mysteryCO2$interp_co2,type='l',col='blue',ylim=c(-250,3700),xlab='Date',ylab=expression(Time~Series))
# lines(mysteryCO2$Date,mysteryCO2$interp_nep,col='red')
# lines(mysteryCO2$Date,mysteryCO2$interp_toc,col='brown')
# lines(mysteryCO2$Date,hypo_poc_daily_mmol_short,col='purple')
# legend('topleft',c('TS CO2 Flux via CO2 (mmol/m2/day)','TS CO2 Flux via DO (mmol/m2/day)','TS TOC Inflow (mmol/m3)','Hypolimnetic POC (not cumulative; mg/L)'),lty=c(1,1,1,1),col=c('blue','red','brown','purple'))
# 

####Both Panels Together####
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
par(mfrow=c(2,1))

xlab=expression(Log~(Surface~CO[2])~(mmol~m^-3))
ylab=expression(Log~(Surface~CH[4])~(mmol~m^-3))
plot(log10(co2$CO2),log10(ch4$CH4),pch=16,xlab=xlab,ylab=ylab)
mod<-lm(log10(ch4$CH4)~log10(co2$CO2))
abline(mod)
summary(mod)

plot(cumsums$mysteryCO2.Date,cum_co2,type='l',col='blue',ylim=c(-12000,35000),xlab='Date',ylab=expression(Cumulative~Sum))
lines(cumsums$mysteryCO2.Date,cum_nep,col='red')
lines(cumsums$mysteryCO2.Date,cum_toc,col='brown')
lines(cumsums$mysteryCO2.Date,hypo_poc_daily_mmol_short,col='purple')
legend('topleft',c('Cumulative CO2 Flux via CO2 (mmol/m2/day)','Cumulative CO2 Flux via DO (mmol/m2/day)','Cumulative TOC Inflow (mmol/m2/day)','Hypolimnetic POC (not cumulative; mg/L)'),lty=c(1,1,1,1),col=c('blue','red','brown','purple'))

# plot(mysteryCO2$Date,mysteryCO2$interp_co2,type='l',col='blue',ylim=c(-250,4200),xlab='Date',ylab=expression(Time~Series))
# lines(mysteryCO2$Date,mysteryCO2$interp_nep,col='red')
# lines(mysteryCO2$Date,mysteryCO2$interp_toc,col='brown')
# lines(mysteryCO2$Date,hypo_poc_daily_mmol_short,col='purple')
# legend('topleft',c('TS CO2 Flux via CO2 (mmol/m2/day)','TS CO2 Flux via DO (mmol/m2/day)','TS TOC Inflow (mmol/m3)','Hypolimnetic POC (not cumulative; mg/L)'),lty=c(1,1,1,1),col=c('blue','red','brown','purple'))
