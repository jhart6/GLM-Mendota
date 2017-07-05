#Discussion Figure Re-do
#10 April 2017
#JAH

library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)
library(imputeTS)

####ALLOCHTHONY####
#calculate TOC Loads
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

#calculate toc for that short time period
dorn_toc<-dorn_short$OGM_poc+dorn_short$OGM_docr
sixmile_toc<-sixmile_short$OGM_poc+sixmile_short$OGM_docr
pb_toc<-pb_short$OGM_poc+pb_short$OGM_docr
yahara_toc<-yahara_short$OGM_poc+yahara_short$OGM_docr

#sum ALL inflow TOC (all in mmol/m3)
total_toc<-dorn_toc+sixmile_toc+pb_toc+yahara_toc #inflow toc
interp_toc<-na.interpolation(total_toc,option='linear')
plot(as.Date(dorn_short$Time),interp_toc,type='l')

#manipulating flow vars
dorn_flow<-dorn_short$FLOW*86400 #m3/day
sixmile_flow<-sixmile_short$FLOW*86400
pb_flow<-pb_short$FLOW*86400
yahara_flow<-yahara_short$FLOW*86400
all_flow<-dorn_flow+sixmile_flow+pb_flow+yahara_flow

lake.area = 3961 * 10000

#calculating load
toc_load<-(interp_toc*all_flow)/lake.area #mmol/m2
toc.load<-c(na.interpolation(toc_load,option='linear'))
plot(as.Date(dorn_short$Time),toc.load,type='l')

#calculating export
outflow_flow<-outflow_short$FLOW*86400 #m^3

setwd('~/Dropbox/Mendota Summer 16/R/')
surface_doc<-read.csv("weekly_doc_stream.csv")
surface_poc<-read.csv('weekly_poc_stream.csv')
lake.doc<-na.interpolation(surface_doc$DOC,option='spline')
lake.poc<-na.interpolation(surface_poc$POC,option='spline')
surface_toc_mg.L<-lake.doc+lake.poc #g/m^3 or mg/L
surface_toc<-surface_toc_mg.L * 1000/12 #mmol/m3

toc_export<-(outflow_flow*surface_toc)/lake.area #mmol/m2
toc.export<-na.interpolation(toc_export,option='linear')

oc.in<-cumsum(toc.load)
oc.out<-cumsum(toc.export)
alloch<-cumsum(toc.load-toc.export) #in mmol/m2/day




####AUTOCHTHONY####
setwd('~/Dropbox/Masters Writing/Flux/')
surface_do<-read.csv('surface_do.csv')
surface.do<-na.interpolation(surface_do$DO,option='spline') #mg/L
dDO<-diff(surface.do) #mg/L
dDO<-append(dDO,NA,after=0) #mg/L or g/m3

#multiply dDO by thermocline depth to get to g/m2
setwd('~/Dropbox/Mendota Summer 16/R/')
thermocline<-read.csv('weekly_zmix.csv')
z.mix<-na.interpolation(thermocline$z.mix,option='linear')
dDO.zmix<-dDO * z.mix #g/m2

#convert units from grams to mmol
dDO.mmol <- dDO.zmix * 1000/32 #mmol/m2

#get do flux data
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
flux<-read.csv('flux.data.csv')
do.flux<-na.interpolation(flux$do.flux.read.obs,option='spline') #mmol m-2 day-1

autoch.daily<-do.flux-dDO.mmol
autoch.daily.interp<-na.interpolation(autoch.daily)

autoch<-cumsum(autoch.daily.interp)

####TOC LOAD####
toc<-(toc.load-toc.export)+autoch.daily.interp
cum.toc<-cumsum(toc)

####EXTRACT CO2 FLUX DATA####
co2.flux<-flux$co2.flux.read.obs
co2.flux.interp<-na.interpolation(co2.flux,option='spline')

cum.co2<-cumsum(co2.flux.interp)




####CREATE PLOT#####
date<-as.Date(flux$datetime)
poly_x <- c(date,rev(date))
auto_y <- c(autoch,rep(0,length(autoch)))
alloch_y <- c(alloch, rep(0,length(alloch)))
toc_y <- c(cum.toc,rep(0,length(cum.toc)))

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~OC~Load~(mmol~m^-2))
plot(poly_x,toc_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-500,9500))
polygon(poly_x,toc_y,col='firebrick')
polygon(poly_x,auto_y,col='forestgreen')
par(new=TRUE)
polygon(poly_x,alloch_y,col='lightsalmon4')
lines(date,cum.co2,type='l',lwd=3)
abline(0,0,col='darkgrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~Observed~CO[2]~Flux),'Cumulative Total OC','Cumulative Autochthonous OC','Cumulative Allochthonous OC'),lty=c(1,NA,NA,NA),lwd=c(2,NA,NA,NA),pch=c(NA,15,15,15),col=c('black','firebrick','forestgreen','lightsalmon4'))

#same plot with color-blind friendly colors
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~OC~Load~(mmol~m^-2))
plot(poly_x,toc_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-500,9500))
polygon(poly_x,toc_y,col='dodgerblue3')
polygon(poly_x,auto_y,col='forestgreen')
par(new=TRUE)
polygon(poly_x,alloch_y,col='lightsalmon4')
lines(date,cum.co2,type='l',lwd=3)
abline(0,0,col='darkgrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~Observed~CO[2]~Flux),'Cumulative Total OC','Cumulative Autochthonous OC','Cumulative Allochthonous OC'),lty=c(1,NA,NA,NA),lwd=c(2,NA,NA,NA),pch=c(NA,15,15,15),col=c('black','dodgerblue3','forestgreen','lightsalmon4'))



####to add DIC to polygon figure, source the DIC plot script, then continue####
source("~/Dropbox/GitHub Repos/GLM-Mendota/DIC Plot.R")
net.dic<-dic.load-dic.export
cum.dic<-cumsum(net.dic)

dic.in<-cumsum(dic.load)
dic.out<-cumsum(dic.export)

allC<-(toc.load-toc.export)+autoch.daily.interp+net.dic
cum.allC<-cumsum(allC)

date<-as.Date(flux$datetime)
poly_x <- c(date,rev(date))
auto_y <- c(autoch,rep(0,length(autoch)))
alloch_y <- c(alloch, rep(0,length(alloch)))
toc_y <- c(cum.toc,rep(0,length(cum.toc)))
dic_y <-c(cum.dic,rep(0,length(cum.dic)))
allC_y <- c(cum.allC,rep(0,length(cum.allC)))

quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~C~Load~(mmol~m^-2))
plot(poly_x,allC_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-500,35000))
polygon(poly_x,allC_y,col='firebrick')
polygon(poly_x,dic_y,col='dodgerblue3')
polygon(poly_x,auto_y,col='forestgreen')
par(new=TRUE)
polygon(poly_x,alloch_y,col='lightsalmon4')
lines(date,cum.co2,type='l',lwd=3)
abline(0,0,col='darkgrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~Observed~CO[2]~Flux),'Cumulative Total C','Cumulative DIC','Cumulative Autochthonous OC','Cumulative Allochthonous OC'),lty=c(1,NA,NA,NA,NA),lwd=c(2,NA,NA,NA,NA),pch=c(NA,15,15,15,15),col=c('black','firebrick','dodgerblue3','forestgreen','lightsalmon4'))

#same plot, color-blind friendly colors
quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~C~Load~(mmol~m^-2))
plot(poly_x,allC_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-500,35000))
polygon(poly_x,allC_y,col='dodgerblue3')
polygon(poly_x,dic_y,col='lightsalmon')
polygon(poly_x,auto_y,col='forestgreen')
par(new=TRUE)
polygon(poly_x,alloch_y,col='lightsalmon4')
lines(date,cum.co2,type='l',lwd=3)
abline(0,0,col='darkgrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Cumulative~Observed~CO[2]~Flux),'Cumulative Total C','Cumulative DIC','Cumulative Autochthonous OC','Cumulative Allochthonous OC'),lty=c(1,NA,NA,NA,NA),lwd=c(2,NA,NA,NA,NA),pch=c(NA,15,15,15,15),col=c('black','dodgerblue3','lightsalmon','forestgreen','lightsalmon4'))



#plot with DIC and TOC polygons
date<-as.Date(flux$datetime)
poly_x <- c(date,rev(date))
auto_y <- c(autoch,rep(0,length(autoch)))
alloch_y <- c(alloch, rep(0,length(alloch)))
toc_y <- c(cum.toc,rep(0,length(cum.toc)))
dic_y <-c(cum.dic,rep(0,length(cum.dic)))
allC_y <- c(cum.allC,rep(0,length(cum.allC)))

quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~C~Retention~(mmol~m^-2))
plot(poly_x,allC_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-500,35000))
polygon(poly_x,allC_y,col='dodgerblue3')
polygon(poly_x,dic_y,col='palevioletred4')
polygon(poly_x,toc_y,col='slategrey')
polygon(poly_x,auto_y,col='springgreen4')
polygon(poly_x,alloch_y,col='sienna1')
lines(date,cum.co2,type='l',lwd=3)
abline(0,0,col='darkgrey',lty=2,lwd=1.5)
legend('topleft',c(expression(Observed~CO[2]~Flux),'Cumulative Total C',expression(DIC[IN]-DIC[OUT]),'Cumulative TOC','NPP',expression(OC[IN]-OC[OUT])),lty=c(1,NA,NA,NA,NA,NA),lwd=c(2,NA,NA,NA,NA,NA),pch=c(NA,15,15,15,15,15),col=c('black','dodgerblue3','palevioletred4','slategrey','springgreen4','sienna1'))


legend('topleft',c(expression(Observed~CO[2]~Flux),expression(DIC[IN]-DIC[OUT]),'Cumulative TOC','NPP',expression(OC[IN]-OC[OUT])),lty=c(1,NA,NA,NA,NA),lwd=c(2,NA,NA,NA,NA),pch=c(NA,15,15,15,15),col=c('black','palevioletred4','slategrey','springgreen4','sienna1'))
legend('topleft',c(expression(Observed~CO[2]~Flux),'Cumulative TOC','NPP',expression(OC[IN]-OC[OUT])),lty=c(1,NA,NA,NA),lwd=c(2,NA,NA,NA),pch=c(NA,15,15,15),col=c('black','slategrey','springgreen4','sienna1'))
legend('topleft',c(expression(Observed~CO[2]~Flux),'NPP',expression(OC[IN]-OC[OUT])),lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,15,15),col=c('black','springgreen4','sienna1'))
legend('topleft',c(expression(Observed~CO[2]~Flux),expression(OC[IN]-OC[OUT])),lty=c(1,NA),lwd=c(2,NA),pch=c(NA,15),col=c('black','sienna1'))
legend('topleft',c(expression(Observed~CO[2]~Flux)),lty=c(1),lwd=c(2),col=c('black'))





#separate out imports and exports
poly.dic.in<-c(dic.in,rep(0,length(dic.in)))
poly.dic.out<-c((-1*dic.out),rep(0,length(dic.out)))
poly.oc.in<-c(oc.in,rep(0,length(oc.in)))
poly.oc.out<-c((-1*oc.out),rep(0,length(oc.out)))

#DIC: net, in and out
quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~C~Load~(mmol~m^-2))
plot(poly_x,allC_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-10000,35000))
polygon(poly_x,poly.dic.in,col='violetred1')
polygon(poly_x,poly.dic.out,col='pink')
polygon(poly_x,dic_y,col='violetred4')
legend('topleft',c(expression(DIC[IN]-DIC[OUT]),expression(DIC[IN]),expression(DIC[OUT])),col=c('violetred4','violetred1','pink'),pch=c(15,15,15))

#OC: net, in and out
quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~C~Load~(mmol~m^-2))
plot(poly_x,allC_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-1000,5000))
polygon(poly_x,poly.oc.in,col='salmon3')
polygon(poly_x,poly.oc.out,col='lightsalmon')
polygon(poly_x,alloch_y,col='salmon4')
legend('topleft',c(expression(OC[IN]-OC[OUT]),expression(OC[IN]),expression(OC[OUT])),col=c('salmon4','salmon3','lightsalmon'),pch=c(15,15,15))

#NPP polygon
polygon(poly_x,auto_y,col='forestgreen')
abline(0,0,col='midnightblue',lwd=3,lty=2)


####Manuscript Worthy TIFF for L&O####
setwd("~/Dropbox/Masters Writing/Figures/Tiffs for L&O/")

png('Figure 7.png',width=7,height=5,res=300,units='in')
tiff('Figure 7.tiff',width=7,height=5,res=300,units='in')

#quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
xlab='Date'
ylab=expression(Cumulative~C~Retention~(mmol~m^-2))
plot(poly_x,allC_y,type='n',xlab=xlab,ylab=ylab,ylim=c(-500,35000))
polygon(poly_x,allC_y,col='orchid4')
polygon(poly_x,dic_y,col='royalblue4')
polygon(poly_x,toc_y,col='deepskyblue3') #royalblue1
polygon(poly_x,auto_y,col='green3')
polygon(poly_x,alloch_y,col='deeppink3') #magenta2 #maroon1
lines(date,cum.co2,type='l',lwd=3)
abline(0,0,col='black',lty=2,lwd=1.5)
legend('topleft',c(expression(Observed~CO[2]~Flux),'Cumulative Total C',expression(DIC[IN]-DIC[OUT]),'Cumulative TOC','NPP',expression(OC[IN]-OC[OUT])),lty=c(1,NA,NA,NA,NA,NA),lwd=c(2,NA,NA,NA,NA,NA),pch=c(NA,15,15,15,15,15),col=c('black','orchid4','royalblue4','deepskyblue3','green3','deeppink3'))

dev.off()

