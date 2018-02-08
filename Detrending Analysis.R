#script to determine autocorrelation & remove autocorrelation for cross-correlation analysis
#AKA de-trending analysis for L&O manuscript revisions
#1/21/18

#DATE RANGE for this analysis:
#START: 6/1/16
#END: 11/1/16

library(astsa)
library(imputeTS)

####remove autocorrelation from CH4 residuals and POC time series####
#import ch4 residuals data (exported from Calculate CH4 Residuals.R)
setwd("~/Dropbox/Mendota Summer 16/R/")
residuals<-read.csv("ch4_residuals.csv")

#first differences date range
#START: 6/2/16
#END: 11/1/16
first.diff.date<-seq(as.Date("2016-06-02"),as.Date("2016-11-01"),by='days')

#does ch4 residuals TS have autocorrelation?
  #build model
ch4mod<-lm(residuals$LogResidual~as.Date(residuals$Date))
plot(as.Date(residuals$Date),residuals$LogResidual)
abline(ch4mod)
  
  #get acf for model residuals
test=acf(resid(ch4mod))
test$acf[2]

#does poc TS have autocorrelation?
pocmod<-lm(residuals$InterpObsPOC~as.Date(residuals$Date))
test.poc=acf(resid(pocmod))
test.poc$acf[2]
  
#what is the largest cross-correlation coefficient for this time series? and what lag?
ccf_mod<-ccf(ts(residuals$LogResidual),ts(residuals$InterpObsPOC),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(residuals$InterpObsPOC),max.lag = 24) 

#remove autocorrelation via first difference
diff.ch4residuals<-diff(residuals$LogResidual,differences=1)
plot(first.diff.date,diff.ch4residuals)

#check new ts for autocorrelation
  #build new model
ch4mod2<-lm(diff.ch4residuals~as.Date(first.diff.date))
plot(as.Date(first.diff.date),diff.ch4residuals)
abline(ch4mod2)

  #get acf for new model residuals
test2=acf(resid(ch4mod2))
test2$acf[2]

#remove POC autocorrleation via first difference
diff.poc<-diff(residuals$InterpObsPOC,differences=1)

#get new ACF for poc
pocmod2<-lm(diff.poc~as.Date(first.diff.date))
test2.poc<-acf(resid(pocmod2))
test2.poc$acf[2]

#new crosscorr
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.poc),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.poc),max.lag = 24) 







####CREATE NEW TABLE####
#####import & interpolate all necessary data#####
setwd("~/Dropbox/Mendota Summer 16/R/")

#thermocline
weekly_thermo<-read.csv('weekly_thermocline_gas.csv')
t<-weekly_thermo$Thermocline[1:154]
thermo.date<-as.Date(weekly_thermo$Date[1:154])
interp.thermocline<-c(na.interpolation(t,option = 'spline'))
plot(thermo.date,interp.thermocline,type='l')

#DO
weekly_do<-read.csv('weekly_do_gas.csv')
do<-weekly_do$DO[1:154]
do.date<-as.Date(weekly_do$DateTime[1:154])
interp.do<-c(na.interpolation(do,option = 'spline'))
plot(do.date,interp.do,type='l')

#DOC
weekly_doc<-read.csv("weekly_doc_gas.csv")
d<-weekly_doc$DOC[1:154]
doc.date<-as.Date(weekly_doc$DATETIME)[1:154]
interp.doc<-c(na.interpolation(d,option = 'spline'))
plot(doc.date,interp.doc,type='l')

#TN
weekly_tn<-read.csv("weekly_tn_gas.csv")
tn<-weekly_tn$TN[1:154]
tn.date<-as.Date(weekly_tn$DateTime)[1:154]
interp.tn<-c(na.interpolation(tn,option = 'spline'))
plot(tn.date,interp.tn,type='l')

#TP
weekly_tp<-read.csv("weekly_tp_gas.csv")
tp<-weekly_tp$TP[1:154]
tp.date<-as.Date(weekly_tp$DateTime)[1:154]
interp.tp<-c(na.interpolation(tp,option = 'spline'))
plot(tp.date,interp.tp,type='l')

#CO2
weekly_co2<-read.csv("weekly_co2_gas.csv")
co2<-weekly_co2$CO2[1:154]
co2.date<-as.Date(weekly_co2$DATETIME)[1:154]
interp.co2<-c(na.interpolation(co2,option='spline'))
plot(co2.date,interp.co2,type='l')


#setwd for flow + precip data
#flow
setwd('~/Dropbox/LaMe GLM Calibration/Greedy/')
yahara<-read.csv("Mendota_yahara.csv")
yahara.flow<-yahara[62:215,2]
sixmile<-read.csv("Mendota_sixmile.csv")
sixmile.flow<-sixmile[62:215,2]
pb<-read.csv("Mendota_pheasant.csv")
pb.flow<-pb[62:215,2]
dorn<-read.csv("Mendota_dorn.csv")
dorn.flow<-dorn[62:215,2]
total.flow = yahara.flow + sixmile.flow + pb.flow + dorn.flow
total.flow <- c(na.interpolation(total.flow,option = 'spline'))
flow.date<-as.Date(yahara[62:215,1])

#precip
setwd("~/Dropbox/Mendota Summer 16/R/")
dailyRain<-read.csv("precipDaily.csv")
dailyPrecip<-dailyRain[2344:2497,2]
rain.date<-as.Date(dailyRain[2344:2497,1])


####determine acf of natural data####
#build models & test acf
thermo.mod<-lm(interp.thermocline~thermo.date)
test.thermo<-acf(resid(thermo.mod))
test.thermo$acf[2]

do.mod<-lm(interp.do~do.date)
test.do<-acf(resid(do.mod))
test.do$acf[2]

doc.mod<-lm(interp.doc~doc.date)
test.doc<-acf(resid(doc.mod))
test.doc$acf[2]

tn.mod<-lm(interp.tn~tn.date)
test.tn<-acf(resid(tn.mod))
test.tn$acf[2]

tp.mod<-lm(interp.tp~tp.date)
test.tp<-acf(resid(tp.mod))
test.tp$acf[2]

co2.mod<-lm(interp.co2~co2.date)
test.co2<-acf(resid(co2.mod))
test.co2$acf[2]

flow.mod<-lm(total.flow~as.Date(flow.date))
test.flow<-acf(resid(flow.mod))
test.flow$acf[2]

precip.mod<-lm(dailyPrecip~as.Date(rain.date))
test.precip<-acf(resid(precip.mod))
test.precip$acf[2]

#####determine cross corr coefficients for natural data####
#thermocline
ccf_mod<-ccf(ts(residuals$LogResidual),ts(interp.thermocline),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(interp.thermocline),max.lag = 24) 

#do
ccf_mod<-ccf(ts(residuals$LogResidual),ts(interp.do),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(interp.do),max.lag = 24) 

#doc
ccf_mod<-ccf(ts(residuals$LogResidual),ts(interp.doc),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(interp.doc),max.lag = 24) 

#tn
ccf_mod<-ccf(ts(residuals$LogResidual),ts(interp.tn),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(interp.tn),max.lag = 24) 

#tp
ccf_mod<-ccf(ts(residuals$LogResidual),ts(interp.tp),lag.max = 40)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(interp.tp),max.lag = 40)

#co2
ccf_mod<-ccf(ts(residuals$LogResidual),ts(interp.co2),lag.max = 30)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(interp.co2),max.lag = 30) 

#flow
ccf_mod<-ccf(ts(residuals$LogResidual),ts(total.flow),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(total.flow),max.lag = 24) 

#precip
ccf_mod<-ccf(ts(residuals$LogResidual),ts(dailyPrecip),lag.max = 24)
quartz()
lag2.plot(ts(residuals$LogResidual),ts(dailyPrecip),max.lag = 24) 

####calculate first difference for all vars####
diff.thermo<-diff(interp.thermocline,differences=1)
diff.do<-diff(interp.do,differences=1)
diff.doc<-diff(interp.doc,differences=1)
diff.tn<-diff(interp.tn,differences=1)
diff.tp<-diff(interp.tp,differences=1)
diff.co2<-diff(interp.co2,differences=1)
diff.flow<-diff(total.flow,differences=1)
diff.precip<-diff(dailyPrecip,differences=1)

####calculate new ACF post 1st difference#####
#thermocline
thermo.mod2<-lm(diff.thermo~thermo.date[-1])
test.thermo2<-acf(resid(thermo.mod2))
test.thermo2$acf[2]

#do
do.mod2<-lm(diff.do~do.date[-1])
test.do2<-acf(resid(do.mod2))
test.do2$acf[2]

#doc
doc.mod2<-lm(diff.doc~doc.date[-1])
test.doc2<-acf(resid(doc.mod2))
test.doc2$acf[2]

#tn
tn.mod2<-lm(diff.tn~tn.date[-1])
test.tn2<-acf(resid(tn.mod2))
test.tn2$acf[2]

#tp
tp.mod2<-lm(diff.tp~tp.date[-1])
test.tp2<-acf(resid(tp.mod2))
test.tp2$acf[2]

#co2
co2.mod2<-lm(diff.co2~co2.date[-1])
test.co22<-acf(resid(co2.mod2))
test.co22$acf[2]

#flow
flow.mod2<-lm(diff.flow~flow.date[-1])
test.flow2<-acf(resid(flow.mod2))
test.flow2$acf[2]

#precip
precip.mod2<-lm(diff.precip~rain.date[-1])
test.precip2<-acf(resid(precip.mod2))
test.precip2$acf[2]

####calculate new cross corr coefficient####
#thermocline
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.thermo),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.thermo),max.lag = 24) 

#do
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.do),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.do),max.lag = 24) 

#doc
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.doc),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.doc),max.lag = 24) 

#tn
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.tn),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.tn),max.lag = 24) 

#tp
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.tp),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.tp),max.lag = 24) 

#co2
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.co2),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.co2),max.lag = 24) 

#flow
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.flow),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.flow),max.lag = 24) 

#precip
ccf_mod<-ccf(ts(diff.ch4residuals),ts(diff.precip),lag.max = 24)
quartz()
lag2.plot(ts(diff.ch4residuals),ts(diff.precip),max.lag = 24) 



####manuscript worthy figure of 1st diff ch4 resid + POC####
# #get discrete 1st diff data on sampling days
# sampling.days<-as.Date(residuals$Date[which(is.na(residuals$ObsPOC)==FALSE)])
# first.diff<-cbind(first.diff.date,diff.poc,diff.ch4residuals)
# maybe don't need discrete data points since they don't show the real values measured in the field anymore?

library(astsa)
library(imputeTS)

setwd("~/Dropbox/Masters Writing/Figures/Tiffs for L&O/")
setwd("~/Dropbox/Masters Writing/PUB5 (L&O)/For Resubmission/Figures/")

png('Figure 5_1st diff.png',width=7,height=5,units='in',res=300)
tiff('Figure 5.tiff',width=7,height=5,units='in',res=300)

#quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(first.diff.date),diff.poc,type='l',xlab = expression(Date),ylab = expression (POC[(t)]-POC[(t-1)]~(mg~L^-1)),ylim=c(-0.73,0.76),lwd=2)
par(new=TRUE)
lines(as.Date(first.diff.date),diff.ch4residuals,col='firebrick',lwd=2)
axis(side = 4)
side4label <- expression(log(CH[4])~Residual[(t)]-log(CH[4])~Residual[(t-1)])
mtext(side = 4, line = 1.75, side4label,col='firebrick')
methane.legend.text=expression(First~Difference~log(CH[4])~Residuals)
legend('topleft',c('Observed First Difference POC',methane.legend.text),lwd=c(2,2),col=c('black','firebrick'),lty=c(1,1))
dev.off()




#####generate p-values for largest lags in Xcorr#####
poc.lag.21<-lag(diff.poc,21)
reg1<-lm(poc.lag.21~diff.ch4residuals)
summary(reg1)
plot(diff.ch4residuals,poc.lag.21)

thermo.lag.17<-lag(diff.thermo,17)
reg2<-lm(thermo.lag.17~diff.ch4residuals)
summary(reg2) #not same length
plot(diff.ch4residuals,thermo.lag.17)
abline(reg2)

do.lag.2<-lag(diff.do,2)
reg3<-lm(do.lag.2~diff.ch4residuals)
summary(reg3)
plot(diff.ch4residuals,do.lag.2)

doc.lag.7<-lag(diff.doc,7)
reg4<-lm(doc.lag.7~diff.ch4residuals)
summary(reg4) 
plot(diff.ch4residuals,doc.lag.2)

tn.lag.9<-lag(diff.tn,9)
reg5<-lm(tn.lag.9~diff.ch4residuals)
summary(reg5) #not same length
plot(diff.ch4residuals,tn.lag.9)

reg6<-lm(diff.tp~diff.ch4residuals)
summary(reg6) #not same length
plot(diff.ch4residuals,diff.tp)

reg7<-lm(diff.co2~diff.ch4residuals)
summary(reg7)
plot(diff.ch4residuals,diff.co2)

flow.lag.11<-lag(diff.flow,11)
reg8<-lm(flow.lag.11~diff.ch4residuals)
summary(reg8)
plot(diff.ch4residuals,flow.lag.11)

flow.lag.12<-lag(diff.flow,12)
reg9<-lm(flow.lag.12~diff.ch4residuals)
summary(reg9)
plot(diff.ch4residuals,flow.lag.12)

precip.lag.10<-lag(diff.precip,10)
reg10<-lm(precip.lag.10~diff.ch4residuals)
summary(reg10)
plot(diff.ch4residuals,precip.lag.10)

precip.lag.11<-lag(diff.precip,11)
reg11<-lm(precip.lag.11~diff.ch4residuals)
summary(reg11)
plot(diff.ch4residuals,precip.lag.11)
