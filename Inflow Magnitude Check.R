#Inflow Magnitude Comparison#

####Import Paul's Inflows####

setwd("~/Dropbox/LaMe GLM Calibration/PCH 2009 Calibration/Sims/Sim1/")
pch_yahara<-read.csv("Mendota_yahara_new.csv")
pch_pheasant<-read.csv("Mendota_pheasant_new.csv")

####Import Julia's Inflow#####
setwd("~/Dropbox/LaMe GLM Calibration/AdjustedGLM_PaulsAED/Sims/Sim1/")
jh_yahara<-read.csv("Mendota_yahara.csv")
jh_pheasant<-read.csv("Mendota_pheasant.csv")

####Yahara flow comparison####
quartz()
plot(1:nrow(pch_yahara),pch_yahara$FLOW,type='l')
quartz()
plot(1:nrow(jh_yahara),jh_yahara$FLOW,type='l')

mean(pch_yahara$FLOW)
mean(jh_yahara$FLOW,na.rm=TRUE)

####Pheasant flow comparison####
quartz()
plot(1:nrow(pch_pheasant),pch_pheasant$FLOW,type='l')
quartz()
plot(1:nrow(jh_pheasant),jh_pheasant$FLOW,type='l')

mean(pch_pheasant$FLOW)
mean(jh_pheasant$FLOW,na.rm=TRUE)
