#DO state and flux controls 
#24 February 2017
#JAH

library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)

SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 

#create epi data frame
DO_3 <- get_var(SimFile, var_name = 'DO',reference = 'surface', z_out = 3)
epi_do<-DO_3$DO_3

nitrif_3<-get_var(SimFile, var_name = 'NIT_nitrif',reference = 'surface', z_out = 3)
epi_nitrif<-nitrif_3$NIT_nitrif_3
epi_nitrif_sign <- epi_nitrif*-1

GPP_3<-get_var(SimFile, var_name = 'PHY_PPR',reference = 'surface', z_out = 3)
epi_gpp <- GPP_3$PHY_PPR_3

NEP_3<-get_var(SimFile, var_name = 'PHY_NPR', reference = 'surface', z_out =3)
epi_nep<-NEP_3$PHY_NPR_3

epi_resp<-(epi_gpp-epi_nep)
epi_resp_sign <-epi_resp*-1

setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
flux<-read.csv('flux.data.csv')
atm_exch <-flux$do.flux.read.mod
atm_exch_sign <- atm_exch*-1

epi_DO <- data.frame(DO_3,epi_nitrif_sign,epi_gpp,epi_resp_sign,atm_exch_sign)
colnames(epi_DO) <- c('DateTime','DO','Nitrif','GPP','R','AtmExch')

#create the hypo data frame
DO_20<-get_var(SimFile, var_name = 'DO', reference = 'surface', z_out = 20)
hypo_do<-DO_20$DO_20

Fsed_oxy <- get_var(SimFile, var_name = 'OXY_sed_oxy')
hypo_sed_demand<-Fsed_oxy$OXY_sed_oxy

nitrif_20<-get_var(SimFile, var_name = 'NIT_nitrif',reference = 'surface', z_out = 20)
hypo_nitrif<-nitrif_20$NIT_nitrif_20
hypo_nitrif_sign <- hypo_nitrif*-1

GPP_20<-get_var(SimFile, var_name = 'PHY_PPR',reference = 'surface', z_out = 20)
hypo_gpp<-GPP_20$PHY_PPR_20

NEP_20<-get_var(SimFile, var_name = 'PHY_NPR', reference = 'surface', z_out = 20)
hypo_nep<-NEP_20$PHY_NPR_20

hypo_resp<-(hypo_gpp - hypo_nep)
hypo_resp_sign <- hypo_resp*-1

hypo_DO <- data.frame(DO_20,hypo_sed_demand,hypo_nitrif_sign,hypo_gpp,hypo_resp_sign)
colnames(hypo_DO) <- c("DateTime",'DO',"SedDemand",'Nitrif','GPP','R')

#two panel plot
xlab = expression(Date)
state_ylab = expression(DO~(mg~L^-1))

quartz()

#setwd("~/Dropbox/Masters Writing/Figures/")
#png("Oxygen Controls.png",width = 8, height = 8, units = 'in', res = 500)

par(mfrow=c(2,1),mar=c(0,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,oma = c(1,1.5,1,3))
plot(as.Date(epi_DO$DateTime),epi_DO$DO,type='l',xlab=xlab,ylab = state_ylab,lwd=3,xaxt='n')
par(new=TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$AtmExch, col=c('blue'),axes=FALSE, xlab = NA, ylab = NA,type = 'l',ylim=c(-250,250))
axis(side = 4)
mtext(side = 4, line = 2, expression(mmol~m^-2~day^-1),col=c('red'))
par(new = TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$GPP,col=c('green'),axes = FALSE, xlab = NA, ylab = NA, type = 'l',ylim=c(-250,250))
mtext(side = 4, line = 3.25, expression(mmol~m^-3~day^-1), col=c('green'))
par(new = TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$R,col=c('red'),axes = FALSE, xlab = NA, ylab = NA, type = 'l',ylim=c(-250,250))
mtext(side = 4, line = 4.2, expression(mmol~m^-3~day^-1), col=c('blue'))
par(new = TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$Nitrif,col = c('purple'),axes = FALSE, xlab = NA, ylab = NA, type = 'l',ylim=c(-250,250))
mtext(side = 4, line = 5.3, expression(mmol~m^-3~day^-1), col=c('purple'))
legend('topleft',c('EPILIMNION','DO (mg/L)','DO Atm Flux','GPP','Total Respiration','Nitrification Rate'),lty=c(NA,1,1,1,1,1),col=c(NA,'black','blue','green','red','purple'),lwd=c(NA,3,1,1,1,1))

par(mar=c(3,3,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(hypo_DO$DateTime),hypo_DO$DO,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$R, col=c('red'),axes=FALSE, xlab = NA, ylab = NA,type = 'l',ylim=c(-0.2,0.2))
axis(side = 4)
mtext(side = 4, line = 2, expression(mmol~m^-3~day^-1),col=c('red'))
par(new = TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$GPP,col=c('green'),axes = FALSE, xlab = NA, ylab = NA, type = 'l',ylim=c(-0.2,0.2))
mtext(side = 4, line = 3.25, expression(mmol~m^-3~day^-1), col=c('green'))
par(new = TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$SedDemand,col=c('blue'),axes = FALSE, xlab = NA, ylab = NA, type = 'l',ylim=c(-0.2,0.2))
mtext(side = 4, line = 4.2, expression(mmol~m^-2~day^-1), col=c('blue'))
par(new = TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$Nitrif,col = c('purple'),axes = FALSE, xlab = NA, ylab = NA, type = 'l',ylim=c(-0.2,0.2))
mtext(side = 4, line = 5.3, expression(mmol~m^-3~day^-1), col=c('purple'))
legend('topleft',c('HYPOLIMNION','DO (mg/L)','Sediment Demand','GPP','Total Respiration','Nitrification Rate'),lty=c(NA,1,1,1,1,1),col=c(NA,'black','blue','green','red','purple'),lwd=c(NA,3,1,1,1,1))

#dev.off()
