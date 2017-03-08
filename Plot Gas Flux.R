#Plot Gas Flux
#7 March 2017
#JAH

setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
flux<-read.csv('flux.data.csv')

############################################################
###############DISSOLVED OXYGEN FLUX########################
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'l',lwd=2,ylab = expression(DO~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date))
lines(as.Date(flux$datetime),flux$do.flux.read.obs,type= 'l',lwd=2,col='red')
abline(0,0,col='blue',lty=2,lwd=2)
legend('topright',c('Modeled DO Flux','Observed DO Flux'),lty = c(1,1),lwd=c(2,2),col=c('black','red'))



############################################################
#####################METHANE FLUX###########################
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='l', lwd=2, ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim=c(-3,20))
lines(as.Date(flux$datetime),flux$ch4.flux.read.obs,type = 'l', lwd = 2, col = 'red')
abline(0,0,col='blue',lty=2,lwd=2)
legend('topright',c(expression(Modeled~CH[4]~Flux),expression(Observed~CH[4]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','red'))



############################################################
#####################CARBON DIOXIDE FLUX####################
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-300,2000))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'red')
lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='purple')
abline(0,0,col='blue',lty =2,lwd=2)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','red'))



############################################################
#####################THREE PANEL PLOT#######################

quartz()
par(mfrow=c(3,1),mar=c(0,4,1,2),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'l',lwd=2,xlab=NA,ylab = expression(DO~Flux~(mmol~m^-2~day^-1)),xaxt='n')
lines(as.Date(flux$datetime),flux$do.flux.read.obs,type= 'l',lwd=2,col='red')
abline(0,0,col='blue',lty=2,lwd=2)
legend('topright',c('Modeled DO Flux','Observed DO Flux'),lty = c(1,1),lwd=c(2,2),col=c('black','red'))

par(mar=c(0,4,0,2),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='l', lwd=2, ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = NA,ylim=c(-3,20),xaxt='n')
lines(as.Date(flux$datetime),flux$ch4.flux.read.obs,type = 'l', lwd = 2, col = 'red')
abline(0,0,col='blue',lty=2,lwd=2)
legend('topright',c(expression(Modeled~CH[4]~Flux),expression(Observed~CH[4]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','red'))

par(mar=c(3,4,0,2),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-500,2000))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'red')
lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='purple')
abline(0,0,col='blue',lty =2,lwd=2)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux~(via~CO[2])),expression(Observed~CO[2]~Flux~(via~DO))),lty = c(1,1,1),lwd=c(2,2,2),col=c('black','red','purple'))

