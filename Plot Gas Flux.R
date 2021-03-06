#Plot Gas Flux
#7 March 2017
#JAH

setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
#flux<-read.csv('flux.data.csv')
flux<-read.csv('flux.data.PaulCalibration.csv')
############################################################
###############DISSOLVED OXYGEN FLUX########################
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'l',lwd=2,ylab = expression(DO~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date))
lines(as.Date(flux$datetime),flux$do.flux.read.obs,type= 'l',lwd=2,col='dodgerblue3')
abline(0,0,col='slategrey',lty=2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'n',xlab=NA,ylab=NA,xaxt='n',ylim=c(-7.2,7.2),axes=FALSE)
axis(side=4,ylim=c(-7.2,7.2))
DOexp<-expression(DO~Flux~(g~O[2]~m^-2~day^-1))
mtext(side=4,line=2,DOexp)
legend('topright',c('Modeled DO Flux','Observed DO Flux'),lty = c(1,1),lwd=c(2,2),col=c('black','dodgerblue3'))



############################################################
#####################METHANE FLUX###########################
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='l', lwd=2, ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim=c(-3,15))
lines(as.Date(flux$datetime),flux$ch4.flux.read.obs,type = 'l', lwd = 2, col = 'firebrick')
abline(0,0,col='slategrey',lty=2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-0.036,0.18))
axis(side=4,ylim=c(-0.036,0.18))
CH4exp<-expression(CH[4]~Flux~(g~C~m^-2~day^-1))
mtext(side=4,line=2,CH4exp)
legend('topright',c(expression(Modeled~CH[4]~Flux),expression(Observed~CH[4]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','firebrick'))



############################################################
#####################CARBON DIOXIDE FLUX####################
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-300,500))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'gold1')
#lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='purple')
abline(0,0,col='slategrey',lty =2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-6,9))
axis(side=4,ylim=c(-6,9))
CO2exp<-expression(CO[2]~Flux~(g~C~m^-2~day^-1))
mtext(side=4, line=2, CO2exp)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','gold1'))



############################################################
#####################THREE PANEL PLOT#######################

#do
quartz()
par(mfrow=c(3,1),mar=c(0,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'l',lwd=2,xlab=NA,ylab = expression(DO~Flux~(mmol~O[2]~m^-2~day^-1)),xaxt='n',ylim=c(-225,225))
lines(as.Date(flux$datetime),flux$do.flux.read.obs,type= 'l',lwd=2,col='dodgerblue3')
abline(0,0,col='slategrey',lty=2,lwd=2)

par(new=TRUE)
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'n',xlab=NA,ylab=NA,xaxt='n',ylim=c(-7.2,7.2),axes=FALSE)
axis(side=4,ylim=c(-7.2,7.2))
DOexp<-expression(DO~Flux~(g~O[2]~m^-2~day^-1))
mtext(side=4,line=2,DOexp,cex=0.675)
legend('topright',c('Modeled DO Flux','Observed DO Flux'),lty = c(1,1),lwd=c(2,2),col=c('black','dodgerblue3'))

#ch4
par(mar=c(0,4,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='l', lwd=2, ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = NA,ylim=c(-3,15),xaxt='n')
lines(as.Date(flux$datetime),flux$ch4.flux.read.obs,type = 'l', lwd = 2, col = 'firebrick')
abline(0,0,col='slategrey',lty=2,lwd=2)

par(new=TRUE)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-0.036,0.18))
axis(side=4,ylim=c(-0.036,0.18))
CH4exp<-expression(CH[4]~Flux~(g~C~m^-2~day^-1))
mtext(side=4,line=2,CH4exp,cex=0.675)
legend('topright',c(expression(Modeled~CH[4]~Flux),expression(Observed~CH[4]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','firebrick'))

#co2
par(mar=c(3,4,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-500,750))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'gold1')
#lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='darkorange2')
abline(0,0,col='slategrey',lty =2,lwd=2)

par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-6,9))
axis(side=4,ylim=c(-6,9))
CO2exp<-expression(CO[2]~Flux~(g~C~m^-2~day^-1))
mtext(side=4, line=2, CO2exp, cex=0.675)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','gold1'))





####CO2 Plot with White Background####
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-500,750))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'gold1')
#lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='darkorange2')
abline(0,0,col='slategrey',lty =2,lwd=2)

par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-6,9))
axis(side=4,ylim=c(-6,9))
CO2exp<-expression(CO[2]~Flux~(g~C~m^-2~day^-1))
mtext(side=4, line=2, CO2exp)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','gold1'))


####flux plot with dots for discrete sampling days####
#do not run flux calculation script again - have altered the resulting csv to make this plot
quartz()
par(mfrow=c(3,1),mar=c(0,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'l',lwd=2,xlab=NA,ylab =expression(DO~Flux~(mmol~O[2]~m^-2~day^-1)),xaxt='n',ylim=c(-225,225))
lines(as.Date(flux$datetime),flux$do.flux.read.obs,type= 'l',lwd=2,col='dodgerblue3',ylim=c(-225,225))
abline(0,0,col='slategrey',lty=2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$do.sampling.days,pch=16,cex=1.5,col='dodgerblue3',xlab=NA,ylab=NA,xaxt='n',ylim=c(-225,225))

par(new=TRUE)
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'n',xlab=NA,ylab=NA,xaxt='n',ylim=c(-7.2,7.2),axes=FALSE)
axis(side=4,ylim=c(-7.2,7.2))
DOexp<-expression(DO~Flux~(g~O[2]~m^-2~day^-1))
mtext(side=4,line=2,DOexp,cex=0.675)
legend('topright',c('Modeled DO Flux','Observed DO Flux'),lty = c(1,1),lwd=c(2,2),col=c('black','dodgerblue3'))
mtext("a",side=3,line=-2,adj=0.05)

#ch4
par(mar=c(0,4,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='l', lwd=2, ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = NA,ylim=c(-3,15),xaxt='n')
lines(as.Date(flux$datetime),flux$ch4.flux.read.obs,type = 'l', lwd = 2, col = 'firebrick')
abline(0,0,col='slategrey',lty=2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$ch4.sampling.days,pch=16,cex=1.5,col='firebrick',xlab=NA,ylab=NA,xaxt='n',ylim=c(-3,15))

par(new=TRUE)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-0.036,0.18))
axis(side=4,ylim=c(-0.036,0.18))
CH4exp<-expression(CH[4]~Flux~(g~C~m^-2~day^-1))
mtext(side=4,line=2,CH4exp,cex=0.675)
legend('topright',c(expression(Modeled~CH[4]~Flux),expression(Observed~CH[4]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','firebrick'))
mtext("b",side=3,line=-2,adj=0.05)

#co2
par(mar=c(3,4,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-500,750))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'gold1')
#lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='darkorange2')
abline(0,0,col='slategrey',lty =2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.sampling.days,pch=16,cex=1.5,col='gold1',xlab=NA,ylab=NA,xaxt='n',ylim=c(-500,750))

par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-6,9))
axis(side=4,ylim=c(-6,9))
CO2exp<-expression(CO[2]~Flux~(g~C~m^-2~day^-1))
mtext(side=4, line=2, CO2exp, cex=0.675)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','gold1'))
mtext("c",side=3,line=-2,adj=0.05)

####Manuscript Worthy Tiff for L&O####
#make PNG, import into Ai, add multipanel letter labels

setwd('~/Dropbox/Masters Writing/Figures/Tiffs for L&O/')

png('gasflux.png',width=7,height=7,units = 'in',res=300)
par(mfrow=c(3,1),mar=c(0,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'l',lwd=2,xlab=NA,ylab =expression(DO~Flux~(mmol~O[2]~m^-2~day^-1)),xaxt='n',ylim=c(-225,225))
lines(as.Date(flux$datetime),flux$do.flux.read.obs,type= 'l',lwd=2,col='dodgerblue3',ylim=c(-225,225))
abline(0,0,col='slategrey',lty=2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$do.sampling.days,pch=16,cex=1.5,col='dodgerblue3',xlab=NA,ylab=NA,xaxt='n',ylim=c(-225,225))

par(new=TRUE)
plot(as.Date(flux$datetime),flux$do.flux.read.mod,type = 'n',xlab=NA,ylab=NA,xaxt='n',ylim=c(-7.2,7.2),axes=FALSE)
axis(side=4,ylim=c(-7.2,7.2))
DOexp<-expression(DO~Flux~(g~O[2]~m^-2~day^-1))
mtext(side=4,line=2,DOexp,cex=0.675)
legend('topright',c('Modeled DO Flux','Observed DO Flux'),lty = c(1,1),lwd=c(2,2),col=c('black','dodgerblue3'))

#ch4
par(mar=c(0,4,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='l', lwd=2, ylab = expression(CH[4]~Flux~(mmol~m^-2~day^-1)),xlab = NA,ylim=c(-3,15),xaxt='n')
lines(as.Date(flux$datetime),flux$ch4.flux.read.obs,type = 'l', lwd = 2, col = 'firebrick')
abline(0,0,col='slategrey',lty=2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$ch4.sampling.days,pch=16,cex=1.5,col='firebrick',xlab=NA,ylab=NA,xaxt='n',ylim=c(-3,15))

par(new=TRUE)
plot(as.Date(flux$datetime),flux$ch4.flux.read.mod,type='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-0.036,0.18))
axis(side=4,ylim=c(-0.036,0.18))
CH4exp<-expression(CH[4]~Flux~(g~C~m^-2~day^-1))
mtext(side=4,line=2,CH4exp,cex=0.675)
legend('topright',c(expression(Modeled~CH[4]~Flux),expression(Observed~CH[4]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','firebrick'))

#co2
par(mar=c(3,4,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='l',lwd =2,ylab = expression(CO[2]~Flux~(mmol~m^-2~day^-1)),xlab = expression(Date),ylim = c(-500,750))
lines(as.Date(flux$datetime),flux$co2.flux.read.obs,type = 'l', lwd = 2, col = 'gold1')
#lines(as.Date(flux$datetime),flux$co2.flux.read.obs.do,type='l',lwd=2, col='darkorange2')
abline(0,0,col='slategrey',lty =2,lwd=2)
par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.sampling.days,pch=16,cex=1.5,col='gold1',xlab=NA,ylab=NA,xaxt='n',ylim=c(-500,750))

par(new=TRUE)
plot(as.Date(flux$datetime),flux$co2.flux.read.mod,type ='n',xlab=NA,ylab=NA,xaxt='n',axes=FALSE,ylim=c(-6,9))
axis(side=4,ylim=c(-6,9))
CO2exp<-expression(CO[2]~Flux~(g~C~m^-2~day^-1))
mtext(side=4, line=2, CO2exp, cex=0.675)
legend('topright',c(expression(Modeled~CO[2]~Flux),expression(Observed~CO[2]~Flux)),lty = c(1,1),lwd=c(2,2),col=c('black','gold1'))

dev.off()
