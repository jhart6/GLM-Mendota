#Net GH Forcing Calculation and Figure
#2 March 2017
#JAH

#run Gas Flux script first
#assemble data frame: datetime, co2 flux, ch4 flux
#using gas flux from Read method
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
net.forcing<-read.csv('flux.data.csv')
datetime = as.Date(net.forcing$datetime)

########################################################################
######################NGHF USING MODELED FLUXES#########################
#convert gas fluxes to mmol of C m-2 day-1
mod.co2.flux.mmolC <- net.forcing$co2.flux.read.mod 
mod.ch4.flux.mmolC <- net.forcing$ch4.flux.read.mod 

net.forcing <- cbind(net.forcing, mod.co2.flux.mmolC, mod.ch4.flux.mmolC)

#calculate Net GH Forcing
mod.netGHG <- (34*net.forcing$mod.ch4.flux.mmolC) + net.forcing$mod.co2.flux.mmolC

xlab = expression(Date)
ylab = expression(Net~Greenhouse~Forcing~(mmol~C~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,mod.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab)
abline(0,0,lty=2, col='blue',lwd=2)

########################################################################
############NGHF USING OBSERVED FLUXES (CO2 via CO2)####################
#convert gas fluxes to mmol of C m-2 day-1
obs.co2.flux.mmolC <- net.forcing$co2.flux.read.obs 
obs.ch4.flux.mmolC <- net.forcing$ch4.flux.read.obs 

net.forcing <- cbind(net.forcing, obs.co2.flux.mmolC, obs.ch4.flux.mmolC)

#calculate Net GH Forcing
obs.netGHG <- (34*net.forcing$obs.ch4.flux.mmolC) + net.forcing$obs.co2.flux.mmolC

xlab = expression(Date)
ylab = expression(Net~Greenhouse~Forcing~(mmol~C~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,obs.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab)
abline(0,0,lty=2, col='blue',lwd=2)

########################################################################
#############NGHF USING OBSERVED FLUXES (CO2 via DO)###################
obs.co2.flux.mmolC.do <- net.forcing$co2.flux.read.obs.do 
obs.ch4.flux.mmolC.do <- net.forcing$ch4.flux.read.obs 

net.forcing <- cbind(net.forcing, obs.co2.flux.mmolC.do, obs.ch4.flux.mmolC.do)

#calculate NGHF
obs.netGHG.do <- (34*net.forcing$obs.ch4.flux.mmolC.do) + net.forcing$obs.co2.flux.mmolC.do

xlab = expression(Date)
ylab = expression(Net~Greenhouse~Forcing~(mmol~C~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,obs.netGHG.do,type = 'l',lwd=2,ylab=ylab, xlab = xlab)
abline(0,0,lty=2, col='blue',lwd=2)

net.forcing<-cbind(net.forcing, mod.netGHG, obs.netGHG, obs.netGHG.do)
write.csv(net.forcing, file = 'netGHGforcing.csv',row.names = FALSE)

########################################################################
#######################ALL NGHF, ONE PLOT###############################
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
nghf<-read.csv("netGHGforcing.csv")

xlab = expression(Date)
ylab = expression(Net~Greenhouse~Forcing~(mmol~C~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(datetime,nghf$mod.netGHG,type = 'l', lwd = 2,ylab = ylab, xlab = xlab,ylim=c(-300,725))
lines(datetime, nghf$obs.netGHG,type = 'l', lwd = 2, col='darkgreen')
#lines(datetime, nghf$obs.netGHG.do,type = 'l', lwd = 2, col = 'darkolivegreen3')
abline(0,0,col='slategrey',lty=2,lwd=2)
legend('topright',c("NGHF from Modeled Fluxes",expression(NGHF~from~Observed~Fluxes)),lwd=c(2,2),col=c('black','darkgreen'))






# ########################################################################
# ##############################INCLUDE EBULLITION########################
# ####from MODELED gas fluxes####
# #Bastviken et al. 2004: 45% ebullition based on lake size
# #Bastviken et al. 2004: 60% ebullition, generally
# #Casper et al. 2000: 96% ebullition, based on tropic state
# 
# mod.bastviken.total.ch4.flux <- net.forcing$ch4.flux.read.mod * (100/55)
# mod.bastviken.high.total.ch4.flux <- net.forcing$ch4.flux.read.mod * (100/40)
# mod.casper.total.ch4.flux <- net.forcing$ch4.flux.read.mod * (100/4)
# 
# #increased CH4 flux plot
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(datetime, net.forcing$ch4.flux.read.mod, type = 'l',ylim = c(-10,300),xlab = 'Date', ylab = expression(mmol~m^-2~day^-1),lwd=2)
# lines(datetime, mod.bastviken.total.ch4.flux, type = 'l', col='red',lwd=2)
# lines(datetime, mod.bastviken.high.total.ch4.flux, type = 'l', col = 'blue',lwd=2)
# lines(datetime, mod.casper.total.ch4.flux, type = 'l', col = 'purple',lwd=2)
# legend('topleft',c('CH4 Diffusive Flux','45% Ebullition','60% Ebullition','96% Ebullition'),lty = c(1,1,1,1), col = c('black','red','blue','purple'))
# abline(0,0,lty=2,lwd=2,col='blue')
# 
# #convert to mmol C m^-2 day^-1
# mod.inc.ch4.flux.mmolC <- mod.bastviken.total.ch4.flux * (12/16)
# 
# mod.inc.netGHG <- (25*mod.inc.ch4.flux.mmolC) + mod.co2.flux.mmolC
# 
# #increased NGHF plot
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(datetime,mod.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab,ylim=c(-60,500))
# lines(datetime,mod.inc.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab, col = 'red')
# abline(0,0,lty=2, col='blue',lwd=2)
# legend('topleft',c(expression(CH[4]~Diffusive~Flux~Only),expression(CH[4]~Diffusion~and~Ebullition)),lty=c(1,1),lwd=c(2,2), col=c('black','red'))
# 
# 
# 
# #####from OBSERVED gas fluxes (CO2 via CO2)#####
# obs.bastviken.total.ch4.flux <- net.forcing$ch4.flux.read.obs * (100/55)
# obs.bastviken.high.total.ch4.flux <- net.forcing$ch4.flux.read.obs * (100/40)
# obs.casper.total.ch4.flux <- net.forcing$ch4.flux.read.obs * (100/4)
# 
# #increased CH4 flux plot
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(datetime, net.forcing$ch4.flux.read.obs, type = 'l',ylim = c(0, 450),xlab = 'Date', ylab = expression(mmol~m^-2~day^-1),lwd=2)
# lines(datetime, obs.bastviken.total.ch4.flux, type = 'l', col='red',lwd=2)
# lines(datetime, obs.bastviken.high.total.ch4.flux, type = 'l', col = 'blue',lwd=2)
# lines(datetime, obs.casper.total.ch4.flux, type = 'l', col = 'purple',lwd=2)
# legend('topleft',c('CH4 Diffusive Flux','45% Ebullition','60% Ebullition','96% Ebullition'),lty = c(1,1,1,1), col = c('black','red','blue','purple'))
# abline(0,0,lty=2,lwd=2,col='blue')
# 
# #convert to mmol C m^-2 day^-1
# obs.inc.ch4.flux.mmolC <- obs.bastviken.total.ch4.flux * (12/16)
# 
# obs.inc.netGHG <- (25*obs.inc.ch4.flux.mmolC) + obs.co2.flux.mmolC
# 
# ####THIS NGFH WITH EBULLITION PLOT####
# #increased NGHF plot
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(datetime,obs.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab,ylim=c(-60,1200),col='darkgreen')
# lines(datetime,obs.inc.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab, col = 'violetred4')
# abline(0,0,lty=2, col='slategrey',lwd=2)
# legend('topleft',c(expression(NGHF~from~CH[4]~Diffusive~Flux),expression(NGHF~from~CH[4]~Diffusion~+~Ebullition)),lty=c(1,1),lwd=c(2,2), col=c('darkgreen','violetred4'))
# 
# 
# 
# 
# ####from OBSERVED gas fluxes (CO2 via DO)####
# #use same obs.bastviken.total.ch4.flux estimates
# #use same obs.inc.ch4.flux.mmolC
# 
# obs.inc.netGHG.do <- (25*obs.inc.ch4.flux.mmolC) + obs.co2.flux.mmolC.do
# 
# quartz()
# par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(datetime,obs.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab,ylim=c(-60,1200))
# lines(datetime,obs.inc.netGHG.do,type = 'l',lwd=2,ylab=ylab, xlab = xlab, col = 'red')
# abline(0,0,lty=2, col='blue',lwd=2)
# legend('topleft',c(expression(CH[4]~Diffusive~Flux~Only),expression(CH[4]~Diffusion~and~Ebullition)),lty=c(1,1),lwd=c(2,2), col=c('black','red'))
# 
# 
# 
