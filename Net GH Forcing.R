#Net GH Forcing Calculation and Figure
#2 March 2017
#JAH

#run Gas Flux script first
#assemble data frame: datetime, co2 flux, ch4 flux
#using gas flux from Read method
datetime <- as.POSIXct(strptime(metHourly$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
co2.flux.mmol.m2.day <- co2.flux.read
ch4.flux.mmol.m2.day <- ch4.flux.read

net.forcing <- data.frame(datetime, co2.flux.mmol.m2.day,ch4.flux.mmol.m2.day)

#convert gas fluxes to mmol of C m-2 day-1
co2.flux.mmolC <- net.forcing$co2.flux.mmol.m2.day * (12/44)
ch4.flux.mmolC <- net.forcing$ch4.flux.mmol.m2.day * (12/16)

net.forcing <- cbind(net.forcing, co2.flux.mmolC, ch4.flux.mmolC)

#calculate Net GH Forcing
netGHG <- net.forcing$ch4.flux.mmolC + (25*net.forcing$co2.flux.mmolC)

xlab = expression(Date)
ylab = expression(Net~Greenhouse~Forcing~(mmol~C~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab)
abline(0,0,lty=2, col='red',lwd=2)

net.forcing<-cbind(net.forcing, netGHG)
write.csv(net.forcing, file = 'netGHGforcing.csv',row.names = FALSE)


####include ebullition estimate####
#Bastviken et al. 2004: 45% ebullition based on lake size
#Bastviken et al. 2004: 60% ebullition, generally
#Casper et al. 2000: 96% ebullition, based on tropic state

bastviken.total.ch4.flux <- ch4.flux.read * (100/55)
bastviken.high.total.ch4.flux <- ch4.flux.read * (100/40)
casper.total.ch4.flux <- ch4.flux.read * (100/4)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime, ch4.flux.read, type = 'l',ylim = c(0, 350),xlab = 'Date', ylab = expression(mmol~m^-2~day^-1),lwd=2)
lines(datetime, bastviken.total.ch4.flux, type = 'l', col='red',lwd=2)
lines(datetime, bastviken.high.total.ch4.flux, type = 'l', col = 'blue',lwd=2)
lines(datetime, casper.total.ch4.flux, type = 'l', col = 'purple',lwd=2)
legend('topleft',c('CH4 Diffusive Flux','45% Ebullition','60% Ebullition','96% Ebullition'),lty = c(1,1,1,1), col = c('black','red','blue','purple'))


#####convert to mmol C m^-2 day^-1####
inc.ch4.flux.mmolC <- bastviken.total.ch4.flux * (12/16)

inc.netGHG <- inc.ch4.flux.mmolC + (25*net.forcing$co2.flux.mmolC)


quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab)
lines(datetime,inc.netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab, col = 'blue')
abline(0,0,lty=2, col='red',lwd=2)
legend('topleft',c(expression(CH[4]~Diffusive~Flux~Only),expression(CH[4]~Diffusion~and~Ebullition)),lty=c(1,1),lwd=c(2,2), col=c('black','blue'))
