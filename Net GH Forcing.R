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
ylab = expression(Net~Greenhouse~Forcing)
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(datetime,netGHG,type = 'l',lwd=2,ylab=ylab, xlab = xlab)
abline(0,0,lty=2, col='red')

