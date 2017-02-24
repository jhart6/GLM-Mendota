#ch4 state + fluxes figure 
#24 February 2017
#JAH

####epilimnion plot####
#assemble necessary data
state_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 3)
atm_exch<- get_var(SimFile, var_name = 'CAR_atm_ch4_exch')
ox_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out =3)

#create plotting dataframe
epi_ch4 <- data.frame(state_ch4_3,atm_exch$CAR_atm_ch4_exch,ox_ch4_3$CAR_ch4ox_3)

#creating independent plot
xlab = expression(Date)
state_ylab = expression(CH[4]~(mu*mol~L^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(epi_ch4$DateTime),epi_ch4$CAR_ch4_3,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$atm_exch.CAR_atm_ch4_exch, col=c('red'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 3, 'mmol/m2/day')
par(new = TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$ox_ch4_3.CAR_ch4ox_3,col=c('blue'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c(expression(CH[4]),expression(CH[4]~Flux),expression(CH[4]~Oxidation)),lty=c(1,1,1),col=c('black','red','blue'),lwd=c(3,1,1))



####hypolimnion plot####
#assemble necessary data
state_ch4_20<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 20)
ox_ch4_20<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out = 20)
prod_ch4<-get_var(SimFile, var_name = 'SDF_Fsed_ch4', reference = 'surface', z_out = 20)

#create data frame
hypo_ch4 <- data.frame(state_ch4_20,ox_ch4_20$CAR_ch4ox_20,prod_ch4$SDF_Fsed_ch4)

#create independent plot
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$CAR_ch4_20,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$ox_ch4_20.CAR_ch4ox_20, col=c('blue'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 3, 'mmol/m2/day')
par(new = TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$prod_ch4.SDF_Fsed_ch4,col=c('red'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c(expression(CH[4]),expression(CH[4]~Sediment~Flux),expression(CH[4]~Oxidation)),lty=c(1,1,1),col=c('black','red','blue'),lwd=c(3,1,1))



####two panel plot####
quartz()

setwd("~/Dropbox/Masters Writing/Figures/")

png("Methane Controls.png",width = 8, height = 8, units = 'in', res = 500)

par(mfrow=c(2,1),mar=c(0,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,oma = c(1,1.5,1,1.5))
plot(as.Date(epi_ch4$DateTime),epi_ch4$CAR_ch4_3,type='l',xlab=xlab,ylab = state_ylab,lwd=3,xaxt='n')
par(new=TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$atm_exch.CAR_atm_ch4_exch, col=c('red'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 2, expression(mmol~m^-2~day^-1),col=c('red'))
mtext(side = 4, line = 3.25, expression(mmol~day^-1), col=c('blue'))
par(new = TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$ox_ch4_3.CAR_ch4ox_3,col=c('blue'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c('EPILIMNION',expression(CH[4]),expression(CH[4]~Flux~to~Atm),expression(CH[4]~Oxidation)),lty=c(NA,1,1,1),col=c(NA,'black','red','blue'),lwd=c(NA,3,1,1))

par(mar=c(3,3,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$CAR_ch4_20,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$ox_ch4_20.CAR_ch4ox_20, col=c('blue'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 2, expression(mmol~m^-2),col=c('red'))
mtext(side = 4, line = 3.25, expression(mmol~day^-1), col=c('blue'))
par(new = TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$prod_ch4.SDF_Fsed_ch4,col=c('red'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c('HYPOLIMNION',expression(CH[4]),expression(CH[4]~Sediment~Flux),expression(CH[4]~Oxidation)),lty=c(NA,1,1,1),col=c(NA,'black','red','blue'),lwd=c(NA,3,1,1))

dev.off()
