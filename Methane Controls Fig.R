#ch4 state + fluxes figure 
#24 February 2017
#JAH

####epilimnion plot####
#assemble necessary data
state_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 3)
atm_exch<- get_var(SimFile, var_name = 'CAR_atm_ch4_exch')
ox_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out =3)

#convert all fluxes to mmol/m2/day
area_at_3m <- 33926717
ox_ch4_correct_units <- ox_ch4_3$CAR_ch4ox_3/area_at_3m

#create plotting dataframe
epi_ch4<-cbind(state_ch4_3,atm_exch$CAR_atm_ch4_exch,ox_ch4_3$CAR_ch4ox_3)
colnames(epi_ch4)<- c('DateTime','CH4','AtmExch','Ox')
write.csv(epi_ch4,file = 'epi_ch4.csv',row.names = FALSE)

#calculate diffusive flux in Excel with exported csv
#read back in csv with calculated diffusive flux
epi_ch4<-read.csv('epi_ch4+diff.csv')

#convert fluxes to the proper sign
atm_exch_sign <- epi_ch4$AtmExch*-1
ox_ch4_sign <- epi_ch4$Ox*-1

#final plotting dataset
epi_ch4 <- cbind(state_ch4_3,atm_exch_sign,ox_ch4_sign,epi_ch4$Diff)
colnames(epi_ch4)<-c('DateTime','CH4','AtmExch','Ox','Diff')


#creating independent plot
xlab = expression(Date)
state_ylab = expression(CH[4]~(mu*mol~m^-3))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(epi_ch4$DateTime),epi_ch4$CH4,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$AtmExch, col=c('red'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4,)
mtext(side = 4, line = 3, 'mmol/m2/day')
par(new = TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$Ox,col=c('blue'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c(expression(CH[4]),expression(CH[4]~Flux),expression(CH[4]~Oxidation)),lty=c(1,1,1),col=c('black','red','blue'),lwd=c(3,1,1))

#new independent plot
xlab = expression(Date)
ylab = expression(CH[4]~Fluxes~(mu*mol~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(epi_ch4$DateTime),epi_ch4$AtmExch,type = 'l', xlab = xlab, ylab=ylab, ylim = c(-0.005,0.007),col='red')
lines(as.Date(epi_ch4$DateTime),epi_ch4$Ox,col='darkblue')
#lines(as.Date(epi_ch4$DateTime),epi_ch4$Diff,col='dark green')
par(new=TRUE)
plot(as.Date(epi_ch4$DateTime),epi_ch4$CH4,lty = 1, lwd = 3,col='black',axes = FALSE,type = 'l',xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, text = state_ylab)



####calculate sediment ch4 flux#####
Fsed_ch4<-10
Ksed_ch4<-100
theta_sed_ch4<- 1.07

oxy<-get_var(SimFile, var_name = 'OXY_oxy', reference = 'surface', z_out = 24.5)
temp<-get_var(SimFile, var_name = 'temp', reference = 'surface', z_out = 24.5)

ch4_flux = Fsed_ch4 * (Ksed_ch4/(Ksed_ch4+oxy$OXY_oxy_24.5)) * (theta_sed_ch4)^(temp$temp_24.5-20)

plot(as.Date(oxy$DateTime),ch4_flux,ylim= c(0,10),type='l')

ch4_flux_ts<-data.frame(oxy$DateTime,ch4_flux)
View(ch4_flux_ts)


####hypolimnion plot####
#assemble necessary data
state_ch4_20<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 20)
ox_ch4_20<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out = 20)
prod_ch4<-ch4_flux

#convert units for all fluxes
area_at_20m <- 8481679
ox_ch4_20_correct_units <- ox_ch4_20$CAR_ch4ox_20/area_at_20m

#export dataset
hypo_ch4<-cbind(state_ch4_20,ox_ch4_20_correct_units,prod_ch4)
write.csv(hypo_ch4,file = 'hypo_ch4.csv',row.names = FALSE)

#open Excel to calculate diffusive flux (sink)
#read in new dataset
hypo_ch4<-read.csv('hypo_ch4+diff.csv')

#convert signs for all fluxes
ox_ch4_20_sign = hypo_ch4$ox_ch4_20_correct_units*-1
diff = hypo_ch4$diff


#create data frame
hypo_ch4 <- cbind(state_ch4_20,ox_ch4_20_sign,prod_ch4,diff)
colnames(hypo_ch4)=c('DateTime','CH4','Ox','Prod','Diff')

#create independent plot
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$CAR_ch4_20,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$ox_ch4_20.CAR_ch4ox_20, col=c('blue'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 3, 'mmol/m2/day')
par(new = TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$prod_ch4,col=c('red'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c(expression(CH[4]),expression(CH[4]~Sediment~Flux),expression(CH[4]~Oxidation)),lty=c(1,1,1),col=c('black','red','blue'),lwd=c(3,1,1))

#new independent plot
xlab = expression(Date)
ylab = expression(CH[4]~Fluxes~(mu*mol~m^-2~day^-1))
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$Prod,type = 'l', xlab = xlab, ylab=ylab,col='red',ylim=c(-1,7))
lines(as.Date(hypo_ch4$DateTime),hypo_ch4$Ox,col='darkblue')
lines(as.Date(hypo_ch4$DateTime),hypo_ch4$Diff,col='dark green')
par(new=TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$CH4,lty = 1, lwd = 3,col='black',axes = FALSE,type = 'l',xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, text = state_ylab)

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
mtext(side = 4, line = 2, expression(mmol~m^-2~day^-1),col=c('red'))
mtext(side = 4, line = 3.25, expression(mmol~day^-1), col=c('blue'))
par(new = TRUE)
plot(as.Date(hypo_ch4$DateTime),hypo_ch4$prod_ch4,col=c('red'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c('HYPOLIMNION',expression(CH[4]),expression(CH[4]~Sediment~Flux),expression(CH[4]~Oxidation)),lty=c(NA,1,1,1),col=c(NA,'black','red','blue'),lwd=c(NA,3,1,1))

dev.off()







