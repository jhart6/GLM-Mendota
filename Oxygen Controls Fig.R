#DO state and flux controls 
#24 February 2017
#JAH

#create epi data frame
DO_3 <- get_var(SimFile, var_name = 'DO',reference = 'surface', z_out = 3)
atm_exch <-get_var(SimFile, var_name = 'OXY_atm_oxy_exch')
nitrif_3<-get_var(SimFile, var_name = 'NIT_nitrif',reference = 'surface', z_out = 3)

GPP_3<-get_var(SimFile, var_name = 'PHY_PPR',reference = 'surface', z_out = 3)
NEP_3<-get_var(SimFile, var_name = 'PHY_NPR', reference = 'surface', z_out =3)
Resp_3<-(GPP_3$PHY_PPR_3-NEP_3$PHY_NPR_3)

epi_DO <- data.frame(DO_3,atm_exch$OXY_atm_oxy_exch,nitrif_3$NIT_nitrif_3,GPP_3$PHY_PPR_3,Resp_3)


#create the hypo data frame
DO_20<-get_var(SimFile, var_name = 'DO', reference = 'surface', z_out = 20)
Fsed_oxy <- get_var(SimFile, var_name = 'OXY_sed_oxy')
nitrif_20<-get_var(SimFile, var_name = 'NIT_nitrif',reference = 'surface', z_out = 20)

GPP_20<-get_var(SimFile, var_name = 'PHY_PPR',reference = 'surface', z_out = 20)
NEP_20<-get_var(SimFile, var_name = 'PHY_NPR', reference = 'surface', z_out = 20)
Resp_20<-(GPP_20$PHY_PPR_20-NEP_20$PHY_NPR_20)


hypo_DO <- data.frame(DO_20,Fsed_oxy$OXY_sed_oxy,nitrif_20$NIT_nitrif_20,GPP_20$PHY_PPR_20,Resp_20)


#two panel plot
xlab = expression(Date)
state_ylab = expression(DO~(mg~L^-1))

quartz()

setwd("~/Dropbox/Masters Writing/Figures/")

png("Oxygen Controls.png",width = 8, height = 8, units = 'in', res = 500)

par(mfrow=c(2,1),mar=c(0,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02,oma = c(1,1.5,1,3))
plot(as.Date(epi_DO$DateTime),epi_DO$DO_3,type='l',xlab=xlab,ylab = state_ylab,lwd=3,xaxt='n')
par(new=TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$atm_exch.OXY_atm_oxy_exch, col=c('red'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 2, expression(mmol~m^-2~day^-1),col=c('red'))
mtext(side = 4, line = 3.25, expression(mmol~m^-2~day^-1), col=c('green'))
mtext(side = 4, line = 4.2, expression(mmol~m^-3~day^-1), col=c('blue'))
mtext(side = 4, line = 5.3, expression(mmol~m^-3~day^-1), col=c('purple'))
par(new = TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$GPP_3.PHY_PPR_3,col=c('green'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
par(new = TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$Resp_3,col=c('blue'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
par(new = TRUE)
plot(as.Date(epi_DO$DateTime),epi_DO$nitrif_3.NIT_nitrif_3,col = c('purple'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c('EPILIMNION','DO (mg/L)','DO Atm Flux','GPP','Total Respiration','Nitrification Rate'),lty=c(NA,1,1,1,1,1),col=c(NA,'black','red','green','blue','purple'),lwd=c(NA,3,1,1,1,1))

par(mar=c(3,3,0,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(hypo_DO$DateTime),hypo_DO$DO_20,type='l',xlab=xlab,ylab = state_ylab,lwd=3)
par(new=TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$Fsed_oxy.OXY_sed_oxy, col=c('red'),axes=FALSE, xlab = NA, ylab = NA,type = 'l')
axis(side = 4)
mtext(side = 4, line = 2, expression(mmol~m^-2~day^-1),col=c('red'))
mtext(side = 4, line = 3.25, expression(mmol~m^-2~day^-1), col=c('green'))
mtext(side = 4, line = 4.2, expression(mmol~m^-3~day^-1), col=c('blue'))
mtext(side = 4, line = 5.3, expression(mmol~m^-3~day^-1), col=c('purple'))
par(new = TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$GPP_20.PHY_PPR_20,col=c('green'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
par(new = TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$Resp_20,col=c('blue'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
par(new = TRUE)
plot(as.Date(hypo_DO$DateTime),hypo_DO$nitrif_20.NIT_nitrif_20,col = c('purple'),axes = FALSE, xlab = NA, ylab = NA, type = 'l')
legend('topleft',c('HYPOLIMNION','DO (mg/L)','DO Sediment Flux','GPP','Total Respiration','Nitrification Rate'),lty=c(NA,1,1,1,1,1),col=c(NA,'black','red','green','blue','purple'),lwd=c(NA,3,1,1,1,1))
