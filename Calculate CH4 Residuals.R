#Calculating CH4 Residuals
#CH4 Residual = ObsCH4 - SimCH4
#both need to be at the daily time step
#27 February 2017
#JAH

####CH4 Interpolation####
#import weekly observed CH4 data
setwd("~/Dropbox/Mendota Summer 16/R/")
weekly_ch4<-read.csv("weekly_ch4_20.csv")

#visualize weekely observed CH4 data
plot(as.Date(weekly_ch4$DATETIME),weekly_ch4$CH4,pch=20)

#call interpolation package and create weekly ts vector
library(imputeTS)
x<-weekly_ch4$CH4

#linear interpolation
linear_interp_x<-c(na.interpolation(x, option = 'linear'))
plot(as.Date(weekly_ch4$DATETIME),linear_interp_x,type='l',main='Linear')

#spline interpolation
spline_interp_x<-c(na.interpolation(x,option = 'spline'))
plot(as.Date(weekly_ch4$DATETIME),spline_interp_x,type='l',main = 'Spline')

#stine interpolation 
stine_interp_x<-c(na.interpolation(x,option = 'stine'))
plot(as.Date(weekly_ch4$DATETIME),stine_interp_x,type='l',main = 'Stine')



####LogCH4 Interpolation####
#import weekly observed log_CH4 data
setwd("~/Dropbox/Mendota Summer 16/R/")
weekly_logch4<-read.csv("weekly_logch4_20.csv")

log_x<-weekly_logch4$LOGCH4

#linear interpolation
linear_interp_logx<-c(na.interpolation(log_x, option = 'linear'))
plot(as.Date(weekly_logch4$DATETIME),linear_interp_logx,type='l',main='Linear')

#spline interpolation
spline_interp_logx<-c(na.interpolation(log_x,option = 'spline'))
plot(as.Date(weekly_logch4$DATETIME),spline_interp_logx,type='l',main = 'Spline')

#stine interpolation 
stine_interp_logx<-c(na.interpolation(log_x,option = 'stine'))
plot(as.Date(weekly_logch4$DATETIME),stine_interp_logx,type='l',main = 'Stine')


####Extract Modeled CH4####
library(glmtools)
SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 

daily_mod_ch4<-get_var(SimFile, var_name = 'CAR_ch4',reference='surface',z_out = 20)
daily_mod_logch4<-get_var(SimFile, var_name = 'log_CAR_ch4',reference = 'surface',z_out = 20)

#match modeled data dates to interpolated observed data dates 
#june 1 to nov 1
daily_mod_ch4_short<-daily_mod_ch4[47:200,]
daily_mod_logch4_short<-daily_mod_logch4[47:200,]

#assemble data frame with all relevant data
col_headers <- c('Date','DailyObsCH4','DailyObsLogCH4','DailyModCH4','DailyModLogCH4')
residual<-data.frame(weekly_ch4$DATETIME)
residual<- cbind(residual,spline_interp_x,spline_interp_logx,daily_mod_ch4_short$CAR_ch4_20,daily_mod_logch4_short$log_CAR_ch4_20) 
colnames(residual) = col_headers



####calculate CH4 residuals####
ch4_residual <- residual$DailyObsCH4-residual$DailyModCH4
logch4_residual <- residual$DailyObsLogCH4-residual$DailyModLogCH4


####plot residual over time####
quartz()
par(mfrow = c(2,1))
plot(as.Date(residual$Date),ch4_residual,type='l',main = 'CH4 Residuals')
plot(as.Date(residual$Date),logch4_residual,type = 'l',main = 'LOG CH4 Residuals')


####COMPARE CH4 RESIDUALS TO POC, BOTH OBS AND MODELED####
####load obs POC data####
setwd("~/Dropbox/Mendota Summer 16/R/")
weekly_poc<-read.csv("weekly_poc_gas.csv")
p<-weekly_poc$POC

####interpolate weekly poc to daily####
interp_spline_p<-c(na.interpolation(p,option = 'spline'))
plot(as.Date(weekly_poc$DATETIME),interp_spline_p,type='l')

####extract modeled POC at 3m####
daily_mod_POC<-get_var(SimFile, var_name = 'TOT_POC',reference = 'surface',z_out=3)
daily_mod_POC_short<-daily_mod_POC[47:200,]



####plot interp obs poc with log CH4 residuals####
##WINNER##
quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(residual$Date),interp_spline_p,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (POC~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='firebrick',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='firebrick')
legend('topleft',c('POC @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','firebrick'),lty=c(1,1))

# ####plot sim poc with log CH4 residuals####
# #this figure not used#
# quartz()
# par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(as.Date(residual$Date),daily_mod_POC_short$TOT_POC_3,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (POC~(mg~L^-1)))
# par(new= TRUE)
# plot(as.Date(residual$Date),logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
# axis(side = 4)
# mtext(side = 4, line = 2.5, 'log(CH4) Residuals',col='red')
# legend('topleft',c('POC @ 3m','log(CH4) Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))
# 
# ####plot sim poc with CH4 residuals####
# #this figure not used#
# quartz()
# par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(as.Date(residual$Date),interp_spline_p,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (POC~(mg~L^-1)))
# par(new= TRUE)
# plot(as.Date(residual$Date),ch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
# axis(side = 4)
# mtext(side = 4, line = 2.5, 'CH4 Residuals',col='red')
# legend('topleft',c('POC @ 3m','CH4 Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))

# ####plot mod poc with CH4 residuals####
# #this figure not used#
# quartz()
# par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
# plot(as.Date(residual$Date),daily_mod_POC_short$TOT_POC_3,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (POC~(mg~L^-1)))
# par(new= TRUE)
# plot(as.Date(residual$Date),ch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
# axis(side = 4)
# mtext(side = 4, line = 2.5, 'CH4 Residuals',col='red')
# legend('topleft',c('POC @ 3m','CH4 Residuals'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))


####Add discrete sampling points to obs POC vs log CH4 residuals plot####
discrete<-cbind(residual,weekly_poc$POC,logch4_residual)
colnames(discrete) <- c('Date','DailyObsCH4','DailyObsLogCH4','DailyModCH4','DailyModLogCH4','ObsPOC','LogResidual')
write.csv(discrete,'ch4_residuals.csv',row.names=FALSE)

quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(discrete$Date),discrete$ObsPOC,pch=20,xlab = expression(Date),ylab = expression (POC~(mg~L^-1)),ylim=c(0.6,1.7),cex=2)
par(new=TRUE)
plot(as.Date(residual$Date),interp_spline_p,type = 'l',lwd = 2,xlab=NA,ylab=NA,ylim=c(0.6,1.7),yaxt='n')
par(new=TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='firebrick',lwd=2,axes=FALSE, xlab = NA, ylab = NA,ylim=c(-3,2.5))
axis(side = 4)
side4label <- expression(log(CH[4])~Residuals)
mtext(side = 4, line = 2.5, side4label,col='firebrick')
par(new=TRUE)
plot(as.Date(discrete$Date[which(is.na(discrete$ObsPOC)==FALSE)]),discrete$LogResidual[which(is.na(discrete$ObsPOC)==FALSE)],,axes=FALSE,xlab=NA,ylab=NA,ylim=c(-3,2.5),pch=20,col=c('firebrick'),cex=2)
obsch4resid <- expression(Observed~log(CH[4])~Residual)
legend('topleft',c('POC @ 3m', 'Observed POC',side4label,obsch4resid),lwd=c(2,NA,2,NA),col=c('black','black','firebrick','firebrick'),lty=c(1,NA,1,NA),pch=c(NA,20,NA,20))

quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(discrete$Date[which(is.na(discrete$ObsPOC)==FALSE)]),discrete$LogResidual[which(is.na(discrete$ObsPOC)==FALSE)],ylab=obsch4resid,xlab='Date',ylim=c(-3,2.5),pch=20,col=c('firebrick'),cex=2)
par(new=TRUE)
plot(as.Date(residual$Date),logch4_residual,type = 'l',col='firebrick',lwd=2,axes=FALSE, xlab = NA, ylab = NA,ylim=c(-3,2.5))
abline(0,0,col='slategrey',lty=2,lwd=2)
