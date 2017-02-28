####Cross Correlation Analysis####
#27 February 2017
#JAH

library(astsa)

poc<-ts(interp_spline_p)
methane<-ts(logch4_residual)

ccf_mod<-ccf(methane,poc,lag.max = 24)

quartz()
lag2.plot(methane,poc,max.lag = 24)


####plot two series with time lag of 18 days####
lag_date<-as.Date(residual$Date)-19

lag_ch4<-data.frame(lag_date,logch4_residual)
write.csv(lag_ch4,file = 'lag_ch4.csv',row.names = FALSE)
interp_poc<-data.frame(residual$Date,interp_spline_p)
write.csv(interp_poc,file = 'interp_poc.csv',row.names = FALSE)

lagged_ts<-read.csv('lagged_ts.csv')

quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(lagged_ts$lag_date),lagged_ts$interp_spline_p,type = 'l',lwd = 2,xlab = expression(Date),ylab = expression (POC~(mg~L^-1)))
par(new= TRUE)
plot(as.Date(lagged_ts$lag_date),lagged_ts$logch4_residual,type = 'l',col='red',lwd=2,axes=FALSE, xlab = NA, ylab = NA)
axis(side = 4)
mtext(side = 4, line = 2.5, 'log(CH4) Residuals (t-18)',col='red')
legend('topleft',c('POC @ 3m','log(CH4) Residuals (t-18)'),lwd=c(2,2),col=c('black','red'),lty=c(1,1))


quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
plot(lagged_ts$logch4_residual,lagged_ts$interp_spline_p,pch=16,ylab=expression(POC~(mg~L^-1)),xlab=expression(CH[4]~(mu*mol~L^-1)))
mod<-lm(lagged_ts$interp_spline_p~lagged_ts$logch4_residual)
abline(mod,col='red')
cor.test(lagged_ts$logch4_residual,lagged_ts$interp_spline_p)
