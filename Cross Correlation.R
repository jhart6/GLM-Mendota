####Cross Correlation Analysis####
#27 February 2017
#JAH

library(astsa)

# poc<-ts(interp_spline_p)
# methane<-ts(logch4_residual)

poc<-ts(residuals$InterpObsPOC)
methane<-ts(residuals$LogResidual)

ccf_mod<-ccf(methane,poc,lag.max = 24)

quartz()
lag2.plot(methane,poc,max.lag = 24) #now max negative correlation is with 22 day time lag

