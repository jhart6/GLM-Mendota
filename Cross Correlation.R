####Cross Correlation Analysis####
#27 February 2017
#JAH

poc<-ts(interp_spline_p)
methane<-ts(logch4_residual)

ccf_mod<-ccf(methane,poc)

quartz()
lag2.plot(methane,poc,max.lag = 8)
