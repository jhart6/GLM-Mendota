#Use plot_field_obspanel function
  #original formatting for large plot
  #includes collim argument

setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')

png('obspoc.png',width=7,height=2.5,units='in',res=300)
plot_field_obspanel('field_poc.csv','POC','mg/L',collim=c(0,3))
dev.off()

png('obsch4.png',width=7,height=2.5,units='in',res=300)
plot_field_obspanel('field_log_ch4.csv','log(CH4)','umol/L',collim=c(-3,3),xlims=as.Date(c('2016-04-15','2016-11-14')))
dev.off()

png('obsco2.png',width=7,height=2.5,units='in',res=300)
plot_field_obspanel('field_log_co2.csv','log(CO2)','umol/L',xlims=as.Date(c('2016-04-15','2016-11-14')))
dev.off()
