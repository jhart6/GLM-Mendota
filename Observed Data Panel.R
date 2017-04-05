#observed data panel: POC, log(CH4), log(CO2)
#23 February 2017
#JAH

#need plot_field.R script for plot_field function
#Dropbox/Mendota Summer 16/R/plot_field for field data

setwd('~/Dropbox/Mendota Summer 16/R/plot_field/')

#correct labels (don't work yet)
gas_unit = expression(paste(mu*mol ~ L^{-1}))
log_methane = expression(paste('log' ~ CH[4]))
log_carbon_dioxide = expression(paste('log' ~ CO[2]))

#3-paned figure
png('Observed Data Panel.png', width = 6.5, height = 8, units = 'in', res = 500)
plot_field(c('field_poc.csv','field_log(CH4).csv','field_log(CO2).csv'),c('POC','log(CH4)','log(CO2)'),c('mg/L','umol/L','umol/L'),xlims = as.Date(c('2016-04-15','2016-11-15')))
dev.off()

#observed POC
png('Observed POC.png', width = 6.5, height = 3, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_field('field_poc.csv','POC','mg/L')
dev.off()

#observed CH4
png('Observed logCH4.png', width = 6.5, height = 3, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_field('field_log(CH4).csv','log(CH4)','umol/L',xlims = as.Date(c('2016-04-15','2016-11-15')))
dev.off()

#observed CO2
png('Observed logCO2.png', width = 6.5, height = 3, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_field('field_log(CO2).csv','log(CO2)','umol/L',xlims = as.Date(c('2016-04-15','2016-11-15')))
dev.off()
