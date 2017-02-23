#Calibration Verification Figure
#23 February 2017
#JAH

#plot_var_compare from sim output (Greedy)
setwd('~/Dropbox/Masters Writing/Figures/Calibration Figures')

png('temp calibration.png', width = 6, height = 6, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_temp_compare(nc_file = SimFile, obsTEMP)
dev.off()

png('oxygen calibration.png', width = 6, height = 6, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_var_compare(nc_file = SimFile, obsDO, var_name = 'DO')
dev.off()

png('TN calibration.png', width = 6, height = 6, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_var_compare(nc_file = SimFile, obsTN, var_name = 'TotN2',col=c(0,15))
dev.off()

png('TP calibration.png', width = 6, height = 6, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_var_compare(nc_file = SimFile, obsTP, var_name = 'TotP2',col=c(0,1))
dev.off()

png('POC calibration.png', width = 6, height = 6, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_var_compare(nc_file = SimFile, obsPOC, var_name='TOT_POC',col=c(0,5))
dev.off()

png('DOC calibration.png', width = 6, height = 6, units = 'in', res = 500)
par(mar=c(2,4,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot_var_compare(nc_file = SimFile, obsALLDOC, var_name = 'all_DOC',col=c(4,7.5))
dev.off()


