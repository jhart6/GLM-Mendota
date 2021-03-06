####Nitrogen Calibration#####

SimDir = '~/Dropbox/LaMe GLM Calibration/Nitrogen Calibration/Results/Experiment_2017-02-02_16_04_03/Sims/Sim1/Results/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') 

convert_sim_var(nc_file, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
NSpecies(SimFile, 3)
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
NSpecies(SimFile, 20)

plot_var(file=nc_file, 'TotN2', fig_path = FALSE)
plot_var_compare(nc_file = SimFile, obsTN, var_name = 'TotN2')

df <- resample_to_field(SimFile, obsTN, method = 'interp', precision = 'days', var_name = 'TotN2')
sqrt((sum((df$Modeled_TotN2-df$Observed_TotN2)^2, na.rm=TRUE))/nrow(df))


####Phosphorus Calibration#####
SimDir = '~/Dropbox/LaMe GLM Calibration/Nitrogen Calibration/Results/Experiment_2017-02-02_16_04_03/Sims/Sim1/Results/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') 

convert_sim_var(nc_file, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
PSpecies(SimFile, 3)
quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)
PSpecies(SimFile, 20)

plot_var(file=nc_file, 'TotP2', fig_path = FALSE)
plot_var_compare(nc_file = SimFile, obsTP, var_name = 'TotP2')

df <- resample_to_field(SimFile, obsTP, method = 'interp', precision = 'days', var_name = 'TotP2')
sqrt((sum((df$Modeled_TotP2-df$Observed_TotP2)^2, na.rm=TRUE))/nrow(df))

