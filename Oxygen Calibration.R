#Oxygen Calibration

SimDir = '~/Dropbox/LaMe GLM Calibration/Oxygen Calibration/Results/Experiment_2017-02-01_13_47_25/Sims/Sim1/Results/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') 

convert_sim_var(nc_file, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)

plot_var_compare(nc_file = SimFile, obsDO, var_name = 'DO')

df <- resample_to_field(SimFile, obsDO, method = 'interp', precision = 'days',var_name = 'DO')
sqrt((sum((df$Modeled_DO-df$Observed_DO)^2, na.rm=TRUE))/nrow(df))
