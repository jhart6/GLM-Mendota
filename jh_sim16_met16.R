#jh_sim16_met16
#11/22/16

####Simulation Details####
#4/15/16 JAH starting conditions in NML file
#met data from 2016 (1/1/10 - 11/11/16)
#stream data from 2016  
#compare to 2016 observed data
#model run from 4/15/16 through 11/11/2016 

####Run GLM####
library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)

#where is the model on your computer & set working directory
#SimDir = '~/Dropbox/Mendota Simulations/Sim4Julia/MECalibrated_sim16/'
#SimDir = '~/Dropbox/LaMe GLM Calibration/MECalibrated_sim16/'
SimDir = '~/Dropbox/LaMe GLM Calibration/LaMe New Params/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') #designate an output file that you 
#can use to plot from the output.nc file later

#Fix the time columns in the inflow files after opening in excel to check column names
yahara <- read.csv("Mendota_yahara.csv", header=TRUE)
yahara$Time <-as.POSIXct(strptime(yahara$Time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(yahara, "Mendota_yahara.csv", row.names=FALSE, quote=FALSE)

pheasant <- read.csv("Mendota_pheasant.csv", header=TRUE)
pheasant$Time <-as.POSIXct(strptime(pheasant$Time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(pheasant, "Mendota_pheasant.csv", row.names=FALSE, quote=FALSE)

dorn <- read.csv("Mendota_dorn.csv", header=TRUE)
dorn$Time <-as.POSIXct(strptime(dorn$Time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(dorn, "Mendota_dorn.csv", row.names=FALSE, quote=FALSE)

sixmile <- read.csv("Mendota_sixmile.csv", header=TRUE)
sixmile$Time <-as.POSIXct(strptime(sixmile$Time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(sixmile, "Mendota_sixmile.csv", row.names=FALSE, quote=FALSE)

outflow<-read.csv("Mendota_outflow.csv",header=TRUE)
outflow$Time <- as.POSIXct(strptime(outflow$Time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(outflow, "Mendota_outflow.csv", row.names=FALSE, quote=FALSE)

met<-read.csv("NLDAS2_Mendota_2010_2016_cell_5.csv",header=TRUE)
met$time <- as.POSIXct(strptime(met$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(met,"NLDAS2_Mendota_2010_2016_cell_5.csv",row.names=FALSE, quote=FALSE)

#run GLM
#START: 2016-04-15 00:00:00
#END: 2016-11-11 23:00:00
run_glm(SimDir)


####Plot GLM Results####
#know your options of things to plot from output.nc
vars<-sim_vars('output.nc') #sim_vars = simulation variables
View(vars)

#Convert Variables
ConvertVariables = TRUE

#Need to write some more of these
if (ConvertVariables){
  convert_sim_var(nc_file, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, DOC = OGM_doc * 12/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(nc_file, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, log_CAR_ch4 = log10(CAR_ch4) , unit = 'umol/L', overwrite = T)
  #needs to be in atm! #convert_sim_var(nc_file, log_CAR_pCO2 = log10(CAR_pCO2), unit = 'umol/L', overwrite = T)
}

#plot 2016 simulation results
quartz()
plot_temp(file=nc_file, fig_path=FALSE) #standard plot temp function
plot_var(file=nc_file,c('temp','evap')) #to plot two vars at once
plot_var(file=nc_file,'DO',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'CAR_pH',fig_path = FALSE)
plot_var(file=nc_file,'POC',fig_path=FALSE,col_lim = c(0,3)) #AED vars
plot_var(file=nc_file, 'CAR_ch4',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'DOC',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'CAR_pCO2',fig_path=FALSE,col_lim=c(0,3))


####Compare 16 Sim to 16 Obs####
#import 2016 observational data
#2016 obs data from 4/15/2016 through 11/14/2016 
temp<-read.csv("field_temp.csv",header=TRUE) #creates R object with obs data
obsTEMP<-paste(SimDir, 'obsTEMP.csv',sep='') #creates file path for eventual obs data
write.csv(temp,file = obsTEMP, row.names=FALSE, quote=FALSE)

DO<-read.csv("field_do.csv",header=TRUE)
obsDO<-paste(SimDir,'obsDO.csv',sep='')
write.csv(DO, file=obsDO, row.names=FALSE, quote=FALSE)

ph<-read.csv("field_ph.csv",header=TRUE)
obsPH<-paste(SimDir, 'obsPH.csv',sep='')
write.csv(ph, file=obsPH, row.names=FALSE, quote=FALSE)

poc<-read.csv("field_poc.csv",header=TRUE)
obsPOC<-paste(SimDir, 'obsPOC.csv', sep='')
write.csv(poc, file=obsPOC, row.names=FALSE, quote=FALSE)

methane<-read.csv("field_ch4.csv",header=TRUE)
obsCH4<-paste(SimDir, 'obsCH4.csv', sep='')
write.csv(methane, file=obsCH4, row.names=FALSE, quote=FALSE)

carbondioxide<-read.csv("field_co2.csv",header=TRUE)
obsCO2<-paste(SimDir, 'obsCO2.csv', sep='')
write.csv(carbondioxide, file=obsCO2, row.names=FALSE, quote=FALSE)

doc<-read.csv("field_doc.csv",header=TRUE)
obsDOC<-paste(SimDir, 'obsDOC.csv', sep='')
write.csv(doc, file=obsDOC, row.names=FALSE, quote=FALSE)

#logmethane<-read.csv("field_log_ch4.csv",header = TRUE)
#obsLOGCH4<-paste(SimDir, 'obsLOGCH4.csv', sep = '')
#write.csv(logmethane, file=obsLOGCH4, row.names=FALSE, quote=FALSE)

#logcarbondioxide<- read.csv("field_log_co2.csv",header=TRUE)
#obsLOGCO2 <- paste(SimDir, 'obsLOGCO2', sep='')
#write.csv(logcarbondioxide, file=obsLOGCO2, row.names = FALSE, quote = FALSE)

#compare 2016 modeled TEMP to 2016 obs TEMP
quartz()
plot_temp_compare(nc_file = SimFile, obsTEMP)
plot_var_compare(nc_file = SimFile, obsDO, var_name = 'DO')
plot_var_compare(nc_file = SimFile, obsPH, var_name = 'CAR_pH')
plot_var_compare(nc_file = SimFile, obsPOC, var_name='POC',col_lim = c(0,3))
plot_var_compare(nc_file = SimFile, obsCH4, var_name='CAR_ch4')
plot_var_compare(nc_file = SimFile, obsCO2, var_name = 'CAR_pCO2')
plot_var_compare(nc_file = SimFile, obsDOC, var_name = 'DOC')

#plot_var_compare(nc_file = SimFile, obsLOGCH4, var_name = 'log_CAR_ch4')
#plot_var_compare(nc_file = SimFile, obsLOGCO2, var_name = 'log_CAR_pCO2')


####Plot Water Balance####
plot(get_var(SimFile, var_name='evap'),type='l')
plot(get_surface_height(SimFile),type='l')

#in compare_to_field, as_value=F will return the RMSE for the specific metric
  #for water.temperature, <1.5 is good
  #for thermo.depth, <2.5 is good


####Starting RMSE Analysis####
#value comparison
compare_to_field(SimFile, obsTEMP, metric = 'thermo.depth',as_value=TRUE, na.rm = TRUE, precision = 'days', method = 'interp')
#rmse 
compare_to_field(SimFile, obsTEMP, metric = 'thermo.depth', as_value=FALSE, na.rm = TRUE, precision = 'days', method = 'interp')
#value comparison
compare_to_field(SimFile, obsTEMP, metric = 'water.temperature', as_value=TRUE, na.rm = TRUE, precision = 'days', method = 'interp')
#rmse
compare_to_field(SimFile, obsTEMP, metric = 'water.temperature', as_value=FALSE, na.rm = TRUE, precision = 'days', method = 'interp')



#how to get RMSE for metrics that aren't available through sim_metrics or rLakeAnalyzer
#use resample_to_field
#or rename things to "temp" so you can cheat/force the sim_metrics to work

#to keep track of nml changes - use this function to override nml values & comment what you've tried
set_nml()


plot(get_var(SimFile, var_name = 'TOT_tp',z_out=1),type='l')

plot_var(SimFile, var_name = 'PHY_TPHYS')
plot_var(SimFile, var_name = 'TOT_tp')
