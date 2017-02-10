#jh_sim16_met16
#11/22/16

####Simulation Details####
#4/15/16 JAH starting conditions in NML file
#met data from 2016 (1/1/10 - 11/11/16)
#stream data from 2016  
#compare to 2016 observed data
#model run from 4/15/16 through 11/11/2016 

####GLM Prep####
library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)
    
#where is the model on your computer & set working directory
SimDir = '~/Dropbox/LaMe GLM Calibration/PCH OC Fixes/Results/Experiment_2017-02-10_12_45_45/Sims/Sim1/Results/'

setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') #designate an output file that you 
#can use to plot from the output.nc file later

#Fix the time columns in the inflow files after opening in excel to check column names
#run only once! do NOT ever run again or else will populate DateTime column with NA's
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

#####Run GLM#####
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
  convert_sim_var(nc_file, all_DOC = (OGM_doc + OGM_docr) * 12/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, DOC = OGM_doc *12/1000, unit = 'mg/L', overwrite=T)
  convert_sim_var(nc_file, POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(nc_file, DIC = CAR_dic * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(nc_file, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, log_CAR_ch4 = log10(CAR_ch4) , unit = 'umol/L', overwrite = T)
  convert_sim_var(nc_file, CH4 = CAR_ch4, unit = 'mg/L', overwrite=T)
  convert_sim_var(nc_file, TOT_POC = ((OGM_poc + PHY_TPHYS) * 12/1000), unit = 'mg/L', overwrite=T)
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
plot_var(file=nc_file, 'DIC', fig_path=FALSE)
plot_var(file=nc_file,'CAR_pCO2',fig_path=FALSE,col_lim=c(0,3))
plot_var(file=nc_file, 'TotP2', fig_path = FALSE)
plot_var(file=nc_file, 'TotN2', fig_path = FALSE)
plot_var(SimFile, var_name = 'PHY_TPHYS')

plot_var(SimFile, c('PHY_TPHYS','DOC'))

####Compare 16 Sim to 16 Obs####
#import 2016 observational data
#2016 obs data from 4/15/2016 through 11/14/2016 
setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")
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

dic<-read.csv("field_dic.csv",header=TRUE)
obsDIC<-paste(SimDir, 'obsDIC.csv', sep='')
write.csv(dic, file=obsDIC, row.names=FALSE, quote=FALSE)

logmethane<-read.csv("field_log_ch4.csv",header = TRUE)
obsLOGCH4<-paste(SimDir, 'obsLOGCH4.csv', sep = '')
write.csv(logmethane, file=obsLOGCH4, row.names=FALSE, quote=FALSE)

#logcarbondioxide<- read.csv("field_log_co2.csv",header=TRUE)
#obsLOGCO2 <- paste(SimDir, 'obsLOGCO2', sep='')
#write.csv(logcarbondioxide, file=obsLOGCO2, row.names = FALSE, quote = FALSE)

totN2<-read.csv("TotN2.csv",header=TRUE)
obsTN<-paste(SimDir, 'obsTN.csv', sep='')
write.csv(totN2, file=obsTN, row.names = FALSE, quote=FALSE)

totP2<-read.csv("TotP2.csv",header=TRUE)
obsTP<-paste(SimDir, 'obsTP.csv', sep='')
write.csv(totP2, file=obsTP, row.names = FALSE, quote=FALSE)

#####compare 2016 modeled TEMP to 2016 obs TEMP####
quartz()
plot_temp_compare(nc_file = SimFile, obsTEMP)
plot_var_compare(nc_file = SimFile, obsDO, var_name = 'DO')
plot_var_compare(nc_file = SimFile, obsPH, var_name = 'CAR_pH')
plot_var_compare(nc_file = SimFile, obsPOC, var_name='TOT_POC')
plot_var_compare(nc_file = SimFile, obsCH4, var_name='CAR_ch4')
plot_var_compare(nc_file = SimFile, obsCO2, var_name = 'CAR_pCO2')
plot_var_compare(nc_file = SimFile, obsDOC, var_name = 'DOC')
plot_var_compare(nc_file = SimFile, obsDIC, var_name = 'DIC')
plot_var_compare(nc_file = SimFile, obsLOGCH4, var_name = 'log_CAR_ch4')
plot_var_compare(nc_file = SimFile, obsTN, var_name = 'TotN2',col=c(0,15))
plot_var_compare(nc_file = SimFile, obsTP, var_name = 'TotP2',col=c(0,1))



####WATER BALANCE CALIBRATION CHECK####
####Plot Water Balance####
plot(get_var(SimFile, var_name='evap'),type='l')
plot(get_surface_height(SimFile),type='l')


####WATER TEMPERATURE CALIBRATION####
####Starting RMSE Analysis####
#in compare_to_field, as_value=F will return the RMSE for the specific metric
#for water.temperature, <1.5 is good
#for thermo.depth, <2.5 is good

###Thermocline Depth Comparison###
#value comparison
compare_to_field(SimFile, obsTEMP, metric = 'thermo.depth',as_value=TRUE, na.rm = TRUE, precision = 'days', method = 'interp')
#rmse 
compare_to_field(SimFile, obsTEMP, metric = 'thermo.depth', as_value=FALSE, na.rm = TRUE, precision = 'days', method = 'interp')

therm_depths <- compare_to_field(SimFile, obsTEMP, metric="thermo.depth", as_value=TRUE, na.rm=TRUE)
plot(therm_depths$DateTime, therm_depths$obs, type="l", col="blue", ylim=c(0,32), ylab="Thermocline depth in meters")  
lines(therm_depths$DateTime, therm_depths$mod, col="red") 
legend("topright",c("Observed", "Modeled"),lty=c(1,1), col=c("blue", "red")) 

###Water Temperature Comparison###
#value comparison
compare_to_field(SimFile, obsTEMP, metric = 'water.temperature', as_value=TRUE, na.rm = TRUE, precision = 'days', method = 'interp')
#rmse
compare_to_field(SimFile, obsTEMP, metric = 'water.temperature', as_value=FALSE, na.rm = TRUE, precision = 'days', method = 'interp')

temps<-compare_to_field(SimFile, obsTEMP, metric = 'water.temperature', as_value=TRUE, na.rm = TRUE, precision = 'days', method = 'interp')
plot(temps$DateTime, temps$obs, type='l', col='blue')
lines(temps$DateTime, temps$mod, type='l', col = 'red')
legend("topright",c("Observed", "Modeled"),lty=c(1,1), col=c("blue", "red")) 

plot(temps$obs,temps$mod,pch=16)
abline(0,1,col='red')

#####OXYGEN CALIBRATION####
#how to get RMSE for metrics that aren't available through sim_metrics or rLakeAnalyzer
#use resample_to_field
#or rename things to "temp" so you can cheat/force the sim_metrics to work
convert_sim_var(nc_file, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
plot_var_compare(nc_file = SimFile, obsDO, var_name = 'DO')

#value comparison
resample_to_field(SimFile, obsDO, method = 'interp', precision = 'days',var_name = 'DO')
#rmse
df <- resample_to_field(SimFile, obsDO, method = 'interp', precision = 'days',var_name = 'DO')
sqrt((sum((df$Modeled_DO-df$Observed_DO)^2, na.rm=TRUE))/nrow(df))


#####NITROGEN CALIBRATION#####
df <- resample_to_field(SimFile, obsTN, method = 'interp', precision = 'days', var_name = 'TotN2')
sqrt((sum((df$Modeled_TotN2-df$Observed_TotN2)^2, na.rm=TRUE))/nrow(df))


#####PHOSPHORUS CALIBRATION####
df <- resample_to_field(SimFile, obsTP, method = 'interp', precision = 'days', var_name = 'TotP2')
sqrt((sum((df$Modeled_TotP2-df$Observed_TotP2)^2, na.rm=TRUE))/nrow(df))


#####DOC CALIBRATION####
df <- resample_to_field(SimFile, obsDOC, method='interp', precision = 'days', var_name = 'DOC')
sqrt((sum((df$Modeled_DOC-df$Observed_DOC)^2, na.rm=TRUE))/nrow(df))


####DIC CALIBRATION####
df <- resample_to_field(SimFile, obsDIC, method='interp', precision = 'days', var_name = 'DIC')
sqrt((sum((df$Modeled_DIC-df$Observed_DIC)^2, na.rm=TRUE))/nrow(df))


#####POC CALIBRATION####
df <- resample_to_field(SimFile, obsPOC, method = 'interp', precision = 'days', var_name = 'TOT_POC')
sqrt((sum((df$Modeled_TOT_POC-df$Observed_TOT_POC)^2, na.rm=TRUE))/nrow(df))


####LOG_CH4 CALIBRATION####
df <- resample_to_field(SimFile, obsLOGCH4, method = 'interp', precision = 'days', var_name = 'log_CAR_ch4')
sqrt((sum((df$Modeled_log_CAR_ch4-df$Observed_log_CAR_ch4)^2, na.rm=TRUE))/nrow(df))


####PH CALIBRATION####
df <- resample_to_field(SimFile, obsPH, method = 'interp', precision = 'days', var_name = 'CAR_pH')
sqrt((sum((df$Modeled_CAR_pH-df$Observed_CAR_pH)^2, na.rm=TRUE))/nrow(df))


#####Extract Modeled Secchi#####
df<-get_var(SimFile, 'extc_coef', z_out= 2.5,reference = 'surface')
secchi<-(1.7/df$extc_coef_2.5)
df<-cbind(df,secchi)
plot(df$DateTime,df$secchi,type='l')






