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
SimDir = '~/Dropbox/Mendota Simulations/Sim4Julia/MECalibrated_sim16/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'/','output.nc',sep = '') 
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
  #convert_sim_var(nc_file, Methane = log(CAR_ch4) , unit = 'umol/L', overwrite = T)
}



#plot 2016 simulation results
quartz()
plot_temp(file=nc_file, fig_path=FALSE) #standard plot temp function
plot_var(file=nc_file,c('temp','evap')) #to plot two vars at once
plot_var(file=nc_file,'DO',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'POC',fig_path=FALSE,col_lim = c(0,3)) #AED vars
plot_var(file=nc_file, 'CAR_ch4',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'DOC',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'CAR_pCO2',fig_path=FALSE,col_lim=c(0,3))





####Compare 16 Sim to 16 Obs####
#import 2016 observational data
#2016 obs data from 4/15/2016 through 11/14/2016 
obsTEMP<-read.csv("field_temp.csv",header=TRUE)
colnames(obsTEMP)<-c('datetime','depth','temp')
obsTEMP$datetime<-as.POSIXct(strptime(obsTEMP$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(obsTEMP, 'obsTEMP.csv', row.names=FALSE, quote=FALSE)

obsDO<-read.csv("field_do.csv",header=TRUE)
obsPH<-read.csv("field_ph.csv",header=TRUE)
obsCH4<-read.csv("field_ch4.csv",header=TRUE)
obsCO2<-read.csv("field_co2.csv",header=TRUE)
obsPOC<-read.csv("field_poc.csv",header=TRUE)



#compare 2016 modeled TEMP to 2016 obs TEMP
quartz()
obsTEMP<-read.csv('obsTEMP.csv')
plot_temp_compare(nc_file = SimFile, obsTEMP)



#compare 2016 modeled DO to 2016 obs DO
quartz()
plot_var_compare(nc_file = SimFile,juliaDO,var_name = 'DO')

#compare 2016 modeled POC to 2016 obs POC
quartz()
plot_var_compare(nc_file = SimFile, juliaPOC, var_name='POC',col_lim = c(0,3))

#compare 2016 modeled CH4 to 2016 obs CH4
quartz()
plot_var_compare(nc_file = SimFile, juliaCH4, var_name='CAR_ch4')

