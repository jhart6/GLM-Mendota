#jh_sim16
#10/25/16

####Simulation Details####
#4/15/16 JAH starting conditions in NML file
#met data from 2009 calibration
#stream data from 2009 calibration
#compare to 2016 observed data
#model run from 4/15/16 through 10/2016 (well, really 2009)

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

#Code producing some sort of weird error - don't even need to run the model
#nml_file<-paste0(SimDir,"/glm2.nml") #designate the nml file
#nml<-read_nml(nml_file = file.path(SimDir,'glm2.nml')) #also designates the nml file
#read_nml reads the nml file into R where it can be edited within R
#print(nml)


#plot meteo data, still 2009 model data - this doesn't work now, bc the nml_file lines didn't work
#quartz()
#plot_meteo(nml_file)

#Fix the time columns in the inflow files after opening in excel to check column names
yahara <- read.csv("Mendota_yahara.csv", header=TRUE)
yahara$time <-as.POSIXct(strptime(yahara$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(yahara, "Mendota_yahara.csv", row.names=FALSE, quote=FALSE)

pheasant <- read.csv("Mendota_pheasant.csv", header=TRUE)
pheasant$time <-as.POSIXct(strptime(pheasant$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(pheasant, "Mendota_pheasant.csv", row.names=FALSE, quote=FALSE)

springharbor <- read.csv("Mendota_springharbor.csv", header=TRUE)
springharbor$time <-as.POSIXct(strptime(springharbor$time, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(springharbor, "Mendota_springharbor.csv", row.names=FALSE, quote=FALSE)

#run GLM
#START: 2009-04-15 00:00:00
#END: 2009-11-01 23:00:00
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
  convert_sim_var(nc_file,POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(nc_file, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
}

#plot 2016 simulation results
quartz()
plot_temp(file=nc_file, fig_path=FALSE) #standard plot temp function
plot_var(file=nc_file,c('temp','evap')) #to plot two vars at once
plot_var(file=nc_file,'DO',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'POC',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'CAR_ch4',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'DOC',fig_path=FALSE) #AED vars



####Compare 16 Sim to 16 Obs####
#import 2016 observational data
#2016 obs data from 4/2016 through 11/1/16 ##NEED TO UPDATE
temp16<-read.csv('obs_temp16.csv')
juliaTEMP<-paste(SimDir,'/','juliaTEMP.csv',sep="")
write.csv(temp16,file = juliaTEMP,row.names = FALSE, quote = FALSE)

do16<-read.csv('obs_DO16.csv')
juliaDO<-paste(SimDir,'/','juliaDO.csv',sep ="")
write.csv(do16,file = juliaDO, row.names = FALSE, quote = FALSE)

poc16<-read.csv('obs_POC16.csv')
juliaPOC<-paste(SimDir,'/','juliaPOC.csv',sep="")
write.csv(poc16,file = juliaPOC, row.names=FALSE, quote=FALSE)

ch416<-read.csv('obs_CH416.csv')
juliaCH4<-paste(SimDir, '/', 'juliaCH4.csv',sep="")
write.csv(ch416,file=juliaCH4, row.names=FALSE, quote=FALSE)


methane <- read.csv("juliaCH4.csv", header=TRUE)
methane$datetime <-as.POSIXct(strptime(methane$datetime, "%Y-%m-%d %H:%M:%S", tz="EST"))
write.csv(methane, "juliaCH4.csv", row.names=FALSE, quote=FALSE)


#compare 2016 modeled TEMP to 2016 obs TEMP
quartz()
plot_temp_compare(nc_file = SimFile, juliaTEMP) #changed all 2016 observed dates to 2009 (but really 2016 data)

#compare 2016 modeled DO to 2016 obs DO
quartz()
plot_var_compare(nc_file = SimFile,juliaDO,var_name = 'DO')

#compare 2016 modeled POC to 2016 obs POC
quartz()
plot_var_compare(nc_file = SimFile, juliaPOC, var_name='POC')

#compare 2016 modeled CH4 to 2016 obs CH4
quartz()
plot_var_compare(nc_file = SimFile, juliaCH4, var_name='CAR_ch4')
