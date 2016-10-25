#J.A. Hart
#Let's try to run 2009 and compare to 2016 observed data

####Run GLM####
#call necessary GLM packages
library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)

#where is the model on your computer & set working directory
SimDir = '~/Dropbox/Mendota Simulations/Sim4Julia/MECalibrated/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'/','output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') #designate an output file that you 
#can use to plot from the output.nc file later

nml_file<-paste0(SimDir,"/glm2.nml") #designate the nml file
nml<-read_nml(nml_file = file.path(SimDir,'glm2.nml')) #also designates the nml file
#read_nml reads the nml file into R where it can be edited within R
print(nml)

#plot meteo
quartz()
plot_meteo(nml_file)

#run GLM
run_glm(SimDir)




####Plot GLM Results####
#know your options of things to plot from output.nc
vars<-sim_vars('output.nc') #sim_vars = simulation variables
View(vars)

#plot 2009 simulation results
quartz()
plot_temp(file=nc_file, fig_path=FALSE) #standard plot temp function
plot_var(file=nc_file,c('temp','evap')) #to plot two vars at once
plot_var(file=nc_file,'DO',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'OGM_poc',fig_path=FALSE) #AED vars
plot_var(file=nc_file,'CAR_ch4',fig_path=FALSE) #AED vars



####Compare 2009 Sim to 2009 Obs####
if (ConvertVariables){
  convert_sim_var(nc_file, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, DOC = OGM_doc * 12/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(nc_file, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
}

#observed data files
ObsFilePhysical = "~/Dropbox/CNH-GLM/Mendota/LTERdata/Mendota_physical.csv"
ObsFilechlA = "~/Dropbox/CNH-GLM/Mendota/LTERdata/Mendota_chlA.csv"
ObsFileNutrients = "~/Dropbox/CNH-GLM/Mendota/LTERdata/Mendota_nutrients.csv"

#read those observed data csv's into R
myPhysical = read.csv(ObsFilePhysical) #temp and DO measurements
myTemp = myPhysical %>% filter(var == 'temp') %>%  #creates a dataframe with date, depth, and temp ONLY
  select(datetime = sampledate,depth,temp = value) 
myDO = myPhysical %>% filter(var == 'OXY_oxy') %>% 
  select(datetime = sampledate,depth, DO = value) 
myNutrients = read.csv(ObsFileNutrients)
myTP = myNutrients %>% filter(var == 'TOT_tp') %>%
  select(datetime = sampledate,depth, TotP2 = value)
myTN = myNutrients %>% filter(var == 'TOT_tn') %>%
  select(datetime = sampledate,depth, TotN2 = value)

# Write temporary obs file
tempTemp = paste(SimDir,'/','Temp.csv',sep="") #names the new non-existent CSV as an object to be referenced later
write.csv(myTemp,file = tempTemp,row.names = FALSE, quote = FALSE) #now uses that path to write the new CSV
tempDO = paste(SimDir,'/','DO.csv',sep="")
write.csv(myDO,file = tempDO,row.names = FALSE, quote = FALSE)
tempTP = paste(SimDir,'/','TotP2.csv',sep="")
write.csv(myTP,file = tempTP,row.names = FALSE, quote = FALSE)
tempTN = paste(SimDir,'/','TotN2.csv',sep="")
write.csv(myTN,file = tempTN,row.names = FALSE, quote = FALSE)

#comparing thermocline depth (modeled VS observed)
compare_to_field(SimFile, tempTemp, metric = 'thermo.depth', as_value = TRUE,
                 na.rm = TRUE, precision = 'days',method = 'interp')

#Plot comparisons
quartz()
plot_var_compare(SimFile,tempTemp,var_name = 'temp')
plot_var_compare(SimFile,tempDO,var_name = 'DO',resample = TRUE, precision = 'hours', col_lim = c(0,15))
plot_var_compare(SimFile,tempTP,var_name = 'TotP2',resample = TRUE,precision = 'hours',col_lim = c(0,0.3))
plot_var_compare(SimFile,tempTN,var_name = 'TotN2',resample = TRUE,precision = 'hours',col_lim = c(0,2))

#plot variables that aren't heatmaps
plot(get_var(SimFile,var_name = 'PHY_NPR',reference = "surface",z_out=1),type='l')
plot(get_var(SimFile,var_name = 'CAR_atm_ch4_exch', reference = 'surface'),type='l')

#modeled phytoplankton succession
Z = 2 # depth from which to grab the modeled time series
plot(get_var(SimFile,var_name = 'PHY_TPHYS',reference = "surface",z_out=Z),type='l')
lines(get_var(SimFile,var_name = 'PHY_DIATOMPCH4',reference = "surface",z_out=Z),type='l',col='brown')
lines(get_var(SimFile,var_name = 'PHY_CHLOROPCH3',reference = "surface",z_out=Z),type='l',col='green')
lines(get_var(SimFile,var_name = 'PHY_CYANONPCH2',reference = "surface",z_out=Z),type='l',col='red')
lines(get_var(SimFile,var_name = 'PHY_CYANOPCH1',reference = "surface",z_out=Z),type='l',col='blue')


####Compare 2009 Sim to 2016 Obs####
#import 2016 observational data
#2016 obs data from 4/2016 through 7/2016
temp16<-read.csv('obs_temp16.csv')
juliaTEMP<-paste(SimDir,'/','juliaTEMP.csv',sep="")
write.csv(temp16,file = juliaTEMP,row.names = FALSE, quote = FALSE)

do16<-read.csv('obs_DO16.csv')
juliaDO<-paste(SimDir,'/','juliaDO.csv',sep ="")
write.csv(do16,file = juliaDO, row.names = FALSE, quote = FALSE)

#compare 2009 modeled TEMP to 2016 obs TEMP
quartz()
plot_temp_compare(nc_file = SimFile, juliaTEMP) #changed all 2016 observed dates to 2009 (but really 2016 data)

#compare 2009 modeled DO to 2016 obs DO
quartz()
plot_var_compare(nc_file = SimFile,juliaDO,var_name = 'DO')

                 
                 