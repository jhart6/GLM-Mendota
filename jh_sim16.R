#jh_sim16
#10/25/16

####Simulation Details####
#4/15/16 JAH starting conditions in NML file
#met data from 2009 calibration
#stream data from 2009 calibration
#compare to 2016 observed data
#model run from 4/15/16 through 10/2016

####Run GLM####
library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)

#establish working directory & output file
SimDir = '~/Dropbox/Mendota Simulations/Sim4Julia/MECalibrated_sim16/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'/','output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc')

#establish nml file
nml_file<-paste0(SimDir,"/glm2.nml") #designate the nml file
nml<-read_nml(nml_file = file.path(SimDir,'glm2.nml'))
print(nml)

#plot meteo data, still 2009 model data
quartz()
plot_meteo(nml_file)

#run GLM
run_glm(SimDir)
