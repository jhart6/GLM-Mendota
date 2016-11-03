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
