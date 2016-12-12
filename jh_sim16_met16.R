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

#run GLM
#START: 2016-04-15 00:00:00
#END: 2016-11-11 23:00:00
run_glm(SimDir)
