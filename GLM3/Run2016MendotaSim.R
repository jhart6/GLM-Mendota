#2016 Mendota Simulation in GLM3
#Run GLM
#Compare model output to observed data visually
#Compare model output to observed data quantitatively


####Preparation####
PrintPlots = FALSE
RunSimulation = TRUE
PlotPhytosIntegrated = TRUE

library(glmtools)
library(GLMr)
library(dplyr)

SimDir = '~/Dropbox/LaMe GLM Calibration/GLM3/V30Mendota25_Jul_2018/'
SimDir = '~/Dropbox/LaMe GLM Calibration/GLM3/16MendotaSim_Julia/'
SimFile = paste(SimDir,'Outputs/output.nc',sep="")


####Run the Model####
if (RunSimulation){
  # Run GLM
  setwd(SimDir)
  system2('glm+') # must be in the dir that has glm2.nml when this is called
}

plot_temp(SimFile)


