#2016 Mendota Simulation in GLM3
#Script 1 of 3: Run GLM
#Developed 8/27/18 by JAH

#PURPOSE: Run GLM 3.0 for 2016 Season 

#NOTES:
  #To begin: adjust the SimDir file path to wherever your sim is located locally
  #This script also includes code to convert units for various sim vars
  


####1. Preparation####
RunSimulation = TRUE

library(glmtools)
library(GLMr)
library(dplyr)

#Julia's 2016 Sim
SimDir = '~/Dropbox/LaMe GLM Calibration/GLM3/16MendotaSim_Julia/'
SimFile = paste(SimDir,'Outputs/output.nc',sep="")



####2. Run the Model####
if (RunSimulation){
  # Run GLM
  setwd(SimDir)
  system2('glm+') # must be in the dir that has glm2.nml when this is called
}



####3. Convert Variables####
#Convert sim vars to useful units for comparison with obs data#

ConvertVariables = TRUE
if (ConvertVariables){
  convert_sim_var(SimFile, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  #convert_sim_var(SimFile, all_DOC = (OGM_doc + OGM_docr) * 12/1000, unit = 'mg/L',overwrite = T) #why doesn't this work anymore? OGM_docr not in GLM 3.0?
  convert_sim_var(SimFile, DOC = OGM_doc *12/1000, unit = 'mg/L', overwrite=T)
  convert_sim_var(SimFile, DIC = CAR_dic * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(SimFile, POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(SimFile, all_POC = ((OGM_poc + PHY_TPHYS) * 12/1000), unit = 'mg/L', overwrite=T)
  convert_sim_var(SimFile, log_CAR_ch4 = log10(CAR_ch4) , unit = 'umol/L', overwrite = T)
}


