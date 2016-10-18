#P.C. Hanson
# Script to compare simulation and observational data
library(dplyr)
library(glmtools)
library(GLMr)

# Switches
ConvertVariables = TRUE

SimDir = '~/Dropbox/Mendota Simulations/Sim4Julia/MECalibrated/'
SimFile = paste(SimDir,'/','Output.nc',sep = '')

# Run the model
run_glm(SimDir)

# Assuming the simulation run, now continue analyzing the results
# Create new variables with units we undersand
if (ConvertVariables){
  convert_sim_var(SimFile, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, DOC = OGM_doc * 12/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
}

# Low frequency observational files
ObsFilePhysical = "~/Dropbox/CNH-GLM/Mendota/LTERdata/Mendota_physical.csv"
ObsFilechlA = "~/Dropbox/CNH-GLM/Mendota/LTERdata/Mendota_chlA.csv"
ObsFileNutrients = "~/Dropbox/CNH-GLM/Mendota/LTERdata/Mendota_nutrients.csv"

# Read observational data
myPhysical = read.csv(ObsFilePhysical)
myTemp = myPhysical %>% filter(var == 'temp') %>% 
  select(datetime = sampledate,depth,temp = value) 
myDO = myPhysical %>% filter(var == 'OXY_oxy') %>% 
  select(datetime = sampledate,depth, DO = value) 
myNutrients = read.csv(ObsFileNutrients)
myTP = myNutrients %>% filter(var == 'TOT_tp') %>%
  select(datetime = sampledate,depth, TotP2 = value)
myTN = myNutrients %>% filter(var == 'TOT_tn') %>%
  select(datetime = sampledate,depth, TotN2 = value)

# myNutrients %>% filter(var == 'TOT_tp', depth==0) %>%
#   select(datetime = sampledate, depth, TotP2 = value)

# Write temporary obs file
tempTemp = paste(SimDir,'/','Temp.csv',sep="")
write.csv(myTemp,file = tempTemp,row.names = FALSE, quote = FALSE)
tempDO = paste(SimDir,'/','DO.csv',sep="")
write.csv(myDO,file = tempDO,row.names = FALSE, quote = FALSE)
tempTP = paste(SimDir,'/','TotP2.csv',sep="")
write.csv(myTP,file = tempTP,row.names = FALSE, quote = FALSE)
tempTN = paste(SimDir,'/','TotN2.csv',sep="")
write.csv(myTN,file = tempTN,row.names = FALSE, quote = FALSE)

# Use that to create two vectors on which I can run my own stats# 
myTempResampled = resample_to_field(SimFile,tempTemp, method = 'interp')
#myDOResampled = resample_to_field(SimFile,tempDO, method = 'interp')
#myTPResampled = resample_to_field(SimFile,tempTP, method = 'interp', precision = 'hours')

compare_to_field(SimFile, tempTemp, metric = 'thermo.depth', as_value = TRUE,
                 na.rm = TRUE, precision = 'days',method = 'interp')

# Plot comparisons of field and 
plot_var_compare(SimFile,tempTemp,var_name = 'temp')

rprofile_path = file.path(Sys.getenv("HOME"),
                          ".Rprofile")

# Now dissolved oxygen
plot_var_compare(SimFile,tempDO,var_name = 'DO',resample = TRUE, precision = 'hours', col_lim = c(0,15))

# And TP
plot_var_compare(SimFile,tempTP,var_name = 'TotP2',resample = TRUE,precision = 'hours',col_lim = c(0,0.3))

# And TN
plot_var_compare(SimFile,tempTN,var_name = 'TotN2',resample = TRUE,precision = 'hours',col_lim = c(0,2))

# And phytoplankton

plot(get_var(SimFile,var_name = 'PHY_NPR',reference = "surface",z_out=1),type='l')

Z = 2 # depth from which to grab the modeled time series
plot(get_var(SimFile,var_name = 'PHY_TPHYS',reference = "surface",z_out=Z),type='l')
lines(get_var(SimFile,var_name = 'PHY_DIATOMPCH4',reference = "surface",z_out=Z),type='l',col='brown')
lines(get_var(SimFile,var_name = 'PHY_CHLOROPCH3',reference = "surface",z_out=Z),type='l',col='green')
lines(get_var(SimFile,var_name = 'PHY_CYANONPCH2',reference = "surface",z_out=Z),type='l',col='red')
lines(get_var(SimFile,var_name = 'PHY_CYANOPCH1',reference = "surface",z_out=Z),type='l',col='blue')

