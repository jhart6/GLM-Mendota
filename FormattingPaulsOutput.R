#Script to Convert Paul's Output.nc files e.g. convert_sim_var
#modified from top part of jh_sim16_met16.R, but specific to needs for second MS submission
#should be able to just "source" this script to format all data

library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)

#designate output.nc file as SimFile
SimDir = '~/Dropbox/Mendota Simulations/2017DecJulia/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output_2017_Dec_14.nc',sep = '') 


####CONVERT VARS####
ConvertVariables = TRUE

if (ConvertVariables){
  convert_sim_var(SimFile, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, all_DOC = (OGM_doc + OGM_docr) * 12/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, DOC = OGM_doc *12/1000, unit = 'mg/L', overwrite=T)
  convert_sim_var(SimFile, POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(SimFile, DIC = CAR_dic * 12/1000, unit = 'mg/L', overwrite = T)
  convert_sim_var(SimFile, TotP2 = TOT_tp * 30.97/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, TotN2 = TOT_tn * 14/1000, unit = 'mg/L',overwrite = T)
  convert_sim_var(SimFile, log_CAR_ch4 = log10(CAR_ch4) , unit = 'umol/L', overwrite = T)
  convert_sim_var(SimFile, TOT_POC = ((OGM_poc + PHY_TPHYS) * 12/1000), unit = 'mg/L', overwrite=T)
}


####Option to Plot Vars####
# plot_temp(file=SimFile, fig_path=FALSE)
# plot_var(file=SimFile,c('temp','evap'))
# plot_var(file=SimFile,'DO',fig_path=FALSE)
# plot_var(file=SimFile,'CAR_pH',fig_path = FALSE)
# plot_var(file=SimFile,'POC',fig_path=FALSE,col_lim = c(0,3))
# plot_var(file=SimFile, 'CAR_ch4',fig_path=FALSE)
# plot_var(file=SimFile,'all_DOC',fig_path=FALSE)
# plot_var(file=SimFile, 'DIC', fig_path=FALSE)
# plot_var(file=SimFile, 'TotP2', fig_path = FALSE)
# plot_var(file=SimFile, 'TotN2', fig_path = FALSE)
# plot_var(SimFile, var_name = 'PHY_TPHYS')