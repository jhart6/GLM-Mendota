#2016 Mendota Simulation in GLM3
#Script 2 of 3: Visual Comparison of Observed and Simulated Vars
#Developed 8/27/18 by JAH

#PURPOSE: Visually compare observed and simulated 2016 Lake Mendota data
  # Source this script to generate a PDF of all calibration plots at once
  # That exported PDF will be automatically versioned with the Date/Time 
  # when you run the function.

#NOTES:
  #To begin, either 1) source the script to run the model 
    # (which establishes wd as SimDir) OR 2) setwd to output file/SimFile location
    # Comment out the line you don't use
  #This script can generate comparison plots for individual vars.
    # Those lines of code are below, commented out for sourcing purposes.



####1. Run GLM####
source("~/Dropbox/GitHub Repos/GLM-Mendota/GLM3/To Run & Calibrate 2016 Sim/Run2016MendotaSim.R")



####2. Call Observed Data Files####
#tell R where to find the folder of observed data CSV's
ObsDataDir <- '~/Dropbox/LaMe GLM Calibration/GLM3/16MendotaSim_Julia/Observed Data/'

#Secchi Calculations
obs.secchi<-read.csv(paste(ObsDataDir,"secchi.csv",sep=""))
ext.coef<-get_var(SimFile, 'extc_coef', z_out= 2.5,reference = 'surface')
mod.secchi<-(1.7/ext.coef$extc_coef_2.5)


####3.Visualize Invidual Vars####
#Individual Variables (if you want to work on a specific variable)
# plot_temp_compare(SimFile,paste(ObsDataDir,"field_temp.csv",sep=""),fig_path=FALSE)
# plot_var_compare(SimFile, paste(ObsDataDir,"field_do.csv",sep=""), var_name = 'DO')
# plot_var_compare(SimFile, paste(ObsDataDir,"TotN2.csv",sep=""), var_name = 'TotN2')
# plot_var_compare(SimFile, paste(ObsDataDir,"TotP2.csv",sep=""), var_name = 'TotP2')
# plot_var_compare(SimFile, paste(ObsDataDir,"field_doc.csv",sep=""), var_name = 'DOC')
# plot_var_compare(SimFile, paste(ObsDataDir,"field_dic.csv",sep=""), var_name = 'DIC')
# plot_var_compare(SimFile, paste(ObsDataDir,"field_all_poc.csv",sep=""), var_name = 'all_POC')
# plot_var_compare(SimFile, paste(ObsDataDir,"field_log_ch4.csv",sep=""), var_name = 'log_CAR_ch4')

#Secchi
# plot(as.Date(ext.coef$DateTime),mod.secchi,type='l',ylim=rev(c(0,7)),xlab="Date",ylab="Secchi Depth (m)")
# points(as.Date(obs.secchi$Date),obs.secchi$Depth,pch=16,col='red')
# legend('bottomright',c("Modeled","Observed"),pch=c(NA,16),col=c('black','red'),lty=c(1,NA))



####4. Generate all comparison plots at once####
generate_all_plots<-function(PDFName){
  pdfpath = paste(SimDir,"CalibrationChecks/",sep="")
  pdf(file=paste(pdfpath,PDFName,sep=""),width=8.5,height=11)
  plot_temp_compare(SimFile,paste(ObsDataDir,"field_temp.csv",sep=""),fig_path=FALSE)
  plot_var_compare(SimFile, paste(ObsDataDir,"field_do.csv",sep=""), var_name = 'DO')
  plot_var_compare(SimFile, paste(ObsDataDir,"TotN2.csv",sep=""), var_name = 'TotN2')
  plot_var_compare(SimFile, paste(ObsDataDir,"TotP2.csv",sep=""), var_name = 'TotP2')
  plot_var_compare(SimFile, paste(ObsDataDir,"field_doc.csv",sep=""), var_name = 'DOC')
  plot_var_compare(SimFile, paste(ObsDataDir,"field_dic.csv",sep=""), var_name = 'DIC')
  plot_var_compare(SimFile, paste(ObsDataDir,"field_all_poc.csv",sep=""), var_name = 'all_POC')
  plot_var_compare(SimFile, paste(ObsDataDir,"field_log_ch4.csv",sep=""), var_name = 'log_CAR_ch4')
  plot(as.Date(ext.coef$DateTime),mod.secchi,type='l',ylim=rev(c(0,7)),xlab="Date",ylab="Secchi Depth (m)")
  points(as.Date(obs.secchi$Date),obs.secchi$Depth,pch=16,col='red')
  legend('bottomright',c("Modeled","Observed"),pch=c(NA,16),col=c('black','red'),lty=c(1,NA))
  dev.off()
}

function.arg<-paste(paste("VisualCheck_",Sys.time(),sep=" "),".pdf",sep="")
generate_all_plots(function.arg)
