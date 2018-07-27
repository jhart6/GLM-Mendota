# After downloading and installing the Mac High Sierra upgrade,
# you must do the following: 
# (1) obtain the GLM v2.7 for Mac binaries; Matt Hipsey sent these to me
# (2) install on your Mac by draggint the glm+ package into your Applications folder; 
# (3) update your Mac's global environmental variable by
# (a) open a terminal
# (b) issue the following command to edit the paths, sudo nano /etc/paths
# (c) add the glm+ to the path, e.g., /Applications/glm+.app/Contents/MacOS
# (4) to ensure RStudio uses the environment paths, you have to start it
#     from a command line, e.g., open -a RStudio
# (5) to run a simulation, change the working directory to your simulation and 
#     execute a system call

PrintPlots = FALSE
RunSimulation = TRUE
PlotPhytosIntegrated = TRUE
# For post processing purposes
Depth1 = 2
Depth2 = 20

#source('~/Dropbox/Hanson/Simulations/Mendota/R/AnalysisHelpers.R') 
#source('~/Dropbox/Hanson/Simulations/Mendota/R/AnalysisHelpersJulia.R')
#Paul's scripts with helpful GLM analysis functions (e.g., calculating secchi depth)
library(glmtools)
library(GLMr)
library(dplyr)

# SimDir = '/Users/paul/documents/GLMSims/v27Julia'
# SimDir = '/Users/paul/documents/GLMSims/Mendota/Experiment_13April2018_a/Sims/Sim1'
# SimFile = paste(SimDir,'/output.nc',sep="")
#SimDir = '/Users/paul/documents/GLMSims/V30Mendota25_Jul_2018'
SimDir = '~/Dropbox/LaMe GLM Calibration/GLM3/V30Mendota25_Jul_2018/'
# SimDir = '/Users/paul/documents/GLMSims/GLM_Examples_3.0/Sparkling'
# SimDir = '~/DropBox/CNH-GLM/3.0_testing/Sunapee'
SimFile = paste(SimDir,'/Outputs/output.nc',sep="")

if (RunSimulation){
  # Run GLM
  setwd(SimDir)
  system2('glm+') # must be in the dir that has glm2.nml when this is called
}

plot_temp(SimFile)
Temperature(SimFile,Depth1,TRUE)
Temperature(SimFile,Depth2,TRUE)

plot_var(SimFile,var_name = "OXY_oxy")
Secchi(SimFile,Depth1,TRUE)
DOConcentrations(SimFile,Depth1,TRUE)
myIce = get_var(SimFile,'hice')
plot(myIce,type='l',ylim=c(-0.5,0.5))

# Evaporation
myEvap  = get_var(SimFile,'evap') # in m/s
cumEvap = cumsum(-1*myEvap$evap*(60*1440)) # in m
plot(as.POSIXct(myEvap$DateTime),cumEvap,type='l')
SimDurationDays = (myEvap$DateTime[dim(myEvap)[1]] - myEvap$DateTime[1])
SimDurationYears = SimDurationDays[[1]]/365
AvgYearlyEvap = cumEvap[length(cumEvap)]/SimDurationYears # in meters
print(paste('Avg yearly evap:',AvgYearlyEvap))
#plot_var(SimFile,'OGM_poc')
#plot_var(SimFile,'OGM_docr')
#plot_var(SimFile,'CAR_dic')
#plot_var(SimFile,'TOT_tp')

#par(mfrow = c(3, 2),     # 3x2 layout
par(oma = c(2, 1, 1, 1), # four rows of text at the outer left and bottom margin
    mar = c(0.5, 3.7, 0.5, 2.0), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = FALSE)            # allow content to protrude into outer margin (and beyond)

OCSpecies(SimFile,Depth1,FALSE,c(0,10))
NSpecies(SimFile,Depth1,FALSE)
PSpecies(SimFile,Depth1,FALSE)
text(par("usr")[1],0.8*(par("usr")[4]-par("usr")[3]),paste('                Depth',Depth1,'m'))
PSpecies(SimFile,Depth2,FALSE)
text(par("usr")[1],0.8*(par("usr")[4]-par("usr")[3]),paste('                Depth',Depth2,'m'))
Silica(SimFile,Depth1,TRUE)
DOConcentrations(SimFile,Depth1,TRUE)
Secchi(SimFile,Depth1,TRUE)
Temperature(SimFile,Depth1,TRUE)

#######################################
# convert_sim_var(SimFile, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
# convert_sim_var(SimFile, DOsat = OXY_sat * 32/1000, unit = 'mg/L',overwrite = T)
# 
# # Print oxy
# #png('myOxyHeat.png',width=600,height=150,units='px',bg=NA)
# png('myOxyHeat.png',width=1200,height=300,units='px',bg=NA,res=150)
# plot_var(SimFile,var_name = "DO",cex=1.5)
# plot_var(SimFile,var_name = "DOsat",cex=1.5)
# dev.off()
# 
# # Print line data only
# StartDate = as.POSIXct('2013-01-01')
# EndDate = as.POSIXct('2013-12-31')
# 
# # Oxygen line
# myYlim = c(50,200)
# myDataDO = get_var(SimFile,var_name='DO',reference = 'surface',z_out=1)
# myDataDOHypo = get_var(SimFile,var_name='DO',reference = 'surface',z_out=20)
# iGood = which(myDataDO$DateTime>=StartDate & myDataDO$DateTime<=EndDate)
# myDataDOsat = get_var(SimFile,var_name='DOsat',reference = 'surface',z_out=1)
# PercentSat = myDataDO$DO_1/myDataDOsat$DOsat * 100
# #png('myOxySat.png',width=600,height=300,units='px',bg=NA,res=150)
# #par(mai = c(0.6, 1.0, 0.1, 0.1))
# # plot(myDataDO$DateTime[iGood],myDataDO$DO_1[iGood],ylim=myYlim,type='l',lwd = 1, col='blue',
# #      xlab='',ylab='DO (mg/L)',main='')
# # lines(myDataDOsat$DateTime[iGood],myDataDOsat$DOsat[iGood])
# plot(myDataDO$DateTime[iGood],PercentSat[iGood],ylim=myYlim,type='l',lwd = 3, col='blue',
#            xlab='',ylab='DO (% sat)',main='')
# abline(h=100,lty=2)
# #dev.off()
# 
# # Line plot of O2
# par(mai = c(0.6, 1.2, 0.1, 0.1))
# plot(myDataDO,type='l',ylim=c(0,20),lwd=2,xlab='',ylab='O2 (mg/L)',col='blue')
# lines(myDataDOHypo,type='l',lwd=2)
# # Line plot of phytoplankton
# myPhys = get_var(SimFile,var_name='PHY_TPHYS',reference='surface',z_out=1)
# myPhys$PHY_TPHYS_1 = myPhys$PHY_TPHYS_1*12/1000
# plot(myPhys,type='l',lwd=2,
#      xlab='',ylab='Phytoplankton (mgC/L)')

#######################################

if (PlotPhytosIntegrated){
  # Plot species-specific time series
  # This is over the same depth range as the manual samples
  Z = 1:8
  # png('Results/phytos.png',width = 4,height=2.5,units='in',res=500,bg='transparent')
  # par(mar=c(2,3,1,1),mgp=c(1.5,0.3,0),tck=-0.03)
  Tot = get_var(SimFile,var_name = 'PHY_TPHYS',reference = "surface",z_out=Z)
  TotMean = rowMeans(Tot[,2:length(Z)+1],na.rm=TRUE) # Get the mean across depths
  Tot[,2] = TotMean # set the second column to the mean
  Tot[,2] = Tot[,2]*12/1000
  plot(Tot[,1],Tot[,2],type='l',xlab='',ylab='Phytoplankton 0-8m (mg/L)')
  
  Dia = get_var(SimFile,var_name = 'PHY_DIATOMPCH4',reference = "surface",z_out=Z)
  DiaMean = rowMeans(Dia[,2:length(Z)+1],na.rm=TRUE) # Get the mean across depths
  Dia[,2] = DiaMean # set the second column to the mean
  Dia[,2] = Dia[,2]*12/1000
  lines(Dia[,1],Dia[,2],type='l',col='brown')
  
  Chl = get_var(SimFile,var_name = 'PHY_CHLOROPCH3',reference = "surface",z_out=Z)
  ChlMean = rowMeans(Chl[,2:length(Z)+1],na.rm=TRUE) # Get the mean across depths
  Chl[,2] = ChlMean # set the second column to the mean
  Chl[,2] = Chl[,2]*12/1000
  lines(Chl[,1],Chl[,2],type='l',col='green')
  
  CyaN = get_var(SimFile,var_name = 'PHY_CYANONPCH2',reference = "surface",z_out=Z)
  CyaNMean = rowMeans(CyaN[,2:length(Z)+1],na.rm=TRUE) # Get the mean across depths
  CyaN[,2] = CyaNMean # set the second column to the mean
  CyaN[,2] = CyaN[,2]*12/1000
  lines(CyaN[,1],CyaN[,2],type='l',col='red')
  
  Cya = get_var(SimFile,var_name = 'PHY_CYANOPCH1',reference = "surface",z_out=Z)
  CyaMean = rowMeans(Cya[,2:length(Z)+1],na.rm=TRUE) # Get the mean across depths
  Cya[,2] = CyaMean # set the second column to the mean
  Cya[,2] = Cya[,2]*12/1000
  lines(Cya[,1],Cya[,2],type='l',col='blue')

  if (PrintPlots){
    # Now print the plots
    opal = palette()
    par(mai = c(0.5,0.8, 0.2, 0.1),cex = 0.8)
    myLty = 1
    myLwd = 2
    col2 = adjustcolor(palette(),alpha.f=0.5)
    # col2 1=blk, 2=red, 3=green, 4=blue, 5=cyan, 6=purple, 7=yellow, 8=?
    FileName = paste('./Plots/PhytoManualModelTimeseries.png',sep="")
    #png(FileName)
    myYLab = expression('Phytoplankton biomass (g m'^'-3'*')')
    #_____________________________________
    plot(Tot[,1],Tot[,2],type='l',xlab='',ylab=myYLab)
    lines(Dia[,1],Dia[,2],type='l',col='brown')
    lines(Chl[,1],Chl[,2],type='l',col='green')
    lines(CyaN[,1],CyaN[,2],type='l',col='red')
    lines(Cya[,1],Cya[,2],type='l',col='blue')
    #_____________________________________
    dev.copy(png,FileName,width=4,height=3,units="in",res=300)
    dev.off()
    par(opar)
    
  }
  
}

