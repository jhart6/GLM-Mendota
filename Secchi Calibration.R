#Secchi Calibration

#Load observed secchi
setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")
obsSecchi <- read.csv("secchi.csv")

#plotting
par(mfrow=c(1,2))
plot(obsSecchi$Julian,obsSecchi$Depth,type='l')
obsExtc <- (1.7/obsSecchi$Depth)
obsSecchi<- cbind(obsSecchi,obsExtc)

plot(obsSecchi$Julian,obsSecchi$obsExtc,type='l',ylim=c(0,5))
lines(obsSecchi$Julian,obsSecchi$Depth,col='red')

#extracting modeled secchi
plot_var(SimFile, 'extc_coef')
df<-get_var(SimFile, 'extc_coef', z_out= 2.5,reference = 'surface')
secchi<-(1.7/df$extc_coef_2.5)
df<-cbind(df,secchi)

plot(df$DateTime,df$extc_coef.elv_2.5,type='l')
plot(df$DateTime,df$secchi,type='l')

#What could be off
plot_var(SimFile, var_name = 'PHY_TPHYS')
plot_var(file=nc_file,'DOC',fig_path=FALSE)



#####Double Checking LEC Equations####
#Paul PPT Calculation
kw = 0.15
keDOC = 0.00015
kePHY = 0.0025

get_var(SimFile, var_name = 'OGM_doc',z_out = 2.5,reference='surface')
DOC = 460.3181
get_var(SimFile, var_name = 'PHY_TPHYS', z_out = 2.5, reference='surface')
PHY = 235.46697 #ug/L
PHY_mmolm3 = PHY/12.01

LEC_ppt = kw + keDOC*DOC + kePHY*PHY_mmolm3 
#0.26

#Paul Email Calculation
kw = 0.15
keDOM = 0.00015
kePOM = 0.0025
kePHY = 0.0022

get_var(SimFile, var_name = 'OGM_doc', z_out = 2.5, reference = 'surface')
get_var(SimFile, var_name = 'OGM_dop', z_out = 2.5, reference = 'surface')
get_var(SimFile, var_name = 'OGM_don', z_out = 2.5, reference = 'surface')
DOM = 460.3181 + 0.3829843 + 55.18972

get_var(SimFile, var_name = 'OGM_poc', z_out = 2.5, reference = 'surface')
get_var(SimFile, var_name = 'OGM_pop', z_out = 2.5, reference = 'surface')
get_var(SimFile, var_name = 'OGM_pon', z_out = 2.5, reference = 'surface')
POM = 33.33238 + 0.0386590590 + 0.114854366

PHY_mmolm3 = 19.60591

LEC_email = kw + keDOM*DOM + kePOM*POM + kePHY*PHY_mmolm3
#0.354



####DOC Calibration####
SimDir = '~/Dropbox/LaMe GLM Calibration/Water Clarity Calibration/Results/Experiment_2017-02-02_14_48_30/Sims/Sim1/Results/'
setwd(SimDir) #setwd
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') 

convert_sim_var(nc_file, DO = OXY_oxy * 32/1000, unit = 'mg/L',overwrite = T)
convert_sim_var(nc_file, DOC = OGM_doc * 12/1000, unit = 'mg/L',overwrite = T)

plot_var(SimFile, c('PHY_TPHYS','DOC'))
plot_var_compare(nc_file = SimFile, obsDOC, var_name = 'DOC')


df <- resample_to_field(SimFile, obsDOC, method='interp', precision = 'days', var_name = 'DOC')
sqrt((sum((df$Modeled_DOC-df$Observed_DOC)^2, na.rm=TRUE))/nrow(df))



