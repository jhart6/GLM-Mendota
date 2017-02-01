#Secchi Calibration

#Load observed secchi
setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")
obsSecchi <- read.csv("secchi.csv")

#plotting
par(mfrow=c(2,1))
plot(obsSecchi$Julian,obsSecchi$Depth,type='l')
obsExtc <- (1.7/obsSecchi$Depth)
obsSecchi<- cbind(obsSecchi,obsExtc)

plot(obsSecchi$Julian,obsSecchi$obsExtc)

#extracting modeled secchi
plot_var(SimFile, 'extc_coef')
df<-get_var(SimFile, 'extc_coef', z_out= 1)
secchi<-(1.7/df$extc_coef.elv_1)
df<-cbind(df,secchi)

plot(df$DateTime,df$secchi,type='l')

#What could be off
plot_var(SimFile, var_name = 'PHY_TPHYS')
plot_var(file=nc_file,'DOC',fig_path=FALSE)

plot(df$DateTime,df$extc_coef.elv_1,type='l')


