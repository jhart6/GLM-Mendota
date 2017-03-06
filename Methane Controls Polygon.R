library(glmtools)
library(GLMr)
library(lubridate)
library(dplyr)


#########################EPILIMNION#########################
SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/'
setwd(SimDir)
SimFile = paste(SimDir, 'output.nc',sep='')
nc_file = file.path(SimDir, 'output.nc')

#state CH4 at 3m (mmol/m3)
state_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 3) #mmol m^-3
ch4_3 <- state_ch4_3$CAR_ch4_3

#oxidation at 3m (mmol/m3/day)
ox_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out =3) #mmol m^-3 day^-1
ox_ch4_3<-ox_ch4_3$CAR_ch4ox_3

#atm exch at 3m (mmol/m2/day)
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
atm_exch<-read.csv("flux.data.csv")
ch4_atm_exch<-atm_exch$ch4.flux.read

#change sign convention
ox_ch4_3<-ox_ch4_3*-1
ch4_atm_exch <- ch4_atm_exch*-1

datetime = as.Date(state_ch4_3$DateTime)
methane = ch4_3
atm = ch4_atm_exch
oxidation = ox_ch4_3


#create plot
xlab = expression(Date)

poly_x <- c(datetime,rev(datetime))
atm_y <- c(rep(0,length(atm)),atm)
ox_y <- c(rep(0,length(oxidation)),oxidation)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)

plot(poly_x, ox_y, type = 'n', xlab = xlab, ylab = NA)
polygon(poly_x, atm_y,col = 'red') #wrong scale
polygon(poly_x, ox_y, col = 'blue') 



#########################HYPOLIMNION#########################
SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/'
setwd(SimDir)
SimFile = paste(SimDir, 'output.nc',sep='')
nc_file = file.path(SimDir, 'output.nc')

#state CH4 at 3m (mmol/m3)
state_ch4_20<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 20) #mmol m^-3
ch4_20 <- state_ch4_20$CAR_ch4_20

#oxidation at 3m (mmol/m3/day)
ox_ch4_20<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out =20) #mmol m^-3 day^-1
ox_ch4_20<-ox_ch4_20$CAR_ch4ox_20

#calculate sediment CH4 flux
Fsed_ch4<-10
Ksed_ch4<-100
theta_sed_ch4<- 1.07

oxy<-get_var(SimFile, var_name = 'OXY_oxy', reference = 'surface', z_out = 24.5)
temp<-get_var(SimFile, var_name = 'temp', reference = 'surface', z_out = 24.5)

ch4_flux = Fsed_ch4 * (Ksed_ch4/(Ksed_ch4+oxy$OXY_oxy_24.5)) * (theta_sed_ch4)^(temp$temp_24.5-20)

#change sign convention
ox_ch4_20 <- ox_ch4_20*-1


datetime
methane = ch4_20
oxidation = ox_ch4_20
production = ch4_flux


#create plot
xlab = expression(Date)

poly_x <- c(datetime,rev(datetime))
ox_y <- c(rep(0,length(oxidation)),oxidation)
prod_y <- c(rep(0,length(production)),production)

quartz()
par(mar=c(3,6,1,4),mgp=c(1.5,0.5,0),tck=-0.02)

plot(poly_x,prod_y,type = 'n', xlab = xlab, ylab=NA,ylim = c(-1,7))
polygon(poly_x,prod_y, col = 'red')
par(new=TRUE)
axis(2, pos = 4)
polygon(poly_x, ox_y, col = 'blue') #wrong scale
