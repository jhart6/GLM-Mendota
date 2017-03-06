library(glmtools)
library(GLMr)
library(lubridate)
library(dplyr)

SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/'
setwd(SimDir)
SimFile = paste(SimDir, 'output.nc',sep='')
nc_file = file.path(SimDir, 'output.nc')

#state CH4 at 3m (mmol/m3)
state_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = 3) #mmol m^-3

#oxidation at 3m (mmol/m3/day)
ox_ch4_3<-get_var(SimFile, var_name = 'CAR_ch4ox', reference = 'surface', z_out =3) #mmol m^-3 day^-1
ox_ch4_3<-ox_ch4_3$CAR_ch4ox_3

#atm exch at 3m (mmol/m2/day)
setwd("~/Dropbox/GitHub Repos/GLM-Mendota/Data/")
atm_exch<-read.csv("flux.data.csv")
ch4_atm_exch<-atm_exch$ch4.flux.read














####
#convert all fluxes to mmol/m2/day
#area_at_3m <- 33926717
#ox_ch4_correct_units <- ox_ch4_3$CAR_ch4ox_3/area_at_3m

#change sign convention
atm_exch_sign <- atm_exch$CAR_atm_ch4_exch*-1
ox_ch4_sign <- ox_ch4_3$CAR_ch4ox_3*-1

#create plotting data frame
epi_ch4 <- cbind(state_ch4_3,atm_exch_sign,ox_ch4_sign)
colnames(epi_ch4)<-c('DateTime','CH4','AtmExch','Ox')
View(epi_ch4)

#create plot
xlab = expression(Date)
ylab = expression(mmol~m^-2~day^-1)

poly_x <- c(as.Date(epi_ch4$DateTime),as.Date(rev(epi_ch4$DateTime)))
atm_y <- c(rep(0,length(epi_ch4$AtmExch)),epi_ch4$AtmExch)
ox_y <- c(rep(0,length(epi_ch4$Ox)),epi_ch4$Ox)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)

plot(poly_x, ox_y, type = 'n', xlab = xlab, ylab = ylab)
polygon(poly_x, atm_y,col = 'red')
polygon(poly_x, ox_y, col = 'blue')

