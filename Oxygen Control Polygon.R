library(glmtools)
library(GLMr)
library(lubridate)
library(dplyr)

#########################EPILIMNION#########################
SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/'
setwd(SimDir)
SimFile = paste(SimDir, 'output.nc',sep='')
nc_file = file.path(SimDir, 'output.nc')

#state DO at 3m (mmol/m3)
state_do_3 <- get_var(SimFile, var_name = 'DO',reference = 'surface',z_out = 3)
state_do_3 <- state_do_3$DO_3

#GPP (mmol/m3/day)
GPP_3 <- get_var(SimFile, var_name = 'PHY_PPR', reference = 'surface', z_out =3)
GPP <- GPP_3$PHY_PPR_3

#NEP (mmol/m3/day)
NEP_3 <- get_var(SimFile, var_name = 'PHY_NPR', reference = 'surface', z_out = 3)
NEP <- NEP_3$PHY_NPR_3

#total respiration (mmol/m3/day)
Respiration = GPP - NEP

#atmospheric exchange (mmol/m2/day)
atm_exch <- get_var(SimFile, var_name = 'OXY_atm_oxy_exch')
atm <- atm_exch$OXY_atm_oxy_exch

#nitrification (mmol/m3/day)
nit_3 <- get_var(SimFile, var_name = 'NIT_nitrif', reference = 'surface', z_out =3)
nitrif <- nit_3$NIT_nitrif_3

#sign convention
Respiration = Respiration * -1
atm = atm *-1
nitrif = nitrif*-1

datetime <- as.Date(GPP_3$DateTime)
state_do_3
GPP
Respiration
atm
nitrif

#create plot
xlab = expression(Date)

poly_x = c(datetime,rev(datetime))
resp_y = c(rep(0,length(Respiration)),Respiration)
atm_y = c(rep(0,length(atm)),atm)
nitrif_y = c(rep(0,length(nitrif)),nitrif)
gpp_y = c(rep(0, length(GPP)),GPP)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)

plot(poly_x, resp_y, type = 'n', xlab = xlab, ylab = NA,ylim=c(-1,0.5))
polygon(poly_x, resp_y, col = 'red')
polygon(poly_x, gpp_y, col = 'green')
polygon(poly_x, atm_y, col= 'blue') #wrong scale
polygon(poly_x, nitrif_y, col = 'yellow') #wrong scale


#########################HYPOLIMNION#########################

SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/'
setwd(SimDir)
SimFile = paste(SimDir, 'output.nc',sep='')
nc_file = file.path(SimDir, 'output.nc')

#state DO at 3m (mmol/m3)
state_do_20 <- get_var(SimFile, var_name = 'DO',reference = 'surface',z_out = 20)
state_do_20 <- state_do_20$DO_20

#GPP (mmol/m3/day)
GPP_20 <- get_var(SimFile, var_name = 'PHY_PPR', reference = 'surface', z_out =20)
GPP <- GPP_20$PHY_PPR_20

#NEP (mmol/m3/day)
NEP_20 <- get_var(SimFile, var_name = 'PHY_NPR', reference = 'surface', z_out = 20)
NEP <- NEP_20$PHY_NPR_20

#total respiration (mmol/m3/day)
Respiration = GPP - NEP

#sediment oxygen demand (mmol/m2/day)
sed_oxy <- get_var(SimFile, var_name = 'OXY_sed_oxy',reference = 'surface', z_out = 20)
demand = sed_oxy$OXY_sed_oxy

#nitrification (mmol/m3/day)
nit_20 <- get_var(SimFile, var_name = 'NIT_nitrif', reference = 'surface', z_out =20)
nitrif <- nit_20$NIT_nitrif_20

#sign convention
Respiration = Respiration * -1
nitrif = nitrif * -1

datetime
state_do_20
GPP
Respiration
demand
nitrif


#create plot
xlab = expression(Date)

poly_x = c(datetime,rev(datetime))
resp_y = c(rep(0,length(Respiration)),Respiration)
nitrif_y = c(rep(0,length(nitrif)),nitrif)
gpp_y = c(rep(0, length(GPP)),GPP)
demand_y = c(rep(0,length(demand)),demand)

quartz()
par(mar=c(3,3,1,4),mgp=c(1.5,0.5,0),tck=-0.02)


plot(poly_x, resp_y, type = 'n', xlab = xlab, ylab = NA)
polygon(poly_x, resp_y, col = 'red')
polygon(poly_x, gpp_y, col = 'green') #wrong scale
polygon(poly_x, demand_y, col = 'brown')
polygon(poly_x, nitrif_y, col = 'blue') #wrong scale
