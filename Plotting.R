
#TOT_POC
#log_CAR_ch4
#CAR_ch4

quartz()
plot(get_var(file=nc_file, var_name = 'TOT_POC',reference = 'surface',z_out = 3),type='l',ylim=c(-1,5))
lines(get_var(file=nc_file, var_name = 'TOT_POC', reference = 'surface', z_out = 20), type='l', col='red')
legend('topright',c('POC @ 3m','POC @ 20m'),col=c('black','red'),lty=c(1,1))

quartz()
plot(get_var(file=nc_file,var_name = 'log_CAR_ch4', reference = 'surface', z_out = 3), type='l',ylim=c(0,3))
lines(get_var(file=nc_file, var_name = 'log_CAR_ch4', reference = 'surface', z_out = 20), type = 'l', col= 'red')
legend('topleft',c('log(CH4) @ 3m','log(CH4) @ 20m'),col=c('black','red'),lty=c(1,1))

quartz()
plot(get_var(file = nc_file, var_name = 'CAR_atm_co2_exch'),type='l')
lines(get_var(file=nc_file, var_name = 'CAR_atm_ch4_exch'),type='l',col='red')
legend('topleft',c('CO2 Exchange','CH4 Exchange'), col=c('black','red'),lty=c(1,1))
