
methane3<-get_var(SimFile,var_name = 'log_CAR_ch4',reference = 'surface', z_out =3)
methane2<-get_var(SimFile,var_name = 'log_CAR_ch4',reference = 'surface', z_out =2)
methane4<-get_var(SimFile,var_name = 'log_CAR_ch4',reference = 'surface', z_out =4)

plot(as.Date(methane3$DateTime),methane3$log_CAR_ch4_3,type='l')
lines(as.Date(methane3$DateTime),methane4$log_CAR_ch4_4,type = 'l',col='red')
lines(as.Date(methane3$DateTime),methane2$log_CAR_ch4_2,type = 'l',col='blue')
legend('topleft',c('Methane @ 2', 'Methane @ 3', 'Methane @ 4'),lty = c(1,1,1), col = c('blue','black','red'))
