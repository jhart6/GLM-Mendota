#POC Calibration

run_glm()

convert_sim_var(nc_file, POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
convert_sim_var(nc_file, TOT_POC = ((OGM_poc + PHY_TPHYS) * 12/1000), unit = 'mg/L', overwrite=T)

plot_var_compare(nc_file = SimFile, obsPOC, var_name='TOT_POC')

df <- resample_to_field(SimFile, obsPOC, method = 'interp', precision = 'days', var_name = 'TOT_POC')
sqrt((sum((df$Modeled_TOT_POC-df$Observed_TOT_POC)^2, na.rm=TRUE))/nrow(df))

plot(get_var(SimFile, var_name = 'TOT_POC', reference = 'surface', z_out = 3))
plot(poc$DATETIME[which(poc$DEPTH==3)],poc$TOT_POC[which(poc$DEPTH==3)],type='l')
