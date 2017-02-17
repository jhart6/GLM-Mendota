#POC Calibration

run_glm()

convert_sim_var(nc_file, POC = OGM_poc * 12/1000, unit = 'mg/L', overwrite = T)
convert_sim_var(nc_file, TOT_POC = ((OGM_poc + PHY_TPHYS) * 12/1000), unit = 'mg/L', overwrite=T)
convert_sim_var(nc_file, all_DOC = (OGM_doc + OGM_docr) * 12/1000, unit = 'mg/L',overwrite = T)


plot_var_compare(nc_file = SimFile, obsPOC, var_name='TOT_POC')
plot_var_compare(nc_file = SimFile, obsALLDOC, var_name = 'all_DOC',col=c(4,7.5))
plot_var(SimFile, var_name = 'POC')
plot_var(SimFile, var_name = 'PHY_TPHYS')

#POC
df <- resample_to_field(SimFile, obsPOC, method = 'interp', precision = 'days', var_name = 'TOT_POC')
sqrt((sum((df$Modeled_TOT_POC-df$Observed_TOT_POC)^2, na.rm=TRUE))/nrow(df))

#DOC
df <- resample_to_field(SimFile, obsALLDOC, method='interp', precision = 'days', var_name = 'all_DOC')
sqrt((sum((df$Modeled_all_DOC-df$Observed_all_DOC)^2, na.rm=TRUE))/nrow(df))

plot(get_var(SimFile, var_name = 'PHY_TPHYS', reference='surface', z_out = 3),ylim=c(0,325))
plot(get_var(SimFile, var_name = 'TOT_POC', reference = 'surface', z_out = 3))
plot(get_var(SimFile, var_name = 'all_DOC',reference = 'surface',z_out = 3))
plot(get_var(SimFile, var_name = 'POC', reference = 'surface', z_out = 3))


#observed data (what the values should be)
plot(poc$DATETIME[which(poc$DEPTH==3)],poc$TOT_POC[which(poc$DEPTH==3)],type='l')
plot(alldoc$DateTime[which(alldoc$Depth==3)],alldoc$all_DOC[which(alldoc$Depth==3)])
