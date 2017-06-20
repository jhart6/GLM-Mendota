
setwd("~/Dropbox/LaMe GLM Calibration/Modeled Data/")

#Thermocline
thermocline <- compare_to_field(SimFile, obsTEMP, metric = 'thermo.depth',as_value=TRUE, na.rm = TRUE, precision = 'days', method = 'interp')
write.csv(thermocline,file = 'thermocline.csv',row.names = FALSE)
 
#Temperature 
watertemp <- get_var(SimFile, var_name = 'temp', reference='surface',z_out = c(0:20))
write.csv(watertemp, file = 'watertemp.csv',row.names = FALSE)

#Dissolved Oxygen
oxygen <- get_var(SimFile, var_name = 'DO', reference = 'surface', z_out = c(0:20))
write.csv(oxygen, file = 'dissolved oxygen.csv', row.names = FALSE)

#DOC
doc <- get_var(SimFile, var_name = "all_DOC", reference = 'surface', z_out = c(0:20))
write.csv(doc, file = 'doc.csv', row.names = FALSE)

#TN
nitrogen <- get_var(SimFile, var_name = 'TotN2', reference = 'surface', z_out = c(0:20))
write.csv(nitrogen, file = 'tn.csv', row.names = FALSE)

#TP
phosphorus <- get_var(SimFile, var_name = 'TotP2', reference = 'surface', z_out = c(0:20))
write.csv(phosphorus, file = 'tp.csv',row.names = FALSE)

#POC
poc <- get_var(SimFile, var_name = 'TOT_POC', reference = 'surface', z_out = c(0:20))
write.csv(poc, file = 'poc.csv',row.names = FALSE)

#CH4 
methane <- get_var(SimFile, var_name = 'log_CAR_ch4', reference = 'surface', z_out = c(0:20))
write.csv(methane, file = 'log methane.csv',row.names = FALSE)

#DIC
dic <- get_var(SimFile, var_name = 'DIC', reference = 'surface', z_out = c(0:20))
write.csv(dic, file = 'dic.csv', row.names = FALSE)

#pH
ph<- get_var(SimFile, var_name = "CAR_pH", reference = 'surface', z_out = c(0:20))
write.csv(ph, file = 'pH.csv', row.names= FALSE)

