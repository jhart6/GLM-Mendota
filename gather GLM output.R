library(dplyr)
library(tidyr)

####POC####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
poc.wide<-read.csv('poc.csv')

poc.long<-gather(poc.wide, DEPTH, POC, -DateTime)
poc.long$DEPTH<-as.numeric(gsub('TOT_POC_','',poc.long$DEPTH))
colnames(poc.long)<-c('DATETIME','DEPTH','POC')
write.csv(poc.long,"poc_long.csv",row.names=FALSE)
plot_field_glm('poc_long.csv','POC','mg/L')


####DIC####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
dic.wide<-read.csv('dic.csv')

dic.long<-gather(dic.wide, DEPTH, DIC, -DateTime)
dic.long$DEPTH<-as.numeric(gsub('DIC_','',dic.long$DEPTH))
colnames(dic.long)<-c('DATETIME','DEPTH','DIC')
write.csv(dic.long,"dic_long.csv",row.names=FALSE)
plot_field_glm('dic_long.csv','DIC','mg/L')


####DO####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
do.wide<-read.csv('dissolved oxygen.csv')

do.long<-gather(do.wide, DEPTH, DO, -DateTime)
do.long$DEPTH<-as.numeric(gsub('DO_','',do.long$DEPTH))
colnames(do.long)<-c('DATETIME','DEPTH','DO')
write.csv(do.long,"do_long.csv",row.names=FALSE)
plot_field_glm('do_long.csv','DO','mg/L')


####DOC####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
doc.wide<-read.csv('doc.csv')

doc.long<-gather(doc.wide, DEPTH, DOC, -DateTime)
doc.long$DEPTH<-as.numeric(gsub('all_DOC_','',doc.long$DEPTH))
colnames(doc.long)<-c('DATETIME','DEPTH','DOC')
write.csv(doc.long,"doc_long.csv",row.names=FALSE)
plot_field_glm('doc_long.csv','DOC','mg/L')


####logCH4####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
ch4.wide<-read.csv('log methane.csv')

ch4.long<-gather(ch4.wide, DEPTH, logCH4, -DateTime)
ch4.long$DEPTH<-as.numeric(gsub('log_CAR_ch4_','',ch4.long$DEPTH))
colnames(ch4.long)<-c('DATETIME','DEPTH','logCH4')
write.csv(ch4.long,"logCH4_long.csv",row.names=FALSE)
plot_field_glm('logCH4_long.csv','log(CH4)','umol/L')



####pH#####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
ph.wide<-read.csv('pH.csv')

ph.long<-gather(ph.wide, DEPTH, PH, -DateTime)
ph.long$DEPTH<-as.numeric(gsub('CAR_pH_','',ph.long$DEPTH))
colnames(ph.long)<-c('DATETIME','DEPTH','pH')
write.csv(ph.long,"ph_long.csv",row.names=FALSE)
plot_field_glm('ph_long.csv','pH','')


####TN######
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
tn.wide<-read.csv('tn.csv')

tn.long<-gather(tn.wide, DEPTH, TN, -DateTime)
tn.long$DEPTH<-as.numeric(gsub('TotN2_','',tn.long$DEPTH))
colnames(tn.long)<-c('DATETIME','DEPTH','TN')
write.csv(tn.long,"tn_long.csv",row.names=FALSE)
plot_field_glm('tn_long.csv','TN','mg/L')



####TP####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
tp.wide<-read.csv('tp.csv')

tp.long<-gather(tp.wide, DEPTH, TP, -DateTime)
tp.long$DEPTH<-as.numeric(gsub('TotP2_','',tp.long$DEPTH))
colnames(tp.long)<-c('DATETIME','DEPTH','TP')
write.csv(tp.long,"tp_long.csv",row.names=FALSE)
plot_field_glm('tp_long.csv','TP','mg/L')


####Temp####
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')
temp.wide<-read.csv('watertemp.csv')

temp.long<-gather(temp.wide, DEPTH, TEMPERATURE, -DateTime)
temp.long$DEPTH<-as.numeric(gsub('temp_','',temp.long$DEPTH))
colnames(temp.long)<-c('DATETIME','DEPTH','TEMPERATURE')
write.csv(temp.long,"temp_long.csv",row.names=FALSE)
plot_field_glm('temp_long.csv','TEMPERATURE','C')

