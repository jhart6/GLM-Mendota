#script to generate calibration plots to be stitched together in Illustrator
#uses plot_field_pub
#can create PNG and PDF files, but PDF files not as helpful

####To create PNG's on 170622####
source('~/Dropbox/GitHub Repos/GLM-Mendota/plot_field_pub.R')

#temperature
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obstemp.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('field_temp.csv','Temperature','Celsius',addpoints=T,collim=c(3,40))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('modtemp.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('temp_long.csv','Temperature','Celsius',addpoints=F,collim=c(3,40))
dev.off()

#oxygen
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obsdo.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('field_do.csv','Dissolved Oxygen',mg.L.units,addpoints=T,collim=c(0,14))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('moddo.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('do_long.csv','Dissolved Oxygen','mg/L',addpoints=F,collim=c(0,14))
dev.off()

#tn
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obstn.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('TotN2.csv','Total Nitrogen','mg/L',addpoints=T,collim=c(0,5),xlims=as.Date(c('2016-04-15','2016-11-15')))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('modtn.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('tn_long.csv','Total Nitrogen','mg/L',addpoints=F,collim=c(0,5))
dev.off()

#tp
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obstp.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('TotP2.csv','Total Phosphorus','mg/L',addpoints=T,collim=c(0,0.5),xlims=as.Date(c('2016-04-15','2016-11-15')))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('modtp.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('tp_long.csv','Total Phosphorus','mg/L',addpoints=F,collim=c(0,0.5))
dev.off()

#poc
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obspoc.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('field_poc.csv','POC','mg/L',addpoints=T,collim=c(0,3))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('modpoc.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('poc_long.csv','POC','mg/L',addpoints=F,collim=c(0,3))
dev.off()

#doc
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obsdoc.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('field_doc.csv','DOC','mg/L',addpoints=T,collim=c(4.2,5.5))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('moddoc.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('doc_long.csv','DOC','mg/L',addpoints=F,collim=c(4.2,5.5))
dev.off()

#dic
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obsdic.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('field_dic.csv','DIC','mg/L',addpoints=T,collim=c(35,60))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('moddic.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('dic_long.csv','DIC','mg/L',addpoints=F,collim=c(35,60))
dev.off()

#methane
setwd('~/Dropbox/LaMe GLM Calibration/Observed Data/')
png('obsch4.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('field_log_ch4.csv','log(CH4)','umol/L',addpoints=T,collim=c(-3,3))
dev.off()

setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
png('modch4.png',width=3.5,height=2.5,units='in',res=300)
plot_field_pub('logCH4_long.csv','log(CH4)','umol/L',addpoints=F,collim=c(-3,3))
dev.off()


# ####To create PDFs on 170621####
# #for plots of modeled data
# #setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/JuliaCalibration/') for Original MS Submission
# setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/PaulCalibration/')
# 
# pdf(file = 'mod_temp.pdf',bg='white')
# plot_field_glm('temp_long.csv','Temperature','C')
# dev.off()
# 
# pdf(file = 'mod_do.pdf',bg='white')
# plot_field_glm('do_long.csv','Dissolved Oxygen','mg/L')
# dev.off()
# 
# pdf(file = 'mod_tn.pdf',bg='white')
# plot_field_glm('tn_long.csv','Total Nitrogen','mg/L')
# dev.off()
# 
# pdf(file = 'mod_tp.pdf',bg='white')
# plot_field_glm('tp_long.csv','Total Phosphorus','mg/L')
# dev.off()
# 
# pdf(file = 'mod_poc.pdf',bg='white')
# plot_field_glm('poc_long.csv','POC','mg/L')
# dev.off()
# 
# pdf(file = 'mod_doc.pdf',bg='white')
# plot_field_glm('doc_long.csv','DOC','mg/L')
# dev.off()
# 
# pdf(file = 'mod_dic.pdf',bg='white')
# plot_field_glm('dic_long.csv','DIC','mg/L')
# dev.off()
# 
# pdf(file = 'mod_ch4.pdf',bg='white')
# plot_field_glm('logCH4_long.csv','log(CH4)','umol/L')
# dev.off()
# 
# #for plots of observed data
# setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")
# 
# pdf(file = 'obs_temp.pdf',bg='white')
# plot_field('field_temp.csv','Temperature','C')
# dev.off()
# 
# pdf(file = 'obs_do.pdf',bg='white')
# plot_field('field_do.csv','Dissolved Oxygen','mg/L')
# dev.off()
# 
# pdf(file = 'obs_tn.pdf',bg='white')
# plot_field('TotN2.csv','Total Nitrogen','mg/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
# dev.off()
# 
# pdf(file = 'obs_tp.pdf',bg='white')
# plot_field('TotP2.csv','Total Phosphorus','mg/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
# dev.off()
# 
# pdf(file = 'obs_poc.pdf',bg='white')
# plot_field('field_poc.csv',"POC",'mg/L')
# dev.off()
# 
# pdf(file = 'obs_doc.pdf',bg='white')
# plot_field('field_doc.csv','DOC','mg/L')
# dev.off()
# 
# pdf(file = 'obs_dic.pdf',bg='white')
# plot_field('field_dic.csv','DIC','mg/L')
# dev.off()
# 
# pdf(file = 'obs_ch4.pdf',bg='white')
# plot_field('field_log_ch4.csv','log(CH4)','umol/L')
# dev.off()
