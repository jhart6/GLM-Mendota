#for plots of modeled data
setwd('~/Dropbox/LaMe GLM Calibration/Modeled Data/')

pdf(file = 'mod_temp.pdf',bg='white')
plot_field_glm('temp_long.csv','Temperature','C')
dev.off()

pdf(file = 'mod_do.pdf',bg='white')
plot_field_glm('do_long.csv','Dissolved Oxygen','mg/L')
dev.off()

pdf(file = 'mod_tn.pdf',bg='white')
plot_field_glm('tn_long.csv','Total Nitrogen','mg/L')
dev.off()

pdf(file = 'mod_tp.pdf',bg='white')
plot_field_glm('tp_long.csv','Total Phosphorus','mg/L')
dev.off()

pdf(file = 'mod_poc.pdf',bg='white')
plot_field_glm('poc_long.csv','POC','mg/L')
dev.off()

pdf(file = 'mod_doc.pdf',bg='white')
plot_field_glm('doc_long.csv','DOC','mg/L')
dev.off()

pdf(file = 'mod_dic.pdf',bg='white')
plot_field_glm('dic_long.csv','DIC','mg/L')
dev.off()

#for plots of observed data
setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")

pdf(file = 'obs_temp.pdf',bg='white')
plot_field('field_temp.csv','Temperature','C')
dev.off()

pdf(file = 'obs_do.pdf',bg='white')
plot_field('field_do.csv','Dissolved Oxygen','mg/L')
dev.off()

pdf(file = 'obs_tn.pdf',bg='white')
plot_field('TotN2.csv','Total Nitrogen','mg/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
dev.off()

pdf(file = 'obs_tp.pdf',bg='white')
plot_field('TotP2.csv','Total Phosphorus','mg/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
dev.off()

pdf(file = 'obs_poc.pdf',bg='white')
plot_field('field_poc.csv',"POC",'mg/L')
dev.off()

pdf(file = 'obs_doc.pdf',bg='white')
plot_field('field_doc.csv','DOC','mg/L')
dev.off()

pdf(file = 'obs_dic.pdf',bg='white')
plot_field('field_dic.csv','DIC','mg/L')
dev.off()
