#script to calculate R^2, NMAE, and Spearman rank correlation coefficient for GLM calibration
#4/7/16
#Julia

library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)

SimDir = '~/Dropbox/LaMe GLM Calibration/Greedy/' 
setwd(SimDir) 
SimFile = paste(SimDir,'output.nc',sep = '') 
nc_file <- file.path(SimDir, 'output.nc') 

z_out = c(3,10,12,14,20)
t_out = c('2016-04-15','2016-04-22','2016-05-06','2016-05-12','2016-05-20','2016-05-23','2016-06-01','2016-06-06','2016-06-13','2016-06-21','2016-06-27','2016-07-06','2016-07-12','2016-07-19','2016-07-26','2016-08-02','2016-08-08','2016-08-16','2016-08-23','2016-08-30','2016-09-08','2016-09-12','2016-10-04','2016-10-10','2016-10-17','2016-10-25','2016-11-01','2016-11-14')
z_out_nutrients<-c(0,4,8,10,12,14,16,18,20,22)
t_out_nutrients<-c('2016-02-12','2016-04-29','2016-05-14','2016-05-28','2016-06-10','2016-06-24','2016-07-08','2016-07-22','2016-08-05','2016-08-21','2016-09-04','2016-09-30','2016-10-29','2016-12-02')

#get modeled data for observation Z and T
glm.temp<-get_temp(SimFile,reference='surface',z_out=z_out,t_out=t_out)
glm.do<-get_var(SimFile, var_name = 'DO', reference = 'surface', z_out = z_out, t_out = t_out)
glm.tn<-get_var(SimFile, var_name = 'TotN2', reference = 'surface', z_out = z_out_nutrients, t_out = t_out_nutrients)
glm.tp<-get_var(SimFile, var_name = 'TotP2', reference = 'surface', z_out = z_out_nutrients, t_out = t_out_nutrients)
glm.poc<-get_var(SimFile, var_name = 'TOT_POC', reference = 'surface', z_out = z_out, t_out = t_out)
glm.doc<-get_var(SimFile, var_name = 'all_DOC', reference = 'surface', z_out = z_out, t_out = t_out)
glm.ch4<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = z_out, t_out = t_out)
glm.dic<-get_var(SimFile, var_name = 'DIC', reference = 'surface', z_out = z_out, t_out = t_out)
glm.ph<-get_var(SimFile, var_name = 'CAR_pH', reference = 'surface', z_out = z_out, t_out = t_out)

#read observed data
setwd("~/Dropbox/Masters Writing/nmae/")
obs.poc<-read.csv('obs.poc.csv')
obs.dic<-read.csv('obs.dic.csv')
obs.doc<-read.csv('obs.doc.csv')
obs.ch4<-read.csv('obs.ch4.csv')
obs.temp<-read.csv('obs.temp.csv')
obs.do<-read.csv('obs.do.csv')
obs.ph<-read.csv('obs.ph.csv')
obs.tp<-read.csv('obs.tp.csv')
obs.tn<-read.csv('obs.tn.csv')

#nmae calculator functions
nmae<-function(mod.data,obs.data){
  numerator<-sum(abs(mod.data[,2:6]-obs.data[,2:6]),na.rm=TRUE)
  n <- nrow(mod.data)
  mean.obs<-mean(colMeans(obs.data[,2:6],na.rm=TRUE))
  denominator <- n * mean.obs
  nmae <- numerator/denominator
  return(nmae)
}

nmae_nutrients<-function(mod.data,obs.data){
  numerator<-sum(abs(mod.data[,2:11]-obs.data[,2:11]),na.rm=TRUE)
  n <- nrow(mod.data)
  mean.obs<-mean(colMeans(obs.data[,2:11],na.rm=TRUE))
  denominator <- n * mean.obs
  nmae <- numerator/denominator
  return(nmae)
}

nmae_top10<-function(mod.data,obs.data){
  numerator<-sum(abs(mod.data[,2:3]-obs.data[,2:3]),na.rm=TRUE)
  n <- nrow(mod.data)
  mean.obs<-mean(colMeans(obs.data[,2:3],na.rm=TRUE))
  denominator <- n * mean.obs
  nmae <- numerator/denominator
  return(nmae)
}

nmae_bottom10<-function(mod.data,obs.data){
  numerator<-sum(abs(mod.data[,4:6]-obs.data[,4:6]),na.rm=TRUE)
  n <- nrow(mod.data)
  mean.obs<-mean(colMeans(obs.data[,4:6],na.rm=TRUE))
  denominator <- n * mean.obs
  nmae <- numerator/denominator
  return(nmae)
}

nmae_nutrients_top10<-function(mod.data,obs.data){
  numerator<-sum(abs(mod.data[,2:5]-obs.data[,2:5]),na.rm=TRUE)
  n <- nrow(mod.data)
  mean.obs<-mean(colMeans(obs.data[,2:5],na.rm=TRUE))
  denominator <- n * mean.obs
  nmae <- numerator/denominator
  return(nmae)
}

nmae_nutrients_bottom10<-function(mod.data,obs.data){
  numerator<-sum(abs(mod.data[,6:11]-obs.data[,6:11]),na.rm=TRUE)
  n <- nrow(mod.data)
  mean.obs<-mean(colMeans(obs.data[,6:11],na.rm=TRUE))
  denominator <- n * mean.obs
  nmae <- numerator/denominator
  return(nmae)
}

#nmae
nmae.temp<-nmae(glm.temp,obs.temp)
nmae.do<-nmae(glm.do,obs.do)
nmae.tn<-nmae_nutrients(glm.tn,obs.tn)
nmae.tp<-nmae_nutrients(glm.tp,obs.tp)
nmae.poc<-nmae(glm.poc,obs.poc)
nmae.doc<-nmae(glm.doc,obs.doc)
nmae.ch4<-nmae(glm.ch4,obs.ch4)
nmae.dic<-nmae(glm.dic,obs.dic)
nmae.ph<-nmae(glm.ph,obs.ph)

vars.all<-c("Temp",'DO','TN','TP','POC','DOC','CH4','DIC','pH')
nmae.all<-c(nmae.temp,nmae.do,nmae.tn,nmae.tp,nmae.poc,nmae.doc,nmae.ch4,nmae.dic,nmae.ph)

#nmae upper 10m
epi.nmae.temp<-nmae_top10(glm.temp,obs.temp)
epi.nmae.do<-nmae_top10(glm.do,obs.do)
epi.nmae.tn<-nmae_nutrients_top10(glm.tn,obs.tn)
epi.nmae.tp<-nmae_nutrients_top10(glm.tp,obs.tp)
epi.nmae.poc<-nmae_top10(glm.poc,obs.poc)
epi.nmae.doc<-nmae_top10(glm.doc,obs.doc)
epi.nmae.ch4<-nmae_top10(glm.ch4,obs.ch4)
epi.nmae.dic<-nmae_top10(glm.dic,obs.dic)
epi.nmae.ph<-nmae_top10(glm.ph,obs.ph)

epi.nmae.all<-c(epi.nmae.temp,epi.nmae.do,epi.nmae.tn,epi.nmae.tp,epi.nmae.poc,epi.nmae.doc,epi.nmae.ch4,epi.nmae.dic,epi.nmae.ph)


#nmae bottom 10m
hypo.nmae.temp<-nmae_bottom10(glm.temp,obs.temp)
hypo.nmae.do<-nmae_bottom10(glm.do,obs.do)
hypo.nmae.tn<-nmae_bottom10(glm.tn,obs.tn)
hypo.nmae.tp<-nmae_bottom10(glm.tp,obs.tp)
hypo.nmae.poc<-nmae_bottom10(glm.poc,obs.poc)
hypo.nmae.doc<-nmae_bottom10(glm.doc,obs.doc)
hypo.nmae.ch4<-nmae_bottom10(glm.ch4,obs.ch4)
hypo.nmae.dic<-nmae_bottom10(glm.dic,obs.dic)
hypo.nmae.ph<-nmae_bottom10(glm.ph,obs.ph)

hypo.nmae.all<-c(hypo.nmae.temp,hypo.nmae.do,hypo.nmae.tn,hypo.nmae.tp,hypo.nmae.poc,hypo.nmae.doc,hypo.nmae.ch4,hypo.nmae.dic,hypo.nmae.ph)

#R^2
library(tidyr)

gather.obs.data<-function(obs.data,var_name){
  long.data<-gather(obs.data,depth,var_name,z.3:z.20)
  clean.long.data<-long.data %>%
      select(datetime,depth,var_name)
  colnames(clean.long.data)<-c("datetime",'depth',var_name)
  return(clean.long.data)
}


