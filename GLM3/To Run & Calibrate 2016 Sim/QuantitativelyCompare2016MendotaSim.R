#2016 Mendota Simulation in GLM3
#Script 3 of 3: Quantitative Comparison of Observed and Simulated Vars
#Developed 8/27/18 by JAH

#PURPOSE: Quantitatively compare observed and simulated 2016 Lake Mendota data
  # This script will generate a table of R^2, NMAE, and Spearman rank correlation 
  # coefficients for all sim vars, which is automaticaly versioned with the 
  # Date/Time when you run the function.

#NOTES:
  #Source this script to regenerate a time-stamped table with each calibration.


library(dplyr)
library(glmtools)
library(GLMr)
library(lubridate)
library(tidyr)



####1. Get Modeled Data from SimFile####
#specify  dates and depths that match with observed data
z_out = c(3,10,12,14,20)
t_out = c('2016-04-15','2016-04-22','2016-05-06','2016-05-12','2016-05-20','2016-05-23','2016-06-01','2016-06-06','2016-06-13','2016-06-21','2016-06-27','2016-07-06','2016-07-12','2016-07-19','2016-07-26','2016-08-02','2016-08-08','2016-08-16','2016-08-23','2016-08-30','2016-09-08','2016-09-12','2016-10-04','2016-10-10','2016-10-17','2016-10-25','2016-11-01','2016-11-14')
z_out_nutrients<-c(0,4,8,10,12,14,16,18,20,22)
t_out_nutrients<-c('2016-02-12','2016-04-29','2016-05-14','2016-05-28','2016-06-10','2016-06-24','2016-07-08','2016-07-22','2016-08-05','2016-08-21','2016-09-04','2016-09-30','2016-10-29','2016-12-02')

#get modeled data for observation Z and T
glm.temp<-get_temp(SimFile,reference='surface',z_out=z_out,t_out=t_out)
glm.do<-get_var(SimFile, var_name = 'DO', reference = 'surface', z_out = z_out, t_out = t_out)
glm.tn<-get_var(SimFile, var_name = 'TotN2', reference = 'surface', z_out = z_out_nutrients, t_out = t_out_nutrients)
glm.tp<-get_var(SimFile, var_name = 'TotP2', reference = 'surface', z_out = z_out_nutrients, t_out = t_out_nutrients)
glm.poc<-get_var(SimFile, var_name = 'all_POC', reference = 'surface', z_out = z_out, t_out = t_out)
glm.doc<-get_var(SimFile, var_name = 'DOC', reference = 'surface', z_out = z_out, t_out = t_out)
glm.ch4<-get_var(SimFile, var_name = 'CAR_ch4', reference = 'surface', z_out = z_out, t_out = t_out)
glm.dic<-get_var(SimFile, var_name = 'DIC', reference = 'surface', z_out = z_out, t_out = t_out)
glm.ph<-get_var(SimFile, var_name = 'CAR_pH', reference = 'surface', z_out = z_out, t_out = t_out)

colnames(glm.temp)<-c('datetime','z.3','z.10','z.12','z.14','z.20')
colnames(glm.do)<-c('datetime','z.3','z.10','z.12','z.14','z.20')
colnames(glm.tn)<-c('datetime','z.0','z.4','z.8','z.10','z.12','z.14','z.16','z.18','z.20','z.22')
colnames(glm.tp)<-c('datetime','z.0','z.4','z.8','z.10','z.12','z.14','z.16','z.18','z.20','z.22')
colnames(glm.poc)<-c('datetime','z.3','z.10','z.12','z.14','z.20')
colnames(glm.doc)<-c('datetime','z.3','z.10','z.12','z.14','z.20')
colnames(glm.ch4)<-c('datetime','z.3','z.10','z.12','z.14','z.20')
colnames(glm.dic)<-c('datetime','z.3','z.10','z.12','z.14','z.20')
colnames(glm.ph)<-c('datetime','z.3','z.10','z.12','z.14','z.20')



####2. Get Observed Data####
#Already specially formatted to match get_var output format
setwd(paste(SimDir,"Observed Data/nmae",sep=""))
obs.poc<-read.csv('obs.poc.csv')
obs.dic<-read.csv('obs.dic.csv')
obs.doc<-read.csv('obs.doc.csv')
obs.ch4<-read.csv('obs.ch4.csv')
obs.temp<-read.csv('obs.temp.csv')
obs.do<-read.csv('obs.do.csv')
obs.ph<-read.csv('obs.ph.csv')
obs.tp<-read.csv('obs.tp.csv')
obs.tn<-read.csv('obs.tn.csv')



####3. NMAE Calculator Functions
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



####4. Calculate NMAE####
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



####5. Convert Wide Dataframes to Long Dataframes for R^2 and Rho Calculations####
gather.obs.data<-function(obs.data,var_name){
  long.data<-gather(obs.data,depth,var_name,z.3:z.20)
  clean.long.data<-long.data %>%
    select(datetime,depth,var_name)
  colnames(clean.long.data)<-c("datetime",'depth',var_name)
  return(clean.long.data)
}

gather.obs.data.nutrients<-function(obs.data,var_name){
  long.data<-gather(obs.data,depth,var_name,z.0:z.22)
  clean.long.data<-long.data %>%
    select(datetime,depth,var_name)
  colnames(clean.long.data)<-c("datetime",'depth',var_name)
  return(clean.long.data)
}

gather.glm.data<-function(glm.data,var_name){
  long.data<-gather(glm.data,depth,var_name,z.3:z.20)
  clean.long.data<-long.data %>%
    select(datetime,depth,var_name)
  colnames(clean.long.data)<-c("datetime",'depth',var_name)
  return(clean.long.data)
}

long.obs.temp<-gather.obs.data(obs.temp,'temp')
long.obs.do<-gather.obs.data(obs.do,'DO')
long.obs.tn<-gather.obs.data.nutrients(obs.tn,'TN')
long.obs.tp<-gather.obs.data.nutrients(obs.tp,'TP')
long.obs.poc<-gather.obs.data(obs.poc,'POC')
long.obs.doc<-gather.obs.data(obs.doc,'DOC')
long.obs.ch4<-gather.obs.data(obs.ch4,'CH4')
long.obs.dic<-gather.obs.data(obs.dic,'DIC')
long.obs.ph<-gather.obs.data(obs.ph,'pH')

long.glm.temp<-gather.obs.data(glm.temp,'temp')
long.glm.do<-gather.obs.data(glm.do,'DO')
long.glm.tn<-gather.obs.data.nutrients(glm.tn,'TN')
long.glm.tp<-gather.obs.data.nutrients(glm.tp,'TP')
long.glm.poc<-gather.obs.data(glm.poc,'POC')
long.glm.doc<-gather.obs.data(glm.doc,'DOC')
long.glm.ch4<-gather.obs.data(glm.ch4,'CH4')
long.glm.dic<-gather.obs.data(glm.dic,'DIC')
long.glm.ph<-gather.obs.data(glm.ph,'pH')



####6. Calculate R^2 and Spearman's Rho#### 
plot(long.glm.temp$temp,long.obs.temp$temp,pch=16)
temp.mod<-lm(long.obs.temp$temp~long.glm.temp$temp)
abline(temp.mod)
tempR2=summary(temp.mod)$adj.r.squared
temp.cor<-cor.test(long.glm.temp$temp,long.obs.temp$temp,method="spearman")
tempRho=as.numeric(temp.cor$estimate)

plot(long.glm.do$DO,long.obs.do$DO,pch=16)
do.mod<-lm(long.obs.do$DO~long.glm.do$DO)
abline(do.mod)
doR2=summary(do.mod)$adj.r.squared
do.cor<-cor.test(long.glm.do$DO,long.obs.do$DO,method='spearman')
doRho=as.numeric(do.cor$estimate)

plot(long.glm.tn$TN,long.obs.tn$TN)
tn.mod<-lm(long.obs.tn$TN~long.glm.tn$TN)
abline(tn.mod)
tnR2=summary(tn.mod)$adj.r.squared
tn.cor=cor.test(long.glm.tn$TN,long.obs.tn$TN,method='spearman')
tnRho=as.numeric(tn.cor$estimate)

plot(long.glm.tp$TP,long.obs.tp$TP)
tp.mod<-lm(long.obs.tp$TP~long.glm.tp$TP)
abline(tp.mod)
tpR2=summary(tp.mod)$adj.r.squared
tp.cor=cor.test(long.glm.tp$TP,long.obs.tp$TP,method='spearman')
tpRho=as.numeric(tp.cor$estimate)

plot(long.glm.poc$POC,long.obs.poc$POC)
poc.mod<-lm(long.obs.poc$POC~long.glm.poc$POC)
abline(poc.mod)
pocR2=summary(poc.mod)$adj.r.squared
poc.cor=cor.test(long.glm.poc$POC,long.obs.poc$POC,method='spearman')
pocRho=as.numeric(poc.cor$estimate)

plot(long.glm.doc$DOC,long.obs.doc$DOC)
doc.mod<-lm(long.obs.doc$DOC~long.glm.doc$DOC)
abline(doc.mod)
docR2=summary(doc.mod)$adj.r.squared
doc.cor=cor.test(long.glm.doc$DOC,long.obs.doc$DOC,method='spearman')
docRho=as.numeric(doc.cor$estimate)

plot(long.glm.ch4$CH4,long.obs.ch4$CH4)
ch4.mod<-lm(long.obs.ch4$CH4~long.glm.ch4$CH4)
abline(ch4.mod)
ch4R2=summary(ch4.mod)$adj.r.squared
ch4.cor=cor.test(long.glm.ch4$CH4,long.obs.ch4$CH4,method='spearman')
ch4Rho=as.numeric(ch4.cor$estimate)

plot(long.glm.dic$DIC,long.obs.dic$DIC)
dic.mod<-lm(long.obs.dic$DIC~long.glm.dic$DIC)
abline(dic.mod)
dicR2=summary(dic.mod)$adj.r.squared
dic.cor=cor.test(long.glm.dic$DIC,long.obs.dic$DIC,method='spearman')
dicRho=as.numeric(dic.cor$estimate)

plot(long.glm.ph$pH,long.obs.ph$pH)
ph.mod<-lm(long.obs.ph$pH~long.glm.ph$pH)
abline(ph.mod)
phR2=summary(ph.mod)$adj.r.squared
ph.cor=cor.test(long.glm.ph$pH,long.obs.ph$pH,method='spearman')
phRho=as.numeric(ph.cor$estimate)

r2.all<-c(tempR2,doR2,tnR2,tpR2,pocR2,docR2,ch4R2,dicR2,phR2)
rho.all<-c(tempRho,doRho,tnRho,tpRho,pocRho,docRho,ch4Rho,dicRho,phRho)


####7. Collate all quantitative comparison metrics and export####
all.metrics<-data.frame(vars.all,nmae.all,epi.nmae.all,hypo.nmae.all,r2.all,rho.all)
colnames(all.metrics) = c("Var","NMAE/Whole Water Column","NMAE/EPI","NMAE/HYPO","Adj R^2","Spearman's Rho")

data.frame.name=paste("QuantCheck_",paste(Sys.time(),".csv",sep=""),sep="")
export.path=paste(SimDir,"CalibrationChecks/",sep="")
write.csv(all.metrics,file=paste(export.path,data.frame.name,sep=""),row.names=FALSE)
