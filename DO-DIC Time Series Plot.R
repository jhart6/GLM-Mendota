####Surface DO and DIC time series plot####
#for the purposes of showing Cayelan that displaying modeled DIC data would be a bad
  #idea, but also including observed DIC at least somewhere in the manuscript

#extract modeled DIC data
setwd("~/Dropbox/LaMe GLM Calibration/Modeled Data/")
mod.dic<-read.csv('dic.csv')
View(mod.dic)

#extract observed DIC data
setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")
obs.dic<-read.csv('field_dic.csv')
View(obs.dic)

#extract observed DO data
setwd("~/Dropbox/LaMe GLM Calibration/Observed Data/")
obs.do<-read.csv('field_do.csv')
View(obs.do)


#figure
quartz()
par(mar=c(3,3,1,1),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(obs.do$DateTime[which(obs.do$Depth==3)]),obs.do$DO[which(obs.do$Depth==3)],ylim=c(7,60),pch=16,col='blue',xlab='DateTime',ylab="mg/L")
points(as.Date(obs.dic$DateTime[which(obs.dic$Depth==3)]),obs.dic$DIC[which(obs.dic$Depth==3)],pch=16,col='red')
points(as.Date(mod.dic$DateTime),mod.dic$DIC_3,pch=16,col='orange')
legend('topright',c('Observed DO @ 3m','Observed DIC @ 3m','Modeled DIC @ 3m'),col=c('blue','red','orange'),pch=c(16,16,16))



