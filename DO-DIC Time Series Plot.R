####Surface DO and DIC time series plot####
#for the purposes of showing Cayelan that displaying modeled DIC data would be a bad
  #idea, but also including observed DIC at least somewhere in the manuscript

#extract modeled DIC data
setwd("~/Dropbox/LaMe GLM Calibration/Modeled Data/JuliaCalibration/")
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

mod.dic.mmol<-(mod.dic$DIC_3*1000)/12.01
obs.dic.mmol<-(obs.dic$DIC[which(obs.dic$Depth==3)]*1000)/12.01
obs.do.mmol<-(obs.do$DO[which(obs.do$Depth==3)]*1000)/32

#figure
#all in mg/L on one axis
quartz()
par(mar=c(3,3,1,3),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(obs.do$DateTime[which(obs.do$Depth==3)]),obs.do$DO[which(obs.do$Depth==3)],ylim=c(7,60),pch=16,col='blue',xlab='DateTime',ylab="mg/L")
points(as.Date(obs.dic$DateTime[which(obs.dic$Depth==3)]),obs.dic$DIC[which(obs.dic$Depth==3)],pch=16,col='red')
points(as.Date(mod.dic$DateTime),mod.dic$DIC_3,pch=16,col='orange')
legend('topright',c('Observed DO @ 3m','Observed DIC @ 3m','Modeled DIC @ 3m'),col=c('blue','red','orange'),pch=c(16,16,16))

#all in mmol/m3 on two axes
quartz()
par(mar=c(3,3,1,3),mgp=c(1.5,0.5,0),tck=-0.02)
plot(as.Date(mod.dic$DateTime),mod.dic.mmol,pch=16,col='orange',ylim=c(2000,4200),xlab='DateTime',ylab=expression(DIC~(mmol~m^-3)))
points(as.Date(obs.dic$DateTime[which(obs.dic$Depth==3)]),obs.dic.mmol,pch=16,col='red')
lines(as.Date(obs.dic$DateTime[which(obs.dic$Depth==3)]),obs.dic.mmol,col='red')
par(new=TRUE)
plot(as.Date(obs.do$DateTime[which(obs.do$Depth==3)]),obs.do.mmol,pch=16,col='blue',axes=FALSE,xlab=NA,ylab=NA,ylim=c(200,420))
lines(as.Date(obs.do$DateTime[which(obs.do$Depth==3)]),obs.do.mmol,col='blue')
axis(side=4)
mtext(side=4,expression(Dissolved~Oxygen~(mmol~m^-3)),line=1.5)
legend('topright',c('Observed DO @ 3m','Observed DIC @ 3m','Modeled DIC @ 3m'),col=c('blue','red','orange'),pch=c(16,16,16))
