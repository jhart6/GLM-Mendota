#New Figure 4?
#hypolimnetic time series POC, CH4, and CO2

setwd("~/Dropbox/Mendota Summer 16/R/")
lake<-read.csv("lake_budget.csv")

plot(as.Date(lake$date[which(lake$depth==20)]),lake$poc[which(lake$depth==20)])
plot(as.Date(lake$date[which(lake$depth==20)]),log(lake$ch4[which(lake$depth==20)]))
plot(as.Date(lake$date[which(lake$depth==20)]),log(lake$co2[which(lake$depth==20)]))


xlab=expression(Date)
ylab=expression(GHG~(mu*mol~L^-1))
ylab2=expression(POC~(mg~L^-1))

setwd("~/Dropbox/Masters Writing/PUB5 (L&O)/For Resubmission/Figures/")
tiff('Figure 4_v2.tiff',width=7,height=5,units='in',res=300)
#quartz()
par(mar=c(3,4,1,4),mgp=c(1.5,0.5,0),tck=-0.02,bg='white')
plot(as.Date(lake$date[which(lake$depth==20)]),log(lake$ch4[which(lake$depth==20)]),ylim=c(-3,9),pch=16,col='firebrick',xlab=xlab,ylab=ylab)
lines(as.Date(lake$date[which(lake$depth==20)]),log(lake$ch4[which(lake$depth==20)]),col='firebrick')
par(new=T)
plot(as.Date(lake$date[which(lake$depth==20)]),log(lake$co2[which(lake$depth==20)]),ylim=c(-3,9),pch=16,col='gold1',ylab=NA,xlab=NA,axes=FALSE)
lines(as.Date(lake$date[which(lake$depth==20)]),log(lake$co2[which(lake$depth==20)]),col='gold1')
par(new=T)
plot(as.Date(lake$date[which(lake$depth==20)]),lake$poc[which(lake$depth==20)],pch=16,col='black',axes=FALSE,ylab=NA,xlab=NA,ylim=c(0,3))
lines(as.Date(lake$date[which(lake$depth==20)]),lake$poc[which(lake$depth==20)],col='black')
axis(side=4)
mtext(side=4, line=1.75, ylab2)
legend("topleft",pch=c(16,16,16),lty=c(1,1,1),col=c("firebrick",'gold1','black'),c(expression(log(CH[4])),expression(log(CO[2])),expression(POC)))
dev.off()
