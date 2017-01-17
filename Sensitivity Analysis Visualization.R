
#######################################################
png('SensitivityAnalysisVisualization.png',width = 8,height = 45,units = 'in',res = 300)
par(mfrow=c(length(outputAED),1),mar=c(2,7,0.5,0.5),mgp=c(1,0.5,0),tck=-0.04)

for (p in 1:length(outputAED)){
  df = outputAED[[p]]
  allRow = 0
  for (m in 1:13) {
    meanV = mean(unlist(df[m,2:6]))
    X1 = (df[m,2]-meanV)/meanV
    X2 = (df[m,3]-meanV)/meanV
    X3 = (df[m,4]-meanV)/meanV
    X4 = (df[m,5]-meanV)/meanV
    X5 = (df[m,6]-meanV)/meanV
    space = 0
    allRow = c(allRow,X1,X2,X3,X4,X5,space)
  }
  b = barplot(allRow,ylim=c(-0.1,0.1),col=c('white','red4','red3','goldenrod','dodgerblue2','dodgerblue4'),space = 0,axes = T,
              ylab = '',yaxt='n')
  axis(side=1, at= seq(3,76,length.out = 13), 
       labels=c('maxIce','Temp1','Temp24','DO1', 'DO24','DOC1','DOC24','N1',
                'N24','P1','P24','CHL','PHY'), cex.axis=0.9,lty=0)
  rangeP = paste0(names(df)[2],'-',names(df[6]))
  mtext(paste(names(outputAED)[p],'\n',rangeP), side=2, line=-1,las=1)
}
dev.off()

#######################################################
png('sensitivityPars3.png',width = 8,height = 45,units = 'in',res = 300)
par(mfrow=c(length(output),1),mar=c(2,7,0.5,0.5),mgp=c(1,0.5,0),tck=-0.04)
for (p in 1:length(output)){
  df = output[[p]]
  allRow = 0
  for (m in 1:13) {
    l0.1 = (df[m,2]-df[m,5])/df[m,5]
    l0.5 = (df[m,3]-df[m,5])/df[m,5]
    l0.75 = (df[m,4]-df[m,5])/df[m,5]
    h1.5 = (df[m,6]-df[m,5])/df[m,5]
    h2 = (df[m,7]-df[m,5])/df[m,5]
    h10 = (df[m,8]-df[m,5])/df[m,5]
    space = 0
    allRow = c(allRow,l0.1,l0.5,l0.75,h1.5,h2,h10,space)
  }
  b = barplot(allRow,ylim=c(-1,1),col=c('white','red4','red3','red2','dodgerblue2','dodgerblue3','dodgerblue4'),space = 0,axes = T,
              ylab = '',yaxt='n')
  axis(side=1, at= seq(4,93,by = 7), 
       labels=c('maxIce','Temp1','Temp24','DO1', 'DO24','DOC1','DOC24','N1',
                'N24','P1','P24','CHL','PHY'), cex.axis=0.9,lty=0)
  mtext(names(output)[p], side=2, line=-1,las=1)
}
dev.off()

#######################################################
png('sensitivityInflows.png',width = 8,height = 8,units = 'in',res = 300)
par(mfrow=c(length(outputN),1),mar=c(2,4,0.5,0.5),mgp=c(1,0.5,0),tck=-0.04)
for (p in 1:length(outputN)){
  df = outputN[[p]]
  allRow = 0
  for (m in 1:10) {
    x1 = (df[m,3]-df[m,2])/df[m,2]
    x1.5 = (df[m,4]-df[m,2])/df[m,2]
    random = (df[m,5]-df[m,2])/df[m,2]
    space = 0
    allRow = c(allRow,x1,x1.5,random,space)
  }
  b = barplot(allRow,ylim=c(-1,1),col=c('white','red2','red4','darkgreen'),space = 0,axes = T,
              ylab = '',yaxt='n')
  axis(side=1, at= seq(2.25,41.25,by = 4), 
       labels=c('Temp1','Temp24','DO1', 'DO24','DOC1','DOC24','N1',
                'N24','P1','P24'), cex.axis=0.9,lty=0)
  mtext(names(outputN)[p], side=2, line=-1,las=1)
}
dev.off()


##################################################################
png('sensitivityPhytos.png',width = 8,height = 20,units = 'in',res = 300)
par(mfrow=c(length(outputP),1),mar=c(2,7,0.5,0.5),mgp=c(1,0.5,0),tck=-0.04)
for (p in 1:length(outputP)){
  df = outputP[[p]]
  allRow = 0
  for (m in 1:12) {
    l0.5 = (df[m,2]-df[m,4])/df[m,4]
    l0.75 = (df[m,3]-df[m,4])/df[m,4]
    h1.5 = (df[m,5]-df[m,4])/df[m,4]
    h2 = (df[m,6]-df[m,4])/df[m,4]
    space = 0
    allRow = c(allRow,l0.5,l0.75,h1.5,h2,space)
  }
  b = barplot(allRow,ylim=c(-1,1),col=c('white','red4','red3','dodgerblue','dodgerblue4'),space = 0,axes = T,
              ylab = '',yaxt='n')
  
  axis(side=1, at= seq(3,62,by = 5), 
       labels=c('Temp1','Temp24','DO1', 'DO24','DOC1','DOC24','N1',
                'N24','P1','P24','CHL','PHY'), cex.axis=0.9,lty=0)
  mtext(names(outputP)[p], side=2, line=-1,las=1)
  if (p == 1){
    legend('topright',legend = c('x0.5','x0.75','x1.5','x2'),fill=c('red4','red3','dodgerblue','dodgerblue4'),ncol=4)
  }
}
dev.off()







