

PSpecies <- function(SimFile,Depth){
  myZ = Depth
  myYlabel = 'P speciation (mmol/m3)'
  Vars2Plot = c('TOT_tp','OGM_pop','OGM_dop','PHS_frp','PHS_frp_ads','PHY_CYANOPCH1_IP',
                'PHY_CYANONPCH2_IP','PHY_CHLOROPCH3_IP','PHY_DIATOMPCH4_IP')
  ColorSch = c('black','green','blue','red','grey','blue','red','green','black','white')
  LineType = c(1,1,1,1,1,2,2,2,2)
  
  Total<-get_var(SimFile,var_name = Vars2Plot[1],z_out = myZ,reference = 'surface')
  myYlim = c(0,max(Total[,2])*1.2)
  plot(Total,type='l',ylim=myYlim,col=ColorSch[1],lwd=3,ylab = myYlabel)
  for (i in 2:length(Vars2Plot)){
    lines(get_var(SimFile,var_name = Vars2Plot[i],z_out = myZ,reference = 'surface'),
          col=ColorSch[i],lty=LineType[i])
  }
  myLegend = c(Vars2Plot,'Phytos are dashed')
  legend("topright",y=NULL,myLegend,fill = ColorSch)
}
