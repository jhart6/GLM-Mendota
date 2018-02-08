# plot_field: plots heatmap of any variable. Based off of glmtools functions
# filename: either a dataframe of format (datetime, depth, variable) or .csv of same format
# var_name: character string of variable. ex) 'temp' or 'doc'
# units: character string of units. ex) '*C' or 'mg/L'

#gases = Purples
#carbon = PuRd
#ysi vars = OrRd

# #examples
# plot_field('field_ch4.csv','CH4','umol/L')
# plot_field(c('field_poc.csv','field_log(CH4).csv'),c('POC','log(CH4'),c('mg/L','umol/L'),xlims = as.Date(c('2016-04-15','2016-11-15')))
# 
# quartz()
# par(bg='white',mar=c(2,5,2,1))
# plot_field('field_poc.csv','POC','mg/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
# plot_field('field_log(CH4).csv','log(CH4)','umol/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
# plot_field('field_log(CO2).csv','log(CO2)','umol/L',xlims=as.Date(c('2016-04-15','2016-11-15')))
# 
# setwd("~/Dropbox/Mendota Summer 16/R/plot_field/")

# Example code
# png('fieldtemp.png',width = 3.5,height = 2.5,units = 'in',res=300)
# plot_field('field_temp.csv',var_name = 'temp',units = '*C',addpoints = F)
# dev.off()


plot_field_pub <- function(filename,var_name,units = 'units',xlims = NULL,addpoints = T,collim = NULL){
  library(glmtools)
  library(RColorBrewer)
  library(readr)
  library(lubridate)
  library(colorRamps)
  # source('~/Dropbox/Mendota Summer 16/R/plot_field.R', echo=TRUE)
  num_vars = length(var_name)
  colbar_layout(length(var_name))
  
  for (v in 1:num_vars){
    par(cex.lab = 1.1, cex.axis = 1, mgp = c(1.6,0.4,0),tck=-0.02, mar=c(3,3,1,0.5))
    #require(RColorBrewer)
    require(colorRamps)
    require(akima)
    if (is.data.frame(filename[v])){
      data = filename[v]
    } else {
      data = read_csv(filename[v])
      # data = read.csv(filename,stringsAsFactors = F)
      # data[,1] = strptime(data[,1],'%Y-%m-%d %H:%M:%S')
    }
    
    colnames(data) = c('DateTime','Depth',var_name[v])
    data = data[complete.cases(data),]
    data = data[!duplicated(data[,1:2]),]
    x = as.numeric(as.Date(data$DateTime))
    y = data$Depth
    z = unlist(data[,3])
    
    full_x = sort(unique(as.numeric(as.Date(data$DateTime))))
    full_y = sort(unique(data$Depth))
    
    interped = interp(x, y, z, full_x, full_y)
    
    colors = matlab.like(9)
    #colors = blue2green2red(9)
    #colors = rev(rainbow(9))
    #colors = brewer.pal(9,'BuPu')
    if (is.null(collim)) {
      col_lim <- range(data[,3], na.rm = TRUE)
    } else {
      col_lim = collim
    }
    levels <- pretty(col_lim, 6)
    
    if (is.null(xlims)) {
      xlims = range(as.Date(data$DateTime))
    }  
    plot(range(as.Date(data$DateTime)), c(NA, NA), ylim=rev(range(full_y)), 
         xlim=xlims, xlab='', ylab= 'Depth (m)')
    
    .filled.contour(as.Date(full_x, origin='1970-01-01'), full_y, interped$z,
                    levels=levels, col=colors)
    if (addpoints == T){
      points(x,y,cex=0.4)
    }
    
    bar_title <- expression(Temperature~(degree*C))
    #bar_title <- expression(Dissolved~Oxygen~(mg~L^-1))
    #bar_title <- expression(TN~(mg~L^-1))
    #bar_title <- expression(TP~(mg~L^-1))
    #bar_title <- expression(POC~(mg~L^-1))
    #bar_title <- expression(DOC~(mg~L^-1))
    #bar_title <- expression(DIC~(mg~L^-1))
    #bar_title <- expression(log(CH[4])~(mu*mol~L^-1))
    #bar_title <- paste(var_name[v],' (',units[v],')',sep='') #original code
    color_key(levels, colors, subs=levels, col_label = bar_title)
  }
}



################## Modified from glmtools ########################
color_key <- function(levels, colors, subs, cex = 1, col_label){
  old_mgp <- par()$mgp
  old_mai <- par()$mai
  old_mar <- par()$mar
  par(mar=c(old_mar[1],0, old_mar[3], 0.5), mgp = c(0,0.25,0)) 
  
  plot(NA, xlim = c(0,1),ylim=c(0,1), xlab="", ylab="",
       frame=F,axes=F,xaxs="i",yaxs="i") # add feau plot
  
  axis(side = 4, line = -1.2,at = 0.5, tck = NA, labels= col_label, lwd = 0.0)#(\xB0 C)
  spc_pol_rat <- 0.2 # ratio between spaces and bars
  
  p_start <- 0.1
  p_wid <- 0.5
  
  # plotting to a 1 x 1 space
  if (!all(subs %in% levels)) stop('selected values must be included in levels')
  
  num_poly <- length(subs)
  num_spc <- num_poly - 1
  total_height <- num_poly + spc_pol_rat * num_spc
  
  poly_h <- 1/total_height
  spc_h <- spc_pol_rat * poly_h
  
  for (i in 1:num_poly){
    col <- colors[levels==subs[i]]
    b <- (i-1)*(poly_h+spc_h)
    t <- b+poly_h
    m <- mean(c(b,t))
    polygon(c(p_start,p_wid,p_wid,p_start),c(b,b,t,t),col = col, border = NA)
    text(p_wid+0.025,m,as.character(subs[i]), cex = cex, adj = c(0.5, 1), srt = 90)
  }
  par(mai = old_mai, mgp = old_mgp, mar = old_mar)
}


colbar_layout <- function(nrow = length(var_name)){
  # ensures all colorbar plots use same x scaling for divs
  mx <- matrix(c(rep(1,5),2),nrow=1)
  panels <- mx
  if (nrow > 1){
    for (i in 2:nrow){
      panels <- rbind(panels,mx+(i-1)*2)
    }
  }
  layout(panels)
}


