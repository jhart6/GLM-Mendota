# plot_field: plots heatmap of any variable. Based off of glmtools functions
# filename: either a dataframe of format (datetime, depth, variable) or .csv of same format
# var_name: character string of variable. ex) 'temp' or 'doc'
# units: character string of units. ex) '*C' or 'mg/L'

#gases = Purples
#carbon = PuRd
#ysi vars = OrRd


plot_field_glm <- function(filename,var_name,units = 'units',xlims = NULL){
  library(glmtools)
  library(RColorBrewer)
  library(readr)
  library(lubridate)
  # source('~/Dropbox/Mendota Summer 16/R/plot_field.R', echo=TRUE)
  num_vars = length(var_name)
  colbar_layout(length(var_name))
  
  for (v in 1:num_vars){
    par(cex.lab = 1.4, cex.axis = 1.2, mgp = c(2,0.6,0),tck=-0.01)
    require(RColorBrewer)
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
    
    colors = brewer.pal(9,'BuGn')
    col_lim <- range(data[,3], na.rm = TRUE)
    levels <- pretty(col_lim, 6)
    
    if (is.null(xlims)) {
      xlims = range(as.Date(data$DateTime))
    }  
    plot(range(as.Date(data$DateTime)), c(NA, NA), ylim=rev(range(full_y)), 
         xlim=xlims, xlab='', ylab= 'depth (m)')
    
    .filled.contour(as.Date(full_x, origin='1970-01-01'), full_y, interped$z,
                    levels=levels, col=colors)
    
    bar_title <- paste(var_name[v],' (',units[v],')',sep='')
    color_key(levels, colors, subs=levels, col_label = bar_title)
  }
}



################## Modified from glmtools ########################
color_key <- function(levels, colors, subs, cex = 1.2, col_label){
  old_mgp <- par()$mgp
  old_mai <- par()$mai
  old_mar <- par()$mar
  par(mai=c(old_mai[1],0, old_mai[3], 0.3), mgp = c(0,.25,0)) 
  
  plot(NA, xlim = c(0,1),ylim=c(0,1), xlab="", ylab="",
       frame=F,axes=F,xaxs="i",yaxs="i") # add feau plot
  
  axis(side = 4, at = 0.5, tck = NA, labels= col_label, lwd = 0.0)#(\xB0 C)
  spc_pol_rat <- 0.2 # ratio between spaces and bars
  
  p_start <- 0.1
  p_wid <- 0.8
  
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

