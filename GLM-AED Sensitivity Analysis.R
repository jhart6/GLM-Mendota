####Sensitivity Analysis for GLM-AED####
####JAH, adapated from Hilary Dugan (hilarydugan@gmail.com)####
####16 January 2017####

####Set working directory to folder where you want to store .nml files####
setwd("~/Dropbox/LaMe GLM Calibration/Sensitivity Test 2")

####Load Libraries####
library(glmtools)
library(GLMr)
library(googlesheets)
suppressMessages(library(dplyr))

####Get Google Docs Original NML FILE from web####
my_sheets <- gs_ls() #Will have to authenticate Google here (in browser, very easy)
glmJan17 <- gs_title("Julia's LaMe GLM Calibration") #get whole document

# Start here to update changes to GLM2.nml
glm.nml <- glmJan17 %>% gs_read_csv(ws = 'glm2.nml') #work with individual worksheet
write.table(glm.nml,file = 'glm2.nml',row.names = F,quote=F)

# Start here to update changes PHYTOS
phyto.nml <- glmJan17 %>% gs_read_csv(ws = 'phytos.nml') #work with individual worksheet
write.table(phyto.nml,file = 'aed2_phyto_pars.nml',row.names = F,quote=F)

# Start here to update changes ZOOPS
zoops.nml <- glmJan17 %>% gs_read_csv(ws = 'zoops.nml') #work with individual worksheet
write.table(zoops.nml,file = 'aed2_zoop_pars.nml',row.names = F,quote=F)

# Start here to update changes to AED2.nml
aed.nml <- glmJan17 %>% gs_read_csv(ws = 'aed2.nml') #work with individual worksheet
write.table(aed.nml,file = 'aed2.nml',row.names = F,quote=F)

# Get min max values from GoogleSheet
parAED <- glmJan17 %>% gs_read_csv(ws = 'AED Parameters') #work with individual worksheet
parAED$par2 = paste(parAED$X1,parAED$Parameter_1,sep = '::')
toTest = data.frame(par = parAED$par2, parLong = parAED$X1, parShort = parAED$Parameter_1, min = parAED$Min, max = parAED$Max, stringsAsFactors = F)
toTest = toTest[!is.na(toTest$min),] #only keep rows with min/max values 

toTestSmall = toTest[1,]

#Fsed parameters only
setwd("~/Dropbox/LaMe GLM Calibration/Module 1_Fsed SA/")
toTestFsed = toTest[1:14,]

################# CHANGE PARAMETERS ################

# inputs = data.frame (superlongformat param name, long param name (really module name), short param name, min, max)
newParam <- function(parInput) {
  param = parInput$par
  parShort = parInput$parShort
  parLong = parInput$parLong
  min = parInput$min
  max = parInput$max
  
  mainDir <- getwd() #file path for main directory
  subDir <- paste0('ME_',parShort) #name for subdirectory (param folder)
  dir.create(file.path(mainDir, subDir)) #actually create the subdirectory folder
  # find the files that you want
  list.of.files <- list.files(mainDir, full.names = TRUE)
  # copy the files to the new folder
  file.copy(list.of.files, file.path(mainDir, subDir),overwrite = T)
  # Set working directory to new folder 
  setwd(file.path(mainDir, subDir))
  
  
  require(glmtools)
  df = data.frame('parameter' = c('MaxIce','Temp1m','Temp24m','DO1m','DO24m','DOC1m','DOC24m','TN1m','TN24m',
                                  'TP1m','TP24m','TotCHL','TotPHY'),stringsAsFactors = F)
  aed_nml <- read_nml('aed2.nml') # read AED
  orgValue = get_nml_value(aed_nml,eval(param)) # get original value for param
  
  runValues <- function (param,newValue) {
    aed_nml <- read_nml('aed2.nml') #read AED
    aed_nml <- set_nml(aed_nml, arg_name = param, arg_val = newValue)
    write_nml(aed_nml, file = 'aed2.nml')
    run_glm(verbose = FALSE)
    
    ### Max Ice ###
    maxIce = max(get_var('output.nc','hice')[,2])
    ### Temperature ###
    Temp1m = get_var('output.nc','temp',z_out = 1, reference = 'surface',t_out = '2016-07-01')[,2]
    Temp24m = get_var('output.nc','temp',z_out = 24,reference = 'surface',t_out = '2016-07-01')[,2]
    ### DO ####
    DO1m = get_var('output.nc','OXY_oxy',z_out = 1,reference = 'surface',t_out = '2016-07-01')[,2]* 32/1000 #mg/L
    DO24m = get_var('output.nc','OXY_oxy',z_out = 24,reference = 'surface',t_out = '2016-07-01')[,2]* 32/1000 #mg/L
    ### DOC ####
    DOC1m = get_var('output.nc','OGM_doc',z_out = 1,reference = 'surface',t_out = '2016-07-01')[,2]* 12/1000 #mg/L
    DOC24m = get_var('output.nc','OGM_doc',z_out = 24,reference = 'surface',t_out = '2016-07-01')[,2]* 12/1000 #mg/L
    ### TN ####
    TN1m = get_var('output.nc','TOT_tn',z_out = 1,reference = 'surface',t_out = '2016-07-01')[,2]* 14/1000 #mg/L
    TN24m = get_var('output.nc','TOT_tn',z_out = 24,reference = 'surface',t_out = '2016-07-01')[,2]* 14/1000 #mg/L
    ### TP ####
    TP1m = get_var('output.nc','TOT_tp',z_out = 1,reference = 'surface',t_out = '2016-07-01')[,2]* 30.97/1000 #mg/L
    TP24m = get_var('output.nc','TOT_tp',z_out = 24,reference = 'surface',t_out = '2016-07-01')[,2]* 30.97/1000 #mg/L
    ### Turnover dates ####
    # Total CHL and Phytoplankton
    TotCHL = get_var('output.nc','PHY_TCHLA',z_out = 1,reference = 'surface',t_out = '2016-07-01')[,2] 
    TotPHY = get_var('output.nc','PHY_TPHYS',z_out = 1,reference = 'surface',t_out = '2016-07-01')[,2] 
    
    newValues = c(maxIce,Temp1m,Temp24m,DO1m,DO24m,DOC1m,DOC24m,TN1m,TN24m,TP1m,TP24m,TotCHL,TotPHY)
    
    return(newValues)
  }
  
  seqPar = seq(min, max, length.out = 5)
  
  df$X1 = runValues(param,newValue = seqPar[1])
  df$X2 = runValues(param,newValue = seqPar[2])
  df$X3 = runValues(param,newValue = seqPar[3])
  df$X4 = runValues(param,newValue = seqPar[4])
  df$X5 = runValues(param,newValue = seqPar[5])
  names(df)[-1] = as.character(seqPar)
  
  #unlink(file.path(mainDir, subDir),recursive = T) if you want to delete folder afterwards
  setwd(file.path(mainDir))
  return(df)
}

# Test function
newParam(toTestSmall[1,])



######################### Apply as function in parallel ###########################
library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)

parList <- split(toTestFsed, seq(nrow(toTestFsed))) #has to be as a list for parLapply
outputAED = parLapply(cl, X = parList,fun = newParam) #apply function in parallel

# stop cluster
stopCluster(cl)

# name list
names(outputAED) = toTestFsed$par

# save list (so you don't have to run again)
save(outputAED, file = "outputAED.RData")


