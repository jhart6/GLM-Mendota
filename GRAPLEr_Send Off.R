#Install and configure GRAPLEr on your computer
 install.packages("httr")
 install.packages("RCurl")
 install.packages("jsonlite")
 install.packages("devtools")
 library("devtools")
 devtools::install_github("GRAPLE/GRAPLEr")
library(httr)
library(RCurl)
library(jsonlite)
library(GRAPLEr)
library(glmtools)
library(GLMr)

MyParentDir = '~/Dropbox/LaMe GLM Calibration/170203_Nitrogen Calibration/' #GLM files
# To load an old file...
# LoadFile = paste(MyParentDir,'Experiment_2017-01-18_05_53_36',sep="")
# load(LoadFile)
FilterResults = FALSE # If false, then full NETCDF file is returned
#FilterName = 'ExtractVariables'
WaitForIt = TRUE # Whether to have R wait for the results (will check)
myWaitTime = 60 # Wait time in seconds between status check on experiment

# Trap for filtering just because 1000 sims unfiltered is bad idea
if (!FilterResults){
  Proceed = readline('NOT FILTERING! Press Y to continue...')
  if (toupper(Proceed)!='Y'){
    print('program terminated')
    stop()
  }
}

# No need to edit below here
DestinationEmail = "jhart6@wisc.edu"
MyExpRootDir = paste(MyParentDir,'Sims',sep="")
MyResultsDir = paste(MyParentDir,'Results',sep="")
graplerURL<-"http://graple.acis.ufl.edu"
MyExpName = paste('Experiment_',Sys.time(),sep="")
MyExpName = gsub(" ","_",MyExpName)
MyExpName = gsub(":","_",MyExpName)
ModelDefFile = paste(MyParentDir,MyExpName,sep="")

# check that the service is up and running and print status message
# MyExp1 <- GrapleCheckService(MyExp1)
# print(MyExp1@StatusMsg)
# # you can check check the current list of filters in GRAPLEr
# MyExp1 <- GrapleListPostProcessFilters(MyExp1)
# print(MyExp1@StatusMsg)

if (FilterResults){ # set up your second experiment in the MyExp1 object - this is a run with a filter
  MyExp1 <- new("Graple", GWSURL=graplerURL, ExpRootDir=MyExpRootDir, Email = DestinationEmail, ResultsDir=MyResultsDir,
                ExpName=MyExpName, TempDir = tempdir())
  # submit your experiment, with FilterName, and print status message
  MyExp1 <- GrapleRunExperiment(MyExp1, FilterName)
  print(MyExp1@StatusMsg)
}else{ # set up your first experiment in the MyExp1 object - this is a run without filter
  MyExp1 <- new("Graple", GWSURL=graplerURL, ExpRootDir=MyExpRootDir, Email = DestinationEmail, ResultsDir=MyResultsDir,
                ExpName=MyExpName, TempDir = tempdir())
  # submit your experiment and print status message
  MyExp1 <- GrapleRunExperiment(MyExp1)
  print(MyExp1@StatusMsg)
}

SubmitTime = Sys.time()
print(paste("Submit time:",SubmitTime))
# Just to be safe, save the model definition file 
save(MyExp1,file=ModelDefFile)

if (WaitForIt){
  # Check every myWaitTime to see if experiment is done
  GRAPLErDone = FALSE
  RunningCounter = 0
  while (!GRAPLErDone){
    # check completion % and print status message
    MyExp1 <- GrapleCheckExperimentCompletion(MyExp1)
    # print(MyExp1@StatusMsg)
    myStatus = grep('100',MyExp1@StatusMsg)
    if (length(myStatus)==0){ # Not done yet!
      Sys.sleep(myWaitTime)
    }else{ # It's done
      GRAPLErDone = TRUE
    }
    RunningCounter = RunningCounter + 1
    RunningTime = RunningCounter * myWaitTime/60
    print(paste("Running time: ",RunningTime,' min, ',MyExp1@StatusMsg,sep=""))
  }
  
  if (GRAPLErDone){
    # when your experiment is 100% complete, you can download results
    MyExp1 <- GrapleGetExperimentResults(MyExp1)
    print(paste('Downloaded at',Sys.time()))
  }
}




