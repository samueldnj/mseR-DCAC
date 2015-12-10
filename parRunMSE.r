#---------INSTRUCTIONS-----------#
# If running from R:
# cmdArg <- paste( "Rscript parRunMSE.r ", nBatchFiles, sep=" " )
# system( command=cmdArg, wait=T, ignore.stdout=T )

# or from Terminal (better)
# $ Rscript parRunMSE.r nBatchFiles
# where nBatchFiles is the number (integer) of files to process.
# At the moment, nBatchFiles = 20

# Function that calls runMSE
doBatchRun <- function( batchFolderName )
{
  require(tools)
  cat("Running batchjob:",batchFolderName,"\n")
  # set working directory to batchFolder
  setwd(batchFolderName)
  # source mseR
  source("mseR.r")
  # runMSE with the batch file
  # add random delay to offset simFolder names
  runMSE(ctl="simCtlFile.txt", folderName=batchFolderName) 
}

# Get command line arguments from system call
options( warn=-1 )
args             <- commandArgs(TRUE)
batchFolderNames <- vector("character",length=args[1])
for( i in 1:length(batchFolderNames) )
{
    # create batch folder i
    batchFolderNames[i] <- paste("mseRBat",i,sep="")
    dir.create(batchFolderNames[i])

    # copy in contents of mseRBatch folder
    file.copy( from=list.files("mseRBatch"), 
               to=batchFolderNames[i],
               recursive=TRUE)

    # get batchfilename
    batchFile <- paste(getwd(),"/mseRproject/batch/simCtlBat",i,".txt",sep="")
    # copy batchFile to simCtlFile
    simCtlFile <- paste(getwd(),"/",batchFolderNames[i],"/simCtlFile.txt",sep="")
    file.copy( from=batchFile, to=simCtlFile, recursive=TRUE)
    
    # save full path to batch folder
    batchFolderNames[i] <- paste(getwd(),"/mseRBat",i,sep="")

}

# load parallel processing "snow" package
require(snow)
require(parallel)
# Create cluster
cl     <- makeCluster( spec=rep("localhost",22), type="SOCK" )
# Run batch
tmp    <- clusterApply( cl, x=batchFolderNames, fun=doBatchRun )
stopCluster( cl )

# copy sim folders from each mseRBatX/mseRproject/sim.... file
# to the mseRproject folder in the working directory
require(stringr)
for( i in 1:length(batchFolderNames) )
{
    # get directory where sim folder is stored
    simDir <- paste(batchFolderNames[i],"/mseRproject/",sep="")
    
    # get sim folder name by matching the "sim" prefix
    tmpFolder <- str_match(string=list.files(simDir),pattern="sim_mseRBat.+")
    # Need to select the non-NA match
    tmpFolder <- tmpFolder[!is.na(tmpFolder)]
    # there should only be 1 folder by this name. In other contexts
    # the result may be a vector. 
    fromFile <- paste(simDir,tmpFolder,sep="")

    cat("\n", "Moving simulation ",i," sim folder to: ","\n",
        paste(getwd(),"/mseRproject/",sep=""),"\n")

    destination <- paste(getwd(),"/mseRproject/",sep="")
    dir.create(destination)
    system(command=paste("mv ",fromFile," ",destination, sep=""))
    
    cat("Removing mseRBat",i," folder...","\n")
    system(command=paste("rm -d -R ",batchFolderNames[i],sep=""))

}
cat("Batch run completed, open guiView() in R console...")
