for( i in 3:20 )
{
  batPath <- paste(getwd(),"/mseRproject/batch/",
                   "simCtlBat",i,".txt",sep="")
  simPath <- paste(getwd(),"/simCtlFile.txt",sep="")
  file.copy( from=batPath, to=simPath, overwrite=TRUE )
  runMSE()
}