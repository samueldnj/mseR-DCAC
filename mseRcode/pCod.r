# Read the mcmc files.

#pCodRt <- read.table( "recDevs.mcmc", as.is=TRUE, header=FALSE, sep=" " )
#pCodRt <- pCodRt[ ,-1 ]
#pCodFt <- read.table( "ft.mcmc", as.is=TRUE, header=FALSE, sep=" " )
#pCodFt <- pCodFt[ ,-1 ]

# Extract the mean Rt and Ft, first column is NA, so years 1956:2012 go from col 2-58.

#meanRt <- apply( log(pCodRt),2,mean )
#meanFt <- apply( pCodFt,2,mean )

# Read a rep file.
pCodRep <- read.rep( "iscamdelaydiff.rep" )
print( names( pCodRep ) )

result <- cbind( Year=pCodRep$yr, omegat=pCodRep$log_rec_devs, Ft=pCodRep$ft[1,] )

write.table( result, file="simHistoryFile_PacificCod.csv", row.names=FALSE, col.names=TRUE, sep="," )