require(snow)
require(stats)
# Function that calls DLL
doRefPts <- function( x, nAges=25, fmsy=0.2 )
{
  # DLL parameters
  dyn.load("refptsDLL2.so")
  ans <-   .C("refptsDLL2",nAges=as.integer(nAges),pars=as.double(x),
            FMSY=as.double(fmsy), BMSY=as.double(0.), MSY=as.double(0),
            " -nohess -nox -crit 1.e-3")
  dyn.unload("refptsDLL2.so")
  return( c(ans$BMSY, ans$MSY, ans$FMSY) )
}
# Read mcmc input
xMat <- as.matrix( read.table("xMat.txt", header=F, sep=" " ) )
# Get command line arguments from system call
args     <- commandArgs(TRUE)
nAges    <- as.integer(args[1]) 
fmsy     <- as.double(args[2])
fileName <- paste( args[3], ".txt", sep="" )
# Create cluster
cl     <- makeCluster( spec=rep("localhost",6), type="SOCK" )
# Run doRefPts DLL 
tmp    <- parRapply( cl, x=xMat, fun=doRefPts, nAges=nAges, fmsy=fmsy )
stopCluster( cl ) 
# tmp is a long vector, so convert to matrix
result <- matrix( tmp, nrow=nrow(xMat), ncol=3, byrow=T )
# then data frame for writing
result <- data.frame( BMSY=result[,1], MSY=result[,2], FMSY=result[,3] )
# Column headers for output file
hdr<-c("BMSY","MSY","FMSY")
# Write header
cat(file=fileName, hdr, "\n", sep=" ", append=F)
# Append results
write.table(x=result, file=fileName, col.names=F, row.names=F, sep=" ", append=T)

#---------INSTRUCTIONS-----------#
## Note "##" is comment and "#" is
## code that should be uncommented
## The code below should be run from a stand alone
## R script.

#---------INSTRUCTIONS-----------#

## Write mcmc output to text file
# fileName <- "xMat.txt"
## The object mcmcTmp is a matrix of parameters output from an mceval call
# write.table( x=as.data.frame(mcmcTmp), file=fileName, col.names=F, row.names=F,
#             sep=" ", append=F )

## Call separate R code as script to eliminate ADMB sqwuacking...that
## makes system call unuseable.
# cmdArg <- paste( "Rscript doRefPtsDLL.r", nAges, fmsy, "refPts", sep=" " )
# system( command=cmdArg, wait=T, ignore.stdout=T )

## Get table of ref pts from file.
# result <- as.matrix( read.table( file="refPts.txt", header=T ) )

