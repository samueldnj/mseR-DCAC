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
