# Little script to create a batch file for LOTS of different MPs in mseR


# DeltaMult <- seq ( 0.5, 2, by = 0.5 )
# DeltaSD <- seq ( 0.1, 0.5, by = 0.1 )
# MortMult <- seq ( 0.5, 2, by = 0.5 )
# MortCV <- seq ( 0.2, 1.2, by = 0.2 )
pG <- seq ( from = 0.1, to = 2, by = 0.1)
pT <- seq ( from = 0.1, to = 0.5, by = 0.1)
# nG <- seq ( from = 1, to = 4, by = 1)
# rho <- seq ( 0.2, 1, by = 0.2 )
# freq <- c(1,4,9)

# posGrad <- seq ( 0,3, by = 0.3 )

mps <- expand.grid ( list ( pG = pG, pT = pT ) )

outFile <- "batchOut.bch"

for ( j in 1:(nrow ( mps )) )
{
  if ( j == 1 ) 
    cat ( "# Management Procedure ", j, " : pG", mps$pG[j], "/pT", mps$pT[j],"\n", sep = "", file = outFile,
         append = FALSE)
  else cat ( "# Management Procedure ", j, " : pG", mps$pG[j], "/pT", mps$pT[j],"\n", sep = "", file = outFile,
         append = TRUE )
  cat ( "#\n", file = outFile, append = TRUE )
  cat ( "mp$mp", j, "$gui$mpLabel 'pG", mps$pG[j], "/pT", mps$pT[j],"'\n", sep = "", append = TRUE, file = outFile )
  cat ( "mp$mp", j, "$mp$assess$dcacPosGrad ", mps$pG[j], "\n", sep = "", file = outFile, append = TRUE )
  cat ( "mp$mp", j, "$mp$assess$dcacPosTrendProb ", mps$pT[j], "\n", sep = "", file = outFile, append = TRUE )
  cat ( "#\n", file = outFile, append = TRUE )
}