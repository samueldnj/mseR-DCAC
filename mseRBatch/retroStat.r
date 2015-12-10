retroData  <- read.table("retroFile.txt")
#retroData

# Number of timesteps to complete a retrospective analysis for
n <- 16

# Measure of overall disagreement between esimator and reference; large MSE inidicates large difference between retro estiamte and reference
# MSE: (1/n) * ((Est-True)^2)
MSE <- vector(mode="numeric")
for(i in 1:n) {
  MSE[i] <- (retroData[i+13,i] - retroData[i+13,17])^2
}
MSEStatRetro <- (1/n)*sum(MSE)

# SRE: measure of systematic pattern of difference from reference; consistent sign will result in a larger SRE, i.e. more "pattern" less random scatter
# SRE: sum(Est-True/True)
SRE <- vector(mode="numeric")
for(i in 1:n) {
  SRE[i] <- (retroData[i+13,i] - retroData[i+13,17])/retroData[i+13,17]
  }
SRERetro <- sum(SRE)
