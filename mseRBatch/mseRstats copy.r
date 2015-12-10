# Tasks.

# 1. Duration, State, and Probability statistics.
# 2. Time to USR, TRP.

#------------------------------------------------------------------------------#
#---------------- mseRstats.r: Performance Statistics Functions ---------------#
#------------------------------------------------------------------------------#
#                                                                              #
# (c) mseR: Management Strategy Evaluation in R.                               #
#                                                                              #
#     Copyright 2008, 2009 by A.R. Kronlund and S.P. Cox.                      #
#     This software comes with ABSOLUTELY NO WARRANTY, expressed or implied.   #
#     You have been provided with a copy of mseR for educational purposes.     #
#     You are requested not to redistribute this software without seeking      #
#     permission from the authors.                                             #
#                                                                              #
#     Of course, comments and suggestions for improvement greedily accepted.   #
#                                                                              #
#      "Pretty good management will do."  Bill de la Mare, Dec. 19, 2006.      #
#                                                                              #
#          "Success consists of going from failure to failure without          #
#                  loss of enthusiasm."  Winston Churchill.                    #
#                                                                              #
# CONVENTIONS:                                                                 #
#                                                                              #
# Standard C++ variable naming conventions are applied.  The first letter of a #
# variable name is lower case.  Each concatenated word or abbreviation is      #
# capitalized. For example, Limit Reference Multiplier becomes limitRefMult.   #
#                                                                              #
# NOTES:                                                                       #                                                                              #
# 1. To display hidden functions, e.g., .foo, enter command: ls(all.names=TRUE)#
# 2. .loadVueFile is not actually used here, but is useful for future work     #
#                                                                              #
#------------------------------------------------------------------------------#
#--                                                                          --#
#-- mseRstats.r: An mseR module that computes simulation performance         --#
#                    statistics and saves output to an R object and Excel.   --#
#--                                                                          --#
#-- Authors: S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--          A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--                                                                          --#
#--          T.K. Deering  (contributions from PopSim)                       --#
#--                                                                          --#
#-- First Implementation: 11-Feb-09.                                         --#
#--                                                                          --#
#-- Updates: 01-Sept-09; K.Holt addedd additional sim summary tables that    --#
#--            exclude replicates in which stock assessment model failed to  --#
#--            converge                                                      --#
#                                                                              #                                                                                                                                                          #
# --------------- Performance Statistics Functions (Hidden) ------------------ #
#                                                                              #
# .calcPerfStats      : Calculate performance statistics (calls .calcStatsXXX) #
#                       where XXX refers to one of the functions below.        #
#                                                                              #
# .calcStatsAAV       : Calculate quantiles of AAV statistics.                 #
# .calcStatsCatch     : Calculate quantiles of average catch statistics.       #
# .calcStatsDepletion : Calculate quantiles of average depletion statistics.   #
# .calcStatsFinalDep  : Calculate quantiles of final depletion statistics.     #
# .calcStatsLowCat    : Calculate quantiles of low catch statistics.           #
# .calcStatsLowDep    : Calculate quantiles of low depletion statistics.       #
# .calcStatsPolicy    : Calculate statistics related to Limit and Bmsy policy. #
# .calcStatsTarget    : Calculate target statistics WRT dep, time, certainty.  #
# .calcStatsZones     : Calculate prop. of time SSB in each stock status zone. #
#                                                                              #
# ------------------------ Helper Functions (Hidden) ------------------------- #                                                   
# .excelTable          : Creates and saves a dataframe to Microsoft Excel table#                                                    
# .getSimParsFromRdata : Extracts simulation parameters from simGuiPars.       #
#                                                                              #
# ----------------------- Help Functions (Public) ---------------------------- #
# saveToExcel          : Save data in Excel format.                            #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# ------------------- Performance statistic functions  ----------------------- #
#------------------------------------------------------------------------------#

# .calcStatsAAV (Calculate quantiles of Average Annual Varability statistics)
# Purpose:      Calculate quantiles of AAV statistics over simulation replicates.
# Parameters:   Dt    - catch biomass as an nRep by nT matrix.
#               tdx   - indices of time period for statistics.
#               probs - probabilities for quantiles.
# Returns:      val, a list with the median AAV over replicates and a vector
#               with the quantiles specified by probs.
# Notes:        In contrast to other .calcStatsXXX functions, this function
#               requires the entire 1:nT time period because of the lag in the
#               AAV calculations.
# Source:       A.R. Kronlund
.calcStatsAAV <- function( Dt, tdx, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # The period must be of length >= 2.  Can't form the difference otherwise.
  if ( length(Dt) >= 2 )
  {  
    # Form the absolute catch differences over 1:nT by replicate.      
    diffDt    <- t( apply( Dt,1,diff ) )
    absDiffDt <- abs( diffDt )

    # Shift differences by 1 to catch transition from previous period
    # to current period, i.e., sum(|C_t-C_t-1|) for t=t1,..,t2.  For example,
    # if the current year is 2008, you have to get the first catch difference
    # of 2008-2007 to respect C_t - C_t-1, i.e., when t=2008, t-1=2007.

    # Sum the absolute differences of the shifted absolute catch differences.
    sumAbsDiffDt <- apply( absDiffDt[,(tdx-1), drop=FALSE ],1,sum )
      
    # Sum the catch by replicate over the summary period specified by tdx.
    sumCatch <- apply( Dt[,tdx, drop=FALSE ], 1, sum )
      
    # Compute the AAV by replicate.
    AAV <- ifelse( sumCatch > 0.0, sumAbsDiffDt / sumCatch * 100.0, 0.0 )
    # Compute the median of the AAV values for each replicate.
    medAAV <- median( AAV )
    # Compute the quantiles of the distribution.
    # Use the quantiles specified in the interface (qLower, qUpper).
    qVals <- quantile( AAV, probs=probs )
  }
  else
  {
    # If the length of Dt is insufficent to form AAV, everything gets -1.
    medAAV <- -1
    qVals <- rep( -1, length(probs) )
  }
  val <- list( medAAV=medAAV, qVals=qVals )
  
  val
}


# .calcStatsCatch (Calculate quantiles of average catch statistics)
# Purpose:        Calculate quantiles of average catch statistics over
#                 simulation replicates.
# Parameters:     Dt    - catch biomass as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average catch over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsCatch <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Compute average catch by replicate over the range specified in tdx.
  avgCatch <- apply( Dt,1,mean )
  # Compute the median of the average catch values.
  medAvgCatch <- median( avgCatch )
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgCatch, probs=probs )
  val <- list( medAvgCatch=medAvgCatch, qVals=qVals )
  val
}


# .calcStatsDepletion (Calculate quantiles of average depletion statistics)
# Purpose:        Calculate quantiles of average depletion statistics over
#                 simulation replicates.
# Parameters:     Dept  - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average depletion over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsDepletion <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Average depletion by replicate (avgDept is vector len=ncol(Dept)).
  avgDep <- apply( Dept,1,mean )
  # Compute the median of the average depletion values.
  medAvgDep <- median( avgDep ) 
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgDep, probs=probs )
  val <- list( medAvgDep=medAvgDep, qVals=qVals )
  val
}


# .calcStatsFinalDep (Calculate quantiles of final depletion statistics)
# Purpose:        Calculate quantiles of final depletion statistics over
#                 simulation replicates.
# Parameters:     Dep   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median final depletion over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsFinalDep <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the final depletion in the period passed to function.
  # The passed Dept is an nRep by length(t1:t2) matrix.  Thus, ncol(Dept)
  # is the last column (t2) in the matrix - the final depletion for the period.
  finalDep <- Dept[ ,ncol(Dept) ]
  # Compute the median of the final depletion values, final year of period over reps.
  medFinalDep <- median( finalDep )
  # Compute the quantiles of the distribution.
  qVals <- quantile( finalDep, probs=probs )
  val <- list( medFinalDep=medFinalDep, qVals=qVals )
  val
}


# .calcStatsLowCat (Calculate quantiles of low catch statistics)
# Purpose:         Calculate quantiles of low catch statistics over
#                  simulation replicates.
# Parameters:      Dt  - catch as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low catch over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsLowCat <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low catch values from the replicate.
  # This apply operation returns a vector of length=nRep.
  lowCat <- apply( Dt,1,min )
  # Compute the median of the low depletion values for the period over reps.
  medLowCat <- median( lowCat )
  # Compute the quantiles of the distribution.
  qVals <- quantile( lowCat, probs=probs )
  val <- list( medLowCat=medLowCat, qVals=qVals )
  val
}


# .calcStatsLowDep (Calculate quantiles of low depletion statistics)
# Purpose:         Calculate quantiles of low depletion statistics over
#                  simulation replicates.
# Parameters:      Dept  - depletion as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low depletion over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsLowDep <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low depletion values from the replicate.
  # This apply operation returns a vector of length=nRep.
  lowDep <- apply( Dept,1,min )
  # Compute the median of the low depletion values for the period over reps.
  medLowDep <- median( lowDep )
  # Compute the quantiles of the distribution.
  qVals <- quantile( lowDep, probs=probs )
  val <- list( medLowDep=medLowDep, qVals=qVals )
  val
}


# .calcStatsPolicy (Calculate statistics related to Limit and Bmsy policy)
# Purpose:         (1) Calculate median probability of depletion > Dmsy for
#                      the time period over replicates,
#                  (2) Calculate the median probability of depletion >
#                      Dep at the LRP for the time period over replicates.
# Parameters:      Dept  - depletion as an nRep by length(t1:t2) matrix,
#                          where (t1:t2) defines the summary period.
#                  Dmsy  - depletion at Bmsy.
#                  zoneLimit - depletion at Critical-Cautious zone boundary.
#                  tMP   - start year of management procedure.
# Returns:         val, a list with the median statistics for the following:
#                  Pr(Dep>=Dmsy), Pr(Dep<Dmsy), Pr(Dep>=Dlimit), Pr(Dep<Dlimit).
# Source:          A.R. Kronlund
.calcStatsPolicy <- function( Dept, Dmsy, zoneLimit, tMP )
{
  isGTE <- function( x, bound )
  {
    result <- rep( "Below",length(x) )
    result <- ifelse( x >= bound, "Above", result )
    result
  }
  
  calcPcats <- function( x, bound )
  {
    # Calculate number and proportion GTE specified bound for each replicate.
    tmp <- t( apply( x,1,FUN=isGTE, bound ) )

    catLabels <- c( "Below","Above" )
    nCats <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(catLabels) ) )
    names( nCats ) <- catLabels
    pCats <- nCats
  
    for ( i in 1:nrow(tmp) )
      nCats[ i, ] <- table( factor(tmp[i,],levels=catLabels ) )
  
    # Calculate the proportions.
    pCats <- nCats / apply( nCats,1,sum )
    
    result <- list( nCats=nCats, pCats=pCats )
    result
  }
  
  # Calculate the median proportion GTE Bmsy.
  tmp <- calcPcats( Dept, Dmsy )
  medProbGteDmsy <- median( tmp$pCats[,"Above"] )
  medProbLtDmsy  <- median( tmp$pCats[,"Below"] )

  # Calculate the median proportion GTE zoneLimit.
  tmp <- calcPcats( Dept, zoneLimit )
  
  medProbGteLimit <- median( tmp$pCats[,"Above"] )
  medProbLtLimit  <- median( tmp$pCats[,"Below"] )
  
  # Now calculate irrespective of replicates.
  probGteLimit <- sum( Dept >= zoneLimit ) / length( Dept )
  probGteDmsy  <- sum( Dept >= Dmsy      ) / length( Dept )
  
  val <- list( medProbGteDmsy=medProbGteDmsy,   medProbLtDmsy=medProbLtDmsy,
               medProbGteLimit=medProbGteLimit, medProbLtLimit=medProbLtLimit,
               probGteLimit=probGteLimit, probGteDmsy=probGteDmsy )
  val    
}


# .calcStatsTarget (Calculate target statistics WRT dep, time, certainty)
# Purpose:         Calculate statistics for depletion, year and probability
#                  where the response variable is conditional on the remaining
#                  two variables.  Response variable determined by outcome.
#
# Parameters:  Dept       - entire nRep by length(1:nT) matrix of depletion.
#              tMP        - start year of management procedure.
#              objDep  - user specified depletion.
#              objYear - user specified year.
#              objProb - user specified probability (certainty).
#              outcome - one of "dep", "year", "prob" to indicate target outcome.
# Returns:     result, a list with all three variables outcome to indicate which
#                      is the response.
# Source:          A.R. Kronlund
.calcStatsTarget <- function( Dept, tMP, objDep, objYear, objProb, outcome )
{
  # Assume that the entire time series is passed as nRep by nT matrix.

  nT   <- ncol( Dept )           # Total number of time steps.
  tVec <- c( 1:nT )              # Vector of time index values.
  tdx  <- c( tMP:nT )            # Time index values for projection period.

  # (1) Calculate year when objDep is first attained with targetProb certainty.
  if ( outcome=="year" )
  {
    # Calculate the quantile coresponding to 1-objProb at each time step.
    # This means that objProb*100% of the distribution of depletion values
    # is greater than the quantile at each time step. This is a 1:nT vector.

    qObj <- apply( Dept, 2, quantile, probs=c(1.0-objProb) )

    # Find the index of the first PROJECTION time step where qObj >= objDep,
    # i.e., where the depletion quantile corresponding to objProb is greater
    # than the objDep.  This means you are objProb*100% sure that the objDep
    # has been attained.  No guarantee it will remain attained.

    isObj  <- qObj >= objDep           # TRUE/FALSE vector size 1:nT.
    tdxObj <- tdx[isObj[ tdx ]]        # TRUE in projection period.

    if ( !all(is.na(tdxObj)) )
      objYear <- min( tdxObj )         # 1st time step in projection period.
    else
      objYear <- 0                     # No values GTE the targetDep.
    
    cat( "\nMSG (.calcStatsTarget) Year = ",objYear," when depletion >=",objDep,
         "with",objProb,"probability.\n" )  
  }     # if outcome is "year".

  else if ( outcome=="prob" )
  {
    # (2) Calculate the quantile of objDep at the objYear index (tMP,...,nT).

    # Here we find the proportion of depletion values at time objYear that
    # exceed the objDep, i.e., certainty of exceeding objDep at objYear.

    depVals <- Dept[ ,objYear ]
    objProb <- sum( depVals >= objDep ) / nrow(Dept)
    
    cat( "\nMSG (.calcStatsTarget) Probability is",objProb,"that depletion exceeds",
         objDep,"at year",objYear,"\n" )
  }     # if outcome="prob".

  else if ( outcome=="dep" )
  {
    # (3) Calculate the depletion at objYear and objProb.
    depVals <- Dept[ ,objYear ]    
    objDep <- quantile( depVals,probs=(1.0-objProb) )
    
    cat( "\nMSG (.calcStatsTarget) Depletion >=",objDep," at year",objYear,
         "with",objProb,"probability.\n" )
  }     # if outcome="dep".

  # Return a list result.
  result <- list( objDep=objDep, objYear=objYear, objProb=objProb, outcome=outcome )
  result
}     # END function .calcStatsTarget


# .calcStatsZones (Calculate proportion of time SSB in each stock status zone)
# Purpose:        Calculate quantiles of the proportion of time (probability)
#                 that the SSB was in the Critical, Cautious, and Healthy stock
#                 status zones during time period over the simulation replicates.
# Parameters:     Dept   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 limitD - boundary Crit-Cautious zones on the depletion scale.
#                 upperD - boundary Cautious-Healthy zones on depletion scale.
#                 probs  - probabilities for quantiles (not used yet).
# Returns:        val, a list with the median final depletion over replicates
# Source:         A.R. Kronlund     
.calcStatsZones <- function( Dept, LRP, USR )
{
  # At this time the probs are not used. Only median statistics are computed.
  # Assumed that all stats are in terms of depletion.

  findZoneDFO <- function( x, LRP, USR,
                  zoneLabels=c(.CriticalLAB, .CautiousLAB, .HealthyLAB) )
  {
    val <- rep( zoneLabels[1],length(x) )
    val <- ifelse( x > LRP, zoneLabels[2], val )
    val <- ifelse( x > USR, zoneLabels[3], val )
    val
  }
  
  zoneLabels <- c( .CriticalLAB, .CautiousLAB, .HealthyLAB )
  
  # Calculate number in zone (Critical, Cautious Healthy) for each replicate.
  tmp <- t( apply( Dept,1,FUN=findZoneDFO, LRP, USR, zoneLabels ) )

  nZone <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(zoneLabels) ) )
  for ( i in 1:nrow(tmp) )
    nZone[ i, ] <- table( factor(tmp[i,],levels=zoneLabels ) )
  
  # Calculate the proportions by rows (each replicate).
  pZone <- nZone / apply( nZone,1,sum )
 
  names( pZone ) <- zoneLabels
  
  # Now calculate the medians.
  medCriticalP <- median( pZone[,zoneLabels[1]] )
  medCautiousP <- median( pZone[,zoneLabels[2]] )
  medHealthyP  <- median( pZone[,zoneLabels[3]] )
  
  # Now calculate the proportions ignoring replicates.
  nAllZone <- table( cut( Dept, breaks=c(0,LRP,USR,max(Dept) ) ) )
  pAllZone <- nAllZone / length(Dept)
  
  val <- list( medCriticalP=medCriticalP, medCautiousP=medCautiousP,
               medHealthyP=medHealthyP, nZone=nZone, pZone=pZone,
               nAllZone=nAllZone, pAllZone=pAllZone )
  val    
}     # END function .calcStatsRefPts


# .calcStatsBmsy (Calculate proportion of time SSB above BMSY target)
# Purpose:        Calculate quantiles of the proportion of time (probability)
#                 that the SSB was greater than Bmsy during time period over the 
#                  simulation replicates.
# Parameters:     Dept   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 BmsyD - Bmsy on the depletion scale.
#                 probs  - probabilities for quantiles (not used yet).
# Returns:        val, a list with the median final depletion over replicates
# Source:         K.Holt; modified from A.R. Kronlund .calcStatsZones
.calcStatsBmsy <- function( Dept, Dmsy, probs )
{
  # At this time the probs are not used. Only median statistics are computed.

  findZone <- function( x, Dmsy )
  {
    zone <- rep( "belowBmsy",length(x) )
    zone <- ifelse( x > Dmsy, "aboveBmsy",zone )
    zone
  }
  
  nYrs <- ncol(Dept)
  
  # Calculate number in zone (Critical, Cautious Healthy) for each replicate.
  tmp <- t( apply( Dept,1,FUN=findZone, Dmsy ) )

  zoneLabels <- c( "belowBmsy","aboveBmsy" )
  nZone <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(zoneLabels) ) )
  names( nZone ) <- zoneLabels
  pZone <- nZone
  
  for ( i in 1:nrow(tmp) )
    nZone[ i, ] <- table( factor(tmp[i,],levels=zoneLabels ) )
  
  # Calculate the proportions.
  pZone <- nZone / apply( nZone,1,sum )
  
  # Now calculate the medians.
  medAboveBmsyP <- median( pZone[,"aboveBmsy"] )
  medBelowBmsyP <- median( pZone[,"belowBmsy"] )
  
  # Now calculate the proportions ignoring replicates.
  nAllZone <- table( cut( Dept, breaks=c(0,Dmsy,max(Dept) ) ) )
  pAllZone <- nAllZone / length(Dept)
  
  val <- list( medBelowBmsyP=medBelowBmsyP, medAboveBmsyP=medAboveBmsyP,
             nZone=nZone, pZone=pZone, nAllZone=nAllZone, pAllZone=pAllZone )
  val    
}


# .calcPerfStats (Calculate performance statistics by calling .calcStatsXXX).
# Purpose:       Calculate various performance statistics with the function
#                name .calcStatsXXX, where XXX determined the statistics.
#                (1) Read the simulation tracking file to get the simulation
#                    ID, simulation label, scenario label, and procedure label.
#                (2) Make a data frame that holds the period definitions from
#                    guiPerf entry fields.
#                (3) Build a result data frame that has the simulation identifier
#                    fields, period definitions and statistics as columns. Each
#                    row respresents the peformance statistics for one simulation
#                    and time period.
#                (4) Loop through the time periods for each simulation and
#                    calculate the performance statistics for each combination.
# Parameters:    NONE.
# Returns:       A list with components simSummary1 (spreadsheet "view") and
#                simSummary2 (normalized "Access view").
# Source:        A.R. Kronlund
.calcPerfStats <- function()
{
  # Get the GUI parameters to calculate periods, local scope.
  guiInfo  <- getWinVal( scope="L" )

  # trackObj is a 50 row dataframe with the simulation info, plus
  # group ID (trackObj$group) and rank.
  trackObj <- .getTrackingData( .PRJFLD )

  nSims    <- .getNumSims( trackObj )
  
  trackObj$simID     <- as.character( "Sim",c(1:nSims),sep="" )
  trackObj$simLabel  <- as.character( paste( trackObj$scenario,trackObj$mp,sep="-" ) )
  trackObj$scenario  <- as.character( trackObj$scenario )
  trackObj$procedure <- as.character( trackObj$mp )
  
  # Number of years in simulations.
  nT  <- trackObj$nT[1]  
  tMP <- trackObj$tMP[1]
  
  quantVals <- c( 0.05, pfQlower, 0.5, pfQupper, 0.95 )
  
  # Make a period object with columns "pName","t1", and "t2".
  period <- data.frame( pName=c("Short","Medium","Long"),
              t1=c( pfShort1, pfMed1, pfLong1 ), t2=c( pfShort2, pfMed2, pfLong2),
              stringsAsFactors=FALSE )

  # Make an output object for performance statistics, there are nSim*3 rows
  # because of Short, Medium, and Long summary period.  The number of columns
  # is determined by the simID, simLabel, scenario, mp, period, t1, t2,
  # plus the number of statistics calculated.
  
  nResults    <- nSims * 3
  headerNames <- c("simID","simLabel","scenario","procedure","period","t1","t2")
  statNames   <- c( "medAvgDep","Q1AvgDep","Q2AvgDep",
                    "medFinalDep","Q1finalDep","Q2finalDep",
                    "medLowDep","Q1LowDep","Q2LowDep",
                    "medAAV", "Q1AAV","Q2AAV",
                    "medAvgCatch","Q1AvgCatch","Q2AvgCatch",
                    "medLowCatch","Q1LowCatch","Q2LowCatch",
                    "medProbCritBmsy","medProbCautBmsy","medProbHealBmsy",
                    "medProbCritB0","medProbCautB0","medProbHealB0",                    
#                    "criticalP","cautiousP","healthyP",
                    "yearAtDepProb","probAtDepYear","depAtYearProb",
#                    "medProbGteDmsy","medProbGteLimit",
                    "probGteDmsy","probGteLimit", "omittedReps" )
  colNames    <- c( headerNames, statNames )
  
  # Make results dataframe for case where all replicates used to calculate stats
  result      <- data.frame( matrix( NA, nrow=nResults,ncol=length(colNames) ) )
  names( result ) <- colNames
 
  # This block loops over valid simulations and finds the union of failed reps.
  acrossFailList <- NULL
  if ( pfFailConv==FALSE & pfFailAcross==TRUE )
  {
    tmpFails <- NULL
    cat( "\nMSG (.doGuiPerfPlots) Checking for convergence fails across simulations...\n" )
    
    for ( i in 1:nSims )
    {
      # Load an Rdata working directory containing a list called blob.
      simFile     <- trackObj[ i, "simFile" ]
      simFolder   <- trackObj[ i, "simFolder" ]
      simFilePath <- file.path( .PRJFLD, simFolder, simFile )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.doGuiPerfPlots) Loading",simFilePath,"...\n" )     
      load( file=simFilePath )      
      
      failList <- .getEstimationFails( blob$mp$assess$runStatus,
                     keepFailRefPts=TRUE, keepFailConv=pfFailConv )      

      
      tmpFails <- c( tmpFails, failList$repsFail )
    }

    acrossFailList <- list( failRefPts=NULL, nFailRefPts=0, failConv=tmpFails,
                        repsFail=tmpFails, nRepsFail=length(tmpFails) )
    rm(blob)
  }
  
  # Initialize row counters.
  iRow    <- 0
     
  # Load and calculate statistics for each simulation in tracking object.
  for ( i in 1:nSims )
  {
    # Remove whitespace from field read from GUI text box.
    fileName <- gsub( " ", "", trackObj$Rdata[i] )
     
    simFile     <- trackObj[ i, "simFile" ]
    simFolder   <- trackObj[ i, "simName" ]
    simGroup    <- trackObj[ i, "group"   ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    if( .NONSTATB0 & (trackObj[i,"mp"] != "NoFish") )
    {
      # load NoFish MP for this group
      # Get the NoFish row
      gRow <- NA
      gRow <- which(trackObj$group==simGroup & trackObj$mp=="NoFish")
      nofishFile     <- trackObj[ gRow, "simFile" ]
      nofishFolder   <- trackObj[ gRow, "simName" ]
      nofishFilePath <- file.path( .PRJFLD, 
                                  nofishFolder, 
                                  nofishFile 
                                  )

      # Load blob for NoFish MP
     
      load( file=nofishFilePath )
      assign( "blob", blob, pos=1 )

      # Extract biomass matrix
      nsB0 <- blob$om$Bt[ ,c(2:ncol(blob$om$Bt)), drop=FALSE ]

      remove(blob,pos=1)
      remove(blob,pos=-1)

    }

    cat( "\nMSG (.calcPerfStats) Loading",simFilePath,"...\n" )    
    load( file=simFilePath )

    assign( "blob", blob, pos=1 )            
    
    tMP    <- blob$ctlList$opMod$tMP
    B0     <- blob$ctlList$opMod$B0
    Bmsy   <- blob$refPtList$ssbFmsy
    Dmsy   <- Bmsy / B0
    
    # Get the LRP, USR, TRP status values related to objectives 
    # for Bmsy and B0.
    LrpBmsy <- pfLimitMultBmsy  * Bmsy
    UsrBmsy <- pfUpperMultBmsy  * Bmsy
    TrpBmsy <- pfTargetMultBmsy * Bmsy
    
    LrpB0   <- pfLimitMultB0  * B0
    UsrB0   <- pfUpperMultB0  * B0
    TrpB0   <- pfTargetMultB0 * B0
    
    # Define on depletion scale as well.
    LrpBmsyDep <- LrpBmsy / B0
    UsrBmsyDep <- UsrBmsy / B0
    TrpBmsyDep <- TrpBmsy / B0
    
    if( .HISTLRP )
      LrpB0 <- min(blob$om$Bt[ ,c(1:tMP), drop=FALSE ])
      
    LrpB0Dep   <- LrpB0 / B0
    UsrB0Dep   <- UsrB0 / B0
    TrpB0Dep   <- TrpB0 / B0
    
    # Get the variables to be summarized, these are nReps by nT matrices.
    Bt   <- blob$om$Bt
    Dt   <- blob$om$Dt

    # If there is not a failList (across sims) supplied, get one for this sim.
    if ( is.null( acrossFailList ) )
    {
      failList <- .getEstimationFails( blob$mp$assess$runStatus,
                     keepFailRefPts=TRUE, keepFailConv=pfFailConv )
    }
    else
      failList <- acrossFailList
    
    # Take away the failed reps (blob is a local copy from load).
    if ( failList$nRepsFail > 0 )
    {
      repsFail <- failList$repsFail
      Bt <- Bt[ !is.element( Bt[,"iRep"], repsFail ),,drop=FALSE ]
      Dt <- Dt[ !is.element( Dt[,"iRep"], repsFail ),,drop=FALSE ]
    }    
    
    Bt <- Bt[ ,c(2:ncol(Bt)), drop=FALSE ]
    Dt <- Dt[ ,c(2:ncol(Dt)), drop=FALSE ]

    # Fill depletion matrix based on constant B0
    Dept <- Bt / B0
    # For non-stationary projections, replace B0 with
    # the NoFish MP
    if( .NONSTATB0 & (trackObj[i,"mp"] != "NoFish") )
    {
      Dept[,tMP:nT] <- Bt[,tMP:nT]/nsB0[,tMP:nT]
    }
       
    # Accumulate statistics for each period.
    for ( j in 1:nrow(period) )
    {
      # Get the header information for this simulation and period.
      iRow <- iRow + 1
      result[ iRow, "simID" ]     <- trackObj$simID[i]
      result[ iRow, "simLabel" ]  <- trackObj$simLabel[i]
      result[ iRow, "scenario" ]  <- trackObj$scenario[i]
      result[ iRow, "procedure" ] <- trackObj$procedure[i]
      result[ iRow, "period" ]    <- period$pName[j]
      result[ iRow, "t1"     ]    <- period$t1[j]
      result[ iRow, "t2"     ]    <- period$t2[j]
             
      # Get the time index values that span this period.
      tdx <- c( period$t1[j]:period$t2[j] )
       
      #--- Depletion Statistics                                             ---#

      # Aggregated over period.
      tmp <- .calcStatsDepletion( Dept[,tdx, drop=FALSE ], quantVals )       
      result[ iRow, "medAvgDep" ] <- tmp$medAvgDep
      result[ iRow, "Q1AvgDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgDep" ]  <- tmp$qVals[4]
            
      # Final depletion for period.
      tmp <- .calcStatsFinalDep( Dept[,tdx, drop=FALSE ], quantVals )
      result[ iRow, "medFinalDep" ] <- tmp$medFinalDep
      result[ iRow, "Q1finalDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2finalDep" ]  <- tmp$qVals[4]
     
      # Low depletion for period.
      tmp <- .calcStatsLowDep( Dept[,tdx, drop=FALSE ], quantVals )
      result[ iRow, "medLowDep" ] <- tmp$medLowDep
      result[ iRow, "Q1LowDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2LowDep" ]  <- tmp$qVals[4]
     
      #--- Catch Statistics                                                 ---#
      tmp <- .calcStatsCatch( Dt[,tdx, drop=FALSE ], quantVals )
      result[ iRow, "medAvgCatch" ] <- tmp$medAvgCatch
      result[ iRow, "Q1AvgCatch" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgCatch" ]  <- tmp$qVals[4]
     
      # Low catch for period.
      tmp <- .calcStatsLowCat( Dt[,tdx, drop=FALSE ], quantVals )
      result[ iRow, "medLowCatch" ] <- tmp$medLowCat
      result[ iRow, "Q1LowCatch" ]  <- tmp$qVals[2]
      result[ iRow, "Q2LowCatch" ]  <- tmp$qVals[4]
      
      #--- AAV Catch Statistics                                             ---#
        
      tmp <- .calcStatsAAV( Dt, tdx, quantVals )
      result[ iRow,"medAAV" ] <- tmp$medAAV
      result[ iRow,"Q1AAV" ]  <- tmp$qVals[2]
      result[ iRow,"Q2AAV" ]  <- tmp$qVals[4]

      #--- Objectives Statistics                                             --#
      
      # Note that the LRP and USR the limitBoundMult and upperBoundMult,
      # respectively, not the HCR reference points, and for either (B0, Bmsy).
      # They are translated from the SSB scale to the depletion scale above.
      
      # NOTE: The problem with taking the median prob of depletion is that they
      # do not necessarily sum to 1 over the Cautious, Critical, Healthy zones.
      
      tmp <- .calcStatsZones( Dept[,tdx, drop=FALSE ],LrpBmsyDep,UsrBmsyDep )
      result[ iRow,"medProbCritBmsy"    ] <- tmp$medCriticalP
      result[ iRow,"medProbCautBmsy"    ] <- tmp$medCautiousP
      result[ iRow,"medProbHealBmsy"     ] <- tmp$medHealthyP
      
      tmp <- .calcStatsZones( Dept[,tdx, drop=FALSE ],LrpB0Dep,UsrB0Dep )
      result[ iRow,"medProbCritB0"    ] <- tmp$medCriticalP
      result[ iRow,"medProbCautB0"    ] <- tmp$medCautiousP
      result[ iRow,"medProbHealB0"    ] <- tmp$medHealthyP
      
      #--- Objective Statistics                                              --#
      
      # Note that the entire projection period is required here, not
      # just the current period of interest.
      #tmp <- .calcStatsTarget( Dept, tMP, targDep, targYear, targProb )
      #result[ iRow,"yearAtDepProb"  ] <- tmp$yearAtDepProb
      #result[ iRow,"probAtDepYear"  ] <- tmp$probAtDepYear
      #result[ iRow,"depAtYearProb"  ] <- tmp$depAtYearProb
  
      #--- Policy Statistics                                                 --#
      
      #tmp <- .calcStatsPolicy( Dept[,tdx], Dmsy, limitD, tMP )
      #result[ iRow,"medProbGteDmsy"  ] <- tmp$medProbGteDmsy
      #result[ iRow,"medProbGteLimit" ] <- tmp$medProbGteLimit
      #result[ iRow,"probGteDmsy"     ] <- tmp$probGteDmsy
      #result[ iRow,"probGteLimit"    ] <- tmp$probGteLimit
                                                                             
      # --- Covergence Statistics                                            --#
      #result[ iRow,"omittedReps"     ] <- 0 
    }     # Loop over j periods.
    
    # Do garbage collection to clean memory of loose ends, accumulated blobs.    
    remove( blob, pos=1 )
    gc()
    
  }   # Loop over i simulations.
  
  result2 <- data.frame( matrix( NA, nrow=(nrow(result)*length(statNames)),
                           ncol=(length(headerNames)+2) ) )                           
  names( result2 ) <- c( headerNames,"statistic","value" )
  
  # Now format simulation summary in normalized form.
  iRow <- 0
  for ( i in 1:nrow( result ) )
  {
    for ( j in 1:length(statNames) )
    {
      iRow <- iRow + 1
      
      result2[ iRow,c(1:length(headerNames)) ] <- result[ i,c(1:length(headerNames)) ]
      result2[ iRow,"statistic" ] <- statNames[j]
      result2[ iRow,"value" ]     <- result[ i,(length(headerNames)+j) ]
    }
  }
  
  perResult   <- 0     # Not used at this time.
  simSummary1 <- result
  simSummary2 <- result2

  return( list( simSummary1=simSummary1, simSummary2=simSummary2,
                perResult=perResult ) )
}     # END function .calcPerfStats

#------------------------------------------------------------------------------#
#--- Helper Functions (Hidden)                                              ---#
#------------------------------------------------------------------------------#

# .excelTable (Creates and saves a dataframe to Microsoft Excel table)
# Purpose:    For a given connection and input data, create a dataframe and
#             save the dataframe as a worksheet in Excel.
#             If an Excel .xls file with the same name already exists then
#             delete the file and create a new file, else create a new file.
# Parameters: channel is a RODBC connection string with the .xls file name
#             dat       : a list, vector, or matrix containing worksheet data.
#             tablename : character variable containing the string to appear on
#                         Excel tabs for each worksheet.
#             colnam    : a vector of column names with length ncol(dat).
#             rownam    : a vector of row names with length nrow(dat).
# Returns:    NULL
# Source:     Modified from T.K. Deering (PopSim.r).
.excelTable <- function( channel, dat, tablename, colnam, rownam )
{
  dframe             <- as.data.frame( dat )
  names( dframe )    <- colnam
  rownames( dframe ) <- rownam
  sqlSave( channel, dframe, tablename=tablename )
  
  return()
}


# .getSimParsFromRdata (Extracts simulation parameters from simGuiPars)
# Purpose:    Extracts the simulation parameters from the simGuiPars component
#             of a saved simulation .Rdata file.  Uses .unEvalList to remove
#             any list nesting structure.
# Parameters: hdrObj - the header object from guiView or guiPerf, essentially
#                      holding the simulation tracking information.
# Returns:    result - a data frame where the rows are simulation parameters
#                      and each simulation is stored in the columns.
# Source:     A.R. Kronlund
.getSimParsFromRdata <- function( hdrObj )
{
  for ( iSim in 1:nrow(hdrObj) )
  {
     # Remove whitespace from field read from GUI text box.
     fileName <- gsub( " ", "", hdrObj$Rdata[iSim] )
     
     # Load an Rdata working directory containing a list called blob.
     cat( "\n(.getSimParsFromRdata) Loading",fileName,"...\n" )     
     load( file=fileName )
     assign( "blob", blob, pos=1 )
     
     simGuiPars <- .unEvalList( blob$simGuiPars )
     
     if ( iSim==1 )
       result <- data.frame( simGuiPars )
     else
       result <- rbind( result,data.frame(simGuiPars) )
       
     gc()
  }
  dimnames(result) <- list( hdrObj$simID,names(result) )
  result
}

#------------------------------------------------------------------------------#
#--- Helper Functions (Public)                                              ---#
#------------------------------------------------------------------------------#

# saveToExcel (Save data in Excel format)
# Purpose:    Saves data in obj to a Microsoft Excel .xls file.
# Parameters: fname - a character string file name.
#             statObj - the result of a call to .calcPerfStats.
# Returns:    NULL
# Source:     Modified from saveExcel by T.K. Deering (PopSim.r)
saveToExcel <- function( fname, statObj )
{
	unpackList( statObj,scope="L" )
	#fname <- promptSaveFile(filetype=list(c(".xls", "Microsoft Excel Spreadsheet")))
	
	# No name provided, or "Cancel" selected.
	if ( fname == "" )
    return(invisible())
    
	# If fname already exists, then remove it.
	fileGone <- TRUE
	if ( file.exists(fname) )
    fileGone <- file.remove( fname )
    
  if ( fileGone )
  {
  	conn <- RODBC::odbcConnectExcel( fname, readOnly=FALSE )
	
  	statObj$trackTable$Select <- as.character(statObj$trackTable$Select)
	
	  # Save the simulation tracking table.
	  .excelTable( conn, statObj$trackTable, "Tracking",
                 dimnames(statObj$trackTable)[[2]],dimnames(statObj$trackTable)[[1]] )
  
    # Save the guiSim parameters.             
    .excelTable( conn, t(statObj$parTable), "Sim_Parameters",
                 dimnames(statObj$parTable)[[1]],dimnames(statObj$parTable)[[2]] )
   
    # Save the guiPerf parameters.             
    .excelTable( conn, statObj$perfParTable, "Perf_Parameters",
                 dimnames(statObj$perfParTable)[[2]], NULL )
                            
    # Save the aggregate performance statistics by period (spreadsheet format).
    .excelTable( conn, statObj$perfTable1, "Sim_Summary1",
                 dimnames( statObj$perfTable1)[[2]],dimnames(statObj$perfTable1)[[1]] )
               
    # Save the aggregate performance statistics by period (normalized format).
    .excelTable( conn, statObj$perfTable2, "Sim_Summary2",
                 dimnames( statObj$perfTable2)[[2]],dimnames(statObj$perfTable2)[[1]] ) 
 
   # K.Holt (1-Sept-09): added next two tables.                
   # Save the aggregate performance statistics with failed reps removed by period (spreadsheet format).
    .excelTable( conn, statObj$perfTable3, "Sim_Summary1_OmitFails",
                 dimnames( statObj$perfTable3)[[2]],dimnames(statObj$perfTable3)[[1]] )
               
   # Save the aggregate performance statistics with failed reps removed by period (normalized format).
    .excelTable( conn, statObj$perfTable4, "Sim_Summary2_OmitFails",
                 dimnames( statObj$perfTable4)[[2]],dimnames(statObj$perfTable4)[[1]] )                                              
               	
    odbcClose(conn)
  }
  else
  {
    cat( "\nERROR (saveToExcel): Simulations results NOT saved to Excel, permission denied\n" )
    cat( "\nACTION (saveToExcel): Close or rename file ",fname,"\n " )
  }
	return( fileGone )
}
