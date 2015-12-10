# CHANGES:

# Check to ensure that we have input assessment M.
# Check to ensure that projected fishery exploitable biomass coming out of assessCA.

# (1) mp$assess$Bt has been deprecated and removed.
# (2) mp$assess$estPars has been deprecated and removed.
# (3) All "reliability" code controlled by relBase deprecated and removed.
#     This includes the checkbox widget on the HCR gui.

# Issue List:

# (4) This issue is not resolved!!!!

  # Step 3: Determine linear interpolation of acceptable pDecline within zones
  # based on current stock status at nT, trend, lower and upper limits.

  # ARK (01-Dec-11) *** Should this be based on mean of terminal spawning
  # biomass, not projected as SPC early code (now lost) fragments suggest?
  
# (5) Check use of idxCtlPts - comments on first use below.  Coders adding rules
#     should ensure they honor idxCtlPts or the Control Point Update Frequency
#     options in the HCR GUI will not work.

#------------------------------------------------------------------------------#
#--- mseRsimulation.r: Operating Model & Management Procedure Functions  ----- #
#------------------------------------------------------------------------------#
#                                                                              #
# (c) mseR: Management Strategy Evaluation in R.                               #
#                                                                              #
#     Copyright 2008-2011, 2012 by A.R. Kronlund, S.P. Cox, K.R. Holt, and     #
#     R.E. Forrest (Delay Difference model).                                   #
#                                                                              #
#     This software comes with ABSOLUTELY NO WARRANTY, expressed or implied.   #
#     You have been provided with a copy of mseR for educational purposes.     #
#     You are requested not to redistribute this software without seeking      #
#     permission from the authors.                                             #
#                                                                              #
#     Of course, comments and suggestions for improvement greedily accepted.   #
#                                                                              #
# "Pretty good management will do."  Bill de la Mare, Dec. 19, 2006.           #
#                                                                              #
#          "Success consists of going from failure to failure without          #
#                  loss of enthusiasm."  Winston Churchill.                    #
#                                                                              #
# CONVENTIONS:                                                                 #
#                                                                              #
# Standard C++ variable naming conventions are applied.  The first letter of a #
# variable name is lower case.  Each concatenated word or abbreviation is      #
# capitalized. For example, Lower Bound Multiplier becomes lowerBoundMult.     #
#                                                                              #
#------------------------------------------------------------------------------#
#--                                                                          --#
#-- mseRsimulation.r: An mseR module that conatins functions used to run     --#
#--              the operating model and sub-models used in the management   --#
#--              procedure within a feedback loop                            --#
#--                                                                          --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--          J.S. Cleary (Pacific Biological Station, Nanaimo, B.C.)         --#
#--          K.R. Holt (Pacific Biological Station, Nanaimo, B.C.)           --#
#--                                                                          --#
#-- First Implementation: 01-Dec-09 based on mseSable.r by Cox and Kronlund. --#
#--                                                                          --#
#--  Revisions:                                                              --#
#                                                                              #
# References:                                                                  #
#                                                                              #
# Cox, S.P. and Kronlund, A.R. 2008. Practical stakeholder-driven              #
#   harvest policies for groundfish fisheries in British Columbia, Canada.     #
#   Fisheries Research xx: ppp                                                 #
#                                                                              #
# DFO, 2006. A Harvest Strategy Compliant with the Precautionary Approach.     #
#   DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2006/023.                       #
#                                                                              #
# NOTE: The file structure in simCtlFile.txt can be read using the             #
#  read.table function, e.g.,                                                        #
#                                                                              #
# ctlPars <- read.table( file=parFile,as.is=TRUE,header=TRUE,comment.char="#", #
#                        quote="",sep=" " )                                    #
#                                                                              #
# The resulting dataframe can be converted to a list structure using the call: #
# result <- .createList( ctlPars )                                             #
#                                                                              #
# HARVEST CONTROL RULE NOTES                                                   #
#                                                                              #
# (1) Stock status                                                             #
#                                                                              #
# Base: This is the metric for stock status, i.e., relative to Bmsy, B0, or    #
#       some Data control point like a survey CPUE (Data not implemented).     #
#       Multipliers for lower and upper control points are set to control rule #
#       shape.  These DO NOT have to correspond with the biological reference  #
#       points that demarcate the DFO Cautious, Critical, and Healthy Zones.   #
#                                                                              #
# Source: This is how stock status control points are calculated - from the    #
#         operating model values ("true") or estimated by the stock assessment #
#         method.  Note that this applies only to population dynamics methods. #
#         Eventually the Data option will be added.                            #
#                                                                              #
# (2) Reference removal rate                                                   #
#                                                                              #
# Base: This is the metric for the reference removal rate. The F multiplier    #
#       adjusts the selected F to say, fish at 0.9Fmsy if performance stats    #
#       indicate that a limit fishing mortality (Fmsy) is exceeded with higher #
#       than desired probability (not impleted yet...).                        #
#                                                                              #
# Source: This is how reference removal rate control points are calculated.    #
#         Can be from the operating model (OM) or management procedure (MP).   #
#         Note that this applies only to population dyanmics models, as the    #
#         moving average or Kalman filter always use the operating model.      #
#                                                                              #
# FUNCTIONS INCLUDED IN FILE                                                   #
# (listed in order of occurence)                                               #
#                                                                              #
# ------------------ Simulation Set-up and Execution ------------------------- #
#                                                                              #
# runMSE          : Main function controlling for entire feedback loop         #
# createMP        : Create all operating model & mgmt. procedure objects       #
# mgmtProc        : Runs "nReps" replications of simulation model              #
# initPop         : Initialise all objects, for period 1 - tMP                 #
# solveInitPop    : Initialise spawning biomass Bt to target initial depletion #
#                                                                              #
# ------------------------ Operating Model Functions ------------------------- #
#                                                                              #
# asOMod            : Age-structured operating model                           #
# .calcLenAge       : Calculates von B length-at-age                           #
# .calcLogistic     : Calculates logistic ogive for maturity and selectivity   #
# .calcPat          : Calculates observed proportion-at-age data               #
# .calcRdevs        : Calculates lag-1 autocorrelated recruitment deviations   #
# .calcWtAge        : Calculates weight-at-age                                 #
# .fillRanWalk      : Creates a lag-1 autocorrelated error sequence            #
#                                                                              #
# ---------------------- Management Procedure Functions ---------------------- #
#                                                                              #
# NOTE: callProcedureXXX functions are wrappers for input of data to the       #
#       assessment methods. The only reliable way to isolate code changes due  #
# to assessment methods is to contain them in functions.  New methods can then #
# be added without impacting working code.  It means some redundancy in coding #
# which is not ideal, but sorting out complicated if branches is a very much a #
# Special Kind of Hell that should be avoided.                                 #
#                                                                              #
# assessModMA       : Stock assessment method for Moving Average               #
# assessModKF       : Stock assessment method for Kalman Filter                #
# assessModSP       : Stock assessment method for Surplus Production model     #
# assessModCA       : Stock assessment method for Catch At Age model           #
# callProcedureKalman : Wrapper for Kalman filter based procedures             #
# callProcedureMovingAvg : Wrapper for moving average based procedures         #
# callProcedureSP   : Wrapper for production model based procedures            #
# callProcedureCAA  : Wrapper for catch-at-age model bsaed procedures          #
# .updatePop        : Complete one iteration of feedback system                #
#                                                                              #
# calcHCRpopDyn     : HCR wrapper to call specific HCR for pop dynamics models #
# calcHCRsmoother   : HCR wrapper to call specific HCR for smoothers - Kalman  #
# .calcHCRconstantC : Constant catch HCR - NOT IMPLEMENTED                     #
# .calcHCR : Constant F HCR (status-based rule)                       #
# .calcHCRvariableF : Variable F HCR (status-based rule)                       #
# .calcHCRdeclineRisk : Acceptable prob. future decline rule                   #
#                                                                              #
# Control point calculations.                                                  #
#                                                                              #
# .caaModGetCtlPtsAdmb : Calc CAA HCR control points using refPts.exe          #
# .caaModCalcCtlPts    : Calc CAA HCR control points using .caaModGetCltPtsAdmb#
#                        or via an Rscript to refPts.DLL                       #
# .caaModCtlPtsUseR    : Calc CAA HCR control points using only R              #
#                                                                              #
# .ddModGetCtlPtsAdmb  : Calc DD HCR control points using refPtsDD.exe         #
# .ddModGetCtlPts      : Calc DD HCR control points using refPtsDD.DLL         #
#                                                                              #
# ------------------------------ Helper Functions ---------------------------- #
#                                                                              #
# .calcTimes        : Calculate years where ages on, survey on, assessment on, #
#                     HCR control points updated.  Maybe dependencies among.   #
# .makeFileName     : Builds a filename to avoid collisions with snow calls    #
# .saveBlob         : Save the blob                                            #
# .saveSimResults   : Saves the simulations results and updates list (this     #
#                     needs to be restored from mseRguiSimFuns.r               #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
# INSTRUCTIONS FOR ADDING NEW MODEL OPTIONS                                    #
#                                                                              #
# (1) Add a new operating model:                                               #
#                                                                              #
#     (a) Create a new operating model function (xxxOMod) that follows format  #
#         of the current asOMod() function.                                    #
#           similar format as the current asOMod() function.                   #
#     (b) Modify solveInitPop() function so that new xxxOMod() is called in    #
#         place of asOMod() when needed.                                       #
#     (c) Modify .updatePop() function so new xxxOMod() is called in step 6    #
#         below instead of asOMod() when needed.                               #
#                                                                              #
# (2) Add a new assessment method:                                             #
#                                                                              #
#     (a) Create a new assessModxxx() function that follows the format of      #
#         asessModMA(), assessModKF(), and assessModProd().                    #
#     (b) Modify step 3 of .updatePop() function to call new assessModxxx()    # 
#         function when it is selected from guiSim window.                     #
#                                                                              #
# (3) Add a new harvest control rule:                                          #
#                                                                              #
#     (a) Create a new calcHCRxxx() function that follows a similar format     #
#         as the current calcHCRxxx() functions and add to calcHCR.            #
#     (b) Modify steps 4 and 5 of .updatePop() function to call new            #
#         calcHCRxxx() function when selected by obj$mp$hcr$hcrType.           #
#                                                                              #
# Additional:                                                                  #
#                                                                              #
# (a) Modify mseRguiSimWin.txt to accommodate  new options in guiSim GUI.      #
# (b) Add new simulation control parameters to simCtlFile.txt.                 #
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--                 Simulation Setup and execution functions                 --#
#------------------------------------------------------------------------------#
# To operate in parallel.
#      Execute runMSE in parallel mode using parRunMSE.r. See header
#      in parRunMSE.r for usage details. User needs to be familiar with setup 
#      and execution of batch jobs using guiBatch() and associated control files.
# runMSE
# Purpose:        Main function controlling for entire feedback
#                 loop simulation process. runMSE can be called
#                 by clicking the Run button on guiSim(), clicking Run on
#                 guiBatch() (if using batch processing mode), or by typing
#                 > runMSE() on the R console.
# Parameters:     none. input from guiSim via *.par file (see SPC note above)
# Returns:        none. saves blob to *.RData workspace
# Source:         S.P. Cox, A.R. Kronlund
runMSE <- function(  ctl=NULL, folderName=NULL )
{
  saveBlob=TRUE
  # No validity checking of .CTLFILE variables is done if runMSE is called
  # directly from the console.  In contrast, guiSim() calls via RUN button will
  # enforce checking of valid parmete3rs. prior to calling runMSE.
   
  # Read and load the MP control parameters into a list.
  # The .CTLFILE is usually simCtlFile.txt located in the main
  # working directory.
  if( is.null(ctl) ) 
    ctl <- paste( getwd(), "/simCtlFile.txt", sep="")

  ctlPars <- .readParFile( ctl )
   
  ctlList <- .createList( ctlPars )
  cat( "\nMSG (runMSE) Parameter list created.\n" )
  
  .PRJFLD  <<- "mseRproject"
  .CTLFILE <<- "simCtlFile.txt"

  
  # Create the management procedure object.
  simObj <- .createMP( ctlList )
  
  # Run the management procedure.
  blob <- .mgmtProc( simObj )
  
  # Save the blob particulars.
  blob$ctlPars   <- ctlPars
  blob$ctlList   <- ctlList
  blob$sim       <- TRUE

   #folderName <- "/Users/spcox/Dropbox/mseR-repo/mseR-working/mseRHerring-CC/mseRBat1"
  if( !is.null(folderName) )
  {
      tmpFolder  <- strsplit(folderName,"/")[[1]]
      folderName <- tmpFolder[length(tmpFolder)]
      stamp          <- folderName #.getStamp()    
  }
  else
      stamp          <- .getStamp()    

  folderID <- 0
  blob$stamp <- stamp
  blobFileName   <- paste( "sim_",blob$stamp,".Rdata", sep="" )
  blob$refPtList <- calcRefPoints( ctlList$opMod, 
                                   list( FALL=TRUE, x=40 ) )

  # Save the blob to the working environment.
  assign( "blob",blob,pos=1 )

  # Make the simulation folder.
  simFolder <- paste( "sim_", stamp, sep="" )
  dir.create( file.path( .PRJFLD, simFolder ) )
  cat( "\nMSG (runMSE) Created simulation folder ",simFolder,"in",
       .PRJFLD,"\n" )

  # Save the simXXX.Rdata version of the blob to the simulation folder.
  blobFilePath <- file.path( .PRJFLD,simFolder,blobFileName )
  if ( saveBlob )
    .saveBlob( blobFilePath, blob  )
        
  # Copy the simulation control file into the folder.
  file.copy( .CTLFILE, file.path( .PRJFLD, simFolder, basename(.CTLFILE) ) )
  
  # Copy the TPL file into the folder.
  if ( ctlList$mp$assess$methodId==.CAAMOD )
  {
    file.copy( file.path( getwd(), "assessca.tpl" ),
               file.path( .PRJFLD, simFolder, "assessca.tpl" ) )
               
    file.copy( file.path( getwd(), "refptsca.tpl" ),
               file.path( .PRJFLD, simFolder, "refptsca.tpl" ) )
  }

  if ( ctlList$mp$assess$methodId==.DDMOD )
  {
    file.copy( file.path( getwd(), "assessdd.tpl" ),
               file.path( .PRJFLD, simFolder, "assessdd.tpl" ) )
               
    file.copy( file.path( getwd(), "refptsdd.tpl" ),
               file.path( .PRJFLD, simFolder, "refptsdd.tpl" ) )
  }
    
  if ( ctlList$mp$assess$methodId==.PMOD )
    file.copy( file.path( getwd(), "assesssp.tpl" ),
               file.path( .PRJFLD, simFolder, "assesssp.tpl" ) )
               
  #if ( ctlList$opMod$historyType=="omFile" )
  if ( ctlList$opMod$historyType != "omAuto" )
  {
    .DEFHSTFILE <<- ctlList$opMod$historyType
    file.copy( file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE ),
               file.path( .PRJFLD, simFolder,  .DEFHSTFILE ) )                   
  }

  # Write the *.info file to the simulation folder.
  infoFilePath <- file.path( .PRJFLD, simFolder, paste( "sim_",stamp,".info",sep="" ) )
  #.writeSimInfo( info=list( simTime=stamp,
  #  scenarioLabel=ctlList$gui$scenarioLabel,
  #  mpLabel=ctlList$gui$mpLabel,
  #  tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT, nReps=ctlList$gui$nReps,
  #  rank=1, group=1 ), simFolder, infoFilePath )        
  .writeInfoFile( info=list( simTime=stamp,
    scenarioLabel=ctlList$gui$scenarioLabel,
    mpLabel=ctlList$gui$mpLabel,
    tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT, nReps=ctlList$gui$nReps,
    rank=1, group=1 ), simFolder, infoFilePath )        

  # Get the tracking data for the project prior to this simulation.
  trackData <- .getTrackingData( projDir=.PRJFLD )
       
  # Get the number of simulations.
  nSims <- .getNumSims(trackData )
  cat( "\nMSG (runMSE) Total number of simulations in project = ",nSims,
       "\n" )

  # Write the revised INFO file to the simulation folder.
  #.writeSimInfo( info=list( simTime=stamp,
  #  scenarioLabel=ctlList$gui$scenarioLabel,
  #  mpLabel=ctlList$gui$mpLabel,
  #  tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT, nReps=ctlList$gui$nReps,
  #  rank=nSims, group=1 ), simFolder, infoFilePath )        
  .writeInfoFile( info=list( simTime=stamp,
    scenarioLabel=ctlList$gui$scenarioLabel,
    mpLabel=ctlList$gui$mpLabel,
    tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT, nReps=ctlList$gui$nReps,
    rank=nSims, group=1 ), simFolder, infoFilePath )        
    
  return( invisible() )
}     # END function runMSE


# .createMP
# Purpose:        create all operating model (om), management procedure (mp),
#                 and parameter (opMod) objects, including the reference points
# Parameters:     obj=list containing simulation control parameters.
# Returns:        a new list with objects for all state variables, ref pts, etc.
# Source:         S.P. Cox (SPC modified 1 May 2010 to make ref pts calcs optional
#                 via rpList <- list( FALL=T/F, ...) where "..." is an optional
#                 list of individual ref pts to compute. )
.createMP <- function( obj )
{
  # Add common parameters to all objects for convenience.
  tMP               <- obj$opMod$tMP
  nT                <- obj$opMod$nT

  obj$mp$data$tMP   <- tMP
  obj$mp$assess$tMP <- tMP
  obj$mp$hcr$tMP    <- tMP

  obj$mp$data$nT    <- nT
  obj$mp$assess$nT  <- nT
  obj$mp$hcr$nT     <- nT

  # IMPORTANT!! Calculating reference points for operating model.

  # This adds the reference points to obj$opMod, because it is passed thru calcRefPoints.
  rpList    <- list( FALL=TRUE, x=40 )
  obj$opMod <- calcRefPoints( obj$opMod, rpList )    # Ref pts and life history.

  #----------------------------------------------------------------------------#
  # (1) BEGIN OPERATING MODEL SETUP - stored under obj$om                      #
  #----------------------------------------------------------------------------#
  
  om <- list( Nat=NULL,  Bat=NULL,
              Lat=NULL, alphat=NULL, rhot=NULL,
              Bt=NULL,   Nt=NULL,
              Bexp=NULL, Nexp=NULL,  BexpS=NULL,
              Rt=NULL,
              Ct=NULL,   Dt=NULL,
              Ft=NULL,   Mt=NULL,
              uat=NULL,  uatS=NULL,
              ItTrue=NULL,
              qt=NULL )

  # State variables: numbers at a,t and biomass at a,t.
  A         <- obj$opMod$nAges
  nT        <- obj$opMod$nT

  om$Nat    <- matrix( NA, nrow=A, ncol=nT )
  om$Bat    <- matrix( NA, nrow=A, ncol=nT )
  om$Lat    <- matrix( NA, nrow=A, ncol=nT )
  om$alphat <- rep( NA,nT )
  om$rhot   <- rep( NA,nT )
  om$Mt     <- rep( NA,nT )
  om$Bt     <- rep( NA,nT )
  om$Nt     <- rep( NA,nT )
  om$Bexp   <- rep( NA,nT )
  om$BexpS  <- rep( NA,nT )
  om$Nexp   <- rep( NA,nT )
  om$Rt     <- rep( NA,nT )
  om$uat    <- matrix( data=NA, nrow=A,ncol=nT )   #fishery
  om$uatS   <- matrix( data=NA, nrow=A,ncol=nT ) #survey

  # Controls: fishing mortality, catch numbers, and catch biomass.
  om$Ft <- rep( NA,nT )     # realized (true) fishing mortality
  om$Ct <- rep( NA,nT )
  om$Dt <- rep( NA,nT )

  # Survey: deterministic spawning biomass survey.
  om$ItTrue <- rep( NA, nT )
  # Survey catchability, possibly time-varying
  om$qt     <- rep( NA, nT )

  # Create variable to hold standard normal errors. Filled in initPop.
  #    deltat   : Recruitment deviation std normal errors
  #    epsilont : Observation error std normal errors
  errors <- list( deltat=NULL, epsilont=NULL, epsilonat=NULL, epsilonatS=NULL )

  # Note: stochastic errors in recruitment are generated separately for each
  # simulation replicate in "initPop"...just prior to the initialisation step.
  # Then, if om setup is "auto" initPop finds historical Fs for t=(1,2,...tMP-1) such that the
  # "stochastic" population is depleted to approx initTargDep by time tMP.

  obj$om        <- om
  obj$om$errors <- errors

  #----------------------------------------------------------------------------#
  #  END OPERATING MODEL SETUP - obj$om complete                               #
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # (2) BEGIN STOCK AND FISHERY DATA SETUP - stored in obj$mp$data             #
  #----------------------------------------------------------------------------#

  tmpTimes <- .calcTimes( obj )     # Get years that ages, survey, assess are "on".

  # Survey: variable to hold stochastic spawning biomass survey  
  It <- rep( NA, nT )
  
  # Fill survey attributes
  attr( It, "surveyOn" )       <- tmpTimes$surveyOn
  attr( It, "t1Survey" )       <- tmpTimes$t1Survey
  attr( It, "t2Survey" )       <- tmpTimes$t2Survey

  obj$mp$data$It    <- It
    
  # FISHERY
  pat   <- matrix( NA, nrow=A, ncol=nT )
  attr( pat, "agesOn" ) <- tmpTimes$agesOn
  attr( pat, "t1Ages" ) <- tmpTimes$t1Ages
  attr( pat, "t2Ages" ) <- tmpTimes$t2Ages
 
  obj$mp$data$pat <- pat

  # SURVEY
  patS   <- matrix( NA, nrow=A, ncol=nT )
  attr( patS, "agesOnS" ) <- tmpTimes$agesOnS
  attr( patS, "t1AgesS" ) <- tmpTimes$t1AgesS
  attr( patS, "t2AgesS" ) <- tmpTimes$t2AgesS

  obj$mp$data$patS <- patS
    
  #----------------------------------------------------------------------------#
  #  END STOCK AND FISHERY DATA SETUP - obj$mp$data complete                   #
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # (3) BEGIN ASSESSMENT METHOD SETUP - stored in obj$mp$assess                #
  #----------------------------------------------------------------------------#

  # ARK (27-Dec-12) Should have a different name as already appears under mp$data.
  obj$mp$assess$pat  <- matrix( NA, nrow=A, ncol=nT )
  obj$mp$assess$patS <- matrix( NA, nrow=A, ncol=nT )

  # Vector to store annual biomass assessments, spawning, exploitable.
  Bt         <- rep( NA,nT )
  spawnBt    <- rep( NA,nT )
  exploitBt  <- rep( NA,nT ) # Fishery
  exploitBtS <- rep( NA,nT ) # Survey
  Ft         <- rep( NA,nT ) # RF_added
    
  attr( Bt, "methodOn"  ) <- tmpTimes$methodOn
  attr( Bt, "t1Method"  ) <- tmpTimes$t1Method
  attr( Bt, "t2Method"  ) <- tmpTimes$t2Method
  attr( Bt, "idxMethod" ) <- tmpTimes$idxMethod
  attr( Bt, "tMethod"   ) <- tmpTimes$tMethod
  
  attr( spawnBt, "methodOn"  ) <- tmpTimes$methodOn
  attr( spawnBt, "t1Method"  ) <- tmpTimes$t1Method
  attr( spawnBt, "t2Method"  ) <- tmpTimes$t2Method
  attr( spawnBt, "idxMethod" ) <- tmpTimes$idxMethod
  attr( spawnBt, "tMethod"   ) <- tmpTimes$tMethod
  
  attr( exploitBt, "methodOn"  ) <- tmpTimes$methodOn
  attr( exploitBt, "t1Method"  ) <- tmpTimes$t1Method
  attr( exploitBt, "t2Method"  ) <- tmpTimes$t2Method
  attr( exploitBt, "idxMethod" ) <- tmpTimes$idxMethod

  attr( exploitBtS, "tMethod"   ) <- tmpTimes$tMethod      
  attr( exploitBtS, "methodOn"  ) <- tmpTimes$methodOn
  attr( exploitBtS, "t1Method"  ) <- tmpTimes$t1Method
  attr( exploitBtS, "t2Method"  ) <- tmpTimes$t2Method
  attr( exploitBtS, "idxMethod" ) <- tmpTimes$idxMethod
  attr( exploitBtS, "tMethod"   ) <- tmpTimes$tMethod      

  obj$mp$assess$spawnBt    <- spawnBt
  obj$mp$assess$exploitBt  <- exploitBt 
  obj$mp$assess$exploitBtS <- exploitBtS 
  obj$mp$assess$Ft         <- Ft
  
  obj$mp$assess$Rt     <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  
  # Retrospective statistics.
  obj$mp$assess$retroExpBt   <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )   # Fishery
  obj$mp$assess$retroExpBtS  <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )   # Survey
  obj$mp$assess$retroSpawnBt <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  obj$mp$assess$retroFt      <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )   # RF_added
  obj$mp$assess$retroRt      <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  
  obj$mp$assess$retroStats   <- rep( NA, .NRETROSTATS )  

  # Stock assessment dependent outputs for a particular MP.  Will be transfered
  # to blob, so one column less than the object in blob since no iRep.
  # Problem here is have to hard-wire ncol to match outputs for each model.
    
  nColRunStatus <- 15
  
  if ( obj$mp$assess$methodId == .MOVAVG | obj$mp$assess$methodId == .KALMAN )
  {
    obj$mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nT-tMP+1, ncol=2 ) )
    names( obj$mp$assess$mpdPars ) <- c( "tStep","Estimates" )
    obj$mp$assess$pdfPars   <- obj$mp$assess$mpdPars
    obj$mp$assess$runStatus <- data.frame( matrix( NA, nrow=nT-tMP+1, ncol=2 ) )
  }
  
  if ( obj$mp$assess$methodId == .CAAMOD )
  {
    obj$mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  obj$mp$assess$pdfPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  
	  # runStatus will hold ADMB convergence stats, Hessian status, FISHERYCLOSED, DEADFLAG, etc.
	  obj$mp$assess$runStatus      <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus ) )
  }
  
  if ( obj$mp$assess$methodId == .DDMOD )
  {
	  obj$mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  obj$mp$assess$pdfPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  
	  # runStatus will hold ADMB convergence stats, Hessian status, FISHERYCLOSED, DEADFLAG, etc.
	  obj$mp$assess$runStatus      <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus) )
  }  

  if ( obj$mp$assess$methodId == .PMOD )
  {
	  obj$mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  obj$mp$assess$pdfPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )

	  # runStatus will hold ADMB convergence stats, Hessian status, FISHERYCLOSED, DEADFLAG, etc.
	  obj$mp$assess$runStatus <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus) )
  }

  #----------------------------------------------------------------------------#
  # END ASSESSMENT METHOD SETUP - obj$mp$assess complete                       #
  #----------------------------------------------------------------------------#  
  
  #----------------------------------------------------------------------------#
  # (4) BEGIN HCR SETUP - stored in obj$mp$hcr                                 #
  #----------------------------------------------------------------------------#
  
  # Case 1: Bmsy is base for ref point and operating model equilibrium value used
  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" & obj$mp$hcr$statusSource=="statusSrceEquil" )
  {
    # Set statusBase to Bmsy for all nT years.
    obj$mp$hcr$Bref <- rep( obj$opMod$ssbFmsy, nT )
  }
  
  # Case 2: B0 is base for ref point and operating model equilibrium value used
  if ( obj$mp$hcr$statusBase == "statusBaseB0" & obj$mp$hcr$statusSource=="statusSrceEquil" )
  {
    # Set statusBase to B0 for all nT years.  
    obj$mp$hcr$Bref <- rep( obj$opMod$B0, nT )
  }
  
   # Case 3: Bmsy is base for ref point and is estimated from assessment model
  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" & obj$mp$hcr$statusSource=="statusSrceEst" )
  {
    # Create a vector to be filled each annual estimate.
    obj$mp$hcr$Bref <- rep( NA, nT )
  }
  
  # Case 4: B0 is base for ref point and is estimated from assessment model
  if ( obj$mp$hcr$statusBase == "statusBaseB0" & obj$mp$hcr$statusSource=="statusSrceEst" )
  {
    # Create a vector to be filled each annual estimate.  
    obj$mp$hcr$Bref <- rep( NA, nT )
  }
  
  # Case 5: Historical Bt is base for ref point and is estimated from assessment model
  if ( obj$mp$hcr$statusBase == "statusBaseBt" & obj$mp$hcr$statusSource=="statusSrceEst" )
  {
    # Create a vector to be filled each annual estimate.  
    obj$mp$hcr$lowerBref <- rep( NA, nT )
    obj$mp$hcr$upperBref <- rep( NA, nT )
  }
  
  # Case 6: Historical Bt is base for ref point and is taken from operating model
  if ( obj$mp$hcr$statusBase == "statusBaseBt" & obj$mp$hcr$statusSource=="statusSrceEquil" )
  {
    obj$mp$hcr$lowerBref <- rep( NA, nT )
    obj$mp$hcr$upperBref <- rep( NA, nT )
  }

  #-- Removal Rate Control Point - setup vectors for reference rate remRate  --#

  # Case 1: Reference removal rate input directly from GUI.
  if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
  {
    obj$mp$hcr$remRate <- rep( obj$mp$hcr$inputF, nT )
  }

  # Case 2: Reference removal rate based on Fmsy and operating model equilibrium value used
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" & obj$mp$hcr$remRefSource == "rrSrceEquil" )
  {
    obj$mp$hcr$remRate <- rep( obj$opMod$Fmsy, nT )
  }

  # Case 3: Reference removal rate based on Fmsy and is estimated from assessment model
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" & obj$mp$hcr$remRefSource == "rrSrceEst" )
  {
    # Create Fmsy vector to be filled with annual estimates.
    obj$mp$hcr$remRate <- rep( NA, nT )
  }
  
  # Case 4: Reference removal rate is set to F0.1
  if ( obj$mp$hcr$remRefBase == "rrBaseF01" ) 
  {
    obj$mp$hcr$remRate <- rep( obj$opMod$F01, nT )
  }
  
  # Case 5: Reference removal rate is set to Fspr (with %spr specified by user)
  if ( obj$mp$hcr$remRefBase == "rrBaseFspr" ) 
  {
    fspr<-.getFx( obj$opMod, x=obj$mp$hcr$sprX)$Fx
    obj$mp$hcr$remRate <- rep( fspr, nT )
  }

  # Case 6: Reference removal rate is based on historical Ft, which comes from the OM
  if ( obj$mp$hcr$remRefBase == "rrBaseFt" & obj$mp$hcr$remRefSource == "rrSrceEquil" ) 
  {
    obj$mp$hcr$remRate <- rep( NA, nT )
  }

  # Case 7: Reference removal rate is based on historical Ft, as estimated by assessment model
  if ( obj$mp$hcr$remRefBase == "rrBaseFt" & obj$mp$hcr$remRefSource == "rrSrceEst" ) 
  {
    obj$mp$hcr$remRate <- rep( NA, nT )
  }
  
  # Compile list of years in which reference points will be estimated
  obj$mp$hcr$idxCtlPts <- rep( NA, nT-tMP+1 )
  if ( obj$mp$hcr$remRefSource == "rrSrceEst" | obj$mp$hcr$statusSource == "statusSrceEst" )
  {
    obj$mp$hcr$idxCtlPts <- tmpTimes$idxCtlPts
  }

  # Create vectors for lower and upper bounds on Bref scale.
  obj$mp$hcr$lowerBound <- rep( NA, nT )
  obj$mp$hcr$upperBound <- rep( NA, nT )

  # Vector to store target Ft based on biomass assessment estimate and HCR
  obj$mp$hcr$targetFt <- rep( NA, nT )
  
  # Vector to store paAdj value for eBioAdj, adjusted exploitable biomass.
  obj$mp$hcr$eBioAdj <- rep( NA,nT )
  
  # Vector to store pStar = observed probablity of future decline.
  obj$mp$hcr$pStar <- rep( NA,nT )

  # Adjust MCMC chain and thin if MLE, does not affect GUI.
  if ( obj$mp$hcr$hcrType=="constantF" || obj$mp$hcr$hcrType=="variableF" )
  {
    if ( obj$mp$hcr$useMLE )
    {
      obj$mp$hcr$nMCMC <- 1
      obj$mp$hcr$nThin <- 1
    }
  }

  #----------------------------------------------------------------------------#
  # END HCR SETUP - obj$mp$hcr complete                                        #
  #----------------------------------------------------------------------------#

  # Return the main list object
  obj
}     # END function .createMP.


# .mgmtProc
# Purpose:        runs "nReps" replications of simulation model
# Parameters:     obj=complete simulation object from createMP
# Returns:        the "blob" of all simulated states and data
#                 for "nReps" of "nT" years each
# Source:         S.P. Cox
.mgmtProc <- function( obj )
{
  # writeProgress   Private function
  # Purpose:        writes current status of simulation
  # Parameters:     i=current rep; n=total reps
  # Returns:        nothing
  # Source:         S.P. Cox
  writeProgress <- function( i , n )
  {
    if ( i==1 )
    {
      cat( "Running feedback loop...\n" )
      cat( "  Completed ", i, " of ", n, "replicates ")
    }
    if ( i %% 5 == 0 )
      cat( "\n  Completed ", i, " of ", n, " replicates " )
    if ( i==n )
      cat( "\nFeedback loop completed, writing results...\n\n" )
  }

  # Extract simulation dimensions
  nReps       <- obj$gui$nReps
  tMP         <- obj$opMod$tMP
  nT          <- obj$opMod$nT
  nAges       <- obj$opMod$nAges
  
  # Extract attributes.
  # RF QUERY - why is attrMethod used more than once? Can I use attrAges twice
  attrAges   <- attributes( obj$mp$data$pat )
  attrAgesS  <- attributes( obj$mp$data$patS )
  
  attrMethod <- attributes( obj$mp$assess$spawnBt )
  attrMethod <- attributes( obj$mp$assess$exploitBt )
  attrMethod <- attributes( obj$mp$assess$exploitBtS )
  attrSurvey <- attributes( obj$mp$data$It )
  
  #------------------------------------------------------
  # Create blob to store all simulation results
  #------------------------------------------------------
  # Simulation parameters and random number seed, this is stored locally.
  opMod       <- obj$opMod
  rSeed       <- opMod$rSeed

  # Simulated population and fishery.
  om <- list( iSeed    = rep( NA, nReps ),
              Bt       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Nt       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Lat      = array( data=NA, dim=c(nReps,nAges,nT) ),
              Bexp     = matrix( NA,nrow=nReps,ncol=(nT+1) ),
	          BexpS    = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Nexp     = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Rt       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              It       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              ItTrue   = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Dt       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ft       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Mt       = matrix( NA,nrow=nReps,ncol=(nT+1) ),              
              alphat   = matrix( NA,nrow=nReps,ncol=(nT+1) ),              
              rhot     = matrix( NA,nrow=nReps,ncol=(nT+1) ), 
              qt       = matrix( NA,nrow=nReps,ncol=(nT+1) ),             
              surveyCV = matrix( NA,nrow=nReps,ncol=(nT+1)),
              uat      = array( data=NA, dim=c(nReps,nAges,nT) ),
	          uatS     = array( data=NA, dim=c(nReps,nAges,nT) )
            )

  # Simulated management procedure
  mp                   <- list( data=NULL, assess=NULL, hcr=NULL )
  mp$data$It           <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  mp$data$pat          <- array( data=NA, dim=c(nReps,nAges,nT) )
  mp$data$patS         <- array( data=NA, dim=c(nReps,nAges,nT) )
  
  mp$assess$spawnBt    <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  mp$assess$exploitBt  <- matrix( NA, nrow=nReps, ncol=(nT+1) )	   # Fishery
  mp$assess$exploitBtS <- matrix( NA, nrow=nReps, ncol=(nT+1) )	   # Survey
  mp$assess$Ft         <- matrix( NA, nrow=nReps, ncol=(nT+1) )	   # RF_Added
  
  mp$assess$pat        <- array( data=NA, dim=c(nReps,nAges,nT) )  # Fishery
  mp$assess$patS       <- array( data=NA, dim=c(nReps,nAges,nT) )  # Survey
  
  # Retrospective statistics.
  mp$assess$retroExpBt   <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) ) # Fishery
  mp$assess$retroExpBtS  <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) ) # Survey
  mp$assess$retroSpawnBt <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) )      
  mp$assess$retroRt      <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) )
  mp$assess$retroFt      <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) ) # RF_Added 

  mp$assess$retroVals    <- array( data=NA, dim=c( nReps*(nT-tMP+1), (nT+2), .NRETROSTATS) )
  mp$assess$retroStats   <- matrix( NA, nrow=nReps, ncol=(.NRETROSTATS+1) )
  
  # Control point base values used in HCR.
  mp$hcr$Fref        <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  mp$hcr$Bref        <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  
  # Control rule bounds vary among years if Fref or Bref estimated.
  mp$hcr$lowerBound  <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  mp$hcr$upperBound  <- matrix( NA,nrow=nReps,ncol=(nT+1) )

  # paAdj value for eBioAdj.
  mp$hcr$eBioAdj     <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  
  # Decline risk values.
  mp$hcr$pStar       <- matrix( NA,nrow=nReps,ncol=(nT+1) )

  # Target removal rate based on harvest control rule and estimated biomass
  mp$hcr$targetFt    <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  
  # Assessment method options within mp
  if ( obj$mp$assess$methodId == .PMOD )
  {
    # This is for blob - needs to be 1 more column than createMP version for iRep.
    
	  mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
                                       
	  mp$assess$pdfPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$pdfPars)+1 ) )	  
    
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$runStatus)+1 ) )
	}
	
  if( obj$mp$assess$methodId == .CAAMOD )
  {
    # This is for blob - needs to be 1 more column than createMP version for iRep.

	  mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
                                       
	  mp$assess$pdfPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$pdfPars)+1 ) )	  
    
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$runStatus)+1 ) )
	}
	
  if( obj$mp$assess$methodId == .DDMOD )
  {
    # This is for blob - needs to be 1 more column than createMP version for iRep.
    
	  mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
                                       
	  mp$assess$pdfPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$pdfPars)+1 ) )	  
    
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$runStatus)+1 ) )      
	}	
	
  if( obj$mp$assess$methodId == .MOVAVG | obj$mp$assess$methodId == .KALMAN )
  {
 	  mp$assess$mpdPars <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                     ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
 	  mp$assess$pdfPars <- mp$assess$mpdPars
 	  
 	  # These methods can have FISHERYCLOSED, DEADFLAG, perhaps other things so just make runStatus the same.
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),ncol(obj$mp$assess$runStatus)+3 ) )
	}
	
  mp$assess$Rt       <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) )
  
  # Not strictly assessment values, but if Decline Risk HCR have to store this...
  mp$assess$ssb      <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  mp$assess$trendBio <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  mp$assess$trendVal <- matrix( NA, nrow=nReps, ncol=(nT+1) )

  # Initialize the blob object to hold all simulation results.

  blob <- list( ctlPars=obj, om=om, mp=mp )

  #----------------------------------------------------------------------------#
  #--                  Initiate the FEEDBACK LOOP Replicates                 --#
  #----------------------------------------------------------------------------#
  
  for ( i in 1:nReps )
  {
    # Initialize .DEADFLAG and .FISHERYCLOSED
    .DEADFLAG      <<- FALSE  
    .FISHERYCLOSED <<- FALSE
    
    # Set random number seed in a way that can be easily reconstructed later.
    set.seed( rSeed + i )
    
    blob$om$iSeed[i] <- rSeed + i

    # Initialise simulation object for pre-MP period
    obj$opMod$rep <- i
    obj           <- .initPop( obj )

    # Initialize annual biomass assessments and target F
   
    obj$mp$assess$spawnBt   <- rep( NA,nT )
    attributes( obj$mp$assess$spawnBt ) <- attrMethod
    
    obj$mp$assess$exploitBt <- rep( NA,nT )
    attributes( obj$mp$assess$exploitBt ) <- attrMethod

    obj$mp$assess$exploitBtS <- rep( NA,nT )
    attributes( obj$mp$assess$exploitBtS ) <- attrMethod

    obj$mp$assess$Ft <- rep( NA,nT )
    #attributes( obj$mp$assess$Ft ) <- attrMethod     #RF_added
    
    obj$mp$hcr$targetFt <- rep( NA,nT )


    if ( obj$mp$assess$methodId == .MOVAVG | obj$mp$assess$methodId == .KALMAN )
    {
      #obj$mp$assess$mpdPars   <- data.frame( matrix(NA,nrow=(nT-tMP+1)), ncol=1 )
      #obj$mp$assess$pdfPars   <- obj$mp$assess$mpdPars
    }

    # Initialize Catch-at-age model assessment outputs.                                 
	  if( obj$mp$assess$methodId == .CAAMOD )
    {
      nCol <- ncol( obj$mp$assess$mpdPars )    
      obj$mp$assess$mpdPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=nCol ) )

      nCol <- ncol( obj$mp$assess$pdfPars )    
      obj$mp$assess$pdfPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=nCol ) )
            
      obj$mp$assess$Rt      <- matrix( NA, nrow=(nT-tMP+1), ncol=(nT+1) )
      obj$mp$assess$pat     <- matrix( NA, nrow=nAges, ncol=nT )
      obj$mp$assess$patS    <- matrix( NA, nrow=nAges, ncol=nT )
    }
    
    # Initialize Delay Difference model assessment outputs.
    if( obj$mp$assess$methodId == .DDMOD )
	  {
      nCol <- ncol( obj$mp$assess$mpdPars )    
      obj$mp$assess$mpdPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=nCol ) )

      nCol <- ncol( obj$mp$assess$pdfPars )    
      obj$mp$assess$pdfPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=nCol ) )	    
  	}

    # Initialize Production model assessment outputs.
	  if( obj$mp$assess$methodId == .PMOD )
	  {
      nCol <- ncol( obj$mp$assess$mpdPars )    
      obj$mp$assess$mpdPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=nCol ) )

      nCol <- ncol( obj$mp$assess$pdfPars )    
      obj$mp$assess$pdfPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=nCol ) )	    
  	}
    
    # Retrospective statistics.
    obj$mp$assess$retroExpBt   <- matrix( NA, nrow=(nT-tMP+1),  ncol=(nT+1) )
    obj$mp$assess$retroExpBtS  <- matrix( NA, nrow=(nT-tMP+1),  ncol=(nT+1) )
    obj$mp$assess$retroSpawnBt <- matrix( NA, nrow=(nT-tMP+1),  ncol=(nT+1) )
    obj$mp$assess$retroRt      <- matrix( NA, nrow=(nT-tMP+1),  ncol=(nT+1) )
    obj$mp$assess$retroFt      <- matrix( NA, nrow=(nT-tMP+1),  ncol=(nT+1) )
    
    obj$mp$assess$retroVals    <- array( NA, dim=c( (nT-tMP+1),(nT+1), .NRETROSTATS ) )
    obj$mp$assess$retroStats   <- rep( NA, .NRETROSTATS )
    
    #--------------------------------------------------------------------------#
    #--                   FEEDBACK LOOP tMP to nT.                           --#
    #--------------------------------------------------------------------------#
    # Setting mpLabel to OMNI will generate an omniscient manager simulation that
    # solves for the optimal series of Fs for the whole projection period
    if( obj$gui$mpLabel=="OMNI" | obj$gui$mpLabel=="NoFish" )
    {
       obj <- .solveProjPop( obj )
    }
    else # otherwise, simulate the management procedure
    {
      for ( t in tMP:nT )
      {
        # Method of replacing obj with itself fills the existing state variable and mp objects.
        obj <- .updatePop( obj, t )
      }     # END Feedback loop.   
    }

    # Store results from this rep in current row of the blob 
    blob$om$Bt[i,]       <- c( i, obj$om$Bt )
    blob$om$Nt[i,]       <- c( i, obj$om$Nt )
    blob$om$Lat[i,,]     <- obj$om$Lat
    blob$om$Bexp[i,]     <- c( i, obj$om$Bexp )
    blob$om$BexpS[i,]    <- c( i, obj$om$BexpS )
    blob$om$Nexp[i,]     <- c( i, obj$om$Nexp )
    blob$om$Rt[i,]       <- c( i, obj$om$Rt )
    blob$om$It[i,]       <- c( i, obj$om$It )
    blob$om$ItTrue[i,]   <- c( i, obj$om$ItTrue )    
    blob$om$Dt[i,]       <- c( i, obj$om$Dt )
    blob$om$Ft[i,]       <- c( i, obj$om$Ft )
    blob$om$Mt[i,]       <- c( i, obj$om$Mt )    
    blob$om$qt[i,]       <- c( i, obj$om$qt )    
    blob$om$alphat[i,]   <- c( i, obj$om$alphat )    
    blob$om$rhot[i,]     <- c( i, obj$om$rhot )    
    blob$om$uat[i,,]     <- obj$om$uat
    blob$om$uatS[i,,]    <- obj$om$uatS
    blob$om$surveyCV[i,] <- c( i, obj$om$surveyCV  )

    # Management procedure variables
    blob$mp$data$It[i,]      <- c( i, obj$mp$data$It   )
    blob$mp$data$pat[i,,]    <- obj$mp$data$pat	       # Fishery
    blob$mp$data$patS[i,,]   <- obj$mp$data$patS	     # Survey
    blob$mp$assess$pat[i,,]  <- obj$mp$assess$pat		   # Fishery
    blob$mp$assess$patS[i,,] <- obj$mp$assess$patS	   # Survey
    
    blob$mp$assess$spawnBt[i,]    <- c( i, obj$mp$assess$spawnBt )
    blob$mp$assess$exploitBt[i,]  <- c( i, obj$mp$assess$exploitBt )	# Fishery
    blob$mp$assess$exploitBtS[i,] <- c( i, obj$mp$assess$exploitBtS )	# Survey
    blob$mp$assess$Ft[i,]         <- c( i, obj$mp$assess$Ft )	        # RF added
    blob$mp$hcr$targetFt[i,]      <- c( i, obj$mp$hcr$targetFt )

    # Fill in the blob with estimated or true HCR pars
    blob$mp$hcr$Bref[i,]       <- c( i, obj$mp$hcr$Bref )
    blob$mp$hcr$Fref[i,]       <- c( i, obj$mp$hcr$remRate )
    blob$mp$hcr$lowerBound[i,] <- c( i, obj$mp$hcr$lowerBound )
    blob$mp$hcr$upperBound[i,] <- c( i, obj$mp$hcr$upperBound )
    
    # Include specifications for harvest control rule.
    
    # Remove Bref, remRate, idxCtlPts, lowerBound, uppperBound, targetF, eBioAdj, & pStar.
    
    tmpNames <- names( obj$mp$hcr )
    remNames <- c( "Bref", "remRate", "idxCtlPts", "lowerBound", "upperBound",
                   "targetF", "eBioAdj", "pStar" )
    idx <- !is.element( tmpNames, remNames )
    
    blob$mp$hcr$specs <- obj$mp$hcr[idx]
    
    if ( blob$mp$hcr$specs$paAdj==TRUE )
      blob$mp$hcr$eBioAdj[i,]     <- c( i,obj$mp$hcr$eBioAdj )
    
    # Add in declineRisk HCR parameters.
    if ( blob$mp$hcr$specs$hcrType=="declineRisk" )
    {
      blob$mp$assess$ssb[i,]      <- c( i, obj$mp$assess$ssb )
      blob$mp$assess$trendBio[i,] <- c( i, obj$mp$assess$trendBio )
      blob$mp$assess$trendVal[i,] <- c( i, obj$mp$assess$trendVal )
      
      blob$mp$hcr$pStar[i,] <- c( i,obj$mp$hcr$pStar )     
    }
    else
    {
      if ( blob$mp$hcr$specs$paAdj==TRUE )
        blob$mp$hcr$eBioAdj[i,]     <- c( i,obj$mp$hcr$eBioAdj )
    }

    # Assessment outputs.

    # Fill rows of blob corresponding to multiple years of estimation (retrospective).
    # E.g., Suppose tMP=50 and nT=100, and nRep=10.  Then for replicate i=1:
    # startRow is (1-1)*(100-50) + 1=1 and endRow is 1+(100-50)=51.
    # For replicate 10, startRow is (10-1)*(100-50)+10=460, endRow is 460+(100-50)=510.
    
    startRow  <- (i-1)*(nT-tMP) + i
    endRow    <- startRow + (nT-tMP)
    reps      <- rep( i, (nT-tMP+1) )

    # Maximum posterior density and PDF estimates.
    blob$mp$assess$mpdPars[ startRow:endRow, ] <- cbind( reps, obj$mp$assess$mpdPars )    	  
    blob$mp$assess$pdfPars[ startRow:endRow, ] <- cbind( reps, obj$mp$assess$pdfPars )
    
    # Run status diagnostics.
    blob$mp$assess$runStatus[ startRow:endRow, ] <- cbind( reps, obj$mp$assess$runStatus )
	  
    blob$mp$assess$Rt[ startRow:endRow, ]        <- cbind( reps, obj$mp$assess$Rt )
    
    # Retrospective statistics.
    blob$mp$assess$retroExpBt[ startRow:endRow, ]   <- cbind( reps, obj$mp$assess$retroExpBt )
    blob$mp$assess$retroExpBtS[ startRow:endRow, ]  <- cbind( reps, obj$mp$assess$retroExpBtS )
    blob$mp$assess$retroSpawnBt[ startRow:endRow, ] <- cbind( reps, obj$mp$assess$retroSpawnBt )
    blob$mp$assess$retroFt[ startRow:endRow, ]      <- cbind( reps, obj$mp$assess$retroFt )		 #RF_added
    blob$mp$assess$retroRt[ startRow:endRow, ]      <- cbind( reps, obj$mp$assess$retroRt )    
    
    for ( k in 1:.NRETROSTATS )
      blob$mp$assess$retroVals[ c(startRow:endRow),,k ] <- cbind( reps, obj$mp$assess$retroVals[,,k] )

    blob$mp$assess$retroStats[ i, ] <- c( i, obj$mp$assess$retroStats )
    
    writeProgress( i, nReps )

  }     # END simulation loop over replicates.

  # Label columns of the blob:
  
  # opMod variable names
  tmpNames <- paste("B",c(1:nT),sep="")
  colnames( blob$om$Bt ) <- c( "iRep", tmpNames )
  tmpNames <- paste("N",c(1:nT),sep="")
  colnames( blob$om$Nt ) <- c( "iRep", tmpNames )
  tmpNames <- paste("Bexp",c(1:nT),sep="")
  colnames( blob$om$Bexp ) <- c( "iRep", tmpNames )
  tmpNames <- paste("BexpS",c(1:nT),sep="")
  colnames( blob$om$BexpS ) <- c( "iRep", tmpNames )
  tmpNames <- paste("Nexp",c(1:nT),sep="")
  colnames( blob$om$Nexp ) <- c( "iRep", tmpNames )
  tmpNames <- paste("R",c(1:nT),sep="")
  colnames( blob$om$Rt ) <- c( "iRep", tmpNames )
  tmpNames <- paste("I",c(1:nT),sep="")
  colnames( blob$om$It ) <- c( "iRep", tmpNames )
  tmpNames <- paste("ITrue",c(1:nT),sep="")
  colnames( blob$om$ItTrue ) <- c( "iRep", tmpNames )
  tmpNames <- paste("qt",c(1:nT),sep="")
  colnames( blob$om$qt ) <- c( "iRep", tmpNames )
  tmpNames <- paste("D",c(1:nT),sep="")
  colnames( blob$om$Dt ) <- c( "iRep", tmpNames )
  tmpNames <- paste("F",c(1:nT),sep="")
  colnames( blob$om$Ft ) <- c( "iRep", tmpNames )
  tmpNames <- paste("M",c(1:nT), sep="")
  colnames( blob$om$Mt ) <- c( "iRep", tmpNames )  
  tmpNames <- paste("alphat",c(1:nT), sep="")
  colnames( blob$om$alphat ) <- c( "iRep", tmpNames )  
  tmpNames <- paste("rhot",c(1:nT), sep="")
  colnames( blob$om$rhot ) <- c( "iRep", tmpNames )  
  tmpNames <- paste("surveyCV",c(1:nT), sep="")
  tmpNames <- c( "iRep", tmpNames )
  colnames( blob$om$surveyCV ) <- tmpNames

  # mp variable names
  tmpNames <- paste("targF",c(1:nT),sep="")
  colnames( blob$mp$hcr$targetFt ) <- c( "iRep", tmpNames )
  tmpNames <- paste("I",c(1:nT),sep="")
  colnames( blob$mp$data$It ) <- c( "iRep", tmpNames )
  tmpNames <- paste("Bref",c(1:nT),sep="")
  colnames( blob$mp$hcr$Bref ) <- c( "iRep", tmpNames )
  tmpNames <- paste("Fref",c(1:nT),sep="")
  colnames( blob$mp$hcr$Fref ) <- c( "iRep", tmpNames )
  tmpNames <- paste("LRP",c(1:nT),sep="")
  colnames( blob$mp$hcr$lowerBound ) <- c( "iRep", tmpNames )
  tmpNames <- paste("USR",c(1:nT),sep="")
  colnames( blob$mp$hcr$upperBound ) <- c( "iRep", tmpNames )
 
  tmpNames <- paste("Ft",c(1:nT),sep="")
  colnames( blob$mp$assess$Ft ) <- c( "iRep", tmpNames )	 #RF_added
  
  tmpNames <- paste("spawnBt",c(1:nT),sep="")
  colnames( blob$mp$assess$spawnBt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("exploitB",c(1:nT),sep="")
  colnames( blob$mp$assess$exploitBt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("exploitBS",c(1:nT),sep="")
  colnames( blob$mp$assess$exploitBtS ) <- c( "iRep", tmpNames )

	tmpNames <- names(obj$mp$assess$mpdPars)
	colnames( blob$mp$assess$mpdPars ) <- c( "iRep", tmpNames )

	tmpNames <- names(obj$mp$assess$pdfPars)
	colnames( blob$mp$assess$pdfPars ) <- c( "iRep", tmpNames )
	
	tmpNames <- names(obj$mp$assess$runStatus)
	colnames( blob$mp$assess$runStatus ) <- c( "iRep", tmpNames )
	
	tmpNames <- paste("Rt",c(1:nT),sep="")
	colnames( blob$mp$assess$Rt )<-c("iRep", "tStep", tmpNames )
  	
  tmpNames <- paste( "retroExpBt",c(1:nT),sep="" )
  colnames( blob$mp$assess$retroExpBt )<-c("iRep", "tStep", tmpNames )

  tmpNames <- paste( "retroExpBtS",c(1:nT),sep="" )
  colnames( blob$mp$assess$retroExpBtS )<-c("iRep", "tStep", tmpNames )

  tmpNames <- paste( "retroSpawnBt",c(1:nT),sep="" )
  colnames( blob$mp$assess$retroSpawnBt )<-c("iRep", "tStep", tmpNames )

  tmpNames <- paste( "retroFt",c(1:nT),sep="" )						#RF_added, ARK: Corrected from 1:nT-1
  colnames( blob$mp$assess$retroFt )<-c("iRep", "tStep", tmpNames )

  tmpNames <- paste( "retroRt",c(1:nT),sep="" )
  colnames( blob$mp$assess$retroRt )<-c("iRep", "tStep", tmpNames )
  
  tmpNames <- paste( "retroVals",c(1:nT),sep="" )
  dimnames( blob$mp$assess$retroVals ) <- list( NULL,c( "iRep","tStep", tmpNames ),
    c( "SumRelErr","RetroStat2","RetroStat3","RetroStat4","RetroStat5" ) )
    
  tmpNames <- paste( "retroStats", c(1:.NRETROSTATS), sep="" )
  tmpNames[1] <- "SumRelErr"
  dimnames( blob$mp$assess$retroStats ) <- list( NULL, c("iRep",tmpNames ))

  return( blob )
}     # END function .mgmtProc


# .solveProjPop
# Purpose:        optimisation algorithm to solve for omniscient Fs for whole
#                 projection period tMP:nT.
# Parameters:     obj=list containing all objects and parameters generated by
#                 gui, createMP, and initPop; tMP=time when management procedure begins,
#                 nT=time when simulation ends
# Returns:        a list with initialised operating model states, data, and Fs for
#                 period 1:nT. 
# Source:         S.P. Cox   
# NOTE: need some objectives input via simCtlFile
.solveProjPop <- function( obj )
{
  # runModel
  # Purpose:        Private function to run operating model and calculate objective
  #                 function based on depletion, cumulative catch, and closures
  # Parameters:     pars=log F values for tMP:nT
  # Returns:        List containing operating model states and objective function
  #                 value f=penalty for B < Bmsy plus log(cumulative catch).
  #                 Multiplier 1000 is for desired precision
  # Source:         S.P. Cox
  runModel <- function( pars, obj )
  {
    # target depletion level - input from GUI
    initDepTarg <- obj$opMod$initDepTarg
    maxF        <- obj$opMod$initMaxF
    Fmsy        <- obj$refPts$Fmsy
    tMP         <- obj$opMod$tMP
    nT          <- obj$opMod$nT
    
    # There are n = nT - tMP + 1 F parameters, so set F[nT-1]=F[n]
    # and spread the remaining Fs out over the interval
    # tMP - (nT-2)
    tmpF     <- exp( pars ) # "pars" are the log-Fs
    interval <- nT - tMP - 1
    nKnots   <- length( tmpF )
    space    <- interval / nKnots
    knotPts  <- round( seq( from=space, to=nKnots*space, by=space ) )
    
    # Points for fitting interpolation spline to knot points and Fs
    x <- c( 1, knotPts )
    y <- c( 0, tmpF )
    
    # Use spline to interpolate Fs between knots
    initF <- spline( x,y,n=(nT-tMP+1) )$y
    initF[ initF < 0 ]    <- 0.
    initF[ initF > maxF ] <- maxF
    
    # Set all initial Fs in operating model
    obj$om$Ft[tMP:nT] <- initF*solveObj$mp$hcr$Fmult

    # run model for the pre-MP period
    for ( t in (tMP-1):nT )
    {
      obj <- asOMod( obj,t )
    }
    
    # Barrier penalty on ssb < Bmsy---------------
    Bmsy  <- obj$refPts$ssbFmsy
    Bt    <- obj$om$Bt[tMP:nT]
    bar                <- rep(0,length(Bt))
    bar[ Bt <= Bmsy ]  <- (Bt[Bt<=Bmsy]-Bmsy)^2 # penalty grows quadratically below Bmsy
    bar[ Bt > Bmsy ]   <- (-1.)*log( Bt[Bt>Bmsy] - Bmsy ) 
    
    barBt <- barrierPen( x= Bt, xBar=Bmsy, above=TRUE )
    
    # Average catch------------
    Dt <- obj$om$Dt[(tMP-1):nT] 
    fAvgCatch <- mean( Dt )
    
    # Barrier penalty on AAV > 50%------------
    # Form the absolute catch differences over tMP:nT
    diffDt    <- diff(Dt)
    absDiffDt <- abs( diffDt )

    # Sum the absolute differences
    sumAbsDiffDt <- sum( absDiffDt )
      
    # Sum the catchover the period.
    sumCatch <- sum( Dt )
      
    # Compute the AAV
    AAV <- 100.*sumAbsDiffDt / sumCatch
    
    barAAV <- barrierPen( x=AAV, xBar=50, above=FALSE )
    
    # Count up fishery closures, or Dt < Dmin
    Dmin        <- 0.5
    closedCount <- rep(0,length(Dt)) 
    closedCount[ Dt < Dmin ] <- 1.0
    penClosed   <- 10.*sum( closedCount )
    
    # Total objective function
    f <- - log(fAvgCatch) + 10.*barBt + barAAV + penClosed

    result   <- obj
    result$f <- f
    result
  }     # END function runModel


  # getObjFunctionVal
  # Purpose:        Private function to run operating model and extract objective function
  # Parameters:     pars=log-Fs for t=tMP,...nT
  # Returns:        Objective function f=barrier penalty for biomass less than Bmsy plus
  #                 log(cumulative catch) and penalty for deviation from Fmsy. 
  #                 Multiplier 1000 is for desired precision
  # Source:         S.P. Cox
  getObjFunctionVal <- function( pars, obj ){
    # Function to run asOM and return objective function
    val <- runModel( pars, obj )$f
    val
  }     # END function getObjFunctionVal

  #-----Beginning optimisation step--------#
  # all the stuff needed by optimization procedure
  solveObj <- list( opMod=obj$opMod, om=obj$om, mp=obj$mp, refPts=obj$refPts )

  # Initial F values: pick a bunch of random F multipliers of Fmsy to initialize
  initF    <- log(obj$refPts$Fmsy*exp(rnorm(n=obj$opMod$initKnots,sd=.3)))
  
  # If the mpLabel uses the OMNI keyword, setup and run optimizations
  if( obj$gui$mpLabel=="OMNI" )
  {
      # find Fs that give target depletion and max log(cumulative catch)
      # SPC 12-Aug-2014: need to add some of the optimization outputs to blob so we
      #                  can monitor optimization perfomance
      optFs  <- optim( par=initF, fn=getObjFunctionVal, method="Nelder-Mead",
                         obj=solveObj, control=list(maxit=3000, reltol=0.001,ndeps=c(.01,.01) )  )
      cat( "Optimization for this rep completed with:\n ")
      print( optFs ) 
      logFt  <- optFs$par   
  }
  # If the mpLabel is NoFish keyword, multiply initial guess initF by 0
  if( obj$gui$mpLabel=="NoFish" )
  {
    solveObj$mp$hcr$Fmult <- 0. 
    logFt                 <- initF   
  }

  # re-generate the operating model and data objects with the optimised Fs
  solveObj    <- runModel( logFt, obj=solveObj )    
 
  obj$om      <- solveObj$om
  # Initialize all Fts = NA for t  tMP
  obj$mp$data <- solveObj$mp$data
  obj$opMod   <- solveObj$opMod
  obj
}     # END function .solveProjPop

# barrierPen
# Purpose:        Imposes an increasingly large penalty as a quantity x 
#                 approaches a pre-determined barrier. Approaches from above
#                 and below are treated differently depending on user preference
#                 e.g., for keeping Bt > Bmsy one would use above=TRUE, but for 
#                 AAV < 50%, one would use below=TRUE
# Parameters:     x - vector (or scalar) quantity to test, xBar is the barrier, above
#                 determines from which direction the penalty is applied
# Returns:        scalar penalty value
# Source:         S.P. Cox
barrierPen <- function( x, xBar, above=TRUE)
{
  tmp <- rep(0,length(x))
  if( above )
  {   
    # Penalty grows quadratically below barrier xBar
    tmp[ x <= xBar ]  <- ( x[x<=xBar]-xBar )^2 
  
    # Penalty grows to Inf as Bt approaches Bmsy from above
    tmp[ x > xBar ]   <- (-1.)*log( x[x>xBar] - xBar )
  }
  else
  {
    # Penalty grows quadratically above barrier xBar
    tmp[ x >= xBar ]  <- ( x[x>=xBar]-xBar )^2 

    # Penalty grows to Inf as Bt approaches Bmsy from below
    tmp[ x < xBar ]   <- (-1.)*log( xBar - x[x<xBar] )
  }
  bar <- sum(tmp)
  return( bar )
}



# .initPop
# Purpose:        initialise all operating model (om), management procedure (mp),
#                 and parameter (pars) objects, for period 1 - tMP.
# Parameters:     obj=list containing all objects and parameters generated by
#                 gui and createMP
# Returns:        a new list with objects for all state variables, ref pts, etc.
#                 Operating model states will be initialised for pre-MP period such
#                 that spawning biomass Bt is approx. initDepTarg*B0, where initDepTarg
#                 is input from the gui
# Source:         S.P. Cox
.initPop <- function( obj )
{
  # Get simulation dimensions.
  A   <- obj$opMod$nAges
  nT  <- obj$opMod$nT
  tMP <- obj$opMod$tMP
  rep <- obj$opMod$rep

  # -----Generate time-varying parameters------------------#

  # Natural mortality: lag-1 autocorrelated, log-normal process errors,
  #                    No linear trend, or 50% higher during pulse yrs
  Mt          <- vector( mode="numeric", length=nT )
  
  # Random walk M scaled to mean==1
  
  deltaM      <- rnorm( n=nT,mean=0,sd=1 )  # Random normal deviates.
  sigmaM      <- obj$opMod$sigmaM           # Natural mortality rate CV.
  gammaM      <- obj$opMod$gammaM           # Lag-1 autocorr in M
  ranM        <- .fillRanWalk( gamma=gammaM, sigma=sigmaM, deltat=deltaM )
  ranM        <- ranM/mean( ranM ) 
  Mt[1]       <- obj$opMod$M                # Scale Mt[1] to equal input mean M.
  Mt[2:nT]    <- Mt[1] * ranM[ c(2:nT) ]
  # Trend M
  trendM     <- (log(obj$opMod$endM) - log(Mt[1]))/(nT-1)

  Mt[2:nT]    <- Mt[1]*exp( trendM*c(2:nT) )*ranM[2:nT]
  # PulseM
  nPulse      <- obj$opMod$pulseM*as.integer(nT/10.)
  pulseYrs    <- sample( x=c(1:nT), size=nPulse, replace=F )  
  Mt[pulseYrs]<- Mt[pulseYrs]*1.5
  
  # Output
  obj$om$Mt <- Mt

  # Recruitment standard normals for t = 1,..nT.
  deltat <- rnorm( n=nT,mean=0,sd=1 )

  # Generate lag-1 autocorrelated recruitment deviations.
  gammaR <- obj$opMod$gammaR # lag-1 autocorrel.
  sigmaR <- obj$opMod$sigmaR # std error
  omegat <- .calcRdevs( gamma=gammaR,sigmaR,deltat )

  # Walford intercept: lag-1 autocorrelated, log-normal process errors,
  #                    linear trend
  alphat      <- vector( mode="numeric", length=nT )
  
  # Random walk Walford intercept scaled to mean==1
  delta.alpha <- rnorm( n=nT,mean=0,sd=1 )  # Random normal deviates.
  sigma.alpha <- obj$opMod$sigma.alpha      # Walford intercept.
  gamma.alpha <- obj$opMod$gamma.alpha # Lag-1 autocorr

  ran.alpha   <- .fillRanWalk( gamma=gamma.alpha, sigma=sigma.alpha, 
                               deltat=delta.alpha )
  ran.alpha   <- ran.alpha/mean( ran.alpha ) 
  alphat[1]   <- obj$opMod$alpha_g       # Scale alphat[1] to equal input mean.
  alphat[2:nT]<- alphat[1] * ran.alpha[ c(2:nT) ]
  
  # Trend in alphat
  trend.alpha     <- (log(obj$opMod$end.alpha) - log(alphat[1]))/(nT-1)
  alphat[2:nT]    <- alphat[1]*exp( trend.alpha*c(2:nT) )*ran.alpha[2:nT]

  # Output
  obj$om$alphat <- alphat
  obj$om$rhot   <- obj$opMod$rho.int + obj$om$opMod$rho.slope*alphat

  # Time-varying catchability
  # First, fill whole time series with constant q = qSurvey
  obj$om$qt <- rep(obj$opMod$qSurvey,times=nT)
  # Simulate two qs. qSurvey from simCtlFile applies for
  # qChangeTime <= t <= nT and q1q2Ratio*qSurvey is used for t < qChangeTime
  obj$om$qt[1:(obj$opMod$qChangeTime-1)] <- obj$opMod$q1q2Ratio*obj$opMod$qSurvey
  # To keep 1 constant q over entire series, just set q1q2Ratio=1 in simCtlFile
  
  # -----End time-varying parameters------------------#

  # Survey std normal errors
  epsilont <- rnorm( nT,0,1 )

  # Age proportion errors
  epsilonat <- matrix( rnorm( A*nT, mean=0, sd=1 ), nrow=A, ncol=nT )		   #fishery
  epsilonatS <- matrix( rnorm( A*nT, mean=0, sd=1 ), nrow=A, ncol=nT )		  #survey

  # Age proportions observation standard error.  
  tauAge    <- obj$mp$data$tauAge	      #Fishery
  tauAgeS    <- obj$mp$data$tauAgeS	  #Survey
  
  # Start and end time of surveys.
  t1Survey  <- attributes( obj$mp$data$It )$t1Survey
  t2Survey  <- attributes( obj$mp$data$It )$t2Survey

  # Generate the observation error CVs for the Old and New survey periods
  surveyError <- rep( 0, nT )
  
  # Draw CVs from inverse gamma statistical distribution between Min and Max CV.
  
  # First survey period
  tauSurvey1Mean <- obj$mp$data$tauSurvey1Mean   # Assuming 1st input is mean CV
  tauSurvey1Var  <- obj$mp$data$tauSurvey1SD^2   # Second input is SD of CV
  
  alpha <- tauSurvey1Mean^2/tauSurvey1Var + 2.
  beta  <- tauSurvey1Mean*( tauSurvey1Mean^2/tauSurvey1Var + 1. )
  surveyError[t1Survey:(t2Survey-1)] <- rinvgamma( n=(t2Survey - t1Survey), shape=alpha, scale=beta )
  
  # Second survey period
  tauSurvey2Mean <- obj$mp$data$tauSurvey2Mean
  tauSurvey2Var  <- obj$mp$data$tauSurvey2SD^2

  alpha <- tauSurvey2Mean^2/tauSurvey2Var + 2.
  beta  <- tauSurvey2Mean*( tauSurvey2Mean^2/tauSurvey2Var + 1. )    
  surveyError[t2Survey:nT] <- rinvgamma( n=(nT - t2Survey + 1),   shape=alpha, scale=beta )
  
  if ( !obj$mp$data$tauRandom )
  {
    # Survey errors fixed.
    tauSurvey1Mean <- obj$mp$data$tauSurvey1Mean
    tauSurvey2Mean <- obj$mp$data$tauSurvey2Mean
    # First survey period
    surveyError[t1Survey:(t2Survey-1)] <- tauSurvey1Mean
    # Second survey period
    surveyError[t2Survey:nT]           <- tauSurvey2Mean
  }
  # Normally-distributed survey errors
  epsilont <- surveyError*rnorm(nT,0,1) - surveyError*surveyError/2.

  # Assign to "errors" object within operating model list. BUT IT ISN'T
  obj$om$surveyCV <- surveyError

  obj$om$errors <- list( omegat=omegat,
                         deltat=deltat,
                         epsilont=epsilont,
                         epsilonat=epsilonat,
			             epsilonatS=epsilonatS
                       )

  # Given target depletion level and stochastic recruitment find the fishing
  # rates Ft[1:tMP-1] that give stock status as near as possible to target
  # depletion at tMP-1, then fill operating model object with states, data, etc.
  tmp               <- .solveInitPop( obj, tMP )
  obj$om            <- tmp$om
  obj$om$surveyCV   <- surveyError
  obj$mp$data$It    <- tmp$mp$data$It
  obj$mp$data$pat   <- tmp$mp$data$pat	   # Fishery
  obj$mp$data$patS  <- tmp$mp$data$patS    # Survey

  obj
}     # END function .initPop

# .solveInitPop
# Purpose:        optimisation algorithm to initialise the operating
#                 model spawning biomass Bt to target initial depletion at tMP-1.
# Parameters:     obj=list containing all objects and parameters generated by
#                 gui, createMP, and initPop; tMP=time when management procedure begins
# Returns:        a list with initialised operating model states and data for
#                 period 1 - tMP. Biomass Bt should be near initTargDep as set in gui
# Source:         S.P. Cox
.solveInitPop <- function( obj, tMP )
{
  # runModel
  # Purpose:        Private function to run operating model and calculate objective
  #                 function based on depletion and cumulative catch
  # Parameters:     pars=two multipliers that scale Ft to M for t=1,2,...tMP-1
  # Returns:        List containing operating model states and objective function
  #                 value f=squared deviation from initDepTarg plus log(cumulative catch).
  #                 Multiplier 1000 is for desired precision
  # Source:         S.P. Cox
  runModel <- function( pars, obj )
  {
    # target depletion level - input from GUI
    initDepTarg <- obj$opMod$initDepTarg
    maxF        <- obj$opMod$initMaxF
    Fmsy        <- obj$refPts$Fmsy
    tMP         <- obj$opMod$tMP
    
    # There are n F parameters, so set F[tMP-1]=F[n]
    # and spread the remaining Fs out over the interval
    # 2 - (tMP-2)
    tmpF     <- exp( pars ) # "pars" are the log-Fs
    interval <- tMP - 1
    nKnots   <- length( tmpF )
    space    <- interval / nKnots
    knotPts  <- round( seq( from=space, to=nKnots*space, by=space ) )
    
    # Points for fitting interpolation spline to knot points and Fs
    x <- c( 1, knotPts )
    y <- c( 0, tmpF )
    
    if ( obj$opMod$historyType!="omFile" )
    {    
      # Use spline to interpolate Fs between knots
      initF <- spline( x,y,n=(tMP-1) )$y
    }
    else
    {
      initF <- tmpF
    }
    initF[ initF < 0 ]    <- 0.
    initF[ initF > maxF ] <- maxF
    
    # Set all initial Fs in operating model
    obj$om$Ft[1:(tMP-1)] <- initF
    obj$om$Ft[tMP:nT]    <- NA
    
    # run model for the pre-MP period
    for ( t in 1:(tMP-1) )
    {
      obj <- asOMod( obj,t )
    }

    # realised depletion
    initBio <- obj$opMod$B0
    termBio <- obj$om$Bt[(tMP-1)]
    realDep <- termBio / initBio

    # objective function components
    # Target depletion
    fDep    <- (realDep - initDepTarg)^2
    # Average catch
    fAvgCatch <- mean( obj$om$Dt[1:(tMP-2)] )
    # F penalty for deviations from Fmsy
    ssF <- sum( (initF[2:(tMP-1)]-Fmsy)^2. )
    # Combined
    f <- 1000.*fDep - log(fAvgCatch) + ssF

    result   <- obj
    result$f <- f
    result
  }     # END function runModel


  # getObjFunctionVal
  # Purpose:        Private function to run operating model and extract objective function
  # Parameters:     pars=two multipliers that scale Ft to M for t=1,2,...tMP-1
  # Returns:        Objective function f=squared deviation from initDepTarg plus
  #                 log(cumulative catch). Multiplier 1000 is for desired precision
  # Source:         S.P. Cox
  getObjFunctionVal <- function( pars, obj ){
    # Function to run asOM and return objective function
    val <- runModel( pars, obj )$f
    val
  }     # END function getObjFunctionVal

  #-----Beginning optimisation step--------#
  # Use M to scale Fs to whatever species is being simulated
  M <- obj$opMod$M
  # all the stuff needed by optimization procedure
  solveObj <- list( opMod=obj$opMod, om=obj$om, mp=obj$mp, refPts=obj$refPts )

  if( obj$opMod$historyType=="omAuto" )
  {
    # Target depletion specified in opMod input
    targDep <- obj$opMod$initDepTarg
    # Initial F values: the length depends on opMod input number of spline knots
    initF <- M * rep( 1., obj$opMod$initKnots )
    # log to constrain all initF > 0
    initF <- log( c(initF) )

    # find Fs that give target depletion and max log(cumulative catch)
    optFs  <- optim( par=initF, fn=getObjFunctionVal, method="Nelder-Mead",
                       obj=solveObj, control=list(reltol=0.001,ndeps=c(.01,.01) )  )
    
    # re-generate the operating model and data objects with the optimised Fs
    solveObj    <- runModel( optFs$par, obj=solveObj )    
  }
  
  if( obj$opMod$historyType != "omAuto" )
  {
    .DEFHSTFILE <<- obj$opMod$historyType
    hstFile     <- file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE )
    historyVals <- read.csv( hstFile, header=T)
    # Is the history file consistent with tMP?
    if ( nrow(historyVals)==(obj$opMod$tMP-1) )
    {
      # initial recDevs, Ft, Mt come from input file
      # log-recDevs
      solveObj$om$errors$omegat[1:(tMP-1)] <- exp(historyVals$omegat)
      # Ft
      initF <- log( historyVals$Ft ) # will be back-trans in runModel()
      
      # Mt
      # if Mt exists in the file, then replace simulated vals w history
      # and re-scale projection period Mt vals to match last input
      if ( any(names(historyVals)=="Mt") )
      {
        nT  <- solveObj$opMod$nT
        tMP <- solveObj$opMod$tMP

        # grab existing Mt values
        tmpM <- solveObj$om$Mt 
        
        # Generate random walk for projection period
        tStart    <- tMP-1 
        tInterval <- nT-tMP+2
        Mt        <- vector(mode="numeric",length=tInterval)
        deltaM    <- rnorm( n=tInterval,mean=0,sd=1 )  # Random normal deviates.
        sigmaM    <- obj$opMod$sigmaM           # Natural mortality rate CV.
        gammaM    <- obj$opMod$gammaM           # Lag-1 autocorr in M
        ranM      <- .fillRanWalk( gamma=gammaM, sigma=sigmaM, deltat=deltaM )
        ranM      <- ranM/mean( ranM )
        
        # the first projected Mt value
        Mt[1]  <- historyVals$Mt[tStart]
        # the rest...
        Mt[2:tInterval] <- Mt[1] * ranM[-1]
        
        # adding trend in Mt from tMP-1 to nT
        trendM     <- (log(obj$opMod$endM) - log(Mt[1]))/tInterval
        Mt[2:tInterval] <- Mt[1]*exp( trendM*c(2:tInterval) )*ranM[2:tInterval]
        
        # PulseM
        nPulse      <- obj$opMod$pulseM*as.integer(tInterval/10.)
        pulseYrs    <- sample( x=c(tMP:nT), size=nPulse, replace=F )  
        Mt[pulseYrs]<- Mt[pulseYrs]*1.5

        # replace history with input Mt values
        solveObj$om$Mt[1:(tMP-1)] <- historyVals$Mt
        if( obj$opMod$useBoot )
            solveObj$om$Mt[tMP:nT]    <- tsBoot( x=historyVals$Mt, length=length(tMP:nT), 
                                                 tolSD=obj$opMod$tolSD,
                                                 tolMult=obj$opMod$tolMult )
        else
            solveObj$om$Mt[tMP:nT]    <- Mt[-1]
      }
      if ( any(names(historyVals)=="alphat") )
      {
        nT  <- solveObj$opMod$nT
        tMP <- solveObj$opMod$tMP

        # grab existing Walford alphat values
        tmp.alpha <- solveObj$om$alphat 
        
        # Generate random walk for projection period
        tStart    <- tMP-1 
        tInterval <- nT-tMP+2
        alphat    <- vector(mode="numeric",length=tInterval)
        delta.alpha    <- rnorm( n=tInterval,mean=0,sd=1 )# Random normal deviates.
        sigma.alpha    <- obj$opMod$sigma.alpha           # Natural mortality rate CV.
        gamma.alpha    <- obj$opMod$gamma.alpha           # Lag-1 autocorr in M
        ran.alpha      <- .fillRanWalk( gamma=gamma.alpha, sigma=sigma.alpha, 
                                   deltat=delta.alpha )
        ran.alpha      <- ran.alpha/mean( ran.alpha )
        
        # the first projected Mt value
        alphat[1]  <- historyVals$alphat[tStart]
        # the rest...
        alphat[2:tInterval] <- alphat[1] * ran.alpha[-1]
        
        # adding trend in Mt from tMP-1 to nT
        trend.alpha     <- (log(obj$opMod$end.alpha) - log(alphat[1]))/tInterval
        alphat[2:tInterval] <- alphat[1]*exp( trend.alpha*c(2:tInterval) )*ran.alpha[2:tInterval]
        
        # replace history with input alphat values
        solveObj$om$alphat[1:(tMP-1)] <- historyVals$alphat
        solveObj$om$alphat[tMP:nT]    <- alphat[-1]
        solveObj$om$rhot <- obj$opMod$rho.int + obj$opMod$rho.slope*solveObj$om$alphat
        
      }

      # If a column "initN" exists in the history file, add the non-NA
      # values to operating model. These will be used to set the initial
      # age-composition at t=1 to values other than equilibrium. Otherwise,
      # set multipliers to 1
      if ( any(names(historyVals)=="initN") )
        solveObj$om$initN <- na.omit(historyVals$initN)
      else
        solveObj$om$initN <- rep(1,solveObj$opMod$nAges)
      
    }
    else
    {
      cat( "\nERROR: History file dimensions inconsistent with tMP.\n" )
      stop()
    }

    solveObj$historyType <- obj$opMod$historyType
    solveObj    <- runModel( initF, obj=solveObj )
  }
  
  obj$om      <- solveObj$om
  obj$om$Ft[tMP:nT] <- NA
  obj$mp$data <- solveObj$mp$data
  obj$opMod   <- solveObj$opMod

  # Fish population should now be close to the initial target
  # depletion regardless of the stochastic recruitment history.
  # Note that there might be a consistent, small bias from the target.
  # Initialize DEADFLAG to false in case it was tripped on last rep.  
  obj
}     # END function .solveInitPop

#------------------------------------------------------------------------------#
#--                        Operating Model Functions                         --#
#------------------------------------------------------------------------------#

# asOMod          age-structured operating model
# Purpose:        advance population age-structure one time step,
#                 generate survey observation and Ft for one time step
# Parameters:     obj=complete simulation object up to t-1; t=current time
# Returns:        result=complete simulation object up to t
# Source:         S.P. Cox
asOMod <- function( obj, t )
{
  # Extract constants.
  nT     <- as.numeric( obj$opMod$nT )
  A      <- as.numeric( obj$opMod$nAges )
  tMP    <- as.numeric( obj$opMod$tMP )

  # Set parameter values.
  B0         <- obj$opMod$B0
  rSteepness <- obj$opMod$rSteepness
  M          <- obj$opMod$M            # Average M without trend or pulse.
  Mt         <- obj$om$Mt              # M may be time-varying.

  # maturity ogive parameters
  aMat50    <- obj$opMod$aMat50
  aMat95    <- obj$opMod$aMat95

  # selectivity ogive parameters (fishery)
  aSel50    <- obj$opMod$aSel50
  aSel95    <- obj$opMod$aSel95

   # selectivity ogive parameters (survey)
  aSelS50    <- obj$opMod$aSelS50
  aSelS95    <- obj$opMod$aSelS95

  qSurvey   <- obj$opMod$qSurvey    # survey catchability
  qt        <- obj$om$qt

  sigmaR    <- obj$opMod$sigmaR     # std error in log-recruitment
  sigmaL    <- obj$opMod$sigmaL     # std error in length
  gamma     <- obj$opMod$gamma      # autocorrelation in log-recruitment
  tauSurvey <- obj$opMod$tauSurvey  # std error in log-survey
  tauAge    <- obj$mp$data$tauAge   # std error in age-proportion residuals FISHERY
  tauAgeS    <- obj$mp$data$tauAgeS   # std error in age-proportion residuals  SURVEY

  # Extract derived parameters.
  rec.a     <- obj$opMod$rec.a      # recruitment slope
  rec.b     <- obj$opMod$rec.b      # recruitment dd parameter

  sigmaR <- obj$opMod$sigmaR

  R0         <- obj$opMod$R0        # unfished recruitment
  numAgeYr1  <- obj$opMod$numAgeYr1 # initial age-comp
  wtAge      <- obj$opMod$wtAge     # weight-age schedule

  # Vector of recruitments.
  Rt    <- obj$om$Rt          # recruitment (same as Nat[1,t])

  # Extract objects needed to hold model results.
  # State variables: Numbers at age, time and biomass at age, time.
  Nat   <- obj$om$Nat
  Bat   <- obj$om$Bat

  # SPC(17Oct2013): adding den-dep growth...
  Lat   <- obj$om$Lat    # length-age, matrix here for time-dep growth effect
  alphat<- obj$om$alphat # walford growth intercept
  rhot  <- obj$om$rhot   # walford slope (note: computed from alpha vs rho regression
  vonK  <- obj$opMod$vonK# to keep consistent with historical data         
  L1    <- obj$opMod$L1
  c1    <- obj$opMod$c1
  c2    <- obj$opMod$c2

  Bt    <- obj$om$Bt
  Nt    <- obj$om$Nt
  Bexp  <- obj$om$Bexp
  BexpS <- obj$om$BexpS
  Nexp  <- obj$om$Nexp

  # True age proportions
  uat   <- obj$om$uat #FISHERY
  uatS  <- obj$om$uatS #FISHERY

  # Obs age proportions
  pat   <- obj$mp$data$pat     # Fishery
  patS  <- obj$mp$data$patS    # Survey

  # Deterministic survey biomass index (may be absolute)
  ItTrue <- obj$om$ItTrue

  # Controls: catch numbers, biomass, and fishing rate.
  Ct <- obj$om$Ct
  Dt <- obj$om$Dt
  Ft <- obj$om$Ft

  # Data: survey biomass.
  It <- obj$mp$data$It

  # Extract standard normal deviates for errors.
  deltat     <- obj$om$errors$deltat      # std normal error in log-recruitment
  epsilont   <- obj$om$errors$epsilont    # std normal error in log-survey
  omegat     <- obj$om$errors$omegat      # autocorrelated recruitment errors
  epsilonat  <- obj$om$errors$epsilonat   # std normal age prop errors fishery
  epsilonatS <- obj$om$errors$epsilonatS  # std normal age prop errors survey

  mat  <- .calcLogistic( a50=aMat50,  a95=aMat95,A=A )		
  sel  <- .calcLogistic( a50=aSel50,  a95=aSel95,A=A )	  # Fishery
  selS <- .calcLogistic( a50=aSelS50, a95=aSelS95,A=A )	  # Survey 
  initNmult <- obj$om$initN

  #initNmult <- c(1.26851, 0.728094, 0.413276, 0.678655, 0.351059, 0.968375,
  #               2.61354, 0.737864, 0.168681, 0.231014, 0.257877, 0.0280677
  #               )
  # Initialise population if t=1, else update variables from last time step.
  if ( t==1 )
  {
    # Recruitment, number-at-age, biomass-at-age
    Rt[t]   <- numAgeYr1[1]
    Nat[,t] <- numAgeYr1
    Nat[,t] <- Nat[,t]*initNmult
    Bat[,t] <- Nat[,t]*wtAge

    # Spawning biomass and abundance.
    Bt[t] <- sum( Bat[,t]*mat )
    Nt[t] <- sum( Nat[,t]*mat )

    # time-varying size-at-age
    alpha <- alphat[t]
    rho   <- rhot[t]
    # Time-varying growth via Walford intercept. For t=1, assume growth
    # is all at equil.
    Lat[,t] <- .calcLenAge(A=A, Linf=alpha/(1-rho), L1=alpha*(1+rho), vonK=-log(rho))
    # Fill length-at-age for existing cohorts.
    for( a in 1:(A-1) )
    {   
      # fill down and across age-time until cohort reaches A
      for( tt in (t+1):(t+A) )
      {    
        Lat[a+1,tt] <- alpha + rho*Lat[a,tt-1]
      }    
    }  
    # Current weight-age is the t-th column
    wtAge     <- .calcWtAge( c1=c1,c2=c2,lenAge=Lat[,t], sigmaL=0 )


    # Exploitable biomass and abundance
    Bexp[t]  <- sum( Bat[,t]*sel )	  # Fishery
    BexpS[t] <- sum( Bat[,t]*selS )   # Survey
    Nexp[t]  <- sum( Nat[,t]*sel )

    # Calculate the catch: during t<tMP, Ft is input according to initialisation
    # algorithm.

    catAge <- Nat[,t]*(1.-exp(-Mt[t]-sel*Ft[t]))*sel*Ft[t]/(Mt[t]+sel*Ft[t])    
    Dt[t]  <- sum( catAge*wtAge )
  }
  else  # t > 1, update population 1 time step
  {
    # Calculate age-1 recruitment, Beverton-Holt stock-recruitment.
    Rt[t] <- omegat[t]*rec.a*Bt[t-1]/( 1.0 + rec.b*Bt[t-1] )

    # Time-varying growth via Walford intercept. For t>1, assume growth
    # depends on time-varying alpha.
    alpha <- alphat[t]
    rho   <- rhot[t]
    # Fill length-at-age for this cohort.
    # fill down and across age-time until cohort reaches A
    Lat[1,t] <- alpha*(1+rho)
    for( aa in 2:A )
    {    
      tt         <- min(t + aa -1,nT)
      Lat[aa,tt] <- alpha + rho*Lat[aa-1,tt-1]
    }    
    # Current weight-age is the t-th column
    wtAge     <- .calcWtAge( c1=c1,c2=c2,lenAge=Lat[,t], sigmaL=0 )

    # Update numbers-at-age.
    Nat[1,t] <- Rt[t]
    for ( a in 2:(A-1) )
    {
      Nat[a,t] <- Nat[a-1,t-1]*exp(-Mt[t]-sel[a-1]*Ft[t-1] )      
    }
    Nat[A,t] <- Nat[A-1,t-1]*exp(-Mt[t]-sel[A-1]*Ft[t-1]) + 
                Nat[A,t-1]*exp(-Mt[t]-sel[A]*Ft[t-1])    

    # Update biomass-at-age
    Bat[,t] <- Nat[,t]*wtAge

    # Spawning biomass and abundance.
    Bt[t] <- sum( Bat[,t]*mat )
    Nt[t] <- sum( Nat[,t]*mat )

    # Exploitable biomass and abundance
    Bexp[t]  <- sum( Bat[,t]*sel )    # Fishery
    BexpS[t] <- sum( Bat[,t]*selS )   # Survey
    Nexp[t]  <- sum( Nat[,t]*sel )

    # Calculate this year's catch based on Ft[t] if it is not missing
    if ( !is.na(Ft[t]) )
    {
      catAge <- Nat[,t]*(1.-exp(-Mt[t]-sel*Ft[t]))*sel*Ft[t]/(Mt[t]+sel*Ft[t])      
      
      # total catch biomass
      Dt[t]  <- sum( catAge*wtAge ) # i=2, t=65 this changes Dt to 0
    }
    
    # Calculate this year's F based on tac if Ft[t] is missing
    if( is.na(Ft[t]) )
    {
      # diffCatch
      # Purpose:        compute difference between tac and catch based on F (fin)
      # Parameters:     fin=proposed fishing rate; nat=current age-comp, tmp=parameters
      # Returns:        difference between tac and Baranov catch based on F (fin)
      # Source:         S.P. Cox

      diffCatch <- function( fin, nat, tac, tmp )
      {
        catAge <- nat*(1.-exp(-tmp$Mt[t]-tmp$sel*fin))*tmp$sel*fin/(tmp$Mt[t]+tmp$sel*fin)                
        totCat <- sum( catAge*tmp$wt )
        diff   <- totCat - tac
        diff
      }
      
      # Create list to store necessary parameters
      tmp      <- list()     
      tmp$Mt   <- Mt
      tmp$sel  <- sel	   # Fishery
      tmp$selS <- sel	   # Survey
      tmp$wt   <- wtAge

      ifelse( diffCatch(fin=5, nat=Nat[,t], tac=Dt[t], tmp=tmp  ) < 0,
        Ft[t] <- 5,
        Ft[t] <- uniroot(f=diffCatch, interval=c(0,5), tol=1.e-8, nat=Nat[,t], tac=Dt[t], tmp=tmp)$root )

      # Catch-age based on F from solution to catch equation
      catAge <- Nat[,t]*(1.-exp(-Mt[t]-sel*Ft[t]))*sel*Ft[t]/(Mt[t]+sel*Ft[t])      
      
      # total catch biomass
      tmpD <- sum( catAge*tmp$wt )

    }     # END t >= tMP ifelse
  }     # END t > 1 ifelse

  Ct[t] <- sum( catAge )

  # Generate survey estimate of exploitable biomass if survey is "On"  
  surveyOn <- attributes( obj$mp$data$It )$surveyOn[t]

  # Deterministic survey value from operating model, based on survey exploitable.
  #ItTrue[t] <- qSurvey*BexpS[t]
  ItTrue[t] <- qt[t]*BexpS[t]
 	
  # Generate survey observations when "on"
  if ( as.logical(surveyOn) )
  {
    # Survey index
    It[t] <- ItTrue[t]*exp( epsilont[t] )
  }

  # ARK (27-Dec-12)
  # Generate observed ages when "on".  This is separated to allow ages when
  # there is no survey, i.e., ages might come from commercial fishery.  However
  # this has implications for selectivity... could lock in .calcTimes to force
  # observed ages ONLY in years indicated in agesOn and agesOnS
  agesOn  <- attributes( obj$mp$data$pat )$agesOn[t]	 # Fishery
  agesOnS <- attributes( obj$mp$data$patS )$agesOnS[t] # Survey
  
  # Deterministic age-proportions in fishery and survey exploitable biomass  from operating model. 
  # uat uses fishery selectovity uatS Uses selS - survey selectivity
	uat[,t]  <- sel*Bat[,t]/sum( sel*Bat[,t] )
	uatS[,t] <- selS*Bat[,t]/sum( selS*Bat[,t] )
 
  if ( as.logical( agesOn ) )
  {
	  pat[,t] <- .calcPat( uAge=uat[,t],tauAge=tauAge,epsa=epsilonat[,t] )  
  }

  if ( as.logical( agesOnS ) )
  {
    patS[,t] <- .calcPat( uAge=uatS[,t],tauAge=tauAgeS,epsa=epsilonatS[,t] )  
  }

  # Update all objects in obj for this time step.
  obj$om$Nat   <- Nat       # numbers-at-age
  obj$om$Bat   <- Bat       # biomass-at-age
  obj$om$Lat   <- Lat       # length-at-age
  obj$om$Bt    <- Bt        # spawning biomass
  obj$om$Nt    <- Nt        # spawning population
  obj$om$Bexp  <- Bexp      # fishery exploitable biomass
  obj$om$BexpS <- BexpS     # survey exploitable biomass
  obj$om$Nexp  <- Nexp      # total abundance
  obj$om$uat   <- uat       # true age-proportions fishery
  obj$om$uatS  <- uatS      # true age-proportions survey
  obj$om$Rt    <- Rt        # recruitment

  obj$om$Ct    <- Ct        # catch in numbers
  obj$om$Dt    <- Dt        # catch in biomass
  obj$om$Ft    <- Ft        # realized fishing mortality rate

  obj$om$ItTrue            <- ItTrue  # deterministic index
  obj$om$errors$omegat     <- omegat
  obj$om$errors$epsilont   <- epsilont
  obj$om$errors$epsilonat  <- epsilonat
  obj$om$errors$epsilonatS <- epsilonatS

  obj$mp$data$It   <- It    # survey observations
  obj$mp$data$pat  <- pat   # age-proportion data fishery
  obj$mp$data$patS <- patS  # age-proportion data fishery

  return( obj )
}     # END function asOMod


# .calcLenAge
# Purpose:        calculates von B length-at-age
# Parameters:     A=max age, Linf=asymptotic length; L1=length-at-age 1;
#                 vonK=growth rate
# Returns:        vector of length A with lengths-at-age
# Source:         S.P. Cox
.calcLenAge <- function( A=25, Linf=80., L1=35.0, vonK=0.465 )
{
  age    <- 1:A
  lenAge <- Linf + (L1-Linf)*exp(-vonK*(age-1.))
  lenAge
}     # END function .calcLenAge


# .calcLogistic
# Purpose:        calculates logistic ogive
# Parameters:     A=max age, a50(95)=age-at-50%(95%) maturity or selectivity
# Returns:        vector of length A with proportion-at-age
# Source:         S.P. Cox
.calcLogistic <- function( A, a50=5, a95=8 )
{
  a <- c(1:A)
  g <- log(19.)/( a95 - a50 )
  logis <- 1./( 1. + exp(-g*( a - a50 ) ) )
  return(logis)
}     # END function .calcLogistic


# .calcPat
# Purpose:        calculates observed proportion-at-age data
# Parameters:     uAge-true age props, tauAge-sd of random error,epsa-std normal devs
# Returns:        vector of length A with observed age-proportions rounded to 4 digits
# Source:         S.P. Cox
.calcPat <- function( uAge,tauAge=0,epsa )
{
  # Number of age classes including plus group.
  A <- length( epsa )
  xa <- vector( mode="numeric",length=A )
  pa <- vector( mode="numeric",length=A )

  # Age-proportion logit-residuals
  xa <- uAge
  xa[ xa==0 ] <- 1.e-6
  xa <- log(xa) + tauAge*epsa - mean( log(xa)+tauAge*epsa )

  # Observed proportions
  pa <- exp(xa) / sum( exp(xa) )
}     # END function .calcPat


# .calcRdevs
# Purpose:        calculates lag-1 autocorrelated recruitment deviations
# Parameters:     gamma=lag-1 autocorrel.; sigmaR=recruitment std error,
#                 deltat=vector of std normals
# Returns:        vector of length nT with log-normal recruitment multipliers
# Source:         S.P. Cox
.calcRdevs <- function( gamma,sigmaR,deltat )
{
  # Generate autocorrelated recruitment deviation vector.
  omegat <- vector( mode="numeric",length=length(deltat) )

  sigUnc  <- sigmaR / sqrt( 1.0 - gamma*gamma )
  sig2Unc <- sigmaR*sigmaR / ( 1.0 - gamma*gamma )

  omegat[1] <- sigUnc * deltat[1]
  for ( t in 2:length(deltat) )
    omegat[t] <- omegat[t-1] * gamma + sigmaR*deltat[t]

  # note these are now multpliers > 0
  omegat <- exp( omegat - sig2Unc/2.0)
  omegat
}     # END function .calcRdevs


# .calcWtAge
# Purpose:        calculates weight-at-age
# Parameters:     lenAge=length-at-age; c1=scalar; c2=power
#                 sigmaL=std error in length-at-age
# Returns:        vector of length A with weight-at-age
# Source:         S.P. Cox
.calcWtAge <- function( lenAge, c1=1.e-5,c2=3.0,sigmaL )
{
  # weight-length relationship including bias correction
  wtAge <- c1*lenAge^c2*(1.+0.5*c2*(c2-1.)*sigmaL*sigmaL)
  wtAge
}     # END function .calcWtAge

# .tsBoot
# Purpose:        creates a time-series bootstrap sample from
#                 historical input values (e.g., Mt, alphat,etc)
# Parameters:     x=the values to sample, length=length of projection,
#                 tolSD=std dev for starting value, tolMult=how many
#                 tolSDs to allow.
#                 deltat=vector of std normals
# Returns:        nT-vector of log-normal error multipliers
# Usage:          tmp[t] = state[t]*error[t]
# Source:         Sam Johnson and S.P. Cox
tsBoot <- function ( seed = NULL, x = histData, length = nT,
                     tolSD = 0.1, tolMult = 1 )
{
  # recover length of historical data
  nDat <- length ( x )
  # Initialise bootstrapped vector
  Yt <- numeric ( length = 0 )
  # Initialise bootstrapped TS length
  lSim <- 0
  # Set seed if provided
  if ( !is.null(seed) ) set.seed ( seed )
  while ( lSim < length )
  { 
    # If first block, then constrain the initial value to 
    # lie within predetermined interval
    # defined by tolSD and tolMult (defined in simCtlFile)
    if ( lSim == 0 ) 
    { 
      # Create max and min values for initial point
      minInit <- x [ nDat ] - tolSD * tolMult
      maxInit <- x [ nDat ] + tolSD * tolMult
      # Restrict to those possible starting points within range
      startIdx <- which ( (x <= maxInit) & (x >= minInit) )
      # Sample initial point based on bounds (maxInit, minInit)
      if ( length ( startIdx ) == 1 ) segStart <- startIdx
      else segStart <- sample ( startIdx, size = 1 )

      # if not the first block, sample from x
    } else segStart <- sample ( 1:nDat, size = 1 )
    
    # Set max length of first block based on segStart
    maxLenNew <- min ( nDat - segStart + 1, length - lSim )
    
    # Sample a random segment length uniformly from available lengths
    segLen <- sample ( x = 1:maxLenNew, size = 1 )
    # Compute segment ending position
    segEnd <- segStart + segLen - 1
    Yt <- c ( Yt, x [ segStart:segEnd ] )
    # Add new length to lSim
    lSim <- lSim + segLen
  }
  return ( Yt )  
}

# .fillRanWalk       
# Purpose:        creates a lag-1 autocorrelated error sequence
# Parameters:     gamma=lag-1 autocorrel.; sigma=std error,
#                 deltat=vector of std normals
# Returns:        nT-vector of log-normal error multipliers
# Usage:          tmp[t] = state[t]*error[t]
# Source:         S.P. Cox
.fillRanWalk <- function( gamma=0.5,sigma=0,deltat,init=NULL )
{
  # Generate autocorrelated recruitment deviation vector.
  omegat <- vector( mode="numeric",length=length(deltat) )

  # Uncorrelated error std dev and variance
  sigUnc  <- sigma / sqrt( 1.0 - gamma*gamma )
  sig2Unc <- sigma*sigma / ( 1.0 - gamma*gamma )
  
  # Initialize first value in sequence
  if( is.null(init) ) # if not supplied, scale 1st
    omegat[1] <- sigUnc * deltat[1]
  else            # use init
    omegat[1] <- init
  
  # The random walk...
  for ( t in 2:length(deltat) )
    omegat[t] <- omegat[t-1] * gamma + sigma*deltat[t]
  
  # note these are now multpliers > 0
  omegat <- exp( omegat - sig2Unc/2.0)
  return(omegat)
}     # END function .fillRanWalk

#------------------------------------------------------------------------------#
#--                  Management Procedure Functions                          --#
#                                                                              #
# callProcedureXXX functions are assessment method wrappers introduced since   #
# the only reliable way to isolate code changes due to assessment methods is   #
# to contain them in functions. New methods can then be added without          #
# impacting working code.  It means some redundancy in coding, which is not    #
# ideal, but sorting out complicated if branches is a Special Kind of Hell     #
# that should be avoided.                                                      #
#------------------------------------------------------------------------------#

# assessModMA
# Purpose:       Generates the "stock assessment" for the Moving Average       #
#                stock assessment method (i.e., an n-point moving              #
#                average of the survey                                         #
# Parameters:     obj=list containing It=survey biomasses;                     #
#                 avgPoints=number of points to average                        #
# Returns:        average biomass over the last n surveys                      #
# Source:         S.P. Cox                                                     #
assessModMA <- function( obj )
{
  # Compute the moving average of the survey
  # using the most recent avgPoints surveys
  It        <- obj$It
  avgPoints <- obj$avgPoints

  # Remove missing values.
  tmp  <- It[ !is.na(It) ]
  nPoints <- min( length(tmp),avgPoints )
  tVec <- c( (length(tmp) - nPoints + 1 ):length(tmp) )

  # Moving average of survey values.
  movingAvg          <- mean( tmp[tVec] )

  assessment         <- list()
  #assessment$biomass <- movingAvg
  
  # ARK (01-Mar-13)
  # Exploitable and spawning biomass are the same for the Moving Average.
  assessment$exploitBt <- movingAvg
  assessment$spawnBt   <- movingAvg
  
  return( assessment )
}     # END function assessModMA


# assessModKF
# Purpose:       Generates the "stock assessment" for the Kalman Filter        #
#                stock assessment method.                                      #
# Parameters:     obj=list containing It=survey biomasses;                     #
#                 Kfgain = kalman filter gain parameter                        #
# Returns:        current biomass estimate                                     #
# Source:         S.P. Cox                                                     #
assessModKF <- function( obj )
{
  It        <- obj$It
  kfGain    <- obj$kfGain

  # Take non-missing survey biomass estimates.
  starBt <- It[ !is.na(It) ]
  T      <- length(starBt)

  estBt    <- 0.
  estBt[1] <- starBt[1]
  for( i in 2:T )
    estBt[i] <- estBt[i-1] + kfGain*(starBt[i]-estBt[i-1])

  assessment <- list()
  #assessment$biomass <- estBt[T]

  # ARK (01-Mar-13)
  # Exploitable and spawning biomass are the same for the Kalman filter.
  assessment$exploitBt <- estBt[T]
  assessment$spawnBt   <- estBt[T]
  
  return( assessment )
}     # END function assessModKF



# assessModCA
# Purpose:       Runs a statistical catch-age model to estimate biomass       #
#                and fishery reference points if required.
# Parameters:    obj=list containing everything, because this thing needs it  #
# Returns:       current biomass estimate, stock dynamics and parameters,     #
#                minimization details, yield and ref pt calcs                 #
# Notes:         Execution is platform-dependent - see system() call          #
# Source:        S.P. Cox (1-May-10)                                          #
assessModCA <- function( caObj )
{
  # Statistical catch-age model.
  # This implmentation is a wrapper that simply calls an ADMB program.
  #
  # (1) Extract and write the model parameters to (assessCA.pin).
  # (2) Extract and write the data (assessCA.dat).
  # (3) Make a system call to the ADMB program (windows=assessCA.exe, mac=assessca).
  # (4) Read the ADMB report file (assessCA.rep).
  # (5) Update the pars list component and tack on some ADMB minimizer details.
  #

  t <- caObj$t
  exeName <- "assessCA"
  xFactor <- 0.2  # scalar for incrementing lnB0 if
                  # assessCA needs restarting from 
                  # higher initial lnB0. Max increment
                  # is 4*xFactor

  # Write age-structured model PARAMETERS to ADMB *.pin file.
  
  outFile <- "assessCA.pin" 
  cat( file=outFile, "## assessCA model parameters.\n" )
  cat( file=outFile, "## Written:",date(),                      "\n", append=T )

  cat( file=outFile, "# log_initN_mult\n", rep(0,caObj$A), "\n", append=T )
  cat( file=outFile, "# logit_ySteepness\n", caObj$logit.ySteepness,"\n", append=T )
  cat( file=outFile, "# lnB0\n",         caObj$lnB0+0.5,            "\n", append=T )  
  cat( file=outFile, "# lnM\n",          caObj$lnM,             "\n", append=T )
  cat( file=outFile, "# ln_aSel50\n",    caObj$ln_aSel50,       "\n", append=T ) # Fishery
  cat( file=outFile, "# ln_aSelStep\n",  caObj$ln_aSelStep,     "\n", append=T )
  cat( file=outFile, "# ln_aSelS50\n",    caObj$ln_aSelS50,     "\n", append=T ) # Survey
  cat( file=outFile, "# ln_aSelSStep\n",  caObj$ln_aSelSStep,   "\n", append=T )
  cat( file=outFile, "# rho\n",          caObj$rho,             "\n", append=T )
  cat( file=outFile, "# omega\n",        caObj$omega,           "\n", append=T )
  cat( file=outFile, "# mDevs\n",        rep(0,length(caObj$omega)), "\n", append=T )

  # Write age-structured model DATA to ADMB *.dat file.
  
  outFile <- "assessCA.dat" 
  cat( file=outFile, "## assessCA model data.\n" )
  cat( file=outFile, "## Written:", date(),                     "\n", append=T )
  
  # Model dimensions.
  
  cat( file=outFile, "# nT nAges nProjYrs trendYrs rseed hcrType\n",  append=T )
  cat( file=outFile, caObj$t-1,                                  " ", append=T )
  cat( file=outFile, caObj$A,                                    " ", append=T )
  
  cat( file=outFile, caObj$nProjYears,                           " ", append=T )
  cat( file=outFile, caObj$trendYears,                           " ", append=T )
  cat( file=outFile, caObj$initSeed,                             " ", append=T )
  
  cat( file=outFile, caObj$ruleType,                            "\n", append=T )

  # Biological parameters.
  
  cat( file=outFile, "# aMat50    aMat95    Linf    L1    vonK    c1    c2    sigmaL\n", append=T )
  cat( file=outFile, c( caObj$aMat50, caObj$aMat95,
                        caObj$Linf,   caObj$L1,
	                      caObj$vonK,   caObj$c1,     caObj$c2,
                        caObj$sigmaL ), "\n",                         append=T )

  # Prior distribution parameters.
  
  cat( file=outFile, "# pmSteepness psdSteepness\n",               append=T )
  cat( file=outFile, caObj$pmSteepness, caObj$psdSteepness,  "\n", append=T )

  cat( file=outFile, "# pmM psdM\n",                               append=T )
  cat( file=outFile, caObj$pmM, caObj$psdM,                  "\n", append=T )

  cat( file=outFile, "# qChangeTime\n",                                    append=T )
  cat( file=outFile, caObj$qChangeTime,                              "\n", append=T )

  cat( file=outFile, "# pmQ\n",                                    append=T )
  cat( file=outFile, caObj$pmQ,                              "\n", append=T )

  cat( file=outFile, "# psdQ\n",                                   append=T )
  cat( file=outFile, caObj$psdQ,                             "\n", append=T )

  cat( file=outFile, "# sigmaM\n",                                 append=T )
  cat( file=outFile, caObj$sigmaM,                          "\n", append=T )

  # Estimation bounds.
  cat( file=outFile, "# lowBoundLnB0 upperBoundLnB0\n",               append=T )
  cat( file=outFile, caObj$lowBoundLnB0, caObj$upperBoundLnB0, "\n",  append=T )

  # Fix fishery selectivity parameters?
  cat( file=outFile, "# fixFSel\n ",                                  append=T )
  cat( file=outFile, caObj$caaFixFSel,                          "\n", append=T )

  # Fix survey selectivity parameters?
  cat( file=outFile, "# fixSSel\n ",                                  append=T )
  cat( file=outFile, caObj$caaFixSSel,                          "\n", append=T )
  
  # Effective sample size for ages in fishery
  cat( file=outFile, "# nSamples\n ",                                 append=T )
  cat( file=outFile, caObj$nSamples,                            "\n", append=T )

  # Effective sample size for ages in survey
  cat( file=outFile, "# nSamplesS\n ",                                append=T )
  cat( file=outFile, caObj$nSamplesS,                           "\n", append=T )

  # Catch data.
  cat( file=outFile, "# Catch biomass (katch, tonnes)\n",             append=T )
  cat( file=outFile, caObj$Dt[ (1:(t-1)) ],                     "\n", append=T )
  
  # Abundance index type.
  cat( file=outFile, "# Index series indexType ( 0=abs, 1=rel)\n",    append=T )

  cat( file=outFile, caObj$caaSurveyRel,                        "\n", append=T )

  # Abundance index, use setMissing() to convert NA to -1 for ADMB.
  It <- .setMissing( obj=caObj$It, toR=FALSE )
  cat( file=outFile, "# Index series \n",                             append=T )
  cat( file=outFile, It,                                        "\n", append=T )

  #-- Age proportions	in fishery.
  cat( file=outFile, "# Fishery age proportions (nT,nAges)\n",        append=T )
  pat <- .setMissing( caObj$pat, toR=FALSE )
  write.table( t(pat), file=outFile, sep=" ", row.names=FALSE, col.names=FALSE, 
               append=T )
  
  #-- Age proportions	in survey.
  cat( file=outFile, "# Survey age proportions (nT,nAges)\n",         append=T )
  patS <- .setMissing( caObj$patS, toR=FALSE )
  write.table( t(patS), file=outFile, sep=" ", row.names=FALSE,
               col.names=FALSE, append=T )

  #-- Quota levels for projection.
  nQlevels <- length( caObj$Qlevel )
  
  cat( file=outFile, "# nQlevels \n",                                 append=T )
  cat( file=outFile, nQlevels,                                 "\n",  append=T )
  cat( file=outFile, "# Qlevel \n",                                   append=T )
  cat( file=outFile, caObj$Qlevel,                            "\n",   append=T )
  
  #RF Data file reading error trap for debugging. If using this, need to uncomment
  # last code in data section of assessCA.tpl (RF Aug 2013)
  #cat( file=outFile, "# eof \n",                                     append=T )
  # cat( file=outFile, 999,      "\n",   append=T )		#data file read error trap

  # Call the age-structured ADMB program: assessCA.
  # Concept here is to always run MCMC but for constantF or variableF where the
  # risk adjusted Fmsy option is FALSE we run nMCMC=1 to get the MLEs as the
  # first row.  That way the same code can be used whether MLE or MCMC.
  
  mcOut <- NA
  
  mcmcString <- ""
  mcmcString <- paste( " -mcmc ",caObj$nMCMC," -mcsave ",caObj$nThin, sep="" )

  datFile  <- file.path( getwd(), paste( exeName,".dat", sep="" ) )
  exeFile  <- file.path( getwd(), exeName )
  mcmcFile <- file.path( getwd(), "mcout.dat" )
  pinFile  <- file.path( getwd(), paste( exeName,".pin", sep="" ) )  
  repFile  <- file.path( getwd(), paste( exeName,".rep", sep="" ) )  
  stdFile  <- file.path( getwd(), paste( exeName,".std", sep="" ) )

  # Set start time for optimization
  starttime <- Sys.time()
  
  
  for ( i in 1:4 )
  {
    hessPosDef <- TRUE
    if( .Platform$OS.type=="unix" )
    { 
      system2( command=paste(exeFile),args=paste(" ",mcmcString," -iprint 100 -maxfn 5000",sep="" ),
              wait=TRUE, stdout=FALSE,stderr=FALSE  )

      system2( command=paste(exeFile),args=paste(" ", "-mceval",sep="" ),
                wait=TRUE, stdout=FALSE,stderr=FALSE  )
    }
    else     # Windows branch call to "system".
    {
      system( paste( exeFile,".exe ",mcmcString," -iprint 100 -maxfn 5000"), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
            
      system( command=paste( exeFile," ","-mceval",sep="" ),
              show.output.on.console=.SHOWOUTPUTONCONSOLE, wait=TRUE, ignore.stdout=T  )
    }
  
    # Check if there is a valid mcmc output file, i.e., was the Hessian positive
    # definite, if not must use old mcmc output file and set hessPosDef to FALSE.
    # This condition is then flagged in mp$assess$runStatus$hessPosDef (T,F).
  
    tmpMCMC  <- try( read.table( mcmcFile, header=T, sep=" " ), silent=TRUE )
    # Increment starting B0 value to hopefully get convergence
    if ( class( tmpMCMC )=="try-error" )
    {
      hessPosDef <- FALSE
      outFile <- "assessCA.pin" 
      cat( file=outFile, "## assessCA model parameters.\n" )
      cat( file=outFile, "## Written:",date(),                      "\n", append=T )

      cat( file=outFile, "# log_initN_mult\n",   rep(0,caObj$A), "\n", append=T )
      cat( file=outFile, "# logit_ySteepness\n", caObj$logit.ySteepness,"\n", append=T )
      cat( file=outFile, "# lnB0\n",         caObj$lnB0*(1.0+i*xFactor),  "\n", append=T )
      cat( file=outFile, "# lnM\n",          caObj$lnM,             "\n", append=T )
      cat( file=outFile, "# ln_aSel50\n",    caObj$ln_aSel50,       "\n", append=T )
      cat( file=outFile, "# ln_aSelStep\n",  caObj$ln_aSelStep,     "\n", append=T )
      cat( file=outFile, "# ln_aSelS50\n",   caObj$ln_aSelS50,       "\n", append=T )
      cat( file=outFile, "# ln_aSelSStep\n", caObj$ln_aSelSStep,     "\n", append=T )
      cat( file=outFile, "# rho\n",          caObj$rho,             "\n", append=T )
      cat( file=outFile, "# omega\n",        caObj$omega,           "\n", append=T )
      cat( file=outFile, "# mDevs\n",        rep(0,length(caObj$omega)), "\n", append=T )
      cat( "\ni = ",i,"\n" )
    }
    
    if ( hessPosDef )
      break
  }
  
  if ( class( tmpMCMC )=="try-error" )
  {
    hessPosDef <- FALSE
    cat( "\nHessian not positive definite, using last good mcout.dat","\n" )
    mcmcFile <- file.path( getwd(), "mcout.bck" )          
    if ( file.exists( mcmcFile ) )
      tmpMCMC <- read.table( mcmcFile, header=T, sep=" " )
      
    # Save the *.pin and *.dat files for forensics.
    badPinFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".pin", sep="" ) )
    file.copy( pinFile, badPinFile, overwrite=TRUE )
    badDatFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".dat", sep="" ) )
    file.copy( datFile, badDatFile, overwrite=TRUE )
  }
  else
  {
    # Save the mcout.dat file in case the next Hessian is not positive definite.
    file.copy( "mcout.dat","mcout.bck", overwrite=TRUE )  
  }          
  
  # The thinning of the MCMC chain depends on (a) MLE vs. MCMC, (b) HCR.
  # SPC (18-Apr-13): Not anymore!! mcout.dat not generated by -mceval call
  # so it will already be thinned. Therefore, fixing nThin here == 1.
  nThin <- 1
  if( caObj$useMLE==FALSE )
  {
    if ( caObj$hcrType == "declineRisk" )
    {
      # The MCMC output is in blocks of nQuotaLevels, thin from each quota level.
      idxQ <- split( c(1:nrow(tmpMCMC)), tmpMCMC$Q )
      iRow <- as.vector( sapply( idxQ,
           function(x) { return( x[ seq(1,length(x),nThin)] ) } ) )
    }
    else if ( caObj$hcrType=="variableF" | caObj$hcrType=="constantF" )
    {
      # constantF or variableF: thin MCMC chain based on nThin from GUI.
      tmpN  <- nrow( tmpMCMC )
      iRow  <- seq( 1, tmpN, by=nThin )
    }

    # Extract thinned chain.
    mcOut <- tmpMCMC[ iRow, ]
  }
  else
    mcOut <- tmpMCMC[1,]     # First row of MCMC output are the MLEs.

  # Set end time for optimization
  endtime <- Sys.time()       
  
  # Calculate elapsed time
  elapsed <- difftime( endtime,starttime )     

	# Read the report and mcout files.
	assessCA <- lisread( "assessca.rep", quiet=T )
 
  # d) Save assessment results to output list

  # SPC (26-Jan-13) This list (i.e., rep file output) always contains 
  # projSpawnBio and projExpBio which are 1-year ahead projections of each.
  assessment        <- assessCA
      #cat("\n Printing assessment list from rep file \n")
      #print(assessment)

  assessment$mpdPars <- list( objFun     = assessCA$objFun,
	                            eivNegLnL  = assessCA$eiv_nll,
	                            ageNegLnL  = assessCA$age_nll,
	                            steepNegLnP = assessCA$steepness_nlp,
															logit.ySteepness = assessCA$logit.ySteepness,
															rSteepness = assessCA$rSteepness,
															rec.a      = assessCA$rec.a,
															rec.b      = assessCA$rec.b,
															B0         = assessCA$B0,
															M          = assessCA$M,
															aSel50     = assessCA$aSel50,
															aSel95     = assessCA$aSel95,
															aSelS50    = assessCA$aSelS50,
															aSelS95    = assessCA$aSelS95,
															q          = assessCA$q,
                              dep        = assessCA$D,
                              lastDt     = assessCA$lastDt
														)													
	
	assessment$mcOut <- mcOut
  													
  assessment$runStatus <- list( exeName       = exeName,
                                objFun        = assessCA$objFun,
                                maxGrad       = assessCA$maxGrad,
                                nEval         = assessCA$nEval,
                                convT         = elapsed[[1]],
                                iExit         = assessCA$iExit,
                                hessPosDef    = hessPosDef,
                                fisheryClosed = NA,
                                deadFlag      = NA )														

	assessment$pat       <- assessCA$predPropAge		  #fishery
	assessment$patS      <- assessCA$predPropAgeS	    #survey

	return( assessment )
}     # END function assessModCA


# assessModDD
# Purpose:       Runs a delay difference model to estimate biomass             #
#                and fishery reference points if required.                     #
# Parameters:    obj=list containing everything, because this thing needs it   #
# Returns:       current biomass estimate, stock dynamics and parameters,      #
#                minimization details, yield and ref pt calcs                  #
# Notes:         Execution is platform-dependent - see system() call           #
# Source:        R.E. Forrest (Apr-15-2013)                                    #
assessModDD <- function( ddObj )
{
  # Delay Difference model.
  # This implmentation is a wrapper that simply calls an ADMB program.
  #
  # (1) Extract and write the model parameters to (assessDD.pin).
  # (2) Extract and write the data (assessDD.dat).
  # (3) Make a system call to the ADMB program (windows=assessdd.exe, mac=assessdd).
  # (4) Read the ADMB report file (assessDD.rep).
  # (5) Update the pars list component and tack on some ADMB minimizer details.
  #

  t <- ddObj$t
  exeName <- "assessDD"

  # Write delay difference model PARAMETERS to ADMB *.pin file.

  outFile <- file.path( getwd(), paste( exeName,".pin",sep="" ) ) 
  cat( file=outFile, "## Delay-Difference model parameters (assessDD).\n" )
  cat( file=outFile, "## Written:",date(),                      "\n", append=T )

  cat( file=outFile, "# logit_ySteepness\n", ddObj$logit.ySteepness,"\n", append=T )
  cat( file=outFile, "# lnB0\n",         ddObj$lnB0,            "\n", append=T )
  cat( file=outFile, "# lnM\n",          ddObj$lnM,             "\n", append=T )
  cat( file=outFile, "# rho\n",          ddObj$rho,             "\n", append=T )
  cat( file=outFile, "# lnFt\n",         ddObj$lnFt,            "\n", append=T )
  cat( file=outFile, "# omega\n",        ddObj$omega,           "\n", append=T )

  # Write delay difference model DATA to ADMB *.dat file.

  outFile <- file.path( getwd(), paste( exeName,".dat",sep="" ) ) 
  cat( file=outFile, "## assessDD model data\n" )
  cat( file=outFile, "## Written:", date(),                     "\n", append=T )

  # Model dimensions.
  cat( file=outFile, "# nT nProjYrs trendYrs rseed hcrType\n",  append=T )
  cat( file=outFile, ddObj$t-1,                                  " ", append=T )
  cat( file=outFile, ddObj$nProjYears,                           " ", append=T )
  cat( file=outFile, ddObj$trendYears,                           " ", append=T )
  cat( file=outFile, ddObj$initSeed,                             " ", append=T )
  cat( file=outFile, ddObj$ruleType,                            "\n", append=T )

  # Biological parameters (note ddObj$tauSurvey is not needed in tpl).
  cat( file=outFile, "# Linf    L1    vonK    c1    c2    sigmaL   tau\n", append=T )
  cat( file=outFile, c( ddObj$Linf,   ddObj$L1,
			               ddObj$vonK,   ddObj$c1,     ddObj$c2,
					           ddObj$sigmaL ),                  "\n",           append=T )

  # Delay difference recruitment and growth parameters.
  cat( file=outFile, "# kAge - knife edge age at recruitment\n",      append=T )
  cat( file=outFile, ddObj$kAge,                                "\n", append=T )
  cat( file=outFile, "# alpha_g rho_g wk\n",                          append=T )
		   cat( file=outFile, c( ddObj$ddpars$alpha_g,   ddObj$ddpars$rho_g,
           					      ddObj$ddpars$wk ),                    "\n", append=T )
		  
  # Prior distribution parameters.
  cat( file=outFile, "# pmSteepness psdSteepness\n ",                 append=T )
  cat( file=outFile, ddObj$pmSteepness, ddObj$psdSteepness,     "\n", append=T )
  cat( file=outFile, "# pmM psdM\n ",                                 append=T )
  cat( file=outFile, ddObj$pmM, ddObj$psdM,                     "\n", append=T )

  # Catch data.
  cat( file=outFile, "# Catch biomass (katch, tonnes)\n",             append=T )
  cat( file=outFile, ddObj$Dt[ (1:(t-1)) ],                     "\n", append=T )

  # Abundance index type.
  cat( file=outFile, "# Index series indexType ( 0=abs, 1=rel)\n",    append=T )
  cat( file=outFile, ddObj$ddSurveyRel,                         "\n", append=T )

  # Abundance index, use setMissing() to convert NA to -1 for ADMB.
  It <- .setMissing( obj=ddObj$It, toR=FALSE )
  cat( file=outFile, "# Index series \n",                             append=T )
  cat( file=outFile, It,                                        "\n", append=T )

  # Fixed parameter controlling tightness of fit to catch.
  cat( file=outFile, "# sigmaC\n",                                    append=T )
  cat( file=outFile, ddObj$sigmaC,                              "\n", append=T )
  
  # Initialize at unfished equilibrium, or fished equilibrium.
  cat( file=outFile, "# Initialize at unfished (1=TRUE, 0=FALSE)\n",  append=T )
  cat( file=outFile, ddObj$unfished,                            "\n", append=T )

  #-- Quota levels for projection.
  nQlevels <- length( ddObj$Qlevel )

  cat( file=outFile, "# nQlevels \n",                                 append=T )
  cat( file=outFile, nQlevels,                                 "\n",  append=T )
  cat( file=outFile, "# Qlevel \n",                                   append=T )
  cat( file=outFile, ddObj$Qlevel,                            "\n",   append=T )

  # Call the delay-difference ADMB program: assessDD.
  # Concept here is to always run MCMC but for constantF or variableF where the
  # risk adjusted Fmsy option is FALSE we run nMCMC=1 to get the MPDs as the
  # first row.  That way the same code can be used whether MPD or MCMC.
  
  # Set start time for optimization
  starttime <- Sys.time()

  # ARK (12-Dec-11) Cannot have NULL assignment to mcOut.
  mcOut      <- NA

  # Changed by ARK (01-May-13) to match assessCA.
  mcmcString <- paste( " -mcmc ",ddObj$nMCMC," -mcsave ",ddObj$nThin, sep="" )

  datFile  <- file.path( getwd(), paste( exeName,".dat", sep="" ) )
  exeFile  <- file.path( getwd(), exeName )
  mcmcFile <- file.path( getwd(), "mcout.dat" )
  pinFile  <- file.path( getwd(), paste( exeName,".pin", sep="" ) )  
  repFile  <- file.path( getwd(), paste( exeName,".rep", sep="" ) )  
  stdFile  <- file.path( getwd(), paste( exeName,".std", sep="" ) )

  xFactor <- 0.2
  for ( i in 1:4 )
  {
    hessPosDef <- TRUE
    if( .Platform$OS.type=="unix" )
    { 
      system( command=paste( exeFile," ",mcmcString," -iprint 100 -maxfn 5000", sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )

      system( command=paste( exeFile," ","-mceval",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )
    }
    else     # Windows branch call to "system".
    {
      system( paste( exeFile,".exe ", mcmcString," "," -iprint 100 -maxfn 5000", sep="" ), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
              
      system( paste( exeFile,".exe -mceval", sep="" ), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
    }
    tmpMCMC  <- try( read.table( mcmcFile, header=T, sep=" " ), silent=TRUE )
    
    if ( class( tmpMCMC )=="try-error" )
    {
      hessPosDef <- FALSE

      outFile <- file.path( getwd(), paste( exeName,".pin",sep="" ) ) 
      cat( file=outFile, "## Delay-Difference model parameters (assessDD).\n" )
      cat( file=outFile, "## Written:",date(),                      "\n", append=T )

      cat( file=outFile, "# logit_ySteepness\n", ddObj$logit.ySteepness,"\n", append=T )
      cat( file=outFile, "# lnB0\n",         ddObj$lnB0*(1.0+i*xFactor), "\n", append=T )
      cat( file=outFile, "# lnM\n",          ddObj$lnM,             "\n", append=T )
      cat( file=outFile, "# rho\n",          ddObj$rho,             "\n", append=T )
      cat( file=outFile, "# lnFt\n",         ddObj$lnFt,            "\n", append=T )
      cat( file=outFile, "# omega\n",        ddObj$omega,           "\n", append=T )
    }
      
    if ( hessPosDef )
      break
  }

  # Check if there is a valid mcmc output file, i.e., was the Hessian positive
  # definite, if not must use old mcmc output file and set hessPosDef to FALSE.
  # This condition is then flagged in mp$assess$runStatus$hessPosDef (T,F).
  
  #tmpMCMC  <- try( read.table( mcmcFile, header=T, sep=" " ), silent=TRUE )

  if ( class( tmpMCMC )=="try-error" )
  {
    hessPosDef <- FALSE  
    cat( "\nHessian not positive definite, using last good mcout.dat","\n" )
    mcmcFile <- file.path( getwd(), "mcout.bck" )          
    if ( file.exists( mcmcFile ) )
      tmpMCMC <- read.table( mcmcFile, header=T, sep=" " )
      
    # Save the *.pin and *.dat files for forensics.
    badPinFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".pin", sep="" ) )
    file.copy( pinFile, badPinFile, overwrite=TRUE )
    badDatFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".dat", sep="" ) )
    file.copy( datFile, badDatFile, overwrite=TRUE )
  }
  else
  {
    # Save the mcout.dat file in case the next Hessian is not positive definite.
    file.copy( "mcout.dat","mcout.bck", overwrite=TRUE )  
  }

  # Read the ADMB report file.
  assessDD <- lisread( repFile, quiet=TRUE )

  # Read MPD standard deviations for whatever DD parameters are of interest?
  #stdBmsy       <- NA
  #stdMSY        <- NA
  #stdFmsy       <- NA
  #stdProjExpBio <- NA
 
  if ( hessPosDef )
  {
    if ( file.exists( stdFile ) )
    {
      #stderrs       <- read.table( stdFile, as.is=TRUE, header=FALSE, skip=1, sep="" )
      #stdBmsy       <- as.numeric( stderrs[ stderrs$V2=="Fmsy", ncol(stderrs) ] )
      #stdFmsy       <- as.numeric( stderrs[ stderrs$V2=="Bmsy", ncol(stderrs) ] )
      #stdProjExpBio <- as.numeric( stderrs[ stderrs$V2=="projExpBio", ncol(stderrs) ] )
    }
  }
  
  # The thinning of the MCMC chain depends on (a) MLE vs. MCMC, (b) HCR.
  # SPC (18-Apr-13): Not anymore!! mcout.dat not generated by -mceval call
  # so it will already be thinned. Therefore, fixing nThin here == 1.
  nThin <- 1
  if( ddObj$useMLE==FALSE )
  {
    if ( ddObj$hcrType == "declineRisk" )
    {
      # The MCMC output is in blocks of nQuotaLevels, thin from each quota level.
      idxQ <- split( c(1:nrow(tmpMCMC)), tmpMCMC$Q )
      iRow <- as.vector( sapply( idxQ,
           function(x) { return( x[ seq(1,length(x),nThin)] ) } ) )
    }
    else if ( caObj$hcrType=="variableF" | caObj$hcrType=="constantF" )
    {
      # constantF or variableF: thin MCMC chain based on nThin from GUI.
      tmpN  <- nrow( tmpMCMC )
      iRow  <- seq( 1, tmpN, by=nThin )
    }

    # Extract thinned chain.
    mcOut <- tmpMCMC[ iRow, ]
  }
  else
    mcOut <- tmpMCMC[1,]     # First row of MCMC output are the MLEs.

  # Set end time for optimization
  endtime <- Sys.time()       
  
  # Calculate elapsed time
  elapsed <- difftime( endtime,starttime )     

	# Read the report and mcout files.
	assessDD <- lisread( repFile, quiet=T )
 
  # d) Save assessment results to output list

  # SPC (26-Jan-13) This list (i.e., rep file output) always contains 
  # projSpawnBio and projExpBio which are 1-year ahead projections of each.
  assessment        <- assessDD

  assessment$mpdPars <- list( objFun     = assessDD$objFun,
               						    eivNegLnL   = assessDD$eiv_nll,
						                  steepNegLnP = assessDD$steepness_nlp,
						                  natMortNegLnP = assessDD$M_nlp,
															logit.ySteepness = assessDD$logit.ySteepness,
															rSteepness = assessDD$rSteepness,
															rec.a      = assessDD$rec.a,
															rec.b      = assessDD$rec.b,
															B0         = assessDD$B0,
                              R0         = assessDD$R0,
															M          = assessDD$M,
															q          = assessDD$q,
					                    dep        = assessDD$D,
					                    lastDt     = assessDD$lastDt
														)
														
  assessment$mcOut <- mcOut

  assessment$runStatus <- list( exeName       = exeName,
                                objFun        = assessDD$objFun,
                                maxGrad       = assessDD$maxGrad,
                                nEval         = assessDD$nEval,
                                convT         = elapsed[[1]],
                                iExit         = assessDD$iExit,
                                hessPosDef    = hessPosDef,
                                fisheryClosed = NA,
                                deadFlag      = NA )

	return( assessment )
}     # END function assessModDD


# assessModSP
# Purpose:        Runs a production model to estimate biomass                  #
# Parameters:     obj=list containing It=survey biomasses;                     #
#                 BoProdMod, rProdMod, lnOmega = initial parameters for        #
#                 optimization; rMuPrior, rSDPrior = prior paramaters          #
# Returns:        current biomass estimate as well as estimated parameters     #
#                 from surplus production model fit                            #
# Notes:          Production model is run from an ADMB exe (assessSP.exe)      #
# Source:         A.R. Kronlund (22-Dec-12) modified from K. Holt (4-Aug-09)   #
assessModSP <- function( spObj )
{
  exeName <- "assessSP"
  t <- spObj$t

  # (a) Extract and write the model parameters to assessSP.pin.
  outFile <- paste( exeName, ".pin", sep="" )
 
  cat( file=outFile, "## ", exeName,": Surplus production model parameters.\n" )
  cat( file=outFile, "## Written:",     date(),                 "\n", append=T )
  cat( file=outFile, "# lnMsy\n",       log(spObj$Msy)+.5,      "\n", append=T )
  cat( file=outFile, "# lnFmsy\n",      log(spObj$Fmsy),         "\n", append=T )
  cat( file=outFile, "# pmMsy\n",       spObj$pmMsy,            "\n", append=T )
  cat( file=outFile, "# psdMsy\n",      spObj$psdMsy,           "\n", append=T )
  cat( file=outFile, "# pmFmsy\n",      spObj$pmFmsy,           "\n", append=T )
  cat( file=outFile, "# psdFmsy\n",     spObj$psdFmsy,          "\n", append=T )
  cat( file=outFile, "# rho\n",         spObj$rho,              "\n", append=T )  
  cat( file=outFile, "# lnOmega\n",     spObj$lnOmega,          "\n", append=T )

  # (b) Write the data for the surplus production model to assessSP.dat.
  
  outFile <- paste( exeName, ".dat", sep="" )
  
  # Replace NA's in It vector with -1 for input to ADMB
  It              <- matrix( spObj$It,1,length(spObj$It) )
  It[ is.na(It) ] <- -1
  
  cat( file=outFile, "## ",exeName,": Surplus production model data.\n" )
  cat( file=outFile, "## Written:", date(),                     "\n", append=T )
  cat( file=outFile, "# nT nIndices \n",                              append=T )
  cat( file=outFile, ncol( It ), nrow( It ),                    "\n", append=T )
  cat( file=outFile, "# nProjYears \n",                               append=T )
  cat( file=outFile, spObj$nProjYears,                          "\n", append=T )
  cat( file=outFile, "# trendYears \n",                               append=T )
  cat( file=outFile, spObj$trendYears,                          "\n", append=T )
  cat( file=outFile, "# rSeed \n",                                    append=T )
  cat( file=outFile, spObj$rSeed,                               "\n", append=T )
  cat( file=outFile, "# ruleType \n",                                 append=T )
  cat( file=outFile, spObj$ruleType,                            "\n", append=T )
  cat( file=outFile, "# nQlevels \n",                                 append=T )
  cat( file=outFile, length( spObj$Qlevel ),                    "\n", append=T )
  cat( file=outFile, "# Qlevel \n",                                   append=T )
  cat( file=outFile, spObj$Qlevel,                              "\n", append=T )
  
  cat( file=outFile, "# Catch biomass (katch, tonnes)\n",             append=T )
  cat( file=outFile, spObj$Dt,                                  "\n", append=T )
  cat( file=outFile, "# Index series indexType: 0=abs, 1=rel)\n",     append=T )
  cat( file=outFile, spObj$indexType,                           "\n", append=T )
  cat( file=outFile, "# First year for each index (fYear)\n",         append=T )
  cat( file=outFile, spObj$t1Survey,                            "\n", append=T )
  cat( file=outFile, "# Last year for each index (lYear)\n",          append=T )
  cat( file=outFile, spObj$tSurvey,                             "\n", append=T )
  cat( file=outFile, "# Index series (possibly a matrix)\n",          append=T )
  cat( file=outFile, It,                                        "\n", append=T )
  
  # c) Call the surplus production model (assessSP.exe) ADMB program

  starttime <- Sys.time()                  # Set the start time for minimization.

  # Concept here is to always run MCMC but for constantF or variableF where the
  # risk adjusted Fmsy option is FALSE we run nMCMC=1 to get the MPDs as the
  # first row.  That way the same code can be used whether MPD or MCMC.
  
  # ARK (12-Dec-11) Cannot have NULL assignment to mcOut.
  mcOut      <- NA

  # Changed by ARK (01-May-13) to match assessCA.
  mcmcString <- paste( " -mcmc ",spObj$nMCMC," -mcsave ",spObj$nThin, sep="" )

  datFile  <- file.path( getwd(), paste( exeName,".dat", sep="" ) )
  exeFile  <- file.path( getwd(), exeName )
  mcmcFile <- file.path( getwd(), "mcout.dat" )
  pinFile  <- file.path( getwd(), paste( exeName,".pin", sep="" ) )  
  repFile  <- file.path( getwd(), paste( exeName,".rep", sep="" ) )  
  stdFile  <- file.path( getwd(), paste( exeName,".std", sep="" ) )
  
  # Set start time for optimization
  starttime <- Sys.time()

  xFactor <- 0.2
  for ( i in 1:4 )
  {
    hessPosDef <- TRUE

    if( .Platform$OS.type=="unix" )
    { 
      system( command=paste( exeFile," ",mcmcString," -iprint 100 -maxfn 5000",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )

      system( command=paste( exeFile," ","-mceval",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )
    }
    else     # Windows branch call to "system".
    {
      system( paste( exeFile,".exe ", mcmcString," "," -iprint 100 -maxfn 5000", sep="" ), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
              
      system( paste( exeFile,".exe -mceval", sep="" ), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
    }

  # Check if there is a valid mcmc output file, i.e., was the Hessian positive
  # definite, if not must use old mcmc output file and set hessPosDef to FALSE.
  # This condition is then flagged in mp$assess$runStatus$hessPosDef (T,F).
  
    tmpMCMC  <- try( read.table( mcmcFile, header=T, sep=" " ), silent=TRUE )
    
    if ( class( tmpMCMC )=="try-error" )
    {
      hessPosDef <- FALSE

      outFile <- file.path( getwd(), paste( exeName,".pin",sep="" ) ) 
      cat( file=outFile, "## ", exeName,": Surplus production model parameters.\n" )
      cat( file=outFile, "## Written:",     date(),                 "\n", append=T )
      cat( file=outFile, "# lnMsy\n",       log(spObj$Msy)*(1.0+i*xFactor), "\n", append=T )
      cat( file=outFile, "# lnFmsy\n",      log(spObj$Fmsy),         "\n", append=T )
      cat( file=outFile, "# pmMsy\n",       spObj$pmMsy,            "\n", append=T )
      cat( file=outFile, "# psdMsy\n",      spObj$psdMsy,           "\n", append=T )
      cat( file=outFile, "# pmFmsy\n",      spObj$pmFmsy,           "\n", append=T )
      cat( file=outFile, "# psdFmsy\n",     spObj$psdFmsy,          "\n", append=T )
      cat( file=outFile, "# rho\n",         spObj$rho,              "\n", append=T )  
      cat( file=outFile, "# lnOmega\n",     spObj$lnOmega,          "\n", append=T )
    }
      
    if ( hessPosDef )
      break
  }

  if ( class( tmpMCMC )=="try-error" )
  {
    hessPosDef <- FALSE  
    cat( "\nHessian not positive definite, using last good mcout.dat","\n" )
    mcmcFile <- file.path( getwd(), "mcout.bck" )          
    if ( file.exists( mcmcFile ) )
      tmpMCMC <- read.table( mcmcFile, header=T, sep=" " )
      
    # Save the *.pin and *.dat files for forensics.
    badPinFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".pin", sep="" ) )
    file.copy( pinFile, badPinFile, overwrite=TRUE )
    badDatFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".dat", sep="" ) )
    file.copy( datFile, badDatFile, overwrite=TRUE )      
  }
  else
  {
    # Save the mcout.dat file in case the next Hessian is not positive definite.
    file.copy( "mcout.dat","mcout.bck", overwrite=TRUE )  
  }

  # Read the ADMB report file.
  assessSP <- lisread( repFile, quiet=TRUE )

  # Read MLE standard deviations for Bmsy and Fmsy estimates.
  stdBmsy       <- NA
  stdMSY        <- NA
  stdFmsy       <- NA
  stdProjExpBio <- NA
 
  if ( hessPosDef )
  {
    if ( file.exists( stdFile ) )
    {
      stderrs       <- read.table( stdFile, as.is=TRUE, header=FALSE, skip=1, sep="" )
      stdBmsy       <- as.numeric( stderrs[ stderrs$V2=="Fmsy", ncol(stderrs) ] )
      stdFmsy       <- as.numeric( stderrs[ stderrs$V2=="Bmsy", ncol(stderrs) ] )
      stdProjExpBio <- as.numeric( stderrs[ stderrs$V2=="projExpBio", ncol(stderrs) ] )
    }
  }
  
  # ARK *** (21-Jun-13) Cox changed this elswhere, to put thinning at ADMB call.
  nThin <- 1
  if( spObj$useMLE==FALSE )
  {
    if ( spObj$hcrType=="declineRisk" )
    {
      # The MCMC output is in blocks of nQuotaLevels, thin from each quota level.
      idxQ <- split( c(1:nrow(tmpMCMC)), tmpMCMC$Q )
      iRow <- as.vector( sapply( idxQ,
           function(x) { return( x[ seq(1,length(x),nThin)] ) } ) )
    }
    else if ( spObj$hcrType=="variableF" | spObj$hcrType=="constantF" )
    {
      # constantF or variableF: thin MCMC chain based on nThin from GUI.
      tmpN  <- nrow( tmpMCMC )
      iRow  <- seq( 1, tmpN, by=nThin )
    }

    # Extract thinned chain.
    mcOut <- tmpMCMC[ iRow, ]
  }
  else
    mcOut <- tmpMCMC[1,]     # First row of MCMC output are the MLEs.
  
  endtime <- Sys.time()                      # End time for minization.
  elapsed <- difftime( endtime,starttime )   # Elapsed minimization time.  

  # d) Save assessment results to output list.
  
  assessment <- assessSP

  assessment$mpdPars <- list( objFun  = assessSP$objFun,
                              totLike = assessSP$total_likelihood,
                              totPriors = assessSP$total_priors,
                              fpen    = assessSP$fpen,
                              bpen    = assessSP$bpen,  
                              ssbFmsy = assessSP$Bmsy,
                              Fmsy    = assessSP$Fmsy,
                              B0      = assessSP$Bmsy*2.,
                              q       = assessSP$q,
                              stdBmsy = stdBmsy,
                              stdFmsy = stdFmsy,

                              projExpBio    = assessSP$projExpBio,
                              stdProjExpBio = stdProjExpBio,
                              dep     = assessSP$D,
                              lastDt  = assessSP$lastDt )
                              
  assessment$mcOut <- mcOut                          
                              
  assessment$runStatus <- list( exeName       = exeName,
                                objFun        = assessSP$objFun,
                                maxGrad       = assessSP$maxGrad,
                                nEval         = assessSP$nEval,
                                convT         = elapsed[[1]],
                                iExit         = assessSP$iExit,
                                hessPosDef    = hessPosDef,
                                fisheryClosed = NA,
                                deadFlag      = NA )
  
  # Exploitable and spawning biomass are the same for the production model.
  assessment$exploitBt <- assessSP$Bt
  assessment$spawnBt   <- assessSP$Bt
  
  assessment$lnOmega.lastFit <- assessSP$lnOmega
  
  return( assessment )
}     # END function assessModSP


callProcedureKalman <- function( obj, t )
{
  # Extract operating model and management procedure objects
  om <- obj$om
  mp <- obj$mp

  # Extract catch history.
  Dt <- om$Dt[1:(t-1)]

  # Extract survey index frequency information.
  surveyOn   <- attributes( mp$data$It )$surveyOn

  # Extract index values.
  It <- mp$data$It[ c(1:(t-1)) ]
 
  # Assessment frequency information - not implemented yet
  
  tmp             <- list()                 # Temporary list
  tmp$It          <- It                     # Stock index      
  tmp$kfGain      <- mp$assess$kfGain       # Kalman filter gain parameter
  stockAssessment <- assessModKF( tmp )     # Call Kalman filter method
  
  stockAssessment
}     # END function callAssessKalman

callProcedureMovingAvg <- function( obj, t )
{
  # Extract operating model and management procedure objects
  om <- obj$om
  mp <- obj$mp

  Dt <- om$Dt[1:(t-1)]                  # Catch history

  # Extract survey index attributes before they are lost when It extracted.
  surveyOn   <- attributes( mp$data$It )$surveyOn

  # Extract index values.
  It <- mp$data$It[ c(1:(t-1)) ]
 
  # Assessment frequency information - not implemented yet
  
  tmp             <- list()               # Temporary list
  tmp$It          <- It                   # Stock index      
  tmp$avgPoints   <- mp$assess$avgPoints  # Number of index point to average
  stockAssessment <- assessModMA( tmp )   # Call moving average method
  stockAssessment
}     # END function callAssessMovingAvg


callProcedureSP <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.  So obj$opMod has all
  # reference points and life history schedules.

  spObj   <- list()                        # List to pass to assessModProd
  spObj$t <- t                             # Current time step.
  nT      <- obj$opMod$nT                  # Number of years in simulation.
  tMP     <- obj$opMod$tMP                 # Year MP first applied, tMP.
  
  # (1) Data.
  
  Dt       <- obj$om$Dt[1:(t-1)]           # Extract catch history.
  spObj$Dt <- Dt
 
  # Extract survey index attributes before they are lost when It extracted.
  surveyOn       <- attributes( obj$mp$data$It )$surveyOn

  It             <- obj$mp$data$It[ c(1:(t-1)) ]  # Extract stock index values.
  spObj$It       <- It                            # Stock index

  spObj$t1Survey <- obj$mp$data$t1Survey          # First survey year.
  spObj$tSurvey  <- t - 1                         # Last survey year.

  # (2) Assessment Method

  spObj$pmMsy   <- obj$mp$assess$spPmMsy
  spObj$psdMsy  <- obj$mp$assess$spPsdMsy
  spObj$pmFmsy  <- obj$mp$assess$spPmFmsy
  spObj$psdFmsy <- obj$mp$assess$spPsdFmsy
  spObj$Msy     <- obj$mp$assess$spMsy
  spObj$Fmsy    <- obj$mp$assess$spFmsy
  spObj$lnOmega <- rep( 0,(t-3) )
  spObj$rho     <- obj$mp$assess$spRhoEiv

  spObj$indexType <- as.numeric( as.logical(obj$mp$assess$spSurveyRel) )
  
  # (3) Harvest control rule parameters.

  hcrType       <- obj$mp$hcr$hcrType
  spObj$hcrType <- hcrType

  if ( hcrType == "constantF" )
    spObj$ruleType <-1 
  if ( hcrType == "variableF" )
    spObj$ruleType <- 2
  if ( hcrType == "declineRisk" )
    spObj$ruleType <- 3
   if ( hcrType == "hcrUser" )
    spObj$ruleType <- 4

  spObj$nProjYears <- 1
  if ( hcrType == "declineRisk" )
    spObj$nProjYears <- obj$mp$hcr$nProjYears

  spObj$trendYears   <- obj$mp$hcr$trendYears
  spObj$rSeed        <- obj$opMod$rSeed 

  # Add in parameters to control MLE or MCMC in ADMB.
  spObj$nMCMC         <- obj$mp$hcr$nMCMC
  spObj$nThin         <- obj$mp$hcr$nThin
  spObj$useMLE        <- obj$mp$hcr$useMLE

  # Quota levels for projection.
  spObj$Qlevel <- seq( obj$mp$hcr$lowerQuota, obj$mp$hcr$upperQuota,
                       obj$mp$hcr$binQuota )

  spObj$idxCtlPts <- obj$mp$hcr$idxCtlPts      # From .calcTimes function.
  
  # -- Call the Surplus production model.
  
  result <- assessModSP( spObj )

  result
}     # END function callAssessSP


callProcedureCAA <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.  So obj$opMod has all
  # reference points and life history schedules.

  caObj    <- list()                        # List to pass to assessModProd
  caObj$t  <- t                             # Current time step.
  caObj$nT <- obj$opMod$nT
  tMP      <- obj$opMod$tMP                 # Year MP first applied, tMP.

  # (1) Data.

  caObj$A          <- obj$opMod$nAges
  caObj$pat        <- obj$mp$data$pat[ ,(1:(t-1)) ]
  caObj$patS       <- obj$mp$data$patS[ ,(1:(t-1)) ]  
  
  # Use fishery ages?
  caObj$caaAges    <- as.numeric( as.logical( obj$mp$assess$caaAges ) )
  if ( caObj$caaAges==0 )
    caObj$pat[] <- -1
  
  # Use survey ages?
  caObj$caaAgesS   <- as.numeric( as.logical( obj$mp$assess$caaAgesS ) )
  if ( caObj$caaAgesS==0 )
    caObj$patS[] <- -1
    
  caObj$caaFixFSel <- as.numeric( as.logical( obj$mp$assess$caaFixFSel ) )
  caObj$caaFixSSel <- as.numeric( as.logical( obj$mp$assess$caaFixSSel ) )
  caObj$caaFSelOM  <- as.numeric( as.logical( obj$mp$assess$caaFSelOM) )
  caObj$caaSSelOM  <- as.numeric( as.logical( obj$mp$assess$caaSSelOM) )

  Dt        <- obj$om$Dt[1:(t-1)]           # Extract catch history.
  caObj$Dt  <- Dt
  lastQuota <- Dt[length(Dt)]
  # Extract survey index attributes before they are lost when It extracted.
  surveyOn   <- attributes( obj$mp$data$It )$surveyOn

  It             <- obj$mp$data$It[ c(1:(t-1)) ]  # Extract stock index values.
  caObj$It       <- It                            # Stock index

  # ARK (23-Jun-13) Is this line now dead code?
  caObj$tauSurvey <- obj$mp$data$tauSurvey1Mean

  caObj$t1Survey     <- obj$mp$data$t1Survey          # First survey year.
  caObj$tSurvey      <- t - 1                         # Last survey year.
  caObj$caaSurveyRel <- eval( parse( text=obj$mp$assess$caaSurveyRel ) )
  caObj$aMat50 <- obj$opMod$aMat50
  caObj$aMat95 <- obj$opMod$aMat95
  caObj$Linf   <- obj$opMod$Linf
	caObj$L1     <- obj$opMod$L1
	caObj$vonK   <- obj$opMod$vonK
	caObj$c1     <- obj$opMod$c1
	caObj$c2     <- obj$opMod$c2
	caObj$sigmaL <- obj$opMod$sigmaL

  # (2) Assessment Method

  # ARK - Extract assessment frequency.  Not implemented yet

  # Leading parameters - provide true om parameters to ADMB initial parameter file
  # (assessCA.pin) year t=tMP-1 to help initial fit convergence speed.
  caObj$logit.ySteepness <- log(obj$opMod$rSteepness / (1.-obj$opMod$rSteepness))
  caObj$lnB0             <- log( obj$opMod$B0 )
  caObj$lnM              <- log( obj$mp$assess$caaPmM )
  
  #caObj$ln_aSel50        <- log( obj$opMod$aSel50 )	      #Fishery
  #caObj$ln_aSelStep     <- log( obj$opMod$aSel95-obj$opMod$aSel50 )	 #Fishery
  #caObj$ln_aSelS50      <- log( obj$opMod$aSelS50 )	    #Survey
  #caObj$ln_aSelSStep   <- log( obj$opMod$aSelS95-obj$opMod$aSelS50 )	      #Survey

  if( caObj$caaFSelOM )
  {
	  caObj$ln_aSel50      <- log( obj$opMod$aSel50 )	      #Fishery
	  caObj$ln_aSelStep    <- log( obj$opMod$aSel95-obj$opMod$aSel50 )	 #Fishery
  }
  else
  {
  	caObj$ln_aSel50      <- log( obj$mp$assess$caaSelVal50 )	      #Fishery
	  caObj$ln_aSelStep    <- log( obj$mp$assess$caaSelVal95-obj$mp$assess$caaSelVal50 )	 #Fishery
  }

  if( caObj$caaSSelOM )
  {
	  caObj$ln_aSelS50     <- log( obj$opMod$aSelS50 )	      #Survey
	  caObj$ln_aSelSStep   <- log( obj$opMod$aSelS95-obj$opMod$aSelS50 )	 #Survey
  }
  else
  {
  	caObj$ln_aSelS50     <- log( obj$mp$assess$caaSelSVal50 )	      #Survey
	  caObj$ln_aSelSStep   <- log( obj$mp$assess$caaSelSVal95-obj$mp$assess$caaSelSVal50 )	 #Survey
  }	  
	  
  caObj$omega            <- rep( 0., (t-1) ) #obj$om$errors$omegat[1:(t-1)]
  caObj$rho              <- obj$mp$assess$caaRhoEiv
  
  # sigmaR does not appear to be calculated in assessCA tpl.
  #  caObj$ln_sigmaR        <- log( obj$mp$assess$initSigmaR )  
  caObj$nSamples         <- obj$mp$assess$nSamples	#Fishery
  caObj$nSamplesS         <- obj$mp$assess$nSamplesS #Survey
  
  lastParFile <- file.path( getwd(), paste( "assessCA",".par", sep="" ) )
  mcmcOutFile <- file.path( getwd(), "mcout.dat" )
  
  if ( t == tMP )
  {
    if ( file.exists( lastParFile ) )
      file.remove( lastParFile )
    if ( file.exists( mcmcOutFile ) )
      file.remove( mcmcOutFile )
  }
  
  if ( t > tMP )  
  {
    if ( file.exists( lastParFile ) )
    {
      lastPars <- lisread( lastParFile )
      if ( length( lastPars$omega ) > {t-1} )
      {
        caObj$logit.ySteepness  <- lastPars$logit_ySteepness
        caObj$lnBo              <- lastPars$ln_B0
        caObj$lnM               <- lastPars$ln_M
        caObj$ln_aSel50         <- lastPars$ln_aSel50	  #Fishery
        caObj$ln_aSelStep       <- lastPars$ln_aSelStep
	    caObj$ln_aSelS50        <- lastPars$ln_aSelS50  #Survey
        caObj$ln_aSelSStep      <- lastPars$ln_aSelSStep
        caObj$rho               <- lastPars$rho
        caObj$omega             <- c( lastPars$omega, 0.0 )
      }
    }     # if lastParFile exists.
  }     # if t >= tMP

  # Prior distribution parameters.

  caObj$pmSteepness  <- obj$mp$assess$caaPmSteep
  caObj$psdSteepness <- obj$mp$assess$caaPsdSteep

  caObj$pmM          <- obj$mp$assess$caaPmM
  caObj$psdM         <- obj$mp$assess$caaPsdM
  caObj$pmQ          <- c(obj$mp$assess$caaPmQ1,obj$mp$assess$caaPmQ2)
  caObj$psdQ         <- c(obj$mp$assess$caaPsdQ1,obj$mp$assess$caaPsdQ2)
  caObj$qChangeTime  <- obj$opMod$qChangeTime
  caObj$sigmaM       <- obj$mp$assess$caaSigmaM

  # Estimation bounds on parameters, transform to log scale.
  caObj$lowBoundLnB0   <- log(obj$mp$assess$caaLowBoundB0)
  caObj$upperBoundLnB0 <- log(obj$mp$assess$caaUpBoundB0)

  # (3) Harvest control rule parameters.

  caObj$nProjYears <- 1
  if ( obj$mp$hcr$hcrType == "declineRisk" )
    caObj$nProjYears <- obj$mp$hcr$nProjYears

  caObj$trendYears   <- obj$mp$hcr$trendYears
  caObj$initSeed     <- obj$opMod$rSeed  
  
  hcrType <- obj$mp$hcr$hcrType
  if ( hcrType == "constantF" )
    caObj$ruleType <-1 
  if ( hcrType == "variableF" )
    caObj$ruleType <- 2
  if ( hcrType == "declineRisk" )
    caObj$ruleType <- 3
  if ( hcrType == "hcrUser" )
    caObj$ruleType <- 4

  caObj$hcrType <- hcrType

  # Add in parameters to control MLE or MCMC in ADMB.
  caObj$nMCMC     <- obj$mp$hcr$nMCMC
  caObj$nThin     <- obj$mp$hcr$nThin
  caObj$useMLE    <- obj$mp$hcr$useMLE

  # Put list of years in which control points should be estimated based on
  # user-specified update frequency into index units so that year tMP is
  # set to 1. Only applies to assessment models that can estimate reference points.
   
  caObj$idxCtlPts <- obj$mp$hcr$idxCtlPts     # From .calcTimes function.
  
  caObj$caaSurveyRel <- eval( parse( text=obj$mp$assess$caaSurveyRel ) )

  # Quota levels for projection.
  # SPC (13-Mar-2013): use of obj$refPts$yieldFmsy here is temporary. 
  #                    It should be the current estimate of MSY, not
  #                   the opMod value.
  #meanHistCatch <- mean( obj$om$Dt[1:(t-1)] ) # mean of historical catch
  #maxQLevel     <- max( lastQuota, meanHistCatch )
  maxQLevel    <-obj$refPts$yieldFmsy
  caObj$Qlevel <- seq( from=obj$mp$hcr$lowerQuota*maxQLevel, 
                       to=obj$mp$hcr$upperQuota*maxQLevel,
                       length=obj$mp$hcr$binQuota )

  # -- Call assessment model, which will do its own file read/write
  result <- assessModCA( caObj )

	result  
}     # END function callProcedureCAA


# .calcDDpars
# Purpose:        calculates Ford-Walford growth parameters for delay difference model input
# Parameters:     kAge = age at knife edge recruitment, lenAge=length-at-age; c1=scalar; c2=power
#                 sigmaL=std error in length-at-age
# Returns:        Ford-Walford parameters alpha_g and rho_g, and wk, the weight at recruitment 
# Source:        R.E. Forrest

# NOTES FOR ROB: kAge is the age at knife-edge recruitment. It will be set on
# the delay difference assessment tab.
.calcDDpars <- function( kAge=3, A=25,
                         Linf=80., L1=35.0, vonK=0.465, c1=1.e-5, c2=3.0, sigmaL )
{
  age <- 0:A        # Start at 0 as trap in case user enters 1 as kAge
  # (need weight at kAge-1 for Ford Walford parameters)
	recAge <- kAge:A  # Fully recruited ages.

	lenAge <- Linf + (L1-Linf)*exp(-vonK*(age-1.))  # length-age relationship
	wtAge <- c1*lenAge^c2*(1.+0.5*c2*(c2-1.)*sigmaL*sigmaL) #weight-length relationship including bias correction

	# Fully recruited weight-length relationship.
	wtAgeRec <- wtAge[(kAge+1):(A+1)] #+1 accounts for mean weights starting at age 0

  # Make matrix of Weight at age and Weight at age - 1
  # Need to keep this so Walford plot can be plotted as one of the model outputs
  Walford <- matrix(ncol=3, nrow=length(recAge))
  colnames(Walford) <- c( "Recruited Age", "Wa-1","Wa" )
  Walford[,1] <- recAge

  for(i in 1:length(recAge))
  {
	  iage <- recAge[i]-1
	  Walford[i,2] <- wtAge[iage+1] #+1 accounts for mean weights starting at age 0
  }

  Walford[,3] <- wtAgeRec

  # Fit linear regression to weight at age vs weight at age-1 data to get slope (rho) and intercept (alpha)
	fit     <- lm(Walford[,3]  ~Walford[,2]  )
	alpha_g <- as.numeric(fit$coef[1])
	rho_g   <- as.numeric(fit$coef[2])

	# NOTE HERE that the weight at recruitment can be read from the regression line or from the mean weight at age relationship
	# When kAge (age at recruitment) is young, it seems that a poor fit can be obtained at the lowest end of the wa vs wa-1 curve, since the relationship is not quite linear when dealing with weight at age
	# (not the case with length at age, where the la vs la-1 relationship is nice and linear)
	# Suggest we take the value for kAge from the mean weight at age relationship
	# Looking at the delay difference equations in H&W, wk is not used to derive other parameters anywhere (unless using the Schnute version)
	# So it seems better to use the one from the mean weight at age relationship - the one from the regression line can be significantly higher and may bias results
	wk <- wtAgeRec[1]
		
	ddPar         <- list()
	ddPar$Walford <- Walford #for plotting
	ddPar$alpha_g <- alpha_g
	ddPar$rho_g   <- rho_g
	ddPar$wk      <- wk
		
	return(ddPar)
}     # END function .calcDDpars


callProcedureDD <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.  So obj$opMod has all
  # reference points and life history schedules.

	ddObj      <- list()                 # List to pass to assessModProd
	ddObj$t    <- t                      # Current time step.
	ddObj$nT   <- obj$opMod$nT           # Last year in projection.
	tMP        <- obj$opMod$tMP          # Year MP first applied, tMP.

  # (1) Data.
  ddObj$A    <- obj$opMod$nAges
  ddObj$pat  <- obj$mp$data$pat[ ,(1:(t-1)) ]  		#fishery
  ddObj$patS  <- obj$mp$data$patS[ ,(1:(t-1)) ]	#survey
  ddObj$kAge <- obj$mp$assess$ddkAge

  Dt         <- obj$om$Dt[1:(t-1)]     # Extract catch history.
  ddObj$Dt   <- Dt

  # Extract survey index attributes before they are lost when It extracted.
	surveyOn   <- attributes( obj$mp$data$It )$surveyOn

	It         <- obj$mp$data$It[ c(1:(t-1)) ]  # Extract stock index values.
	ddObj$It   <- It                            # Stock index

  ddObj$tauSurvey <- obj$mp$data$tauSurvey1Mean

  ddObj$t1Survey    <- obj$mp$data$t1Survey          # First survey year.
  ddObj$tSurvey     <- t - 1                         # Last survey year.
  ddObj$unfished  <- as.numeric( as.logical(obj$mp$assess$ddUnfished) )
  ddObj$ddSurveyRel <- as.numeric( as.logical(obj$mp$assess$ddSurveyRel) )

  ddObj$Linf   <- obj$opMod$Linf
  ddObj$L1     <- obj$opMod$L1
  ddObj$vonK   <- obj$opMod$vonK
  ddObj$c1     <- obj$opMod$c1
  ddObj$c2     <- obj$opMod$c2
  ddObj$sigmaL <- obj$opMod$sigmaL
  
  ddObj$sigmaC <- obj$mp$assess$ddSigmaC
		  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## CALL .calcDDpars TO GET PARAMETERS NEEDED FOR DELAY DIFFERENCE MODEL
  # Note this object has four sub-objects: Walford (the matrix of weight at age
  # and weight at age-1 for plotting), rho_g, alpha_g and wk.
  
  ddObj$ddpars <- .calcDDpars( ddObj$kAge ,ddObj$A, ddObj$Linf, ddObj$L1,
                               ddObj$vonK, ddObj$c1, ddObj$c2, ddObj$sigmaL )
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  # ARK (10-Jul-13) Overide using parameters from Method GUI at RF request.
  ddObj$ddpars$alpha_g <- obj$mp$assess$ddAlphaG
  ddObj$ddpars$rho_g   <- obj$mp$assess$ddRhoG
  ddObj$ddpars$wk      <- obj$mp$assess$ddWkG

  # (2) Assessment Method

  # ARK - Extract assessment frequency.  Not implemented yet

  # Leading parameters - provide true om parameters to ADMB initial parameter file.
  # Using true values helps convergence speed.

  ddObj$logit.ySteepness <- log(obj$opMod$rSteepness / (1.-obj$opMod$rSteepness))
	ddObj$lnB0             <- log( obj$opMod$B0 )
	ddObj$lnM              <- log( obj$mp$assess$ddPmM )
  ddObj$rho              <- obj$mp$assess$ddRhoEiv
  ddObj$omega            <- rep( 0., (t-1) ) #obj$om$errors$omegat[1:(t-1)]
  #AssessDD only estimates lnF in years where catch > 0	  #RF_add_25Sep	 2013
     ft_count <- 0
     for(yy in 1:(t-1)) if(Dt[yy]>0 ) ft_count <- ft_count+1;
     ddObj$lnFt            <- rep( log(0.1), ft_count ) #RF_add_25Sep  
  
  lastParFile <- file.path( getwd(), paste( "assessDD",".par", sep="" ) )	   #RF_correction 25 Sept 2013
  mcmcOutFile <- file.path( getwd(), "mcout.dat" )
  
  if ( t == tMP )
  {
    if ( file.exists( lastParFile ) )
      file.remove( lastParFile )
    if ( file.exists( mcmcOutFile ) )
      file.remove( mcmcOutFile )
  }
  
  if ( t > tMP )
  {
    lastParFile <- file.path( getwd(), paste( "assessDD",".par", sep="" ) )
    if ( file.exists( lastParFile ) )
    {
      lastPars <- lisread( lastParFile )
      if ( length( lastPars$omega ) > {t-1} )
      {
        #ddObj$logit.ySteepness  <- lastPars$logit_ySteepness
        #ddObj$lnBo              <- lastPars$ln_B0
        #ddObj$lnM               <- lastPars$ln_M
        ddObj$rho               <- lastPars$rho
        ddObj$lnFt              <- c( lastPars$ln_Ft, 0.0 )
        ddObj$omega             <- c( lastPars$omega, 0.0 )
      }
    }     # if lastParFile exists.
  }     # if t >= tMP

  # Prior distribution parameters.

  ddObj$pmSteepness  <- obj$mp$assess$ddPmSteep
  ddObj$psdSteepness <- obj$mp$assess$ddPsdSteep

  ddObj$pmM          <- obj$mp$assess$ddPmM
  ddObj$psdM         <- obj$mp$assess$ddPsdM

  # (3) Harvest control rule parameters.

  ddObj$nProjYears <- 1
  if ( obj$mp$hcr$hcrType == "declineRisk" )
    ddObj$nProjYears <- obj$mp$hcr$nProjYears

  ddObj$trendYears   <- obj$mp$hcr$trendYears
  ddObj$initSeed     <- obj$opMod$rSeed  

  hcrType <- obj$mp$hcr$hcrType
  if ( hcrType == "constantF" )
    ddObj$ruleType <-1 
  if ( hcrType == "variableF" )
    ddObj$ruleType <- 2
  if ( hcrType == "declineRisk" )
    ddObj$ruleType <- 3
  if ( hcrType == "hcrUser" )
    ddObj$ruleType <- 4

  ddObj$hcrType <- hcrType

  # Add in parameters to control MLE or MCMC in ADMB.
  ddObj$nMCMC         <- obj$mp$hcr$nMCMC
  ddObj$nThin         <- obj$mp$hcr$nThin
  ddObj$useMLE        <- obj$mp$hcr$useMLE

  # Put list of years in which control points should be estimated based on
  # user-specified update frequency into index units so that year tMP is
  # set to 1. Only applies to assessment models that can estimate reference points.

  ddObj$idxCtlPts <- obj$mp$hcr$idxCtlPts     # From .calcTimes function.

  # Quota levels for projection.
  ddObj$Qlevel <- seq( obj$mp$hcr$lowerQuota, obj$mp$hcr$upperQuota,
            		       obj$mp$hcr$binQuota )

  # -- Call assessment model, which will do its own file read/write.
  
  result <- assessModDD( ddObj )

	result  
}     # END function callProcedureDD 


# .updatePop
# Purpose:        Complete one iteration of feedback system
# Parameters:     obj=complete simulation object up to t-1; t=current time
# Returns:        obj=complete simulation object up to t
# Source:         S.P. Cox
.updatePop <- function( obj, t )
{
  
  # Each assessment method does the following:
  
  # Step 1. extract last catch
  # Step 2. extract stock index frequency info
  # Step 3. extract stock assessment frequency info
  # Step 4. extract reference point update frequency info
  # Step 5. perform stock assessment
  # Step 6. calculate estimated reference points for HCR (if required)
  
  # Then, the results of the stock assessment are fed to the HCR:
  
  # Step 7. set catch limit
  # Step 8. update operating model state variables and data
  # Step 9. return updated simulation object

  nT   <- obj$opMod$nT
  tMP  <- obj$opMod$tMP
  tRow <- t - tMP + 1

  # Extract attributes.
  #RF QUERY - why is attrMethod used multiple times? Can I use attrAges mutiple times?
  attrAges   <- attributes( obj$mp$data$pat )
  attrAgesS  <- attributes( obj$mp$data$patS )
  
  attrMethod <- attributes( obj$mp$assess$spawnBt )
  attrMethod <- attributes( obj$mp$assess$exploitBt )
  attrMethod <- attributes( obj$mp$assess$exploitBtS )
  attrSurvey <- attributes( obj$mp$data$It )
  methodId   <- obj$mp$assess$methodId

  # Kalman filter
  if( methodId == .KALMAN )
  {
    stockAssessment <- callProcedureKalman( obj, t )
    obj$mp$assess$mpdPars[ tRow, ] <- c( t, "None" )
    obj$mp$assess$pdfPars[ tRow, ] <- c( t, "None" )    
  }

  # Moving average
  if( methodId == .MOVAVG )
  {
    stockAssessment <- callProcedureMovingAvg( obj, t )
    obj$mp$assess$mpdPars[ tRow, ] <- c( t, "None" )
    obj$mp$assess$pdfPars[ tRow, ] <- c( t, "None" )
  }
  
  # Catch age model.  
  if ( methodId == .CAAMOD )
  {
    stockAssessment <- callProcedureCAA( obj,t )

    nColRS <- length( stockAssessment$runStatus )
    obj$mp$assess$runStatus[ tRow,c(1:(nColRS+1)) ] <- c( t, unlist( stockAssessment$runStatus ) )
    names( obj$mp$assess$runStatus )[1:(nColRS+1)] <- c( "tStep",names(stockAssessment$runStatus) )

    mcmcOut <- stockAssessment$mcOut

    # If useMLE then we want only the first row of mcmcOut for MPDs.
	  if ( obj$mp$hcr$hcrType!="declineRisk" && obj$mp$hcr$useMLE==TRUE )
      mcmcOut <- mcmcOut[ 1,,drop=FALSE ]
       
    #--------------------------------------------------------------------------#
    # Call .caaModCtlPtsUseR here using lapply for efficiency.                 #
    #--------------------------------------------------------------------------#
    #   t1 <- proc.time()
    # Print a message every X calls and for the first 5 to reassure progress.
    #if ( i %% nPrint==0 | i <=5 )
    #{
    #  t2 <- proc.time()
    #  cat( "\nMSG (.calcRefPointsFromMCMC) Completed ",i," of ",nDraws," draws.\n\n" )
    #  cat( "\nMSG (.calcRefPoints) took ",t2-t1," seconds for ",i,"calls.\n" )
    #  cat( "\nMSG (.calcRefPoints) Average time per call is ",(t2-t1)[1]/i," seconds.\n\n" )      
    #  print( result[i,] )
    #}
    
    # ARK (15-Feb-12) Build in options for calculating reference points.
    # 1. Use R mseRrefPoints.r.
    # 2. Use refpts.exe with "for" loop.
    # 3. Use refpts.exe with "apply".
    # 4. Use refpts.exe with snow "parRapply".
  
    # This controls the method used to calculate reference points.
    useSnow <- obj$mp$hcr$useSnow
    if ( useSnow )
    {
      optCtlPts <- "useSnow"
      useDLL <- obj$mp$hcr$useDLL
    }
    else
      optCtlPts <- "useApply"

    refPts <- .caaModCalcCtlPts( mcmcOut, pars=obj$opMod, fmsy=obj$refPts$Fmsy,
                                 hcrList=obj$mp$hcr, method=optCtlPts,
                                 useDLL=useDLL )
                                 
    mcmcOut <- cbind( mcmcOut,refPts )
    
    #--------------------------------------------------------------------------#

    # If hcrType is declineRisk, fill the reference points in empty rows.
    if ( obj$mp$hcr$hcrType=="declineRisk" )
    {
      qLevels <- unique( mcmcOut$Q )
      nLevels <- length( qLevels )
      nRows   <- nrow( mcmcOut )
      idx     <- !is.na( mcmcOut$ssbFmsy )
      mcmcOut$ssbFmsy <- rep( mcmcOut$ssbFmsy[ idx ], rep( nLevels,nRows/nLevels) )
      mcmcOut$Fmsy    <- rep( mcmcOut$Fmsy[ idx ],    rep( nLevels,nRows/nLevels) ) 
      mcmcOut$MSY     <- rep( mcmcOut$MSY[ idx ],     rep( nLevels,nRows/nLevels) ) 
    }
    
    # Get the MCMC estimates.
    mcmcNames <- c( "rSteepness","rec.a","rec.b","M","aSel50","aSel95","B0","ssbFmsy","Fmsy" )
    
    mcmcPars <- apply( mcmcOut[,mcmcNames],2,mean )
    names( mcmcPars ) <- mcmcNames

    # Save the MPD estimates (nCols needs +3 for tStep, ssbFmsy, Fmsy).
    nCols <- length( stockAssessment$mpdPars ) + 3
    obj$mp$assess$mpdPars[ tRow, c(1:nCols)] <- c( tStep=t, stockAssessment$mpdPars, mcmcOut[1,"ssbFmsy"], mcmcOut[1,"Fmsy"] )
    names( obj$mp$assess$mpdPars )[c(1:nCols)] <- c( "tStep", names(stockAssessment$mpdPars),"ssbFmsy","Fmsy" )

    # Save the PDF estimates.
    nCols <- length( mcmcPars ) + 1
    obj$mp$assess$pdfPars[ tRow,c(1:nCols) ] <- c( tStep=t, mcmcPars )
    names( obj$mp$assess$pdfPars )[1:nCols] <- c( "tStep",names(mcmcPars) )

    # The assessment estimates biomass up to t-1 because that is the only data it has.
    # The projection is needed to complete the vector up to time t - so, the final
    # value is the beginning of year biomass.

    # Retrospective statistics.
    obj$mp$assess$retroExpBt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$exploitBt,
                                                       stockAssessment$projExpBio )
    obj$mp$assess$retroExpBtS[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$exploitBtS,
                                                        stockAssessment$projExpBioS )	 #FLAGRF
  
    obj$mp$assess$retroSpawnBt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$spawnBt,
                                                         stockAssessment$projSpawnBio )
 
    obj$mp$assess$retroFt[ tRow, c(1:(t)) ] <- c( t, stockAssessment$Ft)	     #RF_added
    
    obj$mp$assess$retroRt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$Rt, NA )
    
    obj$mp$assess$Rt[ tRow, c(1:t) ] <- c( t, stockAssessment$Rt )    
    #obj$mp$assess$Rt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$Rt )

    obj$mp$assess$pat[ ,c(1:(t-1)) ] <- t( stockAssessment$pat )
    obj$mp$assess$patS[ ,c(1:(t-1)) ] <- t( stockAssessment$patS )     #survey
  }     # ENDIF methodId=.CAAMOD.
  
  # Delay-Difference model.
  if ( methodId == .DDMOD )
  {
    stockAssessment <- callProcedureDD( obj,t )
    
    nColRS <- length( stockAssessment$runStatus )
    obj$mp$assess$runStatus[ tRow,c(1:(nColRS+1)) ] <- c( t, unlist( stockAssessment$runStatus ) )
    names( obj$mp$assess$runStatus )[1:(nColRS+1)] <- c( "tStep",names(stockAssessment$runStatus) )
    
    mcmcOut <- stockAssessment$mcOut
	  
    # If useMLE then we want only the first row of mcmcOut for MPDs.
	  if ( obj$mp$hcr$hcrType!="declineRisk" && obj$mp$hcr$useMLE==TRUE )
      mcmcOut <- mcmcOut[ 1,,drop=FALSE ]

    #--------------------------------------------------------------------------#
    # Call .ddModCtlPtsUseR here using lapply for efficiency.                 #
    #--------------------------------------------------------------------------#
    #   t1 <- proc.time()
    # Print a message every X calls and for the first 5 to reassure progress.
    #if ( i %% nPrint==0 | i <=5 )
    #{
    #  t2 <- proc.time()
    #  cat( "\nMSG (.calcRefPointsFromMCMC) Completed ",i," of ",nDraws," draws.\n\n" )
    #  cat( "\nMSG (.calcRefPoints) took ",t2-t1," seconds for ",i,"calls.\n" )
    #  cat( "\nMSG (.calcRefPoints) Average time per call is ",(t2-t1)[1]/i," seconds.\n\n" )      
    #  print( result[i,] )
    #}
    
    # ARK (31-May-13) Build in options for calculating reference points.
    # 1. Use R mseRrefPoints.r.
    # 2. Use refptsDD.exe with "for" loop.
    # 3. Use refptsDD.exe with "apply".
    # 4. Use refptsDD.exe with snow "parRapply".

    # This controls the method used to calculate reference points.
    # For now only means of doing DD is via useSnow and !useDLL.
    
    useSnow <- obj$mp$hcr$useSnow
    if ( useSnow )
    {
      optCtlPts <- "useSnow"
      useDLL <- obj$mp$hcr$useDLL
    }
    else
      optCtlPts <- "useApply"

    # This controls the method used to calculate reference points.
    if ( obj$mp$hcr$useSnow==FALSE )
    {
      refPts <- lapply( 1:nrow(mcmcOut), .ddModCtlPtsUseR, pars=obj$opMod,
                        mcmcData=mcmcOut, hcrList=obj$mp$hcr )
                         
      # This code "stacks" a the list of vectors created by the call to
      # .calcCtlPoints within .ddModCtlPtsFromMCMC into a data.frame.
      mcmcOut         <- cbind( mcmcOut, do.call(rbind,refPts) )
      mcmcOut$ssbFmsy <- as.numeric(mcmcOut$ssbFmsy)
      mcmcOut$Fmsy    <- as.numeric(mcmcOut$Fmsy)
    }
    else                         
    {
      # Snow job...
      
      if( .Platform$OS.type=="unix" )
      {
        refPts <- .ddModCalcCtlPts( mcmcOut, pars=obj$opMod, fmsy=obj$refPts$Fmsy,
                                           hcrList=obj$mp$hcr,
                                           method=optCtlPts )
      }
      else     # ARK (01-May-13) Windows does not have DLL refpts yet...
      {
        refPts <- .ddModCalcCtlPts( mcmcOut, pars=obj$opMod, fmsy=obj$refPts$Fmsy,
                                        hcrList=obj$mp$hcr,
                                        method=optCtlPts )
      } 
      mcmcOut <- cbind( mcmcOut,refPts )
    }
    
    #--------------------------------------------------------------------------#

    # If hcrType is declineRisk, fill the reference points in empty rows.
    if ( obj$mp$hcr$hcrType=="declineRisk" )
    {
      qLevels <- unique( mcmcOut$Q )
      nLevels <- length( qLevels )
      nRows   <- nrow( mcmcOut )
      idx     <- !is.na( mcmcOut$ssbFmsy )
      mcmcOut$ssbFmsy <- rep( mcmcOut$ssbFmsy[ idx ], rep( nLevels,nRows/nLevels) )
      mcmcOut$Fmsy    <- rep( mcmcOut$Fmsy[ idx ],    rep( nLevels,nRows/nLevels) ) 
      mcmcOut$MSY     <- rep( mcmcOut$MSY[ idx ],     rep( nLevels,nRows/nLevels) ) 
    }

    # Get the MCMC estimates.
    mcmcNames <- c( "rSteepness","rec.a","rec.b","M","B0","ssbFmsy","Fmsy" )
    
    mcmcPars <- apply( mcmcOut[,mcmcNames],2,mean )
    names( mcmcPars ) <- paste( "mcmc",mcmcNames, sep="" )
    
    # Save the MPD pars, take MPD from row 1 of the MCMC output.
    nCols <- length( stockAssessment$mpdPars ) + 3
    obj$mp$assess$mpdPars[ tRow,c(1:nCols) ] <- c( t, stockAssessment$mpdPars, mcmcOut[1,c("ssbFmsy","Fmsy")] )
    names( obj$mp$assess$mpdPars )[c(1:nCols)] <- c( "tStep", names(stockAssessment$mpdPars ),c("ssbFmsy","Fmsy") )
    
    # Save the PDF pars - use MCMC results.
    mcmcNames <- c( "rSteepness","rec.a","rec.b","M","B0","ssbFmsy","Fmsy" )
    mcmcPars <- apply( mcmcOut[,mcmcNames],2,mean )
    
    nVals <- length( mcmcNames ) + 1
    obj$mp$assess$pdfPars[ tRow, c(1:nVals) ] <- c( tStep=t, mcmcPars )
    names( obj$mp$assess$pdfPars )[c(1:nVals)] <- c( "tStep", mcmcNames )
    
    # The assessment estimates biomass up to t-1 because that is the only data it has.
    # The projection is needed to complete the vector up to time t - so, the final
    # value is the beginning of year biomass.

    # Retrospective statistics - this adds the projection year.
    obj$mp$assess$retroExpBt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$exploitBt,
                                                      stockAssessment$projExpBio )

    obj$mp$assess$retroExpBtS[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$exploitBtS,
                                                      stockAssessment$projExpBioS )

    obj$mp$assess$retroSpawnBt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$spawnBt,
                                                      stockAssessment$projSpawnBio )

    obj$mp$assess$retroRt[ tRow, c(1:(t+1)) ] <- c( t, stockAssessment$Rt, NA )

    #Add retros for Ft (no projection)
    obj$mp$assess$retroFt[ tRow, c(1:(t)) ] <- c( t, stockAssessment$Ft)	 #RF_added
     
    # MPDs of recruitments.
    obj$mp$assess$Rt[ tRow, c(1:t) ] <- c( t, stockAssessment$Rt )    
  }     # ENDIF If methodId=.DDMOD    
    
  # Production model.
  if ( methodId == .PMOD )
  {
    stockAssessment <- callProcedureSP( obj, t )

    nColRS <- length( stockAssessment$runStatus )
    obj$mp$assess$runStatus[ tRow,c(1:(nColRS+1)) ] <- c( t, unlist( stockAssessment$runStatus ) )
    names( obj$mp$assess$runStatus )[1:(nColRS+1)] <- c( "tStep",names(stockAssessment$runStatus) )

    # mcmcOut has fields: B0 MSY Fmsy Bmsy BT projExpBio spawnBio depB0 depBmsy.
    mcmcOut <- stockAssessment$mcOut
    
    # Rename Bmsy for symmetry with .CAAMOD code.
    tmpNames <- names( mcmcOut )
    tmpNames[ tmpNames=="Bmsy" ] <- "ssbFmsy"
    names( mcmcOut ) <- tmpNames
                           
    # Now add M since production model does not have M as a parameter.
    mcmcOut <- cbind( mcmcOut, M=rep( obj$opMod$M, nrow(mcmcOut) ) )
    
    # Now add spawnBio and projSpawnBio since production model does not have spawnBio.
    # This is just for symmetry with catch-at-age model and for use with
    # harvest control rules.
    mcmcOut <- cbind( mcmcOut, spawnBio=rep( NA, nrow(mcmcOut) ) )
    mcmcOut <- cbind( mcmcOut, projSpawnBio=mcmcOut$projExpBio )
    
    # If the useMLE is TRUE then we want only the first row of mcmcOut for MPDs.
	  if ( obj$mp$hcr$hcrType!="declineRisk" && obj$mp$hcr$useMLE==TRUE )
      mcmcOut <- mcmcOut[ 1,,drop=FALSE ]    
    
    # Mean of posterior for B0, Bmsy, Fmsy, MSY.
    mcmcNames <- c("M","ssbFmsy","Fmsy","B0","MSY")
    mcmcPars <- apply( mcmcOut[,mcmcNames],2,mean )
    names( mcmcPars ) <- mcmcNames
    
    # Save the MPD pars.
    nCols <- length( stockAssessment$mpdPars ) + 1
    obj$mp$assess$mpdPars[ tRow,c(1:nCols) ] <- c( tStep=t, stockAssessment$mpdPars )
    names( obj$mp$assess$mpdPars )[c(1:nCols)] <- c( "tStep", names(stockAssessment$mpdPars ) )
    
    # Save the PDF pars.
    nCols <- length( mcmcPars ) + 1
    obj$mp$assess$pdfPars[ tRow,c(1:nCols) ] <- c( tStep=t, mcmcPars )
    names( obj$mp$assess$pdfPars )[c(1:nCols)] <- c( "tStep",names(mcmcPars) )

    # Save retrospective biomass, recruitment anomalies.
	  
    # ARK (12-Dec-11) Changed to 1:t.  Why?
    obj$mp$assess$Rt[ tRow, c(1:(t-2)) ] <- c( t, stockAssessment$lnOmega.lastFit )    
   
    # Retrospective statistics - fill next row of "stepwise fit" statistics.
    obj$mp$assess$retroExpBt[ tRow,c(1:(t+1)) ] <- c( t, stockAssessment$exploitBt,
                                                         stockAssessment$projExpBio )
    obj$mp$assess$retroExpBtS[ tRow,c(1:(t+1)) ] <- c( t, stockAssessment$exploitBtS,
                                                         stockAssessment$projExpBioS )
                                                         
    obj$mp$assess$retroSpawnBt[ tRow,c(1:(t+1)) ] <- c( t, stockAssessment$spawnBt,
                                                           stockAssessment$projSpawnBio )

    obj$mp$assess$retroFt[ tRow,c(1:(t)) ] <- c( t, stockAssessment$Ft)			 #RF_added
   
    nOmega <- length( stockAssessment$lnOmega.lastFit )    
    obj$mp$assess$retroRt[ tRow,c(1:(nOmega+1)) ] <- c( t, stockAssessment$lnOmega.lastFit )
 
    obj$mp$assess$pMod$lnOmega.lastFit <- stockAssessment$lnOmega.lastFit
    
  }     # If methodId=.PMOD    

  #----------------------------------------------------------------------------#
  #-- STEP 3b. Retrospective statistics for Adam Keizer 699 Project.           #
  #----------------------------------------------------------------------------#
  
  # ARK (03-Feb-13) Some documentation and musings for Adam...
  
  # obj$mp$assess$retroVals is a 3-D array with dimensions nRep * nT+2 * .NRETROSTATS
  # where nRep is the number of simulation replicates, nT is the maximum time
  # step, and .NRETROSTATS is the number of different retrospective statistics.
  # The second dimension is nT+2 to allow Column 1 to be the replicate ID and
  # column 2 to be the timestep indicator.
  
  # The performance statistics cannot be calculated post-simulation because
  # Adam wants to attempt a correction during a management procedure using the
  # retrospective statistics.  However Adam may wish to also compute diagnostics
  # relative to the operating model exploitable biomass, i.e., "the truth".
  # In the end, it might be more profitable to attempt to determine if a HCR
  # can compensate for restrospective bias with respect to policy outcomes.

  # ARK (01-Mar-13) If block added to wrap retroVals.  
  if ( methodId > 2 )
  {
    # Add the time step.
    for ( k in 1:dim(obj$mp$assess$retroVals)[3] )
      obj$mp$assess$retroVals[ tRow, 1, k ] <- t
  
    # Loop retroYears in the past to the current time step and fill retroVals
    # with retrospective statistics - currently limited to 5 types of stats.
    # The current year is assumed to be the reference year, thus the last year nT
    # is the conventional "reference year" in most treatments of this issue.

    # Retrospective statistic 1 - Sum of the Relative Error: Exploitable biomass.
  
    retroYears  <- obj$mp$assess$retroYears
    retroExpBt  <- obj$mp$assess$retroExpBt
    retroExpBtS <- obj$mp$assess$retroExpBtS
    retroFt     <- obj$mp$assess$retroFt		 # RF_added
  
    # For the current time step, compute the vector(s) of retrospective stats.
    # Remember these have to be computed over the time series, not just from tMP.

    # Need at least two assessments to calculate retrospective stats.  
    if ( t > tMP )
    {
      # Loop from first assessment up to current assessment time step.  
      for ( tRetro in tMP:(t-1) )
      {
        # This advances retroYears of column indices with t so that the same
        # number of years is used in each retrospective step.
      
        minRetroYr <- max( t-retroYears+1, 1 )
 
        # Get the indices of required columns (years) of retroVals, add 1 for tStep.
        idxRetro   <- c( minRetroYr:t ) + 1

        # This is the target time step, ranges from 1 to t-tMP+1.
        tRow   <- tRetro - tMP + 1
      
        # This is the reference time step, i.e., the current assessment.
        refRow <- t - tMP + 1
      
        # This is the annual retrospective stats by time step.  The final retrospective
        # statistic is the sum over all years in the replicate for sumRelErr.
      
        # Retrospective statistic 1 - values for Sum of the Relative Errors.
        #obj$mp$assess$retroVals[ tRow,idxRetro,1 ] <- (retroExpBt[ tRow,idxRetro ] - retroExpBt[ refRow,idxRetro ]) / retroExpBt[ refRow,idxRetro ]
	      obj$mp$assess$retroVals[ tRow,idxRetro,1 ] <- (retroExpBtS[ tRow,idxRetro ] - retroExpBtS[ refRow,idxRetro ]) / retroExpBtS[ refRow,idxRetro ] #FLAGRF -- made this a funcrion of survey expl. biomass

        # Retrospective statistic 2 - values for Parma's Mean Square Error.           
        #      obj$mp$assess$retroVals[ tRow,idxRetro,2 ] <- log( retroExpBt[ refRow,idxRetro ] )
        #           - log( retroExpBt[ tRow,idxRetro ])
      }
    }
  
    # Compute retrospective statistics from retrospective values for replicate.
  
    # Note: retroVals is of dimension nT-tMP+1 by nT+1 by .NRETROSTATS.  Thus, the
    # columns in slice retroVals[,,k] are c(2:(nT+1)) to avoid the tStep column.
  
    idx <- c( 2:(nT+1) )
  
    # Retrospective statistic 1 - Sum of the Relative Error.
    obj$mp$assess$retroStats[ 1 ] <- sum( obj$mp$assess$retroVals[tRow,idx,1], na.rm=TRUE )
  
    # Retrospective statistic 2 - Parma's mean square error.
    #  nVals <- sum( !is.na( obj$mp$assess$retroVals[tRow,idx,2] ) )
    #  obj$mp$assess$retroStats[ 2 ] <- sqrt( sum( obj$mp$assess$retroVals[tRow,idx,2], na.rm=TRUE )^2 / nVals )
  
    # Retrospective statistic 3 through 5 here...   
    
  }     # ENDIF methodId > 2, i.e., a population dynamics method.
  
  #----------------------------------------------------------------------------#
  #-- STEP 4. Extract estimated reference points for HCR.                    --#
  #----------------------------------------------------------------------------#

  # Control point update frequency - vector containing time step for the
  # estimates of control points that should be used for this time step. This
  # vector from .calcTimes is set such that tMP is set to 1.
  # Only applies to assessment models that can estimate control points. 
  
  idxCtlPts <- obj$mp$hcr$idxCtlPts
  
  # This is within function .updatePop, just in case you feel lost...
  
  # Moving average and Kalman filter methods do not have reference points from
  # the assessment method in the procedure, the ref pts come from the opMod.
  
  if ( (obj$mp$assess$methodId==.MOVAVG) | (obj$mp$assess$methodId==.KALMAN) )
  {
    # Stock Status Base.
    if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
      obj$mp$hcr$Bref[t] <- obj$opMod$ssbFmsy
    
    if ( obj$mp$hcr$statusBase == "statusBaseB0" )
      obj$mp$hcr$Bref[t] <- obj$opMod$B0    
    
    # Reference Removal Base.  
    if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
      obj$mp$hcr$remRate[t] <- obj$opMod$Fmsy
      
    if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
      obj$mp$hcr$remRate[t] <- obj$opMod$F01
      
    if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
      obj$mp$hcr$remRate[t] <- .getFx( obj$opMod, x=obj$mp$hcr$sprX)$Fx
      
    if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
      obj$mp$hcr$remRate[t] <- obj$mp$hcr$inputF
    
    # Historical control points.  
    if ( obj$mp$hcr$remRefBase == "rrBaseFt" )
    {  
      FtSyr <- obj$mp$hcr$FtStartYr
      FtNyr <- obj$mp$hcr$FtEndYr
      
      # SPC: this should be a simple get function call, e.g., getRemRate( Ft, method=Fmin )
      if ( obj$mp$hcr$histFtBase == "Fmin" )
      {
        obj$mp$hcr$remRate[t] <- min(obj$om$Ft[FtSyr:FtNyr])
      }
      
      if ( obj$mp$hcr$histFtBase == "Fmax" )
      {
        obj$mp$hcr$remRate[t] <- max(obj$om$Ft[FtSyr:FtNyr])   
      }
      
      if ( obj$mp$hcr$histFtBase == "Fmean" )
      {
        obj$mp$hcr$remRate[t]<-mean(obj$om$Ft[FtSyr:FtNyr])   
      }         
 
      if ( obj$mp$hcr$histFtBase == "Fquant" )
      {
        quant <- obj$mp$hcr$histFtBaseQuant
        obj$mp$hcr$remRate[t] <- quantile(obj$om$Ft[FtSyr:FtNyr],quant)[[1]]   
      }               
    }     # ENDIF remRefBase == "rrBaseFt"
  }     # ENDIF method is Kalman filter or Moving Average.
  else
  {
    # Method is NOT a smoother, i.e., it is a production model or catch-at-age model.
    
    statusBase   <- obj$mp$hcr$statusBase        # Base is Bmsy, Bo, or Bt (for historical ref points)
    statusSource <- obj$mp$hcr$statusSource      # From OM or MP?
    
    remRefBase   <- obj$mp$hcr$remRefBase        # Base is Fmsy, F01, or Historical?
    remRefSource <- obj$mp$hcr$remRefSource      # From OM or MP?

    tRow    <- t - obj$opMod$tMP + 1
    mpdPars <- obj$mp$assess$mpdPars
    
    #-- STOCK STATUS: Select the stock status base and assign to Bref[t].
    
    # Control points from the operating model.
    if ( statusSource == "statusSrceEquil" )
    {
      if ( statusBase == "statusBaseBmsy" )
        obj$mp$hcr$Bref[t] <- obj$opMod$ssbFmsy
      
      if ( statusBase == "statusBaseB0" )
        obj$mp$hcr$Bref[t] <- obj$opMod$B0
                
      # KRH (19-Aug-13) Added historical control points, RF modified to Bt.
      
      if ( statusBase == "statusBaseBt" )
      {
        # Extract start and end years for upper and lower bases
        lowerSyr <- obj$mp$hcr$lowBaseStartYr
        lowerNyr <- obj$mp$hcr$lowBaseEndYr
          
        upperSyr <- obj$mp$hcr$upperBaseStartYr
        upperNyr <- obj$mp$hcr$upperBaseEndYr
          
        # Calculate lower base for historical reference points .
        if ( obj$mp$hcr$histLowBase == "Bmin" )
        {
          obj$mp$hcr$lowerBref[t] <- min(obj$om$Bt[lowerSyr:lowerNyr])
        }
        
        if ( obj$mp$hcr$histLowBase == "Bmax" )
        {
          obj$mp$hcr$lowerBref[t] <- max(obj$om$Bt[lowerSyr:lowerNyr])   
        }
        
        if ( obj$mp$hcr$histLowBase == "Bmean" )
        {
          obj$mp$hcr$lowerBref[t] <- mean(obj$om$Bt[lowerSyr:lowerNyr])
        }
        
        if ( obj$mp$hcr$histLowBase == "Bquant" )
        {
          quant<-obj$mp$hcr$lowBaseQuant
          obj$mp$hcr$lowerBref[t] <- quantile(obj$om$Bt[lowerSyr:lowerNyr],quant)[[1]]
        }
        
        # Calculate upper base for historical reference points
        if ( obj$mp$hcr$histUpperBase == "Bmin" )
        {
          obj$mp$hcr$upperBref[t] <- min(obj$om$Bt[upperSyr:upperNyr])   
        }
        
        if ( obj$mp$hcr$histUpperBase == "Bmax" )
        {
          obj$mp$hcr$upperBref[t] <- max(obj$om$Bt[upperSyr:upperNyr])  
        }
        
        if ( obj$mp$hcr$histUpperBase == "Bmean" )
        {
          obj$mp$hcr$upperBref[t] <- mean(obj$om$Bt[upperSyr:upperNyr])
        }
        
        if ( obj$mp$hcr$histUpperBase == "Bquant" )
        {
          quant <- obj$mp$hcr$upperBaseQuant
          obj$mp$hcr$upperBref[t] <- quantile(obj$om$Bt[upperSyr:upperNyr],quant)[[1]]
        }
      }     # ENDIF statusBase=statusBaseBt.
    }     # ENDIF statusSource=statusSrceEquil
    
    # Control points estimated from the assessment method.
    else
    {
      
      # ARK (01-Sep-13)
      # Explanation of how idxCtlPts works: idxCtlPts is computed by .calcTimes.
      # It holds the time step indices for the control points to use in the HCR.
      # e.g., idxCtlPts could be c( 1, 1, 1, 4, 4, 4, 7, 7 ,7, 10, 10, 10, 13, 13, 13 ).
      # This is for a situation where there are 15 years of projection, with control
      # point updating every 3 years.  tRow is the current time step.  Thus, the
      # expression idxCtlPts[tRow] gives the row index of the control points to
      # use in the HCR.  So, as t goes from tMP, tMP+1, tMP+3, the value of
      # idxCtlPts is always 1.  Therefore, take the value in row 1 of the estimated
      # control points 3 times.  For tMP+4, tMP+5, tMP+5, take the value in the
      # 4th row of the estimated control points 3 times.

      if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
        obj$mp$hcr$Bref[t] <- mpdPars$ssbFmsy[ idxCtlPts[tRow] ]

      if ( obj$mp$hcr$statusBase == "statusBaseB0" )
        obj$mp$hcr$Bref[t] <- mpdPars$B0[ idxCtlPts[tRow] ]
          
      # KRH (19-Aug-13) Added historical control points, RF modified to spawnBt.
      if ( obj$mp$hcr$statusBase == "statusBaseBt" )
      {
        # Extract start and end years for upper and lower bases
        lowerSyr <- obj$mp$hcr$lowBaseStartYr
        lowerNyr <- obj$mp$hcr$lowBaseEndYr
          
        upperSyr <- obj$mp$hcr$upperBaseStartYr
        upperNyr <- obj$mp$hcr$upperBaseEndYr
          
        # Calculate lower base for historical reference points
          
        # NOTE from ARK - This historical code does not respond to the HCDR
        # idxCtlPts update frequency for control points.  Need to address to
        # allow updating at user-specified frequency.  See comments above.
          
        if ( obj$mp$hcr$histLowBase == "Bmin" )
        {
          obj$mp$hcr$lowerBref[t] <- min(stockAssessment$spawnBt[lowerSyr:lowerNyr])    
        }
          
        if ( obj$mp$hcr$histLowBase== "Bmax" )
        {
          obj$mp$hcr$lowerBref[t]<-max(stockAssessment$spawnBt[lowerSyr:lowerNyr])    
        }
          
        if ( obj$mp$hcr$histLowBase == "Bmean" )
        {
          obj$mp$hcr$lowerBref[t] <- mean(stockAssessment$spawnBt[lowerSyr:lowerNyr])
        }
          
        if ( obj$mp$hcr$histLowBase == "Bquant" )
        {
          quant <- obj$mp$hcr$lowBaseQuant
          obj$mp$hcr$lowerBref[t] <- quantile(stockAssessment$spawnBt[lowerSyr:lowerNyr],quant)[[1]]
        }
          
        # Calculate upper base for historical reference points
        if ( obj$mp$hcr$histUpperBase == "Bmin" )
        {
          obj$mp$hcr$upperBref[t] <- min(stockAssessment$spawnBt[upperSyr:upperNyr]) 
        }
          
        if ( obj$mp$hcr$histUpperBase == "Bmax" )
        {
          obj$mp$hcr$upperBref[t] <- max(stockAssessment$spawnBt[upperSyr:upperNyr]) 
        }
          
        if ( obj$mp$hcr$histUpperBase == "Bmean" )
        {
          obj$mp$hcr$upperBref[t] <- mean(stockAssessment$spawnBt[upperSyr:upperNyr])
        }
          
        if ( obj$mp$hcr$histUpperBase == "Bquant" )
        {
          quant <- obj$mp$hcr$upperBaseQuant
          obj$mp$hcr$upperBref[t] <- quantile(stockAssessment$spawnBt[upperSyr:upperNyr],quant)[[1]]
        }
      }     # ENDIF statusBase = statusBaseBt
    }     # ENDIF statusSource != statusSrceEquil
  
    #-- REFERENCE REMOVAL RATE: Select the RR and assign to remRate[t].
  
    # Reference removal rate from the operating model.
    if ( remRefSource == "rrSrceEquil" )
    {
      if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
        obj$mp$hcr$remRate[t] <- obj$opMod$Fmsy
      
      if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
        obj$mp$hcr$remRate[t] <- obj$opMod$F01
      
      # ARK (01-Sep-13) This has to be calculated from the OM for now, so it
      # is not subject to idxCtlPts as it does not change during the projection.
      # It could, but don't worry about it now.  But it could be different than
      # the value of sprX specified in the default OM refPtList, so calculate.
      if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
        obj$mp$hcr$remRate[t] <- .getFx( obj$opMod, x=obj$mp$hcr$sprX)$Fx
      
      if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
        obj$mp$hcr$remRate[t] <- obj$mp$hcr$inputF  

      # KRH (19-Aug-13) Added historical fishing rate control points.        
      if ( obj$mp$hcr$remRefBase == "rrBaseFt" )
      {
        FtSyr <- obj$mp$hcr$FtStartYr
        FtNyr <- obj$mp$hcr$FtEndYr
      
        if ( obj$mp$hcr$histFtBase == "Fmin" )
        {
          obj$mp$hcr$remRate[t] <- min(obj$om$Ft[FtSyr:FtNyr])
        }
        
        if ( obj$mp$hcr$histFtBase == "Fmax" )
        {
          obj$mp$hcr$remRate[t] <- max(obj$om$Ft[FtSyr:FtNyr])   
        }
        
        if ( obj$mp$hcr$histFtBase == "Fmean" )
        {
          obj$mp$hcr$remRate[t] <- mean(obj$om$Ft[FtSyr:FtNyr])   
        }
        
        if ( obj$mp$hcr$histFtBase == "Fquant" )
        {
          quant <- obj$mp$hcr$histFtBaseQuant
          obj$mp$hcr$remRate[t] <- quantile(obj$om$Ft[FtSyr:FtNyr],quant)[[1]]   
        }               
      }     # ENDIF remRefBase = "rrBaseFt"
    }     # ENDIF remRefSource == "rrSrceEquil"
    
    # Reference removal rate estimated from the assessment method.
    else
    {   
      if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
        obj$mp$hcr$remRate[t] <- mpdPars$Fmsy[ idxCtlPts[tRow] ]
      
      # SPC says: I would just use OM to save time by avoiding .calcRefPoints.
      if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
        obj$mp$hcr$remRate[t] <- obj$opMod$F01
      
      if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
      {
        # ARK (01-Sep-13) This has to be calculated from the OM for now, so it
        # is not subject to idxCtlPts as it does not change during the projection.
        # It could, but don't worry about it now.  But it could be different than
        # the value of sprX specified in the default OM refPtList, so calculate.
        proxy <- .getFx( obj$opMod, x=obj$mp$hcr$sprX)$Fx
        obj$mp$hcr$remRate[t] <- .getFx( obj$opMod, x=obj$mp$hcr$sprX)$Fx
      }
      
      if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
        obj$mp$hcr$remRate[t] <- obj$mp$hcr$inputF
      
      # KRH (19-Aug-13) Added historical fishing rate control points.   
      if ( obj$mp$hcr$remRefBase == "rrBaseFt" )
      {
        # Extract start and end years for calculations
        syr <- obj$mp$hcr$FtStartYr
        nyr <- obj$mp$hcr$FtEndYr
          
        if ( obj$mp$hcr$histFtBase == "Fmean" )
           obj$mp$hcr$remRate[t] <- mean(stockAssessment$Ft[syr:nyr])
           
        if ( obj$mp$hcr$histFtBase == "Fmin" )
          obj$mp$hcr$remRate[t] <- min(stockAssessment$Ft[syr:nyr])
        
        if ( obj$mp$hcr$histFtBase == "Fmax" )
          obj$mp$hcr$remRate[t] <- max(stockAssessment$Ft[syr:nyr])
             
        if ( obj$mp$hcr$histFtBase == "Fquant" )
        {
          quant <- obj$mp$hcr$histFtBaseQuant
          obj$mp$hcr$remRate[t] <- quantile(stockAssessment$Ft[syr:nyr],quant)[[1]]
        }
        
      }     # ENDIF remRefBase = "rrBaseFt"
    }     # ENDIF remRefSource != "rrSrceEquil"
 
  }     # ENDIF not .MOVAVG or .KALMAN.
  
  # Update limit and upper stock reference points

  # KRH (19-Aug-13) Added historical update of lower and upper control points.
  if (obj$mp$hcr$statusBase == "statusBaseBt")
  {
    obj$mp$hcr$lowerBound[t] <- obj$mp$hcr$lowerBref[t] * obj$mp$hcr$lowerBoundMult
    obj$mp$hcr$upperBound[t] <- obj$mp$hcr$upperBref[t] * obj$mp$hcr$upperBoundMult
  }
  else
  {
    obj$mp$hcr$lowerBound[t] <- obj$mp$hcr$Bref[t] * obj$mp$hcr$lowerBoundMult
    obj$mp$hcr$upperBound[t] <- obj$mp$hcr$Bref[t] * obj$mp$hcr$upperBoundMult    
  }  
    
  #----------------------------------------------------------------------------#
  #-- STEP 5. Set catch limit for this time step using calcHCR               --#
  #----------------------------------------------------------------------------#
  
  # Extract most recent biomass estimate from t-1.
  # Note: .CAAMOD uses estimated exploitable Bt, based on estimated selectivity.
  #       However sel == mat in opMod, but in general will be different.

  # What is stockAssessment$biomass?
  # Projected T+1 exploitable biomass for both prod and CAA models
  projExpBio   <- stockAssessment$projExpBio
  projSpawnBio <- stockAssessment$projSpawnBio
  
  # RF (28-Aug-13) Estimated fishing mortality from last time step.
  Ft <- stockAssessment$Ft[t-1]

  # Catch from last time step.
  lastCatch <- obj$om$Dt[t-1]

  # Build harvest control rule list object.
  hcrList              <- obj$mp$hcr
  
  # This added to allow check for whether discrete or Baranov catch equation.
  hcrList$methodId     <- obj$mp$assess$methodId
  
  hcrList$t            <- t  
  hcrList$lastCatch    <- lastCatch
  
  # Take the estimated M from mpdPars for the time step.
  hcrList$M            <- obj$mp$assess$mpdPars[ tRow, "M" ]
   
  hcrList$maxCatch     <- max( obj$om$Dt[ 1:(obj$opMod$tMP-1) ] )

  # Projected expl. biomass estimates provided to HCR: (1) Empirical, and (2) Model.
  if ( (obj$mp$assess$methodId==.MOVAVG) | (obj$mp$assess$methodId==.KALMAN) )
  {
    hcrList$biomass   <- stockAssessment$exploitBt
    hcrList$ccBiomass <- obj$mp$data$It[1:(t-1)]
    hcrList$Dt        <- obj$om$Dt[1:(t-1)]
  }
  else
  {
    hcrList$projExpBio   <- projExpBio
    hcrList$projSpawnBio <- projSpawnBio
  }

  #----------------------------------------------------------------------------#
  #-- STEP 6: Apply Harvest Control Rule                                     --#
  #----------------------------------------------------------------------------#

  # hcrData: dataframe containing all variables needed to implement the HCR
  #   (e.g., current biomass, LSR, USR, FMSY, projection outputs for 9-zone rule)
  #   This dataframe will have only one row if MLE estimates from assessment used,
  #   and multiple rows if MCMC estimates are used (1 row for each MCMC trial) 
  # hcrList: list containing all parameters needed to implement HCR from GUI.
  #browser()
  # HCRs based on Empirical methods
  if ( (obj$mp$assess$methodId==.MOVAVG) | (obj$mp$assess$methodId==.KALMAN) ) 
  {
    if ( obj$gui$mpLabel == "ccRule" )
    {
      targetHarv <- calcHCR_ccRule( hcrList )
    }
    else
    {
      targetHarv <- calcHCRsmoother( hcrList )
    }
  }
  else  # HCRs based on Model-based methods
  {
    methodOn <- as.logical( attrMethod$methodOn[t] ) 
    # If using method this year, apply HCR based on current hcr components.
    if ( methodOn )
    {
      targetHarv <- calcHCRpopDyn( hcrList, hcrData=mcmcOut )
    }
    
    # Apply on the following if not using method this year (e.g., no survey or model update).
    else 
    {
      # Use a forecast biomass from previous.  The "forecast" is from the
      # control file via the "Interim model" checkbox on the HCR GUI.
      if ( obj$mp$hcr$forecast == TRUE )
      {
        targetHarv <- calcHCRpopDyn( hcrList, hcrData=mcmcOut )      
      }
      
      # Else use last year's catch limit, i.e., catch does not change.
      else
      {
        tMethod <- attrMethod$tMethod 
        targetHarv <- list( catchLimit=obj$om$D[ tMethod[tRow] ],
                            targetF=NA, eBioAdj=NA )
      }
    }
  }
  # ARK (03-Sep-13) Changing philosophy of what was .DEADFLAG.  The current
  # implementation causes fishery to stay closed for entire projection period.
  # This was originally implemented with reference to the OM biomass to try to
  # keep the stock from going negative, and was changed at some point to projExpBio.
  
  # However, this step actually evaluates whether the perception is that the
  # catch arises from the HCR is going to take 95% of the projected exploitable
  # biomass.  Call it .FISHERYCLOSED which will apply for one year.
  
  # Actually, can this ever be triggered?  projExpBio is fed into the control rule...
  # Yes! Because F is applied, large F~3 can generate U greater than 0.95.
  tmpCatch <- c( targetHarv$catchLimit,0.95*projExpBio )
  choice   <- which.min(tmpCatch)
  
  if ( choice==2 )
    .FISHERYCLOSED <<- TRUE  
  
  # Set catch limit for the year
  obj$om$Dt[t] <- 0
  if ( !.FISHERYCLOSED & !.DEADFLAG )
  {
    obj$om$Dt[t] <- min(tmpCatch)
  }
  obj$mp$assess$runStatus[ tRow, "deadFlag" ]      <- .DEADFLAG
  obj$mp$assess$runStatus[ tRow, "fisheryClosed" ] <- .FISHERYCLOSED
  .FISHERYCLOSED <<- FALSE 
  .DEADFLAG      <<-FALSE  

  # Store various assessment estimates and variables used for this year's HCR.
  
  # Store biomass estimates. 
  if ( (obj$mp$assess$methodId==.MOVAVG) | (obj$mp$assess$methodId==.KALMAN) )
  {
    # Empirical methods - MA and KF, exploitable and spawning are the same and 
    # no projected exploitable and spawning biomasses either.  Always terminal.
    obj$mp$assess$exploitBt[t] <- stockAssessment$exploitBt
    obj$mp$assess$spawnBt[t]   <- stockAssessment$exploitBt
  }
  else
  {  
    # Model-based methods - Prod, DD, and CAA both estimate and project biomasses.
    # Prod assumes Exp==SSB, while CAA is the only one that uses distinct Exp. and SSB.
    obj$mp$assess$exploitBt[t] <- projExpBio
    obj$mp$assess$spawnBt[t]   <- projSpawnBio
    obj$mp$assess$Ft[t]        <- Ft	 
  }
  
  # The target fishing mortality - used in 1.-exp(-F) for empirical 
  # and Prod methods and Baranov equation for CAA
  obj$mp$hcr$targetFt[t]       <- targetHarv$targetF

  # Exploitable biomass adjustment for precautionary rules 
  if ( !obj$mp$hcr$hcrType=="declineRisk" & hcrList$paAdj==TRUE )
    obj$mp$hcr$eBioAdj[t]      <- targetHarv$eBioAdj
                                               
  # Decline-risk quantities
  if ( obj$mp$hcr$hcrType=="declineRisk" )
  {
    obj$mp$assess$ssb[t]       <- targetHarv$ssb        # MCMC spawning stock bio.
    obj$mp$assess$trendVal[t]  <- targetHarv$trendVal   # 1=inc, 2=stable, 3=dec.
    obj$mp$assess$trendBio[t]  <- targetHarv$trendBio   # recent slope.
    obj$mp$hcr$pStar[t]        <- targetHarv$pStar      # Obs. prob. decline.
  }

  # 6. Update the operating model population, applying the catch.
  # SPC: should skip all the assessment stuff above if mpLabel=="No Fish"...no
  #      reason to do stock assessments if F=0 anyway. This can save hours of
  #      simulation time.
  obj <- asOMod( obj,t )

  # ARK (05-Sep-13) I'm not sure if this is where this should go, but trying to
  # let OM tell us when spawning stock is "dead" (<0.05*B0).
  
  # SPC: the problem with DEADFLAG is that the stock will bounce back and look
  #      terrific by nT. Obvious effect will be low catch, but still should consider
  #      implementing some depensation to better reflect the risk of stocks not
  #      recovering. There is some literature reviewing stock recovery from low
  #      abundance - usually they recover sometimes slow, sometimes fast, and
  #      sometimes not (e.g., SGSL cod, northern cod). It might be worthwhile 
  #      investigating ways of linking M to stock size as an option to ranWalk.
  #      Could extend existing meta-analyses to look for changes in M across a wide
  #      range of collapsed stocks.
  #if ( obj$om$Bt[t] < (0.05*obj$opMod$B0) )
  #{
  #  .DEADFLAG <<- TRUE
  #}
  
  return( obj )
}     # END function .updatePop


#------------------------------------------------------------------------------#
#--                   Harvest Control Rule Functions                         --#
#------------------------------------------------------------------------------#

# calcHCRpopDyn
# Purpose:        Wrapper function for harvest control rule to generate the quota; 
#                   redirects to one of three types of rule (Constant F, 3 Zone, 9 Zone)    
# Parameters:     hcrData = dataframe containing all variables needed to implement the HCR
#                 (e.g., current biomass, LB, UB, projection outputs for 9-zone rule)
#                 This dataframe will have only one row if MLE estimates from assessment used,
#                 and multiple rows if MCMC estimates are used (1 row for each MCMC trial) 
#                 hcrList=list containing all parameters needed to implement HCR
# Returns:        the quota (=catch) and target F
# Source:         K. Holt, A.R. Kronlund
calcHCRpopDyn <- function( hcrList, hcrData=NULL )
{
  result <- NULL
  
  # Constant catch option. 
  if ( hcrList$hcrType == "constantC" )
  {
    result <- .calcHCRconstantC( hcrList )
  }
  
  # Constant-F option.
  if ( hcrList$hcrType == "constantF" )  
  {
    # If using MLEs, then only need to compute catch limit once.
    # If hcrData has only 1 row, output is MLE: call calcHCRconstantF once.
    if ( nrow( hcrData ) == 1 ) 
    {
      result     <- .calcHCRconstantF( 1,hcrData,hcrList )
      targetF    <- result[ "targetF" ]
      catchLimit <- result[ "catchLimit" ]
      eBioAdj    <- result[ "eBioAdj" ]
      result <- list( catchLimit=catchLimit, targetF=targetF, eBioAdj=eBioAdj )        
    }
    
    # If hcrData has > 1 row, output is MCMC: call calcHCRconstantF for each row
    # to produce the posterior distribution of catch limit.
    if ( nrow( hcrData ) > 1 )
    {
      nTrials <- nrow( hcrData ) # No. of MCMC draws in hcrData
 
      # Compute 
      result <- lapply( 1:nTrials, .calcHCRconstantF, hcrData=hcrData, hcrList=hcrList )
      
      # This comes back as a list(catchLimit, targetF, eBioAdj) - stack the vectors
      # and then bind as columns to original hcrData list.
      result <- cbind( hcrData, as.data.frame(do.call(rbind,result) ) )      
      
      # Take posterior means, although this should be done based on quantiles
      targetF    <- mean( result[ ,"targetF" ] )
      catchLimit <- mean( result[ ,"catchLimit" ] )
      eBioAdj    <- mean( result[ ,"eBioAdj" ] )
      result <- list( catchLimit=catchLimit, targetF=targetF, eBioAdj=eBioAdj )     
    }
  }     # END constantF branch.
  
  if ( hcrList$hcrType == "variableF" )  
  {
    # Test if output is MCMC or MPD, if MPD call calcHCRvariableF function once.
    if ( nrow( hcrData ) == 1 ) 
    {
      result     <- .calcHCRvariableF( 1,hcrData,hcrList )
      
      targetF    <- result[ "targetF" ] # this is not being done right!
      catchLimit <- result[ "catchLimit" ]
      eBioAdj    <- result[ "eBioAdj" ]
      result <- list( catchLimit=catchLimit, targetF=targetF, eBioAdj=eBioAdj )      
    }
    
    # If MCMC, call .calcHCRvariableF function for every MCMC trial.
    if ( nrow( hcrData ) > 1 )
    {
      nTrials <- nrow( hcrData )
      
      # Note: could use snow for multiple processors on this apply func --- K.Holt
      result <- lapply( 1:nTrials, .calcHCRvariableF, hcrData=hcrData, hcrList=hcrList )
     
      # This comes back as a list with 3-element vectors - stack them.
      result     <- cbind( hcrData, as.data.frame(do.call(rbind,result) ) )
      
      targetF    <- mean( result[,"targetF"] )
      catchLimit <- mean( result[,"catchLimit"] )
      eBioAdj    <- mean( result[,"eBioAdj"] )
      
      result <- list( catchLimit=catchLimit, targetF=targetF, eBioAdj=eBioAdj )
    }
  }     # END variableF branch. 
 
  if ( hcrList$hcrType == "declineRisk" )  
  {
    # Test if output is MCMC or MLE, if MLE call .calcHCRdeclineRisk function once.
    if ( nrow( hcrData ) == 1 ) 
    {
      cat( "\nERROR (calcHCR): No MCMC results, cannot apply Decline Risk HCR.\n" )
      stop()
    }
    
    # IF MCMC, call .calcHCRdeclineRisk function for every MCMC trial.
    if ( nrow(hcrData) > 1 ) 
    {
      result     <- .calcHCRdeclineRisk( hcrData,hcrList )
      targetF    <- NA
      ssb        <- result["ssb"]
      catchLimit <- result["catchLimit"]
      trendBio   <- result["trendBio"]
      trendVal   <- result["trendVal"]
      pStar      <- result["pStar"]
      
      result <- list( catchLimit=catchLimit, targetF=targetF, ssb=ssb,
                      trendBio=trendBio, trendVal=trendVal, pStar=pStar )      
    }
  }     # END declineRisk branch.
  result
}     # END function calcHCRpopDyn  


# .calcHCRsmoother       
# Purpose:        harvest control rule to generate the quota
# Parameters:     obj=list containing all variables and parameters
#                 necessary to compute the quota
# Returns:        the quota=catch
# Source:         S.P. Cox
calcHCRsmoother <- function( hcrList )
{
  # This rule is used for the Moving Average and Kalman filter procedures, and
  # uses the operating model biological reference points as the operational
  # control points.
  # Extract assessment quantities from harvest control rule list.
  biomass    <- hcrList$biomass
  lastCatch  <- hcrList$lastCatch 
  M          <- hcrList$M

  # Extract fixed members of input harvest control rule list
  t          <- hcrList$t
  tMP        <- hcrList$tMP
  lastCatch  <- hcrList$lastCatch      # Catch in last year of projection.
  lambda1    <- hcrList$lambda1        # Weight factor on TAC[t-1].
  remRate    <- hcrList$remRate[t]     # Instantaneous fishing mortality rate.
  lowerBound <- hcrList$lowerBound[t]  # Lower HCR bound, where remRate=0
  upperBound <- hcrList$upperBound[t]  # Upper HCR bound
  
  # Constant catch
  if ( hcrList$hcrType == "constantC" )
  {
    result  <- lambda1*lastCatch + (1.0-lambda1)*hcrList$constCatch
    remRate <- -log(result/biomass)
  }
  # Constant-F
  else if ( hcrList$hcrType == "constantF" )
  {
    tmp <- remRate
    if ( tmp < 0 )
      tmp <- 0
    remRate <- ifelse( biomass < upperBound, tmp, remRate  )
    result  <- lambda1 * lastCatch + (1.0-lambda1)*(1.-exp(-remRate))*biomass
  }
  # Variable-F
  else if ( hcrList$hcrType == "variableF" )
  {
    tmp     <- remRate*(biomass-lowerBound)/(upperBound-lowerBound)
    if ( tmp < 0 )
      tmp <- 0
    remRate <- ifelse( biomass < upperBound, tmp, remRate  )
    result <- lambda1 * lastCatch + (1.0-lambda1)*(1.-exp(-remRate))*biomass
  }
  else
    cat( "\nERROR (calcHCRsmoother) Invalid hcrType for assessment method\n" )
  
  result <- list( catchLimit=max( 0, result ), targetF=remRate, eBioAdj=NA )
  result
}     # END function calcHCRsmoother


# .calcHCRconstantC
# Purpose:        harvest control rule for constant catch strategy with, i.e.,
#                 a rule without feedback.
# Parameters:     hcrData = dataframe containing all variables needed to implement constant F HCR
#                    (e.g., current biomass, FMSY, sigEBio)
#                     This dataframe will have only one row if MLE estimates from assessment used,
#                     and multiple rows if MCMC estimates are used (1 row for each MCMC trial) 
#                 hcrList=list containing all parameters needed to implement HCR
# Returns:        the quota (=catch).
# Source:        K.Holt (modified to allow phasing in of control rule via lambda1 by R. Forrest Aug 26 2013)
.calcHCRconstantC <- function( hcrList )
{
  
  #catchLimit <- hcrList$constCatch #RF_TESTING commented out this line
 catchLimit <-  hcrList$lambda1*hcrList$lastCatch + (1.0-hcrList$lambda1)*hcrList$constCatch # #RF_TESTING added this line
 
  catchLimit <- max( 0., catchLimit )
  result <- c( catchLimit=catchLimit )
  result
}     # END function .calcHCRconstantC



# .calcHCRconstantF
# Purpose:        harvest control rule for constant F strategy with optional risk adjustment.
# Parameters:     hcrData = dataframe containing all variables needed to implement constant F HCR
#                    (e.g., current biomass, FMSY, sigEBio)
#                     This dataframe will have only one row if MLE estimates from assessment used,
#                     and multiple rows if MCMC estimates are used (1 row for each MCMC trial) 
#                 hcrList=list containing all parameters needed to implement HCR
# Returns:        the quota (=catch) and target F
# Source:       SPC (modified to allow phasing in of control rule via lambda1 by R. Forrest Aug 26 2013)
.calcHCRconstantF <- function( i,hcrData,hcrList )
{
  projExpBio <- hcrData$projExpBio[i]
  M          <- hcrData$M[i]
  
  maxRemRate <- hcrList$remRate[hcrList$t] 
  remRate    <- max( 0,maxRemRate ) 
  
  # Apply risk-adjustment to biomass estimate - need to code Hessian if MLE.
  eBioAdj <- NA
  if ( hcrList$paAdj==TRUE )
  {
    logProjExpBio   <- log( hcrData$projExpBio )
    sigmaExpBio <- sqrt( var( logExpBio ) )
    cvExpBio    <- sigmaExpBio / mean( logExpBio )

    # Adjust the biomass according to uncertainty.
    eBioAdj    <- exp( -cvExpBio^2 / 2.0 )
    projExpBio <- projExpBio * eBioAdj
  }
  
  # SPC (5-Mar-2013): make M=NULL for Prod assessment and compute discrete 
  # catch equation. if M is given, then use Baranov
  
  # ARK (14-Sep-13)
  # Methods that do not estimate M, should not use the catch equation. May want
  # to consider changing this to use methodId rather than checking for null M.
  
  # So far .CAAMOD is the only assessment method that estimates M.
  if ( hcrList$methodId!=.CAAMOD )
  {
    # Discrete catch equation.
    catchLimit <- ( 1.0 - exp(-remRate) ) * projExpBio
  }
  else
  {
    # Baranov catch equation.
    catchLimit <- ( 1.0 - exp(-M-remRate) ) * projExpBio*remRate / ( M + remRate )
  }  
      
  catchLimit <- ifelse( is.null(M),
                        (1.-exp(-remRate))*projExpBio,
                        ( 1.-exp(-M-remRate) ) * projExpBio*remRate / ( M + remRate )
                       )
  catchLimit <- hcrList$lambda1 * hcrList$lastCatch+ (1.0-hcrList$lambda1)*catchLimit  #RF_TESTING
  catchLimit <- max( 0., catchLimit )

  result <- c( catchLimit=catchLimit, targetF=remRate, eBioAdj=eBioAdj )
  result
}     # END function .calcHCRconstantF


# .calcHCRvariableF
# Purpose:        harvest control rule to generate the quota using DFO SFF 3-zone rule (variable F)
# Parameters:     hcrData = dataframe containing all variables needed to implement variable F HCR
#                    (e.g., current biomass, FMSY, sigEBio, LRP, USR)
#                     This dataframe will have only one row if MLE estimates from assessment used,
#                     and multiple rows if MCMC estimates are used (1 row for each MCMC trial) 
#                 hcrList=list containing all parameters needed to implement HCR
# Returns:        the quota (=catch) and target F
# Source:        S.P. Cox, with modifications by K.Holt, A.R. Kronlund. (modified to allow phasing in of control rule via lambda1 by R. Forrest Aug 26 2013)
.calcHCRvariableF <- function(i,hcrData,hcrList)
{
  t            <- hcrList$t
  projExpBio   <- hcrData$projExpBio[i]
  projSpawnBio <- hcrData$projSpawnBio[i]
  M            <- hcrData$M[i]
 
  maxRemRate   <- hcrList$remRate[t]*hcrList$Fmult  
  remRate      <- max( 0,maxRemRate )
  remRate      <- remRate
  
  
  limitB <- hcrList$lowerBound[ t ]
  upperB <- hcrList$upperBound[ t ]
  
  # Apply risk-adjustment to biomass estimate - need to code Hessian if MLE.
  # Can't do paAdj if no MCMC.
  eBioAdj <- 1
  if ( hcrList$paAdj==TRUE & nrow(hcrData) > 1 )
  {
    sdExpBio <- sqrt( var( hcrData$projExpBio ) )
    cvExpBio <- sdExpBio / mean( hcrData$projExpBio )
    
    # Adjust the biomass according to uncertainty.
    eBioAdj <- exp( -cvExpBio^2 / 2.0 )
  }
  projExpBio <- projExpBio * eBioAdj
  # SPC: I think what will happen here is that MLE and MCMC w/ adj will
  #      have nearly identical outcomes. If the above adjustment is applied
  #      to mcmc mean of projExpBio, then result is median.
  # Adjusting the MLE in similar way would have actual PA effect of
  # reducing catch limit below point estimate.

  tmp  <- maxRemRate * (projSpawnBio-limitB) / (upperB - limitB)   
  if ( tmp < 0 )
    tmp <- 0    
  remRate <- ifelse( projSpawnBio < upperB, tmp, maxRemRate  ) 
  
  # So far .CAAMOD is the only assessment method that estimates M.
  if ( hcrList$methodId!=.CAAMOD )
  {
    # Discrete catch equation.
    catchLimit <- ( 1.0 - exp(-remRate) ) * projExpBio
  }
  else
  {
    # Baranov catch equation.
    catchLimit <- ( 1.0 - exp(-M-remRate) ) * projExpBio*remRate / ( M + remRate )
  }
 
  # Weight current catch with some fraction of previous catch.  This will smooth
  # the catch stream but at the cost of lag effects.  
  catchLimit <- hcrList$lambda1 * hcrList$lastCatch + (1.0-hcrList$lambda1)*catchLimit
 
  catchLimit <- max( 0., catchLimit )
  result     <- c( catchLimit=catchLimit, targetF=remRate, eBioAdj=eBioAdj )
  
  result
}     # END function .calcHCRvariableF


.calcAcceptProb <- function( stockStatus, trendVal, pDecline,
                             lowerLimit, upperLimit, B0 )
{
  # Input:
  #
  # stockStatus is the stock status (depletion scale).
  # trendVal is the trend value (1,2,3) or increasing, stable, decreasing.
  # pDecline is the matrix (18 numbers!) of acceptable probability
  #          of decline in each zone (6 columns) by trend type (3 rows).
  # lowerLimit is the lower stock status limit (depletion scale).
  # upperLimit is the upper stock status limit (depletion scale).
  #
  # Returns:
  #
  # val is a list with components x (stockStatus), and y (interpolated
  # acceptable probablity of decline).
  
  # NOTE 1: This function uses the R function "approx" for linear interpolation.
  # NOTE 2: Arbitrary upper X-axis bound set dynamically as multiple of previous
  #         catch or MSY if previous catch==0.
  
  x   <- c( 0,lowerLimit, lowerLimit, upperLimit, upperLimit, B0, 3*B0 )  
  y   <- c( pDecline[ trendVal, ], pDecline[ trendVal,ncol(pDecline) ] )
  
  val <- approx( x, y, xout=stockStatus, ties=max )
  val
}     # END function .calcAcceptProb.

# .calcHCRdeclineRisk
# Purpose:        9-zone feed-forward harvest control rule
# Parameters:     hcrData = dataframe containing all variables needed to implement decline risk HCR
#                    (e.g., current biomass, LRP, USR)
#                     This dataframe will always have multiple rows (1 row for each MCMC trial) 
#                     because it cannot be implemented with MLE estimates.  MCMC results needed to 
#                     calculate prohabiliy of future declines
#                 hcrList=list containing all parameters needed to implement HCR
# Returns:        the quota (=catch)
# Source:         A.R. Kronlund, with modifications by K.Holt
#
# Input:
#
# hcrData is the (augmented) mcmc output dataframe:
#   $Q is a vector of quota levels.
#   $Declined is a (0,1) indicator of whether the projected biomass declined.
#   $trendBio is the recent biomass trend based in nTrend points.
#   $BT is the terminal spawning biomass (at nT).
#   $projExpBio is the projected exploitable biomass (?)
#   $termDep is the terminal depletion (at nT)
#
# hcrList is the list of parameters required to specify the nine-zone rule:
#   $nTrend is the number of points in the recent biomass trend calculation.
#   $nProj  is the number of projection years.
#   $quota  is the vector of quota levels.
#   $trendPercentiles is a vector of pecentile bounds for (increasing,
#     stable, decreasing) assessment.
#   $pDecline is the matrix (18 numbers!) of acceptable probability
#     of decline in each zone (6 columns) by trend type (3 rows).
#   $lowerLimit is the lower limit of the hcr (depletion scale).
#   $upperLimit is the upper limit of the hcr (depletion scale).
  
# Returns:
#
# result, a list of results:
#   $hcrList is the input hcrList.
#   $xySplineFun is the y~x spline function.
#   $yxSplineFun is the x~y spline function.
#   $trendVal is the trend indicator (1,2,3), i.e., increasing, stable, decreasing.
#   $acceptProb is the acceptable probablity of decline.
#   $catch is the catch.
#
.calcHCRdeclineRisk <- function( hcrData, hcrList )
{
  
  getPdecline <- function( obj )
  {
    pDecline <- matrix( c( obj$Inc1a, obj$Inc1b, obj$Inc2a,
                           obj$Inc2b, obj$Inc3a, obj$Inc3b,
                           obj$Stable1a, obj$Stable1b, obj$Stable2a,
                           obj$Stable2b, obj$Stable3a, obj$Stable3b,
                           obj$Dec1a, obj$Dec1b, obj$Dec2a,
                           obj$Dec2b, obj$Dec3a, obj$Dec3b ), nrow=3, ncol=6,
                           byrow=TRUE )
                           
    pDecline
  }     # END function getPdecline

  # Step 1: Extract (x,y) for spline function of decline probability against quota level.
  
  x   <- unique( hcrData$Q )    # Unique levels of trial quotas.
  y   <- numeric( length(x) )   # Proportion of declines for quota level i.
  tmp <- numeric( length(x) )   # Vector of (0,1) decline indicators for quota level i.

  upperQuota <- max( x )
  
  for( i in 1:length(x) )
  {
    tmp  <- hcrData$Declined[ hcrData$Q==x[i] ]
    y[i] <- sum(tmp) / length(tmp)
  }
  
  # y~x spline to determine acceptable probability.
  xySplineFun <- splinefun( x=x, y=y )
  
  # x~y spline to take input acceptable probablity of decline and determine catch.
  yxSplineFun <- splinefun( x=y, y=x )
 
  # Step 2: Determine recent spawning biomass trend taken over recent nTrend points.
  #
  # hcrData contains nMCMC draws for each quota level where the recent trend in
  # biomass will vary across the draws.  But the vector of trend values is the
  # same for each quota level, i.e., the vector of nMCMC draws.  Therefore a
  # representative value could be the mean across the draws for a given quota
  # level.
  # Also, we don't need to have the entire hcrData$trendBio for the quantiles,
  # just a given level, say Q[1]. 

  # x holds the unique values of quota levels Q, so x[1] is the first quota level.
  idx       <- hcrData$Q == x[1]  
  trendValL <- quantile( hcrData$trendBio[idx],prob=hcrList$lowerTrendPct )
  trendValU <- quantile( hcrData$trendBio[idx],prob=hcrList$upperTrendPct )
  
  meanTrend <- mean( hcrData$trendBio[idx] )
  
  # SPC (21-March-2013): These trends are now consistent with the signs
  # Check whether slope of 0 falls within the range of the trend percentiles.
  trendVal <- 2              # Stable by default.
  if ( trendValL > 0.0 )
    trendVal <- 1             # Increasing.
  if ( trendValL < 0.0 & trendValU < 0.0 )
    trendVal <- 3             # Decreasing.
  
  # Step 3: Determine linear interpolation of acceptable pDecline within zones
  # based on current stock status at nT, trend, lower and upper limits.

  # ARK (01-Dec-11) *** Should this be based on mean of terminal spawning
  # biomass, not projected as Sean's code fragments suggest?
  
  # This is taking the mean over nMCMC trials, note that each trial results in
  # nLevel projections at the various quota levels.  The terminal spawning
  # biomass is the same for the entire trial (each of which has nLevel rows).
  
  # The problem with taking the projected exploitable biomass is that it differs
  # among the nLevel projections at each possible quota level.
  
  # This is using the MCMC estimate of current spawning biomass, which is the
  # same regardless of the quota level so use the first quota level
  
  # SPC (18-Dec-12): Is the above question now resolved?  ARK: No - Discuss!!!!
  
  # Stock status identifiers
  estB0       <- mean( hcrData$B0[idx] )
  #stockStatus <- mean( hcrData$projSpawnBio[idx] )
  stockStatus <- hcrList$projSpawnBio
  limitB <- hcrList$lowerBound[ hcrList$t ]  
  upperB <- hcrList$upperBound[ hcrList$t ]
  
  # Matrix of parameters to determine pDecline
  pDecline <- getPdecline( hcrList )
  
  # Acceptable probability calculation
  val <- .calcAcceptProb( stockStatus, trendVal, pDecline, limitB, upperB, estB0 )
  pStar <- val$y
  
  # Step 4: Obtain the catch from the spline of probablity of decline as a
  #         function of quota level.
  catchLimit <- try( yxSplineFun( x=pStar ), silent=F )
  if( class(catchLimit)=="try-error" ) 
  {
    cat("catchLimit error - setting catch = -1")
    catchLimit <- -1.
  }
  catchLimit <- max( 0., catchLimit )
  
  # ARK (02-Feb-13) Added to trap extrapolation beyond upperQuota.
  catchLimit <- min( catchLimit, upperQuota )
  
  result <- c( catchLimit=catchLimit, targetF=NA, ssb=stockStatus,
               trendBio=meanTrend, trendVal=trendVal, pStar=pStar )
  result
}     # END function .calcHCRdeclineRisk

#------------------------------------------------------------------------------#
#- Catch-at-age Helper functions for reference point processing.              -#
#------------------------------------------------------------------------------#

.caaModWriteCtlPtsPinDat <- function( x, nAges=25, fmsy=0.2 )
{
  # x     : a vector of parameters.
  # nAges : the maximum number of age classes.
  # fmsy  : initial guess at Fmsy.
  
  # Makes a data file for refptsca.tpl, then calls it.  Here's the DATA_SECTION:
  # DATA_SECTION
  # // read one line of mcmc output from assessCA
  # init_adstring dataFileName; 
  # init_int nAges;
  # init_number B0;
  # init_number R0;
  # init_number rSteepness;
  # init_number rec_a;
  # init_number rec_b;
  # init_number M;
  # init_number aSel50;
  # init_number aSel95;
  # init_number sigmaR;
  # init_number rho;
  # init_number aMat50;
  # init_number aMat95;
  # init_number Linf;
  # init_number L1;
  # init_number vonK;
  # init_number c1;
  # init_number c2;
  # init_number sigmaL;
  # init_number tau;
  # init_number spawnBiomass;
  # init_number eBio;
  # init_number termDep;

  # These are specific to the catch-at-age model.  
  exeName <- "refptsca"
  
  inputVars <- c( "B0","R0","rSteepness","rec.a","rec.b","M","aSel50","aSel95",
                  "sigmaR","rho","aMat50","aMat95","Linf","L1","vonK",
                  "c1","c2","sigmaL","tau",
                  "spawnBio","projSpawnBio","projExpBio","termDep" )
   
  if ( is.vector( x ) )
  {
    nDraws <- 1
    fileList <- exeName
    
     tmp <- as.numeric( c( nAges=nAges,x[ inputVars ] ) )

    # Write the *.dat file.
    dataFileName <- paste( fileList,".dat",sep="" )
    cat( file=dataFileName, dataFileName, "\n" )
    cat( file=dataFileName, tmp, "\n", append=TRUE )
  
    # Write the *.pin file to speed up convergence. 
    
    # ARK (03-Jun-13) This was log(fmsy), but refptsca inputs FMSY.
    pinFileName <- paste( fileList,".pin",sep="" )  
    cat( file=pinFileName, fmsy, "\n" )
  }
  else
  {
    # This could be parallelized...
    nDraws <- nrow( x )
    fileList <- paste( exeName,c(1:nDraws),sep="" )
    for ( i in 1:nDraws )
    {
      tmp <- as.numeric( c( nAges=nAges,x[ i,inputVars ] ) )

      # Write the *.dat file.
      dataFileName <- paste( fileList[i],".dat",sep="" )
      cat( file=dataFileName, dataFileName, "\n" )
      cat( file=dataFileName, tmp, "\n", append=TRUE )
  
      # Write the *.pin file to speed up convergence. 
      pinFileName <- paste( fileList[i],".pin",sep="" )
      
      # ARK (03-Jun-13) This was log(fmsy), but refptsca inputs FMSY.        
      cat( file=pinFileName, fmsy, "\n" )
    }
  }

  # Return vector of filenames.
  fileList
  #cat("filelist\n")
  #print(fileList)
}     # END function .caaModWriteCtlPtsPinDat


.calcCtlPtsAdmb <- function( i=1, exeName="refptsca", fileList="refptsca" )
{  
  # Call reference point calculations via exeName.exe which sets the output
  # report file name to the input *.dat file name.  Also no Hessian calcs are
  # done to speed minimization - not needed.
  
  # Note: dataFileName MUST be used as it is init_adstring in the *.tpl file.
  
  pathExeName  <- file.path( getwd(), exeName )
  dataFileName <- paste( fileList[i],".dat", sep="" )
  pinFileName  <- paste( fileList[i],".pin", sep="" )
  
  starttime <- Sys.time()

  if( .Platform$OS.type=="unix" )
  { 
   # system( command=paste( pathExeName, " -nox -nohess -ind ", dataFileName, 
    #                       " -ainp ", pinFileName, sep=""),
     #       intern=FALSE, wait=TRUE )

    system2(command=paste( pathExeName ),
            args=paste(" -nox -nohess -ind ", dataFileName, 
                           " -ainp ", pinFileName, sep=""),
            stdout=FALSE, stderr=FALSE, wait=TRUE )
  }
  else if ( .Platform$OS.type=="windows" )
  {
    dirPath <- getwd()
    system( command=paste( pathExeName, " -nox -nohess -ind ", dataFileName, 
            " -ainp ", pinFileName, sep="" ),
            wait=TRUE, show.output.on.console=TRUE )
  }

  endtime <- Sys.time()
  cat("Elapsed in refptsca: ", endtime-starttime, "\n")
  
}     # END function .calcCtlPtsAdmb


.caaModCalcCtlPts <- function( mcmcOut, pars, fmsy, hcrList,
                               method="useFor", useDLL=FALSE )
{
  # Logic here is to compute control points Bmsy, MSY, Fmsy for every row 
  # of mcmcOut if status-based rules, but only do first in the block of 
  # quota levels if it a decline risk rule.
  
  # Non-parallel processing methods: useFor, useApply
  # Parallel processing methods: useSnow, with non-DLL or DLL.
  
  # pars is the opMod list so has maturity etc in it.
  # Timing funtions:
  # starttime<-Sys.time(); endtime<-Sys.time(); elapsedFor<-difftime(endtime,starttime)
  failed  <- FALSE
  method = "useFor"
  starttime <- Sys.time()
  
  nAges <- pars$nAges
  
  # 1. Get the row indices of the first quota level in each block.  
  idx <- c(1:nrow(mcmcOut))
  if ( (hcrList$hcrType=="declineRisk") )
    idx <- idx[ mcmcOut[,"Q"]==mcmcOut[1,"Q"] ]
  
  # 2. A copy of mcmcOut, or just the rows corresponding to first quota level.  
  mcmcTmp <- mcmcOut[idx,]
  
  # Write the *.dat and *.pin files, return the list of file names.
  
  exeName      <- "refptsca"
  
  fileList     <- .caaModWriteCtlPtsPinDat( mcmcTmp, nAges, fmsy=0.2 )
  pinFileList  <- paste( fileList,".pin", sep="" )
  dataFileList <- paste( fileList,".dat", sep="" )
  #fileList <- sapply( X=mcmcTmp[,1], FUN=.makeFileName, exeName="refptsca" )  

  #----------------------------------------------------------------------------#
  # Non-parallel processing methods.                                           #
  #----------------------------------------------------------------------------#
  #-- "for" loop version ------------------------------------------------------#
  if ( method=="useFor" )
  {
    for ( i in 1:nrow(mcmcTmp) )
    {
      .calcCtlPtsAdmb( i, exeName=exeName, fileList )
    }
    
    # Remember the ADMB program sends output to the file name used as the dat file.
    result <- t( sapply( dataFileList, FUN=scan, nlines=1,
                 quiet=TRUE, what=numeric(3) ) )
  }

  #--- "lapply" version -------------------------------------------------------#

  if ( method=="useApply" && !useDLL )
  {
    lapply( 1:nrow(mcmcTmp), FUN=.calcCtlPtsAdmb, exeName=exeName, fileList=fileList )
    
    # Remember the ADMB program sends output to the file name used as the dat file.    
    result <- t( sapply( dataFileList, FUN=scan, nlines=1,
                 quiet=TRUE,what=numeric(3) ) )
  }

  #----------------------------------------------------------------------------#
  # Parallel processing methods via "snow" package.                            #
  #----------------------------------------------------------------------------#

  #-- DLL method --------------------------------------------------------------#

  if ( method=="useSnow" && useDLL )
  {
    # Write mcmc output to text file
    fileName <- "xMat.txt"
    write.table( x=as.data.frame(mcmcTmp), file=fileName, col.names=F, row.names=F,
                 sep=" ", append=F )

    # Call separate R code as script to eliminate ADMB sqwaucking...
    cmdArg <- paste( "Rscript doRefPtsDLL.r", nAges, fmsy, "refPts", sep=" " )
    system( command=cmdArg, wait=T, ignore.stdout=T )

    # Get table of ref pts from file.
    result <- as.matrix( read.table( file="refPts.txt", header=T ) )
  }
  
  #-- System call to exe method -----------------------------------------------#
  
  if ( method=="useSnow" && !useDLL )
  {
    if ( nrow(mcmcTmp)==1 )
    {
      .calcCtlPtsAdmb( 1, exeName=exeName, fileList )
    }
    else
    {
      # Start the snow cluster, note considerable overhead starting cluster. 
      # nProc <- .NPROC # find out how many cores there are
      # setup cluster on all available processors
      cl  <- makeCluster( spec=rep("localhost",4), type="SOCK" )

      status <- parSapply( cl, 1:nrow(mcmcTmp), FUN=.calcCtlPtsAdmb, exeName=exeName,
                           fileList=fileList )
                        
      # Check for return status of 1 which is a problem, and retry...
      #doAgain <- c(1:nrow(mcmcTmp))[as.logical(status)]
      #if ( length(doAgain) > 0 )
      #{
      #  for ( i in doAgain )
      #  {
      #    cat( "\nMSG (.caaModCalcCtlPts) Re-trying ", fileList[i],"\n" )
      #    .calcCtlPtsAdmb( i, exeName=exeName, fileList )
      #  }
      #}

      #result <- t( sapply( dataFileList, FUN=scan, nlines=1, quiet=TRUE,
      #             what=numeric(3) ) )

      # Stop the snow cluster, note overhead stopping cluster.
      stopCluster( cl )
    }

    result <- t( sapply( dataFileList, FUN=scan, nlines=1, quiet=TRUE,
                 what=numeric(3) ) )
  }

  # Clean up ADMB *.pin and *.dat files if they exist.
  file.remove( pinFileList[ file.exists(pinFileList) ] )
  file.remove( dataFileList[ file.exists(dataFileList) ] )

  endtime <- Sys.time()
  cat( "\nElapsed time: ", difftime(endtime,starttime),"\n")

  # Now, the result has either every row input from mcmcOut so do nothing.
  # Or, has only the row corresponding to the first occurence of each quota level.
  
  if ( (hcrList$hcrType=="declineRisk") )
  {
    tmp <- matrix( NA, nrow=nrow(mcmcOut),ncol=3 )
    tmp[idx,] <- result
    result <- tmp
  }

  statusSource <- hcrList$statusSource
  remRefSource <- hcrList$remRefSource
  
  ctlPtSrce    <- "MP"

  nRows  <- nrow( result )
  result <- data.frame( ctlPtSrce=rep(ctlPtSrce, nRows), ssbFmsy=result[,1],
                        MSY=result[,2], Fmsy=result[,3] )
  result
}     # END function .caaModCalcCtlPts


.ddModCalcCtlPts <- function( mcmcOut, pars, fmsy, hcrList,
                               method="useFor", useDLL=FALSE )
{
  # Logic here is to compute control points Bmsy, MSY, Fmsy for every row 
  # of mcmcOut if status-based rules, but only do first in the block of 
  # quota levels if it a decline risk rule.
  
  # Non-parallel processing methods: useFor, useApply
  # Parallel processing methods: useSnow, with non-DLL or DLL.
  
  # pars is the opMod list so has maturity etc in it.
  # Timing funtions:
  # starttime<-Sys.time(); endtime<-Sys.time(); elapsedFor<-difftime(endtime,starttime)
  failed  <- FALSE
  
  starttime <- Sys.time()
  
  nAges <- pars$nAges

  # 1. Get the row indices of the first quota level in each block.  
  idx <- c(1:nrow(mcmcOut))
  if ( (hcrList$hcrType=="declineRisk") )
    idx <- idx[ mcmcOut[,"Q"]==mcmcOut[1,"Q"] ]
  
  # 2. A copy of mcmcOut, or just the rows corresponding to first quota level.  
  mcmcTmp <- mcmcOut[idx,]
  
  # Write the *.dat and *.pin files, return the list of file names.
  
  exeName      <- "refptsdd"
  fileList     <- .ddModWriteCtlPtsPinDat( mcmcTmp, nAges, fmsy=0.2 )
  pinFileList  <- paste( fileList,".pin", sep="" )
  dataFileList <- paste( fileList,".dat", sep="" )
  #fileList <- sapply( X=mcmcTmp[,1], FUN=.makeFileName, exeName="refptsca" )  

  #----------------------------------------------------------------------------#
  # Non-parallel processing methods.                                           #
  #----------------------------------------------------------------------------#

  #-- "for" loop version ------------------------------------------------------#
  
  if ( method=="useFor" )
  {
    for ( i in 1:nrow(mcmcTmp) )
    {
      .calcCtlPtsAdmb( i, exeName=exeName, fileList )
    }
    
    # Remember the ADMB program sends output to the file name used as the dat file.
    result <- t( sapply( dataFileList, FUN=scan, nlines=1,
                 quiet=TRUE, what=numeric(3) ) )
  }

  #--- "lapply" version -------------------------------------------------------#

  if ( method=="useApply" )
  {
    lapply( 1:nrow(mcmcTmp), FUN=.calcCtlPtsAdmb, exeName=exeName, fileList=fileList )
    
    # Remember the ADMB program sends output to the file name used as the dat file.    
    result <- t( sapply( dataFileList, FUN=scan, nlines=1,
                 quiet=TRUE,what=numeric(3) ) )
  }

  #----------------------------------------------------------------------------#
  # Parallel processing methods via "snow" package.                            #
  #----------------------------------------------------------------------------#

  #-- DLL method --------------------------------------------------------------#

  if ( method=="useSnow" && useDLL )
  {
    # Write mcmc output to text file
    fileName <- "xMat.txt"
    write.table( x=as.data.frame(mcmcTmp), file=fileName, col.names=F, row.names=F,
                 sep=" ", append=F )

    # Call separate R code as script to eliminate ADMB sqwuacking...
    cmdArg <- paste( "Rscript doRefPtsDLL.r", nAges, fmsy, "refPts", sep=" " )
    system( command=cmdArg, wait=T, ignore.stdout=T )
    
    # Get table of ref pts from file.
    result <- as.matrix( read.table( file="refPts.txt", header=T ) )
  }
  
  #-- System call to exe method -----------------------------------------------#
  
  if ( method=="useSnow" && !useDLL )
  {
    if ( nrow(mcmcTmp)==1 )
    {
      .calcCtlPtsAdmb( 1, exeName=exeName, fileList )
      cat( "\n NO SNOW....\n" )
    }
    else
    {
      # Start the snow cluster, note considerable overhead starting cluster. 
      # nProc <- .NPROC # find out how many cores there are
      # setup cluster on all available processors
      cl  <- makeCluster( spec=rep("localhost",4), type="SOCK" )

      status <- parSapply( cl, 1:nrow(mcmcTmp), FUN=.calcCtlPtsAdmb, exeName=exeName,
                           fileList=fileList )
                        
      # Check for return status of 1 which is a problem, and retry...
      doAgain <- c(1:nrow(mcmcTmp))[as.logical(status)]
      if ( length(doAgain) > 0 )
      {
        for ( i in doAgain )
        {
          cat( "\nMSG (.ddModCalcCtlPts) Re-trying ", fileList[i],"\n" )
          .calcCtlPtsAdmb( i, exeName=exeName, fileList )
        }
      }

      #result <- t( sapply( dataFileList, FUN=scan, nlines=1, quiet=TRUE,
      #           what=numeric(3) ) )

      # Stop the snow cluster, note overhead stopping cluster.
      stopCluster( cl )
    }
  }

  result <- t( sapply( dataFileList, FUN=scan, nlines=1, quiet=TRUE,
               what=numeric(3) ) )

  # Clean up ADMB *.pin and *.dat files if they exist.
  file.remove( pinFileList[ file.exists(pinFileList) ] )
  file.remove( dataFileList[ file.exists(dataFileList) ] )

  endtime <- Sys.time()
  cat( "\nElapsed time: ", difftime(endtime,starttime),"\n")

  # Now, the result has either every row input from mcmcOut so do nothing.
  # Or, has only the row corresponding to the first occurence of each quota level.
  
  if ( (hcrList$hcrType=="declineRisk") )
  {
    tmp <- matrix( NA, nrow=nrow(mcmcOut),ncol=3 )
    tmp[idx,] <- result
    result <- tmp
  }

  statusSource <- hcrList$statusSource
  remRefSource <- hcrList$remRefSource
  ctlPtSrce    <- "MP"

  nRows  <- nrow( result )
  result <- data.frame( ctlPtSrce=rep(ctlPtSrce, nRows), ssbFmsy=result[,1],
                        MSY=result[,2], Fmsy=result[,3] )
  result
}     # END function .ddModCalcCtlPts


.caaModCtlPtsUseR <- function( i, pars, mcmcData, hcrList )
{
  # want obj$refPts$Fmsy for refpts####.pin
  # pars: blob$opMod
  # mcmcData: data.frame created from "mcout.dat" file.
  # hcrList: blob$mp$hcr

  Fmsy    <- NA
  ssbFmsy <- NA

  # Update pars copy with mcmc parameter values.  
  pars$rSteepness <- mcmcData[ i, "rSteepness" ]
  pars$B0         <- mcmcData[ i, "B0" ]
  pars$M          <- mcmcData[ i, "M" ]
  pars$rec.a      <- mcmcData[ i, "rec.a" ]
  pars$rec.b      <- mcmcData[ i, "rec.b" ]
  pars$aSel50     <- mcmcData[ i, "aSel50" ]
  pars$aSel95     <- mcmcData[ i, "aSel95" ]
  pars$aMat50     <- mcmcData[ i, "aMat50" ]
  pars$aMat95     <- mcmcData[ i, "aMat95" ]
  pars$Linf       <- mcmcData[ i, "Linf" ]
  pars$L1         <- mcmcData[ i, "L1" ]
  pars$vonK       <- mcmcData[ i, "vonK" ]
  pars$c1         <- mcmcData[ i, "c1" ]
  pars$c2         <- mcmcData[ i, "c2" ]
  pars$sigmaL     <- mcmcData[ i, "sigmaL" ]

  # ARK (23-Dec-11) Should we add maturity parameters, growth for generality?
  refPoints         <- list()
  refPoints$ssbFmsy <- NA
  refPoints$Fmsy    <- NA

  # ARK (23-Dec-11) Should we grab F0.1 too?
  
  # If decline risk rule the reference points are the same for each replicate
  # of the block of quota levels, Q1, Q2, ..., Qn.  Therefore, look for "Q" and
  # determine if the i-th value of Q is the same as Q[1].  If so, calculate the
  # reference points. Else, fill in the reference points from the last calculated values.
  
  doRefPts <- TRUE
  if ( (hcrList$hcrType=="declineRisk") && (mcmcData$Q[i]!=mcmcData$Q[1]) )
  {
    # Don't calculate reference points.
    doRefPts     <- FALSE
    tmpRefPoints <- list( Fmsy=NA, ssbFmsy=NA )
  }
  
  tmpRefPoints <- NULL
  if ( doRefPts )
  {
    tmpRefPoints <- try( calcRefPoints( pars, 
      rpList=list( FALL=FALSE, Fmsy=TRUE ), application="MP" ),
      silent=TRUE )
    
    if ( i %% 10 == 0 )
      cat( "\nMSG (.caaModCtlPtsUseR) Calculating reference points for draw",i,"\n" )      
  }

  statusSource <- hcrList$statusSource
  remRefSource <- hcrList$remRefSource
  ctlPtSrce    <- "MP"

  # .calcRefPoints succeeded, or was not calculated, so use MSY-based reference points.
  if ( class( tmpRefPoints ) == "list" )
  {
    Fmsy    <- tmpRefPoints$Fmsy
    ssbFmsy <- tmpRefPoints$ssbFmsy
  }
      
  result <- list( ctlPtSrce=ctlPtSrce, ssbFmsy=ssbFmsy, Fmsy=Fmsy )  
  result
}     # END function .caaModCtlPtsUseR

#------------------------------------------------------------------------------#
#- Delay Difference Helper functions for reference point processing.          -#
#------------------------------------------------------------------------------#

.ddModWriteCtlPtsPinDat <- function( x, nAges=25, fmsy=0.2 )
{
  # This is essentially the same code as .caaModWriteCtlPtsPinDAt
   #
  # This makes a data file for refptsDD.tpl, then calls it.  The DATA_SECTION:
  # DATA_SECTION
  # // read one line of mcmc output from assessDD

  # init_int nAges;
  # init_number B0;
  # init_number R0;
  # init_number rSteepness;
  # init_number rec_a;
  # init_number rec_b;
  # init_number M;
  # init_number alpha_g;
  # init_number rho_g;
  # init_number wk;
  
  exeName <- "refptsdd"
  
  #inputVars <- c( "B0","R0","rSteepness","rec.a","rec.b","M",
  #                "sigmaR","rho","Linf","L1","vonK",
  #                "c1","c2","sigmaL","tau",
  #                "spawnBio","projExpBio","termDep",
  #                "alpha_g","rho_g","wk" )
                  
  inputVars <- c( "B0","R0","rSteepness","rec.a","rec.b","M",
                  "alpha_g","rho_g","wk" )                  

  if ( is.vector( x ) )
  {
    nDraws <- 1
    fileList <- exeName
    
    tmp <- as.numeric( c( nAges=nAges,x[ inputVars ] ) )

    # Write the *.dat file.
    dataFileName <- paste( fileList,".dat",sep="" )
    cat( file=dataFileName, dataFileName, "\n" )
    cat( file=dataFileName, tmp, "\n", append=TRUE )
  
    # Write the *.pin file to speed up convergence. 
    
    # ARK (03-Jun-13) This was log(fmsy), but refptsca inputs FMSY.
    pinFileName <- paste( fileList,".pin",sep="" )  
    cat( file=pinFileName, fmsy, "\n" )
  }
  else
  {
    # This could be parallelized...
    nDraws <- nrow( x )
    fileList <- paste( exeName,c(1:nDraws),sep="" )
    for ( i in 1:nDraws )
    {
      tmp <- as.numeric( c( nAges=nAges,x[ i,inputVars ] ) )

      # Write the *.dat file.
      dataFileName <- paste( fileList[i],".dat",sep="" )
      cat( file=dataFileName, dataFileName, "\n" )
      cat( file=dataFileName, tmp, "\n", append=TRUE )
  
      # Write the *.pin file to speed up convergence. 
      pinFileName <- paste( fileList[i],".pin",sep="" )
      
      cat( file=pinFileName, fmsy, "\n" )
    }
  }

  # Return vector of filenames.
  fileList
}     # END function .ddModWriteCtlPtsPinDat

.ddModCalcCtlPts <- function( mcmcOut, pars, fmsy, hcrList,
                               method="useFor", useDLL=FALSE )
{
  # Logic here is to compute control points Bmsy, MSY, Fmsy for every row 
  # of mcmcOut if status-based rules, but only do first in the block of 
  # quota levels if it a decline risk rule.
  
  # Non-parallel processing methods: useFor, useApply
  # Parallel processing methods: useSnow, with non-DLL or DLL.
  
  # pars is the opMod list so has maturity etc in it.
  # Timing funtions:
  # starttime<-Sys.time(); endtime<-Sys.time(); elapsedFor<-difftime(endtime,starttime)
  failed  <- FALSE
  
  starttime <- Sys.time()
  
  nAges <- pars$nAges

  # 1. Get the row indices of the first quota level in each block.  
  idx <- c(1:nrow(mcmcOut))
  if ( (hcrList$hcrType=="declineRisk") )
    idx <- idx[ mcmcOut[,"Q"]==mcmcOut[1,"Q"] ]
  
  # 2. A copy of mcmcOut, or just the rows corresponding to first quota level.  
  mcmcTmp <- mcmcOut[idx,]
  
  # Write the *.dat and *.pin files, return the list of file names.
  
  exeName      <- "refptsdd"
  fileList     <- .ddModWriteCtlPtsPinDat( mcmcTmp, nAges=25, fmsy=0.2 )
  pinFileList  <- paste( fileList,".pin", sep="" )
  dataFileList <- paste( fileList,".dat", sep="" )
  #fileList <- sapply( X=mcmcTmp[,1], FUN=.makeFileName, exeName="refptsca" )  

  #----------------------------------------------------------------------------#
  # Non-parallel processing methods.                                           #
  #----------------------------------------------------------------------------#
  
  #-- "for" loop version ------------------------------------------------------#
  
  if ( method=="useFor" )
  {
    for ( i in 1:nrow(mcmcTmp) )
    {
      .calcCtlPtsAdmb( i, exeName=exeName, fileList )
    }
    
    # Remember the ADMB program sends output to the file name used as the dat file.
    result <- t( sapply( dataFileList, FUN=scan, nlines=1,
                 quiet=TRUE, what=numeric(3) ) )
  }

  #--- "lapply" version -------------------------------------------------------#

  if ( method=="useApply" )
  {
    lapply( 1:nrow(mcmcTmp), FUN=.calcCtlPtsAdmb, exeName=exeName, fileList=fileList )
    
    # Remember the ADMB program sends output to the file name used as the dat file.    
    result <- t( sapply( dataFileList, FUN=scan, nlines=1,
                 quiet=TRUE,what=numeric(3) ) )
  }

  #----------------------------------------------------------------------------#
  # Parallel processing methods via "snow" package.                            #
  #----------------------------------------------------------------------------#

  #-- DLL method --------------------------------------------------------------#

  if ( method=="useSnow" && useDLL )
  {
    # Write mcmc output to text file
    fileName <- "xMat.txt"
    write.table( x=as.data.frame(mcmcTmp), file=fileName, col.names=F, row.names=F,
                 sep=" ", append=F )

    # Call separate R code as script to eliminate ADMB sqwuacking...
    cmdArg <- paste( "Rscript doRefPtsDLL.r", nAges, fmsy, "refPts", sep=" " )
    system( command=cmdArg, wait=T, ignore.stdout=T )
    
    # Get table of ref pts from file.
    result <- as.matrix( read.table( file="refPts.txt", header=T ) )
  }
  
  #-- System call to exe method -----------------------------------------------#
  
  if ( method=="useSnow" && !useDLL )
  {
    if ( nrow( mcmcTmp )==1 )
    {
      .calcCtlPtsAdmb( 1, exeName=exeName, fileList )
    }
    else
    {
      # Start the snow cluster, note considerable overhead starting cluster. 
      # nProc <- .NPROC # find out how many cores there are
      # setup cluster on all available processors
      cl  <- makeCluster( spec=rep("localhost",1), type="SOCK" )

      status <- parSapply( cl, 1:nrow(mcmcTmp), FUN=.calcCtlPtsAdmb, exeName=exeName,
                           fileList=fileList )
                        
      # Check for return status of 1 which is a problem, and retry...
      doAgain <- c(1:nrow(mcmcTmp))[as.logical(status)]
      if ( length(doAgain) > 0 )
      {
        for ( i in doAgain )
        {
          cat( "\nMSG (.ddModCalcCtlPts) Re-trying ", fileList[i],"\n" )
          .calcCtlPtsAdmb( i, exeName=exeName, fileList )
        }
      }

      #result <- t( sapply( dataFileList, FUN=scan, nlines=1, quiet=TRUE,
      #           what=numeric(3) ) )

      # Stop the snow cluster, note overhead stopping cluster.
      stopCluster( cl )
    }
    
    result <- t( sapply( dataFileList, FUN=scan, nlines=1, quiet=TRUE,
                 what=numeric(3) ) )
  }

  # Clean up ADMB *.pin and *.dat files if they exist.
  file.remove( pinFileList[ file.exists(pinFileList) ] )
  file.remove( dataFileList[ file.exists(dataFileList) ] )

  endtime <- Sys.time()
  cat( "\nElapsed time: ", difftime(endtime,starttime),"\n")

  # Now, the result has either every row input from mcmcOut so do nothing.
  # Or, has only the row corresponding to the first occurence of each quota level.
  
  if ( (hcrList$hcrType=="declineRisk") )
  {
    tmp <- matrix( NA, nrow=nrow(mcmcOut),ncol=3 )
    tmp[idx,] <- result
    result <- tmp
  }

  statusSource <- hcrList$statusSource
  remRefSource <- hcrList$remRefSource
  ctlPtSrce    <- "MP"

  nRows  <- nrow( result )
  result <- data.frame( ctlPtSrce=rep(ctlPtSrce, nRows),
                        ssbFmsy=result[,1],MSY=result[,2],
                        Fmsy=result[,3] )
                        
  result
}     # END function .ddModCalcCtlPts

#------------------------------------------------------------------------------#
#--                            Helper Functions                              --#
#------------------------------------------------------------------------------#
   	   
.calcTimes <- function( obj )
{
  # This function calculates when stock indexing survey, ages, assessment or
  # control points are turned on/off. This has to be done in the same function
  # as there are dependencies among the various inputs.  For example, the rule
  # control points cannot be updated when there is no assessment.  Also, there
  # is no option for assessment frequency prior to tMP.
 	 
  nT     <- obj$opMod$nT                 # Maximum year of simulation.
  tMP    <- obj$opMod$tMP                # First year of management procedure.
  
  result     <- list()                   # The result.
  result$nT  <- nT
  result$tMP <- tMP 
  
  #----------------------------------------------------------------------------#
  #-- Survey timing                                                            #
  #                                                                            #
  # Constraints:                                                               #
  #                                                                            #
  # 1 <= t1Survey < tMP <= t2Survey <= nT                                      #
  #----------------------------------------------------------------------------#
  
  # Survey design settings.
  t1Survey <- obj$mp$data$t1Survey   # First year of survey for Period 1.
  t2Survey <- obj$mp$data$t2Survey   # First year of survey for Period 2.
  k1Survey <- obj$mp$data$k1Survey   # Survey frequency in Period 1.
  k2Survey <- obj$mp$data$k2Survey   # Survey frequency in Period 2.
  
  # Create a Boolean control vector t=1,..,nT that determines years in which a
  # survey should be done. Initialize all values to FALSE.
  surveyOn <- rep( FALSE, nT )

  # Set surveyOn==TRUE when a survey is done in Period 1.
  idPer1 <- 0
  if ( obj$mp$data$chkSurvey1 )
    idPer1 <- eval( parse( text=obj$mp$data$idxSurvey1 ) )
  else
  {
    if ( k1Survey > 0 )
      idPer1 <- seq( from=t1Survey, to=(t2Survey-1), by=k1Survey )
  }
  
  if ( idPer1[1] > 0 )    
    surveyOn[ idPer1 ] <- TRUE

  # Set surveyOn==TRUE when a survey is done in Period 2.
  idPer2 <- 0
  if ( obj$mp$data$chkSurvey2 )
    idPer2 <- eval( parse( text=obj$mp$data$idxSurvey2 ) )
  else
  {
    if ( k2Survey > 0 )
      idPer2 <- seq( from=t2Survey, to=nT, by=k2Survey )
  }
  
  if ( idPer2[1] > 0 )  
    surveyOn[ idPer2 ] <- TRUE

  result$t1Survey       <- t1Survey
  result$t2Survey       <- t2Survey
  result$k1Survey       <- k1Survey
  result$k2Survey       <- k2Survey
  result$per1Survey     <- idPer1
  result$per2Survey     <- idPer2
  result$surveyOn       <- surveyOn
  
  #----------------------------------------------------------------------------#
  #-- Ages timing                                                              #
  #                                                                            #
  # Constraints: Can only have survey ages when there is a survey.             #
  #----------------------------------------------------------------------------#

  # Ageing data design settings FISHERY
  t1Ages     <- obj$mp$data$t1Ages   # First year of ages for Period 1.
  t2Ages     <- obj$mp$data$t2Ages   # First year of ages for Period 2.
  k1Ages     <- obj$mp$data$k1Ages   # Age data frequency in Period 1.
  k2Ages     <- obj$mp$data$k2Ages   # Age data frequency in Period 2.
  
  # Ageing data design settings SURVEY
  t1AgesS     <- obj$mp$data$t1AgesS   # First year of ages for Period 1.
  t2AgesS     <- obj$mp$data$t2AgesS   # First year of ages for Period 2.
  k1AgesS     <- obj$mp$data$k1AgesS   # Age data frequency in Period 1.
  k2AgesS     <- obj$mp$data$k2AgesS   # Age data frequency in Period 2.

  #-- FISHERY AGES

  # Create a Boolean control vector t=1,..,nT that determines years in which a
  # age data should be collected. Initialize all values to FALSE.	  
  agesOn  <- rep( FALSE, nT )
  
  # Set agesOn==TRUE when fishery ages available in Period 1.
  if ( obj$mp$data$chkAges1 )
    idPer1 <- eval( parse( text=obj$mp$data$idxAges1 ) )
  else
  {
    if ( k1Ages > 0 )
      idPer1 <- seq( from=t1Ages, to=(t2Ages-1), by=k1Ages )
    else
      idPer1 <- rep( 0, t2Ages-t1Ages+1 )
  }
  agesOn[ idPer1 ] <- TRUE

  # Set agesOn==TRUE when fishery ages available in Period 2.
  if ( obj$mp$data$chkAges2 )
    idPer2 <- eval( parse( text=obj$mp$data$idxAges2 ) )
  else
  {
    if ( k2Ages > 0 )
      idPer2 <- seq( from=t2Ages, to=nT, by=k2Ages )
    else
      idPer2 <- rep( 0, nT-t2Ages+1 )
  }
  agesOn[ idPer2 ] <- TRUE

  result$t1Ages   <- t1Ages
  result$t2Ages   <- t2Ages
  result$k1Ages   <- k1Ages
  result$k2Ages   <- k2Ages
  result$per1Ages <- idPer1
  result$per2Ages <- idPer2
  result$agesOn   <- agesOn

  #-- SURVEY AGES

  agesOnS <- rep( FALSE, nT )
  
	# Set agesOnS==TRUE when survey ages available in Period 1.
	if ( obj$mp$data$chkAges1S )
	  idPer1S <- eval( parse( text=obj$mp$data$idxAges1S ) )
	else
	{
	  if ( k1AgesS > 0 )
      idPer1S <- seq( from=t1AgesS, to=(t2AgesS-1), by=k1AgesS )
    else
      idPer1S <- rep( 0, t2AgesS-t1AgesS+1 )
  }
  agesOnS[ idPer1S ] <- TRUE

	# Set agesOnS==TRUE when survey ages available in Period 2.
	if ( obj$mp$data$chkAges2S )
	  idPer2S <- eval( parse( text=obj$mp$data$idxAges2S ) )
	else
	{
	  if ( k2AgesS > 0 )
      idPer2S <- seq( from=t2AgesS, to=nT, by=k2AgesS )
    else
      idPer2S <- rep( 0, nT-t2AgesS+1 )
  }
  agesOnS[ idPer2S ] <- TRUE

	result$t1AgesS   <- t1AgesS
	result$t2AgesS   <- t2AgesS
	result$k1AgesS   <- k1AgesS
	result$k2AgesS   <- k2AgesS
	result$per1AgesS <- idPer1S
	result$per2AgesS <- idPer2S
	
  # Now ensure that that survey ages are only generated when there is a survey.
  result$agesOnS <- as.logical( surveyOn * agesOnS )

  #----------------------------------------------------------------------------#
  #-- Assessment method timing                                                 #
  #                                                                            #
  # Constraints: There is no timing of assessments prior to tMP.               #
  # First assessment is at tMP-1, which is Period 1. Perido 2 starts at t>=tMP #
  #----------------------------------------------------------------------------#    

  t1Method <- obj$mp$assess$t1Method       # First year of assessment Period 1.
  t2Method <- obj$mp$assess$t2Method       # First year of assessment Period 2.
  k1Method <- obj$mp$assess$k1Method       # Frequency of assessment Period 1.
  k2Method <- obj$mp$assess$k2Method       # Frequency of assessment Period 2.

  # Create a Boolean control vector t=1,..,nT that determines years in which a
  # asessment should be done. Initialize all values to FALSE.
  methodOn <- rep( FALSE, nT )

  # Set methodOn==TRUE when an assessment is done either in Period or Period 2.
  # The value of t1Method must be less than tMP, however, the value of
  # t2Method can be any value t1Method < t2Method <=nT.
  
  # Vector with elements of surveyOn=TRUE for survey years in "old" period.
  idPer1 <- seq( from=t1Method, to=t2Method, by=k1Method )

  # Set methodOn==TRUE for these elements
  methodOn[ idPer1 ] <- TRUE

  # Set methodOn==TRUE when an assessment is done in Period 2.
  idPer2 <- 0
  if ( obj$mp$assess$chkMethod2 )
    idPer2 <- eval( parse( text=obj$mp$assess$idxMethod2 ) )
  else
  {
    if ( k2Method > 0 )
      idPer2 <- seq( from=t2Method, to=nT, by=k2Method )
  }

  # Set methodOn==TRUE for these elements.
  if ( idPer2[1] > 0 )
    methodOn[ idPer2 ] <- TRUE

  result$t1Method   <- t1Method
  result$t2Method   <- t2Method
  result$k1Method   <- k1Method
  result$k2Method   <- k2Method
  result$per1Method <- idPer1
  result$per2Method <- idPer2
  result$methodOn   <- methodOn
  
  # Create a vector tMP:nT that contains the time index where catch is to be taken
  # in interim times between assessments if forecasts are NOT used.  This holds
  # the catch constant at the last assessment and HCR result if applied.
  # For example, if ctlPtFreq=5, tMP=50, nT=60:
  # idx = c( 50, 55, 60 )
  # tCtlPts = c( 50, 50, 50, 50, 50, 55, 55, 55, 55, 55, 60, 60, 60, 60, 60 )
  
  # Get time steps when assessment done.
  tMethod <- c(tMP:nT)[ methodOn[tMP:nT] ]
  
  # Get the difference in the times to calc number times each assessment used.
  tDiff <- diff( tMethod )
  
  # Get the maximum tMethod, and concatenate difference between it and nT.
  tDiff <- c( tDiff, nT-max(tMethod)+1 )
  tMethod <- rep( tMethod, tDiff )
  
  result$tMethod   <- tMethod
  result$idxMethod <- tMethod - (tMP-1)
  
  #----------------------------------------------------------------------------#
  #-- Control Point update timing                                              #
  #                                                                            #
  # Constraints:                                                               #
  #                                                                            #
  # 1. There must be an update on tMP.                                         #
  # 2. No updating of control points in the HCR without a stock assessment.    #
  #    but there can be stock assessments without updating of control points.  #                                             #
  #----------------------------------------------------------------------------#  
  
  ctlPtFreq <- obj$mp$hcr$ctlPtFreq
  
  # Create a Boolean control vector t=1,..,nT that determines years in which
  # control points are updated.  Initialize all values to FALSE.
  ctlPtsOn <- rep( FALSE, nT )
  
  if ( ctlPtFreq > 0 )
  {
    idx <- seq( from=tMP, to=nT, by=ctlPtFreq )
    ctlPtsOn[ idx ] <- TRUE
  }
  else
  {
    ctlPtsOn[ tMP ] <- TRUE
  }

  # Now ensure that the updates are not more frequent than the assessments.
  ctlPtsOn[ tMP:nT ] <- as.logical( ctlPtsOn[ tMP:nT] * methodOn[ tMP:nT ] )
  
  # Create a vector tMP:nT that contains the time index where catch is to be taken
  # in interim times between assessments if model is NOT used.  This holds
  # the catch constant at the last assessment and HCR result if applied.
  # For example, if ctlPtFreq=5, tMP=50, nT=60:
  # idx = c( 50, 55, 60 )
  # tCtlPts = c( 50, 50, 50, 50, 50, 55, 55, 55, 55, 55, 60, 60, 60, 60, 60 )
  
  # Get time steps when assessment done.
  tCtlPts <- c(tMP:nT)[ ctlPtsOn[tMP:nT] ]
  
  # Get the difference in the times to calc number times each assessment used.
  tDiff <- diff( tCtlPts )
  
  # Get the maximum tMethod, and concatenate difference between it and nT.
  tDiff <- c( tDiff, nT-max(tCtlPts)+1 )
  tCtlPts <- rep( tCtlPts, tDiff )

  # Calculate indices relative to tMP, so that times run from 1 to (nT-tMP+1).
  idxCtlPts <- tCtlPts - (tMP-1)

  result$ctlPtFreq <- ctlPtFreq
  result$ctlPtsOn  <- as.logical( ctlPtsOn )
  result$tCtlPts   <- tCtlPts
  result$idxCtlPts <- idxCtlPts
  
  #----------------------------------------------------------------------------#

  result 
}     # END function .calcTimes


.makeFileName <- function( par, exeName="refptsca" )
{
  idx   <- as.integer(1000*par)
  fName <- paste( exeName, idx, ".dat", sep="" )
  return( fName )
}     # END function .makeFileName


# .saveBlob (Saves the simulations results and updates list)
# Purpose:        Saves the simulations results stored in blob to a .RData file.
# Parameters:     obj is a "blob" object holding simulation results.
# Returns:        File name of the saved results.
# Source:         A.R. Kronlund
.saveBlob <- function( blobFileName, blob )
{
  cat( "\nMSG (.saveBlob) Writing simulation results to file: ",blobFileName,"\n" )
  save( blob, file=blobFileName )
}     # END function .saveBlob


# .setMissing 
# Purpose:        Converts missing values from NA -> -1 when going from 
#                 R -> ADMB, and -1 -> NA in opposite direction
# Parameters:     obj is data object; missFlag is ADMB code, toR is ADMB -> R
# Returns:        obj with missing values replaced
# Source:         A.R. Kronlund
.setMissing <- function( obj,missFlag=-1, toR=TRUE )
{
  if ( toR )
    obj[ obj==missFlag ] <- NA
  else
    obj[ is.na(obj) ] <- missFlag
  obj
}     # END function .setMissing

# .calcHCR_ccRule       
# Purpose:        harvest control rule to generate the quota
# Parameters:     obj=list containing all variables and parameters
#                 necessary to compute the quota
# Requires:       global variable Bmin = initial value for lowest biomass
# Returns:        the quota=catch
# Source:         S.P. Cox
calcHCR_ccRule <- function( hcrList )
{
  
  # Function to find the minimum biomass over past 10 years
  # from which the stock has recovered.  
  find_min <- function(x)
  {
    # Goal: find the minumum value of x that has been "recovered" from.
    # In other words, find the smallest datapoint x[t] such that x[t] < x[t+1].
    # If NA is returned, then no such point has been found.
  
    if( length(x) == 1 ) return(NA)
  
    # propose first datapoint as current minimum
    min_curr <- x[1]
  
    # find another datapoint that is less than the current minimum
    # and also has a subsequent datapoint that is smaller than itself
    if( length(x)>2 ) {
      for( i in 2:(length(x)-1) ) {
        if( x[i]<min_curr && x[i]<x[i+1] ) min_curr <- x[i]
      }
    }
    # if the current minimum is still the first datapoint,
    # check the subsequent point
    if( min_curr==x[1] && x[1]>x[2] ) min_curr <- NA
    if(!is.na(min_curr)){
      if(min_curr==0) {
        min_curr <- 0.05
      }
    }
    min_curr
  }

  t   <- hcrList$t - 1
  I   <- hcrList$ccBiomass # this is survey only
  C   <- hcrList$Dt
  u   <- hcrList$remRate[t]

  # Biomass over previous ten years
  if( t > 10 ) B10 <- I[(t-10):t] + C[(t-10):t]
  else         B10 <- I[1:t] + C[1:t]

  # Proposed Bmin
  Bmin_prop <- find_min(B10)



  # Accept Bmin_prop if it exists, otherwise use last year's
  if(!is.na(Bmin_prop)) Bmin <<- Bmin_prop

  if( I[t] > Bmin ) {
    catchLimit <- u*(I[t]+C[t])
  } else {
    catchLimit <- 0
  }

  result <- list( catchLimit=max( 0, catchLimit ), 
                  targetF=NA,
                  eBioAdj=NA )
  result
}     # END function calcHCRsmoother
