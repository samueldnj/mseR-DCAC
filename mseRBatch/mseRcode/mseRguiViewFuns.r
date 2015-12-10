# TASKS
# 2. Documentation pass.
# 3. Clean up plot names.
# 4. Phase plots.
# 5. Add axis 3 and 4 to plots where appropriate.
# 7. Do we want a paAdj on MLEs?
# 8. How do we get mcmc without Hessian?
# 9. Grayed title with method, rule, some parameters for all plots.
# 10. Fix lines on rule when MCMC outputs used.

#------------------------------------------------------------------------------#
# (c) mseR-FinFish: Management Strategy Evaluation in R, Finfish Version 1.0   #
#                                                                              #
#     Copyright 2012 by A.R. Kronlund and S.P. Cox.                            #
#                                                                              #
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
#------------------------------------------------------------------------------#
#--                                                                          --#
#-- mseRguiViewFuns.r: An mseR module that provides graphical user interface --#
#-- capabilities for viewing feedback simulation results for fisheries       --#
#-- management strategy evaluation.                                          --#
#--                                                                          --#
#-- Usage:        guiView()                                                  --#
#-- Side Effects: Involkes GUI for Viewing the feedback simulation results.  --#
#--                                                                          --#
#-- Flow: guiView -> .mseRsetupView -> .wdSetup -> createWin -> .subView     --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--                                                                          --#
#-- First Implementation: 27-Jul-12 from mseR V2, mseR-Prawn and mseR2011.   --#
#--                                                                          --#
#                                                                              #
# NOTES:                                                                       #
#                                                                              #
# 1. To display hidden functions, e.g., .foo, enter command: ls(all.names=TRUE)#
# 2. Unique file names can be created by command:                              #
#                                                                              #
#    paste( "sim", format( Sys.time(), format="%d%m%Y%H%M%S" ), sep="" )       #
#                                                                              #
# 3. Any information read from a text box has a linefeed \n the end of the     #
#    input that needs to be removed, in most cases, before use.                #
# 4. Very inelegant crashes can occur if the browser is called when GUI open.  #
#    This is a PBSmodelling/TkTcl issue and not something we can fix.          #
# 5. PBSModelling has the very undesirable of changing character vectors to    #
#    factor variables within the GUI parameter list.  Not good.  Watch out.    #
#                                                                              #
# REQUIRES: PBSmodelling, RODBC                                                #
#                                                                              #
# References:                                                                  #
#                                                                              #
# DFO, 2006. A Harvest Strategy Compliant with the Precautionary Approach.     #
#   DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2006/023.                       #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., and Egeli, A. 2008. PBS          #
#   Modelling 2.00: users guide revised from Canadian Technical Report of      #
#   Fisheries and Aquatic Science 2674: v+146 p. Last updated October 23, 2008 #
#                                                                              #
# GUI Functions:                                                               #
#                                                                              #
# guiView       : Run mseR simulation Viewer GUI.                              #
#                                                                              #
# GUI Error Checking/Validation:                                               #
#                                                                              #
# .validViewPars: Check if the View GUI parameters are valid.                  #
#                                                                              #
# GUI Submit Control Functions:                                                #
#                                                                              #
# .subView      : Processes guiView submit actions, e.g., buttons.             #
#                                                                              #
# GUI Helper Functions:                                                        #
#                                                                              #
# createList    : Converts a data frame into a possiby nested list object.     #
#                                                                              #
# GUI Hidden Functions (to show hidden function ls(all.names=TRUE):            #
#                                                                              #
# .getWinName   : Get the current GUI window name.                             #
# .viewFile     : View a file saved in the mseRtemp directory.                 #
# .viewHelp     : View a file saved in the mseRhelp directory.                 #
# .wdSetup      : Working directory set-up.                                    #
#------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------##
#-- mseR guiView Functions                                                  --##
#-----------------------------------------------------------------------------##

# guiView     (Run mseR feedback simulation replicate viewer )
# Purpose:    Set up and run the Viewer GUI to inspect individual replicates.
# Parameters: None
# Returns:    NULL (invisibly)
guiView <- function()
{
  return( .mseRsetupView("mseRguiView"))
}     # END function guiView

# .mseRsetupView  (mseR setup for GUI creation)
# Purpose:    Set up and run the specified GUI
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.mseRsetupView <- function( win )
{
  # Get the required libraries.
  require( animation )               # For animating plots.
  require( PBSmodelling )            # For the GUIs
  require( tools )                   # For file_ext etc.

  options( useFancyQuotes=FALSE )    # Required for saveSimPars function.
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  wkDir <- .wdSetup()                  # Setup working directory.
  cat( "\nMSG (.mseRsetupView) Working directory setup in ",wkDir,"\n" )   
  
  closeWin()                         # Close all open windows to prevent crashes.
  graphics.off()                     # Turn off any open graphics.

  .mseRinitProject( wkDir )          # Initialize mseR project, paths, options.

  # Update project and general options.
  source( "mseRoptions.r" )
  
  goMenu <- TRUE                     # Assume menu can be created.

  # Get the tracking data for the project folder.
  trackData <- .getTrackingData( projDir=.PRJFLD )  
  
  # There are valid simulations, go ahead and initialize guiView.
  if ( .getNumSims( trackData ) == 0 )
    goMenu <- FALSE

  #if ( goMenu )
  #{
    # Initialize the GUI from the description file using PBSmodelling createWin.
  createWin( paste( wkDir, "/", win, "Win.txt", sep="" ) )
  
  if ( goMenu )
  {  
    # Get the GUI parameters and make scope local to this function.
    guiInfo    <- getWinVal( scope="L", winName=.getWinName() )
    guiChanges <- list()
    
    # Load GUI view settings from a previous session.
    if ( exists( ".guiViewPars",where=1 ) )
    {
      guiChanges            <- .guiViewPars
      guiChanges$nSim       <- .getNumSims( trackData )
      guiChanges$iSim       <- .guiViewPars$iSim
      if ( guiChanges$iSim > guiChanges$nSim )
        guiChanges$iSim     <- 1
      guiChanges$nReps      <- trackData[ guiChanges$iSim, "nReps" ]
    }
    else
    {
      guiChanges$nSim  <- .getNumSims( trackData )
      guiChanges$iSim  <- 1
      guiChanges$nReps <- trackData[ guiChanges$iSim, "nReps" ]
    }
      
    # Load an Rdata working directory containing a list called blob.
    simFile     <- trackData[ guiChanges$iSim, "simFile" ]
    simFolder   <- trackData[ guiChanges$iSim, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.mseRsetupView) Loading",simFilePath,"...\n" )    
    load( file=simFilePath )
    assign( "blob", blob, pos=1 )      
      
    #refPts <- blob$ctlList$refPts...this doesn't get overwritten
    # when in Batch mode w/o GUI
    refPts <- blob$refPtList
      
    guiChanges$B0        <- blob$ctlList$opMod$B0
    guiChanges$ssbFmsy   <- refPts$ssbFmsy
    guiChanges$yieldFmsy <- refPts$yieldFmsy
    guiChanges$Fmsy      <- refPts$Fmsy
    guiChanges$F0        <- refPts$F0
    guiChanges$F01       <- refPts$F01
    guiChanges$F40       <- refPts$F40
    guiChanges$Fmax      <- refPts$Fmax
    guiChanges$Fcra      <- refPts$Fcra

    setWinVal( guiChanges, winName=.getWinName() )

    # Re-write the GUI parameters to working directory.
    guiInfo <- getWinVal( scope="L", winName=win )
    assign( ".guiViewPars", guiInfo, pos=1 )      
    
    .setGuiViewState()
      
    # Force plotting action on menu creation with default selection.
    .doGuiViewPlots(  blob, iSim, iRep, simFolder=trackData[ iSim,"simFolder" ] ) 
  }
  else
  {
    setWidgetState( varname="vwPlotType", state="disabled" )
    cat( "\nMSG (.mseRsetupView) No simulations available for project folder ",.PRJFLD,"\n" )
  }

  return( invisible() )
}     # END function .mseRsetupView


# .setGuiViewState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiViewState <- function()
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  methodId <- blob$ctlList$mp$assess$methodId
  hcrType  <- blob$ctlList$mp$hcr$hcrType

  #--------------------------- View Page Widgets ------------------------------#
  
  if ( (methodId == .KALMAN) | (methodId == .MOVAVG) )
  {
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwRetroExpBt" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwRetroSpawnBt" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwRetro1" )
    #setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwRetro2" )
    #setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwRetro3" )

    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="refPtEsts" )                    
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="parEsts" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="parCor" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwRefPtSeries" )
    #setWidgetState( varname="vwPlotType",state="disabled",radiovalue="yieldCurve" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="mpRecSpawn" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBub" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBar" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBubS" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBarS" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwDiagsRep" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwDiagsAll" )
    
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwFisherySelFit" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwSurveySelFit" )
        
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="paAdj" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwDecline" )
  }
  
  if ( methodId == .PMOD )
  {
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetroExpBt" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetroSpawnBt" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetro1" )
    #setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetro2" )
    #setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetro3" )
    
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="refPtEsts" )    
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="parEsts" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="parCor" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRefPtSeries" )
    #setWidgetState( varname="vwPlotType",state="normal",radiovalue="yieldCurve")
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="mpRecSpawn" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDiagsRep" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDiagsAll" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBub" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBar" ) 
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBubS" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBarS" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="paAdj" )
    
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwFisherySelFit" )
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwSurveySelFit" )
    
    #if ( hcrType == "declineRisk" )
    #{
    #  setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDeclineHCR" )    
    #}
    #else
    #{
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwDecline" )
    #}
  }
  
    # RF (19-Aug-13) 
    if ( methodId == .DDMOD )
    {
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetroExpBt" )
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetroSpawnBt" )
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetro1" )

      setWidgetState( varname="vwPlotType",state="normal",radiovalue="refPtEsts" )       
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="parEsts" )
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="parCor" )
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRefPtSeries" )
      #setWidgetState( varname="vwPlotType",state="normal",radiovalue="yieldCurve")
  
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="mpRecSpawn" )
      setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBub" )
      setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBarS" )
      setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBubS" )
      setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwAgeBar" )
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDiagsRep" )
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDiagsAll" )
      
      setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwFisherySelFit" )
      setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwSurveySelFit" )
      
      setWidgetState( varname="vwPlotType",state="normal",radiovalue="paAdj" )
    
      #if ( hcrType == "declineRisk" )
      #{
      #  setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDeclineHCR" )    
      #}
      #else
      #{
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwDecline" )
      #}
  }
  
  
  # ARK (05-May-10) Added in anticipation of Catch-at-age model ghosting.
  if ( methodId == .CAAMOD )
  {
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetroExpBt" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetroSpawnBt" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRetro1" )

    setWidgetState( varname="vwPlotType",state="normal",radiovalue="refPtEsts" )    
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="parEsts" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="parCor" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwRefPtSeries" )
    #setWidgetState( varname="vwPlotType",state="normal",radiovalue="yieldCurve")

    setWidgetState( varname="vwPlotType",state="normal",radiovalue="mpRecSpawn" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwAgeBub" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwAgeBarS" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwAgeBubS" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwAgeBar" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDiagsRep" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDiagsAll" )
    
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwFisherySelFit" )
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwSurveySelFit" )
    
    setWidgetState( varname="vwPlotType",state="normal",radiovalue="paAdj" )
  
    #if ( hcrType == "declineRisk" )
    #{
    #  setWidgetState( varname="vwPlotType",state="normal",radiovalue="vwDeclineHCR" )    
    #}
    #else
    #{
    setWidgetState( varname="vwPlotType",state="disabled",radiovalue="vwDecline" )            
    #}
  }
      
  # Update the GUI.
  setWinVal( guiChanges, win )
  
  return( invisible() )
}    # END .setGuiViewState function.


# .subView    (Processes guiView submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiView program flow:
#             - Attempts to check validity of the GUI parameters;
#             - Buttons to move between simluations;
#             - Buttons to move between replicates;
#             - Determines what plot is displayed;
#             - ANIMATE the plot or EXIT the GUI.
# Parameters: NONE
# Returns:    NULL (invisibly)
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subView <- function()
{
  win        <- .getWinName()                       # Get the current window name
  gvar       <- paste( ".", win, sep="" )           # Global variable name
  act        <- getWinAct()[1]                      # Get last menu window action
  
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local scope
  guiChanges <- list()                              # Make a list for changes.

  .setGuiViewState()

  isExit    <- FALSE
  trackData <- .getTrackingData( .PRJFLD )
  nSims <- .getNumSims( trackData )

  valid     <- .validViewPars(trackData)          # Are guiView parameters valid?
    
  if ( valid )
  {
    # NEXT replicate.
    if ( act=="vwNextRep" )
    {
      # Do not permit iRep > nReps.
      guiChanges$iRep <- min( (iRep + 1), nReps )
    }
  
    # PREVIOUS replicate.
    if ( act=="vwPrevRep" )
    {
      # Do not permit iReps < 1.
      guiChanges$iRep <- max( 1, (iRep-1) )
    }

    # NEXT simulation.
    if ( act=="vwNextSim" )
    {
      # Do not permit iSim > nSim.
      guiChanges$iSim <- min( (iSim+1), nSim )
    }
  
    # PREVIOUS simulation.
    if ( act=="vwPrevSim" )
    {
      # Do not permit iSim < 1.
      guiChanges$iSim <- max( 1, (iSim-1) )
    }
  
    setWinVal( guiChanges, winName=win )              # Set changed GUI parameters.
    guiChanges <- getWinVal( scope="L", winName=win ) # Get GUI parameters, local. 
    
    if ( (act=="vwPrevSim") || (act=="vwNextSim") || (act=="iSim") )
    {
      iRepOld <- iRep
      
      # Load an Rdata working directory containing a list called blob.
      simFile     <- trackData[ guiChanges$iSim, "simFile" ]
      simFolder   <- trackData[ guiChanges$iSim, "simFolder" ]
      simFilePath <- file.path( .PRJFLD, simFolder, simFile )

      cat( "\nMSG (.subView) Loading",simFilePath,"...\n" )    
      load( file=simFilePath )
      assign( "blob", blob, pos=1 )      
      
      guiChanges$nReps         <- blob$ctlList$gui$nReps
      guiChanges$scenarioLabel <- blob$ctlList$gui$scenarioLabel
      guiChanges$mpLabel       <- blob$ctlList$gui$mpLabel
      
      #refPts <- blob$ctlList$refPts
      refPts <- blob$refPtList
      
      guiChanges$B0        <- blob$ctlList$opMod$B0
      guiChanges$ssbFmsy   <- refPts$ssbFmsy
      guiChanges$yieldFmsy <- refPts$yieldFmsy
      guiChanges$Fmsy      <- refPts$Fmsy
      guiChanges$F0        <- refPts$F0
      guiChanges$F01       <- refPts$F01
      guiChanges$F40       <- refPts$F40
      guiChanges$Fmax      <- refPts$Fmax
      guiChanges$Fcra      <- refPts$Fcra
      
      # Check if current old iRep exceeds new nReps, if so iRep=1.
      if ( guiChanges$nReps < iRepOld )
        guiChanges$iRep <- 1
      
      setWinVal( guiChanges )
    }
    
    # Clear the graphics windows.
    if ( act=="vwReset" )
    {
      graphics.off()
      if ( exists( ".guiViewPars", where=1 ) )
      {
        rm( .guiViewPars, pos=1 )
        cat( "\nMSG (.subView) Removed .guiViewPars and restoring defaults...\n" )        
      }

      assign( ".PRJFLD",  .DEFPRJFLD,  pos=1 )
      
      if ( .WHINE > 0 )
        cat( "\nMSG (.subView) Reset Project Folder to ",.PRJFLD, "\n" )
             
      closeWin()  
      on.exit( guiView() )
      isExit <- TRUE            
    }   
    
    # Save plot(s) as an enhanced Windows metafile.
    if ( act=="vwSaveEMF" )
    {
      # Get simulation folder path.
      simFolder <- file.path( .PRJFLD,trackData[iSim,"simName"] )
      
      savePlot( filename=file.path( simFolder, vwPlotType ),
                type="emf", restoreConsole = .RESTORECONSOLE )
    }
              
    # Save plot(s) as a PDF file.
    if ( act=="vwSavePDF" )
    {
      # Get simulation folder path.
      simFolder <- file.path( .PRJFLD,trackData[iSim,"simName"] )
      
      savePlot( filename=file.path( simFolder, vwPlotType ),
                type="pdf", restoreConsole = .RESTORECONSOLE )
    }                 
    
    # Save the View GUI parameters to a global in the working environment.
    if ( (!act=="vwExit") && (act!="vwReset")  && nSims > 0 )
    {
      .doGuiViewPlots( blob, iSim, iRep, simFolder=trackData[ iSim,"simFolder" ] )
    }
    
    if ( act=="vwBatch" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiViewPars", guiInfo, pos=1 )
      cat( "\nMSG (.subView) Exiting... saved GUI parameters to .guiViewPars.\n" )

      closeWin( .getWinName() )       
      on.exit( guiBatch() )
      isExit <- TRUE
    }    
    
    # OPTIONS.
    if ( act=="vwOpt" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiViewPars", guiInfo, pos=1 )
      cat( "\nMSG (.subView) Exiting... saved GUI parameters to .guiViewPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiOpt() )
      isExit <- TRUE
    }  
  
    # PERFormance statistics.
    if ( act=="vwPerf" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiViewPars", guiInfo, pos=1 )
      cat( "\nMSG (.subView) Exiting... saved GUI parameters to .guiViewPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiPerf() )
      isExit <- TRUE
    }  

    # TRACKing GUI.
    if ( act=="vwTrack" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiViewPars", guiInfo, pos=1 )
      cat( "\nMSG (.subView) Exiting... saved GUI parameters to .guiViewPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiTrack() )
      isExit <- TRUE
    }  
  
    # Simulation GUI.
    if ( act=="vwSim" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiViewPars", guiInfo, pos=1 )
      cat( "\nMSG (.subView) Exiting... saved GUI parameters to .guiViewPars.\n" )

      closeWin( .getWinName() )       
      on.exit( guiSim() )
      isExit <- TRUE
    }    
    
    if ( !isExit  & nSims > 0 )
      .setGuiViewState()
    
    # Exit the View GUI, saving the current GUI state as global.
    if ( act=="vwExit" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiViewPars", guiInfo, pos=1 )
      cat( "\nMSG (.subView) Exiting... saved GUI parameters to .guiViewPars.\n" )
      
      closeWin( .getWinName() )
    }
  }     # if valid.
  else
  {
    bringToTop(-1)
    # EXIT the Simulation GUI (leave graphics on).
    if ( act=="vwExit" )
    {
      closeWin()
    }
  }
  return( invisible() )  
}     # END function .subView


# .validViewPars (valid parameters for simulation GUI):
# Purpose:       Check whether the parameters supplied in View GUI are valid.
#                If invalid, display an error message in the R console and
#                clear the invalid field.
#                If it is a correctable field, corrects it in the GUI and does
#                not flag it as invalid
# Parameters:    None
# GUI inputs:    Parameters taken directly from active guiSim. 
# Returns:       TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:   Clears invalid fields
#                Corrects correctable fields (if any)
.validViewPars <- function( trackObj )
{
  # Get the GUI  values and make them local to this function.
  guiInfo    <- getWinVal( scope="L", winName=.getWinName() )
  guiChanges <- list()                  
  isValid    <- TRUE

  # Simulation index must be greater than 1 and less than nrow(trackObj).  
  if ( is.na(iSim) || (iSim < 1) || (iSim > nrow(trackObj)) )
  {
    cat( "\nWARNING (.validViewPars) Simulation ID must be 0 < iSim <= nSim.\n" )
    guiChanges$iSim <- 1
    isValid         <- FALSE
  }
  
  # Replicate index must be greater than 1 and less than nReps for current sim.
  if ( is.na(iRep) || (iRep < 1) || ( iRep > nReps ) )
  {
    cat( "\nWARNING (.validViewPars) Replicate ID must be 0 < iRep <= ",nReps,".\n" )
    guiChanges$iRep <- 1
    isValid         <- FALSE
  }
  
  if ( is.na(vwPower) || vwPower <= 0 )
  {
    cat( "\nWARNING (.validViewPars) Bubble expansion power must be positive.\n" )
    guiChanges$vwPower <- 1
  }
  
  # Set the changes into the GUI.  
  setWinVal( guiChanges, winName=.getWinName() )
  return( isValid )
}     # END function .validViewPars

# .doGuiViewPlots  (wrapper to get guiView parameters and call the required plot)
# Purpose:      Calls the plot specified by vwPlotType that is returned from
#               view GUI (guiView).
# Parameters:   Object with saved simulation results and sim and rep number 
#                 to be plotted
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.doGuiViewPlots <- function( obj, iSim=1, iRep=1, simFolder="" )
{
  win     <- .getWinName()                       # Get active window name.
  guiInfo <- getWinVal(scope="L", winName=win )  # GUI information local scope.
  act     <- getWinAct()[1]                      # Last menu window action
  
  nReps     <- blob$ctlList$gui$nReps           # Number of replicates.
  methodId  <- blob$ctlList$mp$assess$methodId  # Assessment method ID code.
  popDynMod <- FALSE
  if ( methodId > 2 )
    popDynMod <- TRUE

  # Save par settings that can be restored (some are readonly).
  oldpar <- par( no.readonly=TRUE )

  if ( .PLTMSG )
    cat( "\nMSG (.doGuiViewPlots) Calling plot function ", vwPlotType, "\n" )

  # xAxis and yAxis arguements for each plot contain the user-specified minimum
  # and maximum axis values that are read in from guiView window
  # (K. Holt; 18-Dec-09)

  if ( vwPlotType == "vwSpecs" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
       
    .plotCtlPars( blob$ctlPars )
  }
  
  else if ( vwPlotType == "vwDesign" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- 6
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
    
    xLim <- NULL
    yLim <- NULL
  
    .plotDesign( blob$ctlList, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
      xLim=xLim, yLim=yLim, useYears=vwYrs ) )   
  }
  
  else if ( vwPlotType == "vwRefPtPlots" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[1] <- 3
      myMar[2] <- 3
      myMar[3] <- 2.5
      myMar[4] <- 3
      par( oma=.OMA, mar=myMar, mfrow=c(3,2) )
    }
    
    checked          <- rep( TRUE, 6 )
    names( checked ) <- c( "Fmsy",  "F0",  "F01",  "F40",  "Fmax",  "Fcra"  )

    # The equilibrium ref pt plots are designed to take a list of blobs for
    # overlaying different simulations.  So when passing just one, the input
    # has to be rendered as the first element in a list.  Hence this stuff.
    # Avoids two separate sets of plotting functions.
    
    tmp <- list()
    tmp$refPtList <- blob$refPtList

    .plotSimRefPoints( list( tmp ), gfx=list( annotate=TRUE, checked=checked,
       doLegend=vwLegend, setXaxis=vwSetXaxis, setYaxis=vwSetYaxis ) )    
  }
  
  else if ( vwPlotType == "bioCatF" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- myMar[2] + 1
      par( oma=.OMA, mar=myMar, mfrow=c(3,1) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
     # Need to define input axes as a matrix bc animation this plot has 3 panels:
      yLim <- matrix( c(vwMinBio, vwMaxBio, vwMinCat, vwMaxCat, vwMinF, vwMaxF),
                nrow=3, ncol=2, byrow=T )

    .plotBtDtFt( blob, iSim, iRep, gfx=list( animate=vwAnimate, annotate=vwAnnotate,
                 doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
                 useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "bioCatR" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- myMar[2] + 1
      par( oma=.OMA, mar=myMar, mfrow=c(3,1) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
     # Need to define input axes as a matrix bc animation this plot has 3 panels:
     yLim <- matrix( c(vwMinBio, vwMaxBio, vwMinCat, vwMaxCat, vwMinRec, vwMaxRec),
               nrow=3, ncol=2, byrow=T )

    .plotBtDtRt( blob, iSim, iRep, gfx=list( animate=vwAnimate, annotate=vwAnnotate,
                 doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
                 useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "biomass" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinBio,vwMaxBio )  

    .plotBt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
             showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "survey" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr, vwMaxYr )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinIdx, vwMaxIdx )

    .plotIt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
             showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }  
  
  else if ( vwPlotType == "biosurvey" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xAxis <- c( vwMinBio, vwMaxBio )
    yAxis <- c( vwMinBio, vwMaxBio )
    .plotItBt( blob, iSim, iRep, xAxis, yAxis, vwAnnotate, vwProj, 
                     vwSetYaxis, vwSetXaxis )
  }
  
  else if ( vwPlotType == "catch" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr, vwMaxYr )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinCat, vwMaxCat )

    .plotDt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=TRUE,
             showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "fmort" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr, vwMaxYr )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinF, vwMaxF )
      
    .plotFt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
                    showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "natMort" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr, vwMaxYr )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinM, vwMaxM )
      
    .plotMt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
                    showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }  
  
  else if ( vwPlotType == "vwHCR" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- myMar[2] + 1
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
        
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinBio,vwMaxBio )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinF,   vwMaxF )

    hcrType <- blob$ctlList$mp$hcr$hcrType

    if ( hcrType == "variableF" )
      .plotHCRmpVariableF( blob, iSim, iRep, gfx=list( animate=FALSE, annotate=vwAnnotate,
                      doGrid=vwGrid, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
                      
    else if ( hcrType == "constantF" )
      .plotHCRmpConstantF( blob, iSim, iRep, gfx=list( animate=FALSE, annotate=vwAnnotate,
                      doGrid=vwGrid, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
                      
    else if ( hcrType == "declineRisk" )
    {
      if ( vwAuto )
      {
        myMar <- .MAR
        myMar[3] <- 2
        par( oma=.OMA, mar=myMar, mfcol=c(3,2) )
      }
        
      yLim <- NULL    
      .plotHCRviewDeclineRisk( blob, iSim, iRep, 
        gfx=list( animate=FALSE, annotate=vwAnnotate, doLegend=vwLegend,
                  showProj=vwProj, xLim, yLim ) )
    }
  }
  
  else if ( vwPlotType == "vwRefPtSeries" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- myMar[2] + 1
      par( oma=.OMA, mar=myMar, mfrow=c(2,1) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinBio,vwMaxBio )

    yLim2 <- NULL
    if ( vwSetYaxis )
      yLim2 <- c( vwMinF,vwMaxF )
    
    .plotRefPtSeries( blob, iSim, iRep,
      gfx=list( annotate=vwAnnotate, colorZones=FALSE, doLegend=vwLegend,
      xLim=xLim, yLim=yLim, yLim2=yLim2, useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "numbers" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <-  NULL  
    if ( vwSetYaxis )
      yLim <- c( vwMinNum, vwMaxNum )
      
    .plotNt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
             showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }
  
  else if ( vwPlotType == "obssurvey" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinBio,vwMaxBio )  

    .plotObsSurvey( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                    doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
                    useYears=vwYrs ) )
  }
  
  # ARK (31-Aug-13) This function is currently not called from GUI.
  else if ( vwPlotType == "foo" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinBio,vwMaxBio )

    .plotModelBtFit( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                     doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
                     useYears=vwYrs ) ) 
  }
  
  else if ( vwPlotType == "recruits" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinRec,vwMaxRec )

    .plotRt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, showProj=vwProj, 
                                     xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }
  
  else if ( vwPlotType =="recspawn" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinBio,vwMaxBio )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinRec,vwMaxRec )

    .plotRtBt( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }
  
  else if ( vwPlotType =="mpRecSpawn" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinBio,vwMaxBio )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinRec,vwMaxRec )

    if ( methodId == .CAAMOD )
      .plotRecSpawnMP( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
         doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
    else
    {
      cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," cannot plot.\n" )
      .plotStatus( paste( "Method is ",.METHODLAB[methodId]," cannot plot" ) )
      alarm()
    }
  }  
  
  #RF##########################################
  else if ( vwPlotType =="vwFisherySelFit" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinAge,vwMaxAge )
        
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( 0,1 )
  
      if ( methodId == .CAAMOD )
        .plotMPSelAtAge(blob, type="Fishery", gfx=list( annotate=vwAnnotate,
           doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
      else
      {
        cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," cannot plot.\n" )
        .plotStatus( paste( "Method is ",.METHODLAB[methodId]," cannot plot" ) )
        alarm()
      }
  }  
  
   else if ( vwPlotType =="vwSurveySelFit" )
      {
        if ( vwAuto )
          par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
      
        xLim <- NULL
        if ( vwSetXaxis )
          xLim <- c( vwMinAge,vwMaxAge )
          
        yLim <- NULL
        if ( vwSetYaxis )
          yLim <- c( 0,1 )
    
        if ( methodId == .CAAMOD )
          .plotMPSelAtAge( blob, type="Survey", gfx=list( annotate=vwAnnotate,
             doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
        else
        {
          cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," cannot plot.\n" )
          .plotStatus( paste( "Method is ",.METHODLAB[methodId]," cannot plot" ) )
          alarm()
        }
  }  
  
  ############################################
  
  else if ( (vwPlotType == "vwRetroExpBt") )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- matrix(rep(c(vwMinBio,vwMaxBio), 3), nrow=2, ncol=2, byrow=T)

    .plotRetroFits( blob, iSim, iRep, varName="exploitBt", gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }
  
  else if ( (vwPlotType == "vwRetroExpBtS") )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- matrix(rep(c(vwMinBio,vwMaxBio), 3), nrow=2, ncol=2, byrow=T)

    .plotRetroFits( blob, iSim, iRep, varName="exploitBtS", gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }  
  
  else if ( (vwPlotType == "vwRetroSpawnBt") )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- matrix(rep(c(vwMinBio,vwMaxBio), 3), nrow=2, ncol=2, byrow=T)

    .plotRetroFits( blob, iSim, iRep, varName="spawnBt", gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }  
 
  else if ( (vwPlotType == "vwRetro1") )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[3] <- 2
      par( oma=.OMA, mar=myMar, mfrow=c(2,1) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL

    .plotRetroStat1( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
  }  
  
  else if ( vwPlotType == "vwFvsSSB" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- myMar[2] + 1
      myMar[3] <- myMar[3] + 1
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinBio,vwMaxBio )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinF,vwMaxF )
      
    .plotFvsSSB( blob, iSim, iRep, phase=FALSE, gfx=list( annotate=vwAnnotate,
                 doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }
  
  else if ( vwPlotType == "vwPhaseOM" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[2] <- myMar[2] + 1
      myMar[3] <- myMar[3] + 1
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( 0, 2 )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( 0, 2 )
      
    .plotFvsSSB( blob, iSim, iRep, phase=TRUE, gfx=list( annotate=vwAnnotate,
                 doGrid=vwGrid, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }  
  
  #----------------------------------------------------------------------------#
  # Age Structure Plots                                                        #
  #----------------------------------------------------------------------------#
   #####################################################################################
  #FISHERY
  #####################################################################################
 
  else if ( vwPlotType=="vwAgeBubOM" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinAge,vwMaxAge )
      
    # Proportions at age are stored in an array[nRep,nAge,nYear].
    uat <- blob$om$uat[iRep,,]     # Operating model age proportions fishery
        
    # True age proportions replicate.
    .plotAgeBubblesOM( uat, pltTitle="Operating Model Fishery Proportions At Age",
      pwr=vwPower,
      gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
      xLim=xLim, yLim=yLim ) )
  }
  
  else if ( vwPlotType=="vwAgeBarOM" )
  {
    if ( vwAuto )
    {
      # Controlled inside .plotAgeFreq.
    }
    
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinAge,vwMaxAge )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinP,vwMaxP )
    
    # Proportions at age are stored in an array[nRep,nAge,nYear].
    uat <- blob$om$uat[iRep,,]         # Operating model age proportions.
    oat <- blob$mp$assess$pat[iRep,,]  # Observed age proportions.
      
    .plotAgeFreq( patBars=uat, pltTitle="Operating Model Fishery Proportions At Age",
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        vwProj=vwProj, xLim=xLim, yLim=yLim, yrs=vwYrs ) )
  }  
  
  else if ( vwPlotType=="vwAgeBubObs" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
        
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c(vwMinYr,vwMaxYr )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinAge,vwMaxAge )
      
    # Proportions at age are stored in an array[nRep,nAge,nYear].
    oat <- blob$mp$data$pat[iRep,,]     # Observed age proportions.

    # obsAgeProp for each replicate.
    .plotAgeBubblesOM( oat, pltTitle="Observed Fishery Proportions At Age",
      pwr=vwPower,
      gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
      xLim=xLim, yLim=yLim ) )
  }
  
  else if ( vwPlotType=="vwAgeBarObs" )
  {
    if ( vwAuto )
    {
      # Controlled inside .plotAgeFreq.
    }
        
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinAge,vwMaxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinYr,vwMaxYr )
    
    # Proportions at age are stored in an array[nRep,nAge,nYear].
    oat <- blob$mp$data$pat[iRep,,]  # Observed age proportions.
        
    .plotAgeFreq( patBars=oat, pltTitle="Observed Fishery Proportions At Age",
      gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
      vwProj=vwProj, xLim=xLim, yLim=yLim, yrs=vwYrs ) )  
  }    
  
  else if ( vwPlotType=="vwAgeBub" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c(vwMinYr,vwMaxYr )
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinAge,vwMaxAge )
      
    # Proportions at age are stored in an array[nRep,nAge,nYear].
    oat <- blob$mp$data$pat[iRep,,]     # Observed age proportions.
    pat <- blob$mp$assess$pat[iRep,,]   # Predicted age proportions.
    
    # obsAgeProp and predAgeProp for each replicate.
    if ( methodId == .CAAMOD )
      .plotAgeBubbles( oat, pat, pltTitle="Observed & Predicted Fishery Proportions At Age",
        pwr=vwPower,
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        xLim=xLim, yLim=yLim ) )
    else
    {
      cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," - no predicted ages.\n" )
      alarm()
    }
  }  
  
  else if ( vwPlotType=="vwAgeBar" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinAge,vwMaxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinYr,vwMaxYr )
    
    oat <- blob$mp$data$pat[iRep,,]    # Observed age proportions.
    pat <- blob$mp$assess$pat[iRep,,]  # Predicted age proportions.
    
    if ( methodId == .CAAMOD )
    {    
      .plotAgeFreq( oat, pat, pltTitle="Observed & Predicted Fishery Proportions At Age",
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        vwProj=vwProj, xLim=xLim, yLim=yLim, yrs=vwYrs ) )
    }  
    else
    {
      cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," - no predicted ages.\n" )
      alarm()
    }      
  }
  #####################################################################################
  #SURVEY
  #####################################################################################
    else if ( vwPlotType=="vwAgeBubOMS" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinYr,vwMaxYr )
        
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinAge,vwMaxAge )
        
      # Proportions at age are stored in an array[nRep,nAge,nYear].
      uat <- blob$om$uatS[iRep,,]     # Operating model age proportions.
      
      # True age proportions replicate.
      .plotAgeBubblesOM( uat, pltTitle="Operating Model Survey Proportions At Age",
        pwr=vwPower,
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        xLim=xLim, yLim=yLim ) )
    }
    
    else if ( vwPlotType=="vwAgeBarOMS" )
    {
      if ( vwAuto )
      {
        # Controlled inside .plotAgeFreq.
      }
        
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinAge,vwMaxAge )
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinYr,vwMaxYr )
      
      # Proportions at age are stored in an array[nRep,nAge,nYear].
      uat <- blob$om$uatS[iRep,,]         # Operating model age proportions.
      oat <- blob$mp$assess$patS[iRep,,]  # Observed age proportions.
          
      .plotAgeFreq( patBars=uat, pltTitle="Operating Model Survey Proportions At Age",
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        vwProj=vwProj, xLim=xLim, yLim=yLim, yrs=vwYrs ) )  
    }  
    
    else if ( vwPlotType=="vwAgeBubObsS" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
          
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c(vwMinYr,vwMaxYr )
      
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinAge,vwMaxAge )
        
      # Proportions at age are stored in an array[nRep,nAge,nYear].
      oat <- blob$mp$data$patS[iRep,,]     # Observed age proportions.
  
      # obsAgeProp for each replicate.
      .plotAgeBubblesOM( oat, pltTitle="Observed Survey Proportions At Age",
        pwr=vwPower,
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        xLim=xLim, yLim=yLim ) )
    }
    
    else if ( vwPlotType=="vwAgeBarObsS" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
          
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinAge,vwMaxAge )
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinYr,vwMaxYr )
      
      # Proportions at age are stored in an array[nRep,nAge,nYear].
      oat <- blob$mp$data$patS[iRep,,]  # Observed age proportions.
          
      .plotAgeFreq( patBars=oat, pltTitle="Observed Survey Proportions At Age",
        gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
        vwProj=vwProj, xLim=xLim, yLim=yLim, yrs=vwYrs ) )  
    }    
    
    else if ( vwPlotType=="vwAgeBubS" )
    {
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c(vwMinYr,vwMaxYr )
      
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinAge,vwMaxAge )
        
      # Proportions at age are stored in an array[nRep,nAge,nYear].
      oat <- blob$mp$data$patS[iRep,,]     # Observed age proportions.
      pat <- blob$mp$assess$patS[iRep,,]   # Predicted age proportions.
      
      # obsAgeProp and predAgeProp for each replicate.
      if ( methodId == .CAAMOD )
        .plotAgeBubbles( oat, pat, pltTitle="Observed & Predicted Survey Proportions At Age",
          pwr=vwPower,
          gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
          xLim=xLim, yLim=yLim ) )
      else
      {
        cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," - no predicted ages.\n" )
        alarm()
      }
    }  
    
    else if ( vwPlotType=="vwAgeBarS" )
    {
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinAge,vwMaxAge )
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinYr,vwMaxYr )
      
      oat <- blob$mp$data$patS[iRep,,]    # Observed age proportions.
      pat <- blob$mp$assess$patS[iRep,,]  # Predicted age proportions.
      
      if ( methodId == .CAAMOD )
      {    
        .plotAgeFreq( oat, pat, pltTitle="Observed & Predicted Survey Proportions At Age",
          gfx=list( annotate=vwAnnotate, doLegend=vwAnnotate,
          vwProj=vwProj, xLim=xLim, yLim=yLim, yrs=vwYrs ) )
      }  
      else
      {
        cat( "\nMSG (.doGuiViewPlots) Method is ",.METHODLAB[ methodId ]," - no predicted ages.\n" )
        alarm()
      }      
  }
  ####################################################################################
  # Plots that require a population dynamics method in MP.

  else if ( vwPlotType == "refPtEsts"  )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
    {
      yLim <- matrix( c(vwMinBmsy, vwMaxBmsy, vwMinFmsy, vwMaxFmsy), 
                nrow=2, byrow=T )
    }
  
    if ( popDynMod )
      .plotRefPtEsts( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                    doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
                    useYears=vwYrs ) )
    else
      .plotStatus( paste( "Cannot plot",vwPlotType,"\nNo population dynamics model in MP" ) )
  }
  
  else if ( vwPlotType == "parEsts"  )
  {
    if ( vwAuto )
    {
      par( oma=c(2,1,2,1), mar=c(2,4,2,1), mfrow=c(2,2) )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
    {
      #yLim <- matrix( c(vwMinBmsy, vwMaxBmsy, vwMinFmsy, vwMaxFmsy), 
      #          nrow=2, byrow=T )
    }
  
    if ( popDynMod )
      .plotParEsts( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                    doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
                    useYears=vwYrs ) )
    else
      .plotStatus( paste( "Cannot plot",vwPlotType,"\nNo population dynamics model in MP" ) )
  }  
  
  else if ( vwPlotType == "vwRetro1"  )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
    {
    }
  
    .plotStatus( paste( vwPlotType,"Not Implemented" ) )
  }
  
  else if ( vwPlotType == "vwRetro2"  )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
    {
    }
  
    .plotStatus( paste( vwPlotType,"Not Implemented" ) )
  }      
  
  else if ( vwPlotType == "parCor" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinBmsy,vwMaxBmsy ) 
    
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinFmsy,vwMaxFmsy )
    
    if ( popDynMod )           
      .plotParCor( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
                   showProj=vwProj, xLim=xLim, yLim=yLim ) )
    else
      .plotStatus( paste( "Cannot plot",vwPlotType,"\nNo population dynamics model in MP" ) )                 
  }
  
  else if ( vwPlotType == "paAdj" )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[3] <- 3
      myOma <- .OMA
      myOma[2] <- 4
      par( oma=myOma, mar=myMar, mfrow=c(3,1) )
    }

    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- matrix( c( vwMinBio,vwMaxBio,vwMinF,vwMaxF,vwMinCat,vwMaxCat),
                         nrow=3, ncol=2, byrow=TRUE )  
  
    if ( popDynMod && blob$ctlList$mp$hcr$hcrType!="declineRisk" )      
      .plotPaAdj( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
         doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim, useYears=vwYrs ) )
    else
      .plotStatus( paste( "Cannot plot",vwPlotType,
                   "\nNo population dynamics model or rule is decline risk" ) )       
  }
  
  else if ( vwPlotType == "vwDecline" )
  {
    if ( vwAuto )
    {
      myOma <- .OMA
      myOma[3] <- 1
      myMar <- .MAR
      myMar[3] <- 2
      par( oma=myOma, mar=myMar, mfrow=c(2,1) )    
    }

    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
     
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- matrix( c(vwMinBio,vwMaxBio,vwMinCat,vwMaxCat), nrow=2, ncol=2, byrow=T )

    if ( blob$ctlList$mp$hcr$hcrType=="declineRisk" )
    {
      .plotDeclineRisk( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, doLegend=vwLegend,
                                showProj=vwProj, xLim=xLim, yLim=yLim ) )
    }
    else
      .plotStatus( "No plot available, HCR type is not Decline Risk" )      
  }  
  
  else if ( vwPlotType == "vwDiagsRep" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(4,1) )
    
    xLim <- NULL
    yLim <- NULL
    
    if ( popDynMod )    
      .plotDiagnostics( blob, iSim=iSim, iRep=iRep,
         gfx=list( annotate=vwAnnotate,doLegend=vwLegend, xLim=xLim, yLim=yLim,
                   useYears=vwYrs ) )
    else
      .plotStatus( paste( "Cannot plot",vwPlotType,"\nNo population dynamics model in MP" ) )
  }
  
  else if ( vwPlotType == "vwDiagsAll" )
  {
    if ( vwAuto )
    {
      par ( oma=.OMA, mar=.MAR, mfrow=c(1,1) )    
    }    
    
    if ( popDynMod )
      .plotDiagSim( blob,
         gfx=list( annotate=vwAnnotate, doLegend=vwLegend, xLim=NULL, yLim=NULL,
                   useYears=vwYrs ) )
    else
      .plotStatus( paste( "Cannot plot",vwPlotType,"\nNo population dynamics model in MP" ) )
  }  
  
  else
    .plotStatus( paste( vwPlotType,"not implemented" ) )
  
  #----------------------------------------------------------------------------#  
  
  .addStamp( iSim, iRep, nReps, simFolder=simFolder,
             scenario=blob$ctlList$gui$scenarioLabel,
             procedure=blob$ctlList$gui$mpLabel,
             showFile=TRUE, outLine=NULL,
             side=1, col=.COLSTAMP, cex=.CEXSTAMP )
  
  # Restore graphics settings.
  par( oldpar )
  return( invisible() )
}      # END function .doGuiViewPlots.