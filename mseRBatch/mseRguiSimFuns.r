# To Do.

# 3. Make sure you document what has to be done to update simCtlFile.  You keep
#    screwing it up.
# 7. Utility function to generate ctlFile from current menus.
# 8. Check to ensure ssbF0 is coming back from calcRefPoints into obj$refPts.
# 9. Documentation to include (a) survey freq, (b) assessment freq, (c) ref pt.
#    frequency from HCR.  Wow that's a lot of design elements.
# 11. Fix guiTrack when trackData is null.
# 12. Reset could set PRJFLD, CTLFILE to current working dir, defaults, moving
#     existing project folders to an archive subfolder?
# 13. Check when ref points are rounded - should be saved full precision.
# 14. When observed ages are not available, do not plot them on bubbles and bars.
#     It appears they are getting used correctly by CAA model when on/off.
# 20. Why so many writes when Method GUI pops?
# 21. Add plots for prior distributions.
#
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
#-- mseRguiSimFuns.r: An mseR module that provides graphical user interface  --#
#-- capabilities for feedback simulation for fisheries management strategy   --#
#-- evaluation.  This is the main GUI used to set up the operating model and --#
#-- management procedure parameterization.                                   --#
#--                                                                          --#
#-- Usage:        guiSim()                                                   --#
#-- Side Effects: Invokes GUI for running the feedback simulations results.  --#
#--                                                                          --#
#-- Flow: guiSim  -> .mseRsetupSim -> .wdSetup -> createWin -> .subSim       --#
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
# 4. Very inelegant crashes can occur if the 3 is called when GUI open.  #
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
#                                                                              #
# Platform independent file and folder access functions worth remembering:     #
#                                                                              #
# dir.create     file.create   file.exists   file.remove    file.rename        #
# file.append    file.copy     file.info     path.expand    list.files         #
# basename       file.path     normalizePath                                   #
#                                                                              #
# Compiling ADMB TPL files:                                                    #
#                                                                              #
# If your *.tpl files is named foo.tpl, then do this for 64-bit executable:    #
# (1) tpl2cpp foo                                                              #
# (2) adcomp64 foo                                                             #
# (3) adlink64 foo                                                             #
#                                                                              #
# NOTE: The 64-bit VCC compiler does not like #include iostream.h              #
#                                                                              #
# GUI Functions:                                                               #
#                                                                              #
# guiSim              : Run mseR main GUI to access operating model. MP GUIs.  #
# mseRsetupSim        : mseR setup for initalization of parameters and guiSim. #
# .subSim             : Processes guiSim  submit actions, e.g., buttons.       #
# .validDataPars      : Checks for valid parameters for guiSim.                #
# .validHcrPars       : Checks for valid parameters for HCR GUI.               #
# .validMethodPars    : Checks for valid parameters for Method GUI.            #
# .validOpModPars     : Checks for valid parmeters for operating model GUI.    #
# .validSimPars       : Checks for valid parameters for guiSim GUI.            #
# .setGuiDataState    : Ghosts/disables widgets for Data GUI.                  #
# .setGuiMethodState  : Ghosts/disables widgets for Method GUI.                #
# .setGuiHcrState     : Ghosts/disables widgets for HCR GUI.                   #
# .setGuiOpModState   : Ghosts/disables widgets for OpMod GUI.                 #
# .doGuiSimPlots      : Wrapper to get parameters and call required plot.      #
# .setGuiSimState     : Ghosts/disables widgets for guiSim GUI.                #
# .formatRefPts       : Rounds reference points to specified digits.           #
# .getPdecline        : Assembles matrix from HCR gui containing P(decline).   #
# .calcIndexCV        : Calculates CV of stock index by period (OUTDATED)      #
#------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------##
#-- mseR guiSim Functions                                                   --##
#-----------------------------------------------------------------------------##

# guiSim      (Run mseR simulation: operating model and management procedure GUI)
# Purpose:    Set, run the Operating Model and Management Procedures parameters.
# Parameters: None
# Returns:    NULL (invisibly)
guiSim <- function()
{
  return( .mseRsetupSim("mseRguiSim") )
}     # END function guiSim


# .mseRsetupSim  (mseR setup for GUI creation for guiSim)
# Purpose:    Set up and run the specified GUI
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.mseRsetupSim <- function(win)
{
  # Get the required libraries.
  require( MCMCpack )                # For inverse gamma density function.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  require( snow )                    # For parallel processing.
  require( tools )                   # For file handling.
  
  options( useFancyQuotes=FALSE )    # Required for saveSimPars function.
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  # Copy the GUI window description files and any .exe files to temp directory.
  wkDir <- .wdSetup()
  
  if ( .WHINE > 0 )
    cat( "\nMSG (.mseRsetupSim) Working directory setup in ",wkDir,"\n" )   
  
  closeWin()                         # Close all open windows to prevent crashes.
  graphics.off()                     # Turn off any open graphics.
  
  goMenu    <- TRUE                  # Assume menu cannot be created.

  .mseRinitProject( wkDir )          # Initialize mseR project, paths, options.

  ctlPars <- NULL
  
  # If required, initialize .CTLFILE to default filename in working directory.
  if ( !exists( ".CTLFILE" ) )
  {
    .CTLFILE <- file.path( wkDir,.DEFCTLFILE )
    assign( ".CTLFILE", .CTLFILE, pos=1 )
  }
  
  # If .CTLFILE initialized, but file does not exist, then set to default.
  if ( !file.exists( .CTLFILE ) )
  {
    # If .CTLFILE initialized, but file does not exist, then set to default.  
    .CTLFILE <- file.path( wkDir,.DEFCTLFILE )
    assign( ".CTLFILE", .CTLFILE, pos=1 )    
  }
  else
  {
    # File exists, make sure it is copied to working directory if required.
    fPath <- dirname( .CTLFILE )
    if ( fPath!=wkDir )
    {
      ctlFile <- file.path( wkDir, basename(.CTLFILE) )
      file.copy( .CTLFILE, ctlFile )
      if ( .WHINE > 0 )
        cat( "\nMSG (mseRsetupSim) Simulation control file",.CTLFILE,"copied to",
             ctlFile,"\n" )
      .CTLFILE <- ctlFile
      assign( ".CTLFILE",.CTLFILE, pos=1 )
    }    
  }
  
  ctlPars <- readParFile( parFile=.CTLFILE )
  ctlList <- .createList( ctlPars )
  
  if ( .WHINE > 3 )
    print( ctlPars )

  if ( .WHINE > 0 )
    cat( "\nMSG (.mseRsetupSim) Loaded simulation control parameters from ",
         .CTLFILE,"\n" )
  assign( "ctlPars", ctlPars, pos=1 )
  update <- TRUE

  if ( goMenu )
  {
    # Initialize the GUI from the description file using PBSmodelling createWin.
    createWin( paste( wkDir, "/", win, "Win.txt", sep="" ) )
    
    # Get the GUI parameters and make scope local to this function.
    win        <- .getWinName()    
    guiInfo    <- getWinVal( scope="L", winName=win )
    guiChanges <- list()
    
    # Initialize guiSim action.
    if ( win == "mseRguiSim" )
    {
      if ( exists( ".guiSimPars" ) )
      {
        setWinVal( .guiSimPars )
        guiInfo <- getWinVal( scope="L", winName=win )
      }        

      # Update the GUI.

      # Update the scenarioLabel and mpLabel
      guiChanges$scenarioLabel <- "S1"
      guiChanges$mpLabel       <- "MP1"
        
      # Update the reference points.  If there were no saved gui parameters then
      # the gui text file default values are displayed.  If there is a saved
      # gui parameter list then the saved list is displayed.  In either case,
      # an update of the reference points from the simulation control file is done:
      
      refPts <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE, x=40 ) )
      guiChanges <- c( guiChanges, .formatRefPts( refPts ) )
      
      guiChanges$simCtlFile <- .CTLFILE
      
      setWinVal( guiChanges, winName=win )      
     
      # (2) Make sure ctlPars is updated.
      getWinVal( scope="L", winName=win )
      ctlPars <- .guiToCtl( ctlPars, prefix="gui" )
      ctlPars <- .guiToCtl( ctlPars, prefix="refPts" )
      assign( "ctlPars", ctlPars, pos=1 )
      saveSimPars( ctlPars, parFile=.CTLFILE, overWrite=TRUE )

      .setGuiSimState( .getSimStateList( ctlList ) )

      # (3) Save the new gui pars to working directory.      
      assign( ".guiSimPars",guiInfo,pos=1 )
      .doGuiSimPlots( ctlPars )
      focusWin( win )
    }
  }
  else
    cat( "\nERROR (.mseRsetupSim): Menu creation not possible... exiting.\n ")

  return( invisible() )
}     # END function .mseRsetupSim


# .subSim     (Processes guiSim submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiSim program flow:
#             - Attempts to check validity of the GUI parameters;
#             - Determines what plot is displayed;
#             - Saves a global copy of simGuiPars to the working directory;
#             - Processes button actions:
#             - RUN the feedback loop;
#             - EXIT the GUI.
# Parameters: NONE
# Returns:    NULL (invisibly).
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subSim <- function()
{
  win        <- .getWinName()                       # Get current window name
  gvar       <- paste( ".", win, sep="" )           # Global variable name
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local scope
  act        <- getWinAct()[1]                      # Get last window action
  
  guiChanges    <- list()                           # List of changes to GUI
  subGuiChanges <- list()                           # List of changes to sub GUI

  isExit <- FALSE                                   # Exiting the GUI?
  update <- FALSE                                   # Update of GUI needed?

  ctlList <- .createList( ctlPars )                 # Get the parameter list.
  
  #----------------------------------------------------------------------------#
  #-- Buttons to pop feedback sub menus (OM, Data, Method, HCR), Tracking,   --#
  #----------------------------------------------------------------------------#

  if ( win=="mseRguiSim" )
  {
    exitButton <- FALSE
    if ( act=="simExit" )
      exitButton <- TRUE
    if ( act=="simBatch" )
      exitButton <- TRUE
    if ( act=="simOpt" )
      exitButton <- TRUE
    if ( act=="simPerf" )
      exitButton <- TRUE      
    if ( act=="simTrack" )
      exitButton <- TRUE
    if ( act=="simView" )
      exitButton <- TRUE
      
    if ( act=="simReset" )
      exitButton <- TRUE
      
    valid <- TRUE
    if ( exitButton==FALSE )
      valid <- .validSimPars()
    
    simName <- scenarioLabel
    
    if ( valid )
    {
      if ( act=="simEdit" )
      {
        # Load ctlPars from file and save to working directory.
        ctlPars <- readParFile( parFile=.CTLFILE )
        assign( "ctlPars", ctlPars, pos=1 )          
      
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Calling mseRguiCtlPars...\n" )    
        createWin( paste( getwd(),"/","mseRguiCtlPars","Win.txt",sep="" ) )
        focusWin( .getWinName() )
      }     # endif act==simEdit
  
      if ( act=="simOM" )
      {
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Calling mseRguiOpMod...\n" )
    
        createWin( paste( getwd(), "/", "mseRguiOpMod", "Win.txt", sep="" ) )
        win <- .getWinName()
        focusWin( win )
      
        # Need to load parameters from ctlPars into GUI.
        .ctlToGui( ctlPars, prefix="opMod" )
      
        # Check to see if parameters are valid.
        if ( .validOpModPars() )
        {
          # Update reference points.
          subGuiInfo    <- getWinVal( scope="L", winName=win )
          subGuiChanges <- list()
          refPts <- calcRefPoints( subGuiInfo, rpList=list( FALL=TRUE, x=40 ) )
          subGuiChanges <- c( subGuiChanges, .formatRefPts( refPts ) )      
      
          # Update Operating Model GUI.
          setWinVal( subGuiChanges, winName=win )
      
          # Update GUI sim reference points.
          setWinVal( subGuiChanges, winName="mseRguiSim" )
          
          # Set GUI state.
          .setGuiOpModState()
        }
        else
        {
          # Bring the console with error reporting to top, report to user.
          alarm()
          bringToTop(-1)
          cat( "\nACTION (.subSim): Check operating model parameters to resolve errors.\n" )      
        }
      }     # endif act==simOM.
  
      if ( act == "simData" )
      {
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Calling mseRguiData...\n" )    
        createWin( paste( getwd(), "/", "mseRguiData", "Win.txt", sep="" ) )
        focusWin( .getWinName() )
      
        # Need to load parameters from ctlPars into GUI.
        .ctlToGui( ctlPars, prefix="mp$data" )
      
        if ( .validDataPars() )
        {
          .setGuiDataState()
          subGuiInfo <- getWinVal( scope="L", winName=.getWinName() )
        }
        else
        {
          # Bring the console with error reporting to top, report to user.
          alarm()
          bringToTop(-1)
          cat( "\nACTION (.subSim): Check MP data parameters to resolve errors.\n" )      
        }      
      }     # endif act==simData
  
      if ( act == "simMethod" )
      {
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Calling mseRguiMethod...\n" )
        createWin( paste( getwd(), "/", "mseRguiMethod", "Win.txt", sep="" ) )
        focusWin( .getWinName() )
      
        # Need to load parameters from ctlPars into GUI.
        # Don't do this here, because mseRguiMethod contains a notebook widget
        # and will make a call to .subSim in initialization that will do the copy.
      
        .ctlToGui( ctlPars, prefix="mp$assess" )
      
        if ( .validMethodPars( tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT ) )
        {
          .setGuiMethodState()
          subGuiInfo <- getWinVal( scope="L", winName=.getWinName() )
        }
        else
        {
          # Bring the console with error reporting to top, report to user.
          alarm()
          bringToTop(-1)
          cat( "\nACTION (.subSim): Check MP Method parameters to resolve errors.\n" )        
        }
      }     # endif act==simMethod
  
      if ( act=="simHCR" )
      {
        if ( .WHINE > 0 )
          cat( "\nMSG (.subSim) Calling mseRguiHcr...\n" )    
        createWin( paste( getwd(), "/", "mseRguiHcr", "Win.txt", sep="" ) )
        focusWin( .getWinName() )
      
        # Need to load parameters from ctlPars into GUI
        .ctlToGui( ctlPars, prefix="mp$hcr" )

	# KRH added; need to get Bexp time series from OM to pass to .validHcrPars
	# 23-Aug-2013
        mpObj   <- .createMP( ctlList )
        initPop <- .initPop( mpObj )

        if ( .validHcrPars(tMP=ctlList$opMod$tMP, Bexp=initPop$om$Bexp) )
        {
          .setGuiHcrState( methodId=ctlList$mp$assess$methodId )
          subGuiInfo <- getWinVal( scope="L", winName=.getWinName() )
        }
        else
        {
          # Bring the console with error reporting to top, report to user.
          alarm()
          bringToTop(-1)
          cat( "\nACTION (.subSim): Check MP HCR parameters to resolve errors.\n" )      
        }      
      }      # endif act==simHCR

  #----------------------------------------------------------------------------#
  #-- Buttons that close guiSim and open a GUI widget (e.g., guiTrack).      --#
  #----------------------------------------------------------------------------#

      # BATCH generator.
      if ( act=="simBatch" )
      {
        # HACK: *** May want to force a save...
     
        closeWin( .getWinName() )       
        on.exit( guiBatch() )
        isExit <- TRUE
      }  

      # OPTIONS.
      if ( act=="simOpt" )
      {
        # HACK: *** May want to force a save...
    
        closeWin( .getWinName() )       
        on.exit( guiOpt() )
        isExit <- TRUE
      }  
  
      # PERFormance statistics.
      if ( act=="simPerf" )
      {
        # HACK: *** May want to force a save...
    
        closeWin( .getWinName() )       
        on.exit( guiPerf() )
        isExit <- TRUE
      }  

      # TRACKing GUI.
      if ( act=="simTrack" )
      {
        closeWin( .getWinName() )       
        on.exit( guiTrack() )
        isExit <- TRUE
      }  
  
      # VIEW the simulation results.
      if ( act=="simView" )
      {
        # HACK: *** May want to force a save...
        closeWin( .getWinName() )       
        on.exit( guiView() )
        isExit <- TRUE
      }

  #----------------------------------------------------------------------------#
  #-- Actions on guiSim                                                      --#
  #----------------------------------------------------------------------------#
  
      # Select the name of the simulation Control File.  
      if ( act=="simCtl" )
      {
        tmpFile <- selectFile( initialfile="", initialdir=getwd(),
                      filetype=list( c(".txt","Text Files") ),
                      title="Select control file", usewidget="simCtlFile" )

        if ( !is.null( tmpFile ) )
          assign( ".CTLFILE", tmpFile, pos=1 )
        else
        {
          alarm()
          cat( "\nMSG (.subSim) No file selected, simulation control file unchanged.\n" )
        }
        guiChanges$simCtlFile <- .CTLFILE
        
        # Load the new simulation control parameters.
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Reading simulation control file ",.CTLFILE,"\n" )
        ctlPars <- .readParFile( .CTLFILE )
        assign( "ctlPars", ctlPars, pos=1 )
        
        update <- TRUE  
      }     # endif act==simCtl
    
    # Did the simulation history type radio button get accessed.
    # I don't think we need to do anything here until user gets a select button
    # for the history file.  It will be read in mseRsimulation.r using the
    # default file name .DEFHSTFILE.  Use .setGuiSimState to determine if the
    # file exists when simFile is selected, if not reset to auto with an alarm.
  
      # EXIT the Simulation GUI (leave graphics on).
      if ( act=="simExit" )
      {
        # Make sure sub-GUIs are also closed prior to guiSim.
        subGuiList <- c( "mseRguiOpMod","mseRguiData","mseRguiMethod","mseRguiHcr" )
        for ( i in 1:length(subGuiList) )
          closeWin( subGuiList[i] )

        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Exiting guiSim at ",date(),"\n" )
        closeWin( "mseRguiSim" )
      }     # endif act==simExit
  
      # RUN the feedback loop, force an update, and no plots.
      if ( act=="simRun" )
      {
        goRun <- TRUE      
      
        # Make the list of control parameters.
        ctlList <- .createList( ctlPars )
        
        goRun <- .validHistoryFile( ctlList$opMod$historyType, ctlList$opMod$tMP )
    
        # Update the reference points.
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Calculating reference points...\n" )
        refPts <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE, x=40 ) )
        guiChanges <- c( guiChanges, .formatRefPts( refPts ) )
      
        guiChanges$simCtlFile <- .CTLFILE      
        setWinVal( guiChanges, win )
      
        # Save ctlPars before calling runMSE().
        ctlPars <- .guiToCtl( ctlPars, prefix="gui" )
        ctlPars <- .guiToCtl( ctlPars, prefix="refPts" )
        assign( "ctlPars", ctlPars, pos=1 )
        saveSimPars( ctlPars, parFile=.CTLFILE, overWrite=TRUE )
      
        # Save GUI parameters.
        guiInfo <- getWinVal( scope="L", winName="mseRguiSim" )
        assign( ".guiSimPars",guiInfo,pos=1 )
      
        if ( goRun )
        {
          # Everything checks out, so close windows, turn graphics off, runMSE.
          closeWin()
          graphics.off()
      
          # ARK (22-Jun-13)  I don't recognize this code.  Why needed?  
          #t1 <- proc.time()["elapsed"]
          #t2 <- t1
          #while( (t2 - t1) < 5.0 )
          #  t2 <- proc.time()["elapsed"]
          
          runMSE()
        
          # Get the tracking data for the project prior to this simulation.
          trackData <- .getTrackingData( projDir=.PRJFLD )

          # Get the number of simulations.
          nSims <- .getNumSims(trackData )
          if ( .WHINE > 1 )
            cat( "\nMSG (.subSim) Total number of simulations in project folder = ",
               nSims,"\n" )
          print( trackData[ 1:nSims, ] )        
        }
        else
        {
          # Bring the console with error reporting to top, report to user.
          bringToTop(-1)
          cat( "\nACTION (.subSim): Check simulation parameters to resolve errors.\n" )
        }
      }     # endif act==simRun
  
  #----------------------------------------------------------------------------#
  #-- Plotting actions                                                       --#
  #----------------------------------------------------------------------------#
  
      # Clear the graphics windows (*** Change this to clear).
      if ( act=="simReset" )
      {
        alarm()
        msg <- paste( "Are you sure you want to reset the project folder?\n",
                      ".guiSimPars deleted to reset guiSim\n",
                      "Control file and project folders set to defaults\n",
                      "Current project folder moved to backup folder\n",
                      "All project folder sub-folders will be empty" )
                      
        if ( getYes( msg ) )
        {
          graphics.off()
          if ( exists( ".guiSimPars", where=1 ) )
            rm( .guiSimPars, pos=1 )
          cat( "\nMSG (.subSim) .guiSimPars cleared...\n" )

          assign( ".CTLFILE", .DEFCTLFILE, pos=1 )        
          assign( ".PRJFLD",  .DEFPRJFLD,  pos=1 )
      
          if ( .WHINE > 0 )
            cat( "\nMSG (.subSim) Reset Project Folder to ",.PRJFLD,
                 "and Control File to ", .CTLFILE,"\n" )
               
          # Now copy previous results to backup folder.
          prjFld    <- basename( .PRJFLD )
          backupFld <- file.path( getwd(), .FBACK, paste(prjFld,.getStamp(),sep="") )

          cat( "\nMSG (.subSim) Copying ",.PRJFLD," to ",backupFld,"\n" )
          file.rename( .PRJFLD, backupFld )        
              
          closeWin()  
          on.exit( guiSim() )
          isExit <- TRUE
        }
      }     # endif act==simReset
    
      # Save plot(s) as an enhanced Windows metafile.
      if ( act=="simSaveEMF" )
      {
        emfFile <- file.path( .PRJFLD, .DEFPLTFLD,
                              paste( simName, simPlotType, sep="" ) )    
        savePlot( filename=emfFile,
                  type="emf", restoreConsole = .RESTORECONSOLE )
      }
              
      # Save plot(s) as a PDF file.
      if ( act=="simSavePDF" )
      {
        pdfFile <- file.path( .PRJFLD, .DEFPLTFLD,
                              paste( simName, simPlotType, sep="" ) )
        savePlot( filename=pdfFile,
                  type="pdf", restoreConsole = .RESTORECONSOLE )
      }

    }     # endif valid (status from .validSimPars)
  }     # endif GUI is mseRguiSim (submenus are NOT active)
  
  else     # Sub-menus are active...
  {
    subGuiInfo    <- getWinVal( scope="L", winName=win )
    subGuiChanges <- list()
  
    # Actions for the Simulation Control Parameters.
    if ( win=="mseRguiCtlPars" )
    {
      if ( act=="ctlPars" )
      {
        # Could do validity check here.
        cat( "\nMSG (.subSim) ctlPars updated...\n" )
        assign( "ctlPars", ctlPars, pos=1 )
      }
    
      # SORT the simulation control parameters.
      if ( act=="ctlSort" )
      {
        ctlPars <- ctlPars[ order(ctlPars$parameter), ]
        assign( "ctlPars", ctlPars, pos=1 )
        guiChanges$ctlPars <- ctlPars
        setWinVal( guiChanges, winName=.getWinName() )
      }
    
      # SAVE the simulation control parameters.
      if ( act=="ctlOK" )
      {
        saveSimPars( ctlPars, parFile=.CTLFILE, overWrite=TRUE )
      
        closeWin( .getWinName() )
        focusWin( "mseRguiSim" )
        win <- .getWinName()
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) Closing sub-menu, active window is ",win,"\n" )        
      }
    }     # endif mseRguiCtlPars active
    
    else if ( win=="mseRguiData" )
    {
      validPars <- .validDataPars( tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT ) 

      if ( validPars )
      {
        # Transfer values in GUI to the ctlPars dataframe.
        ctlPars <- .guiToCtl( ctlPars, prefix="mp$data" )

        .setGuiDataState()
      
        # Update GUI data.
        #setWinVal( subGuiChanges, winName=win )
        subGuiInfo <- getWinVal( scope="L", winName=win )

        # Transfer values in GUI to the ctlPars dataframe.
        ctlPars <- .guiToCtl( ctlPars, prefix="mp$data" )
        assign ( "ctlPars", ctlPars, pos=1 )

        # Save ctlPars to file.
        saveSimPars( ctlPars, parFile=.CTLFILE,overWrite=TRUE )
      }
      
      if ( act=="dataOK" )
      {
        if ( validPars )
        {
          closeWin( .getWinName() )
          focusWin( "mseRguiSim" )
          win <- .getWinName()
          if ( .WHINE > 2 )
            cat( "\nMSG (.subSim) Closing Data GUI, active window is ",win,"\n" )        
        }
      }
      
      if ( !validPars )
      {
        alarm()
        cat( "\nACTION: Invalid Data parameter(s), cannot save.\n" )
      }      
    }     # endif mseRguiData active
    
    else if ( win=="mseRguiHcr" )
    {
      mpObj   <- .createMP( ctlList )
      initPop <- .initPop( mpObj )
      
      validPars <- .validHcrPars(tMP=ctlList$opMod$tMP, Bexp=initPop$om$Bexp)

      if ( validPars )
      {      
        # Transfer values in GUI to the ctlPars dataframe.
        ctlPars <- .guiToCtl( ctlPars, prefix="mp$hcr" )      

        .setGuiHcrState( methodId=ctlList$mp$assess$methodId )
      
        # Update GUI Hcr.
        subGuiInfo <- getWinVal( scope="L", winName=win )

        # Transfer values in GUI to the ctlPars dataframe.
        ctlPars <- .guiToCtl( ctlPars, prefix="mp$hcr" )
        assign ( "ctlPars", ctlPars, pos=1 )

        # Save ctlPars to file.
        saveSimPars( ctlPars, parFile=.CTLFILE,overWrite=TRUE )
      }
      
      if ( act=="hcrOK" )
      {
        if ( validPars )
        {
          closeWin( .getWinName() )
          focusWin( "mseRguiSim" )
          win <- .getWinName()
          if ( .WHINE > 2 )
            cat( "\nMSG (.subSim) Closing HCR GUI, active window is ",win,"\n" )        
        }
      }
      
      if ( !validPars )
      {
        alarm()
        cat( "\nACTION: Invalid HCR parameter(s), cannot save.\n" )
      }
    }     # endif mseRguiHcr active.
    
    else if ( win=="mseRguiMethod" )
    {
      if ( .FIRSTMETHODGUI )
      {
        assign( ".FIRSTMETHODGUI",FALSE,pos=1 )
        if ( .WHINE > 2 )
          cat( "\nMSG (.subSim) First Method GUI call...\n" )      
      }
      else
      {
        validPars <- .validMethodPars( tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT )      
        if ( validPars )
        {
          # Transfer values in GUI to the ctlPars dataframe.
          ctlPars <- .guiToCtl( ctlPars, prefix="mp$assess" )
          .setGuiMethodState()

          # Save ctlPars to working directory and to file.
          assign ( "ctlPars", ctlPars, pos=1 )
          
          # Could remove this?
          saveSimPars( ctlPars, parFile=.CTLFILE, overWrite=TRUE )
        }
      
        if ( act=="amOK" )
        {
          if ( validPars )
          {
            closeWin( .getWinName() )
            focusWin( "mseRguiSim" )
            win <- .getWinName()
            if ( .WHINE > 2 )
              cat( "\nMSG (.subSim) Closing method sub-GUI, active window is ",win,"\n" )
            assign( ".FIRSTMETHODGUI",TRUE,pos=1 )
          }
        }
        
        if ( !validPars )
        {
          alarm()
          cat( "\nACTION: Invalid HCR parameter(s), cannot save.\n" )
        }
      }        
    }     # endif mseRguiMethod active
    
    else if ( win=="mseRguiOpMod" )
    {
      validPars <- .validOpModPars()
      if ( validPars )
      {
        # Update the reference points.
        if ( .WHINE > 1 )
          cat( "\nMSG (subSim) Updating mseRguiOpMod reference points...\n" )
        refPts <- calcRefPoints( subGuiInfo, rpList=list( FALL=TRUE, x=40 ) )
        subGuiChanges <- c( subGuiChanges, .formatRefPts( refPts ) )

        # Update GUI sim.
        setWinVal( subGuiChanges, winName="mseRguiSim" )      
      
        # Update GUI opMod.
        setWinVal( subGuiChanges, winName=win )
        subGuiInfo <- getWinVal( scope="L", winName=win )
        
        # Set the GUI state.
        .setGuiOpModState()

        # Transfer values in GUI to the ctlPars dataframe.
        ctlPars <- .guiToCtl( ctlPars, prefix="opMod" )
        ctlPars <- .guiToCtl( ctlPars, prefix="refPts" )
        assign ( "ctlPars", ctlPars, pos=1 )

        # Save ctlPars to file.
        saveSimPars( ctlPars, parFile=.CTLFILE,overWrite=TRUE )
      }

      # SAVE the simulation control parameters to ctlPars.
      if ( act=="omOK" )
      {
        if ( validPars )
        {
          closeWin( .getWinName() )
          # Update active window...
          focusWin( "mseRguiSim" )
          win <- .getWinName()
          if ( .WHINE > 2 )
            cat( "\nMSG (.subSim) Closing sub-menu, active window is ",win,"\n" )
        }
      }
      
      if ( !validPars )
      {
        alarm()
        cat( "\nACTION: Invalid operating model parameter(s), cannot save.\n" )
      }
    }     # endif mseRguiOpMod active
    
    else
    {
      cat( "\nMSG (.subSim) Submenu is open, close to proceed...\n" )    
      alarm()
    }
  }     # end ifelse submenu active.
  
 
  # Save the guiSim parameters to a global in working environment, update plot.
  if ( !isExit & (act!="simExit") & (act!="simRun") )
  {
    if ( .getWinName() == "mseRguiSim" )
    {
      .setGuiSimState( .getSimStateList( ctlList ) )
      guiInfo <- getWinVal( scope="L", winName="mseRguiSim" )
      assign( ".guiSimPars",guiInfo,pos=1 )
    }
    
    ctlPars <- .guiToCtl( ctlPars, prefix="gui" )
    assign( "ctlPars", ctlPars, pos=1 )
    saveSimPars( ctlPars, parFile=.CTLFILE, overWrite=TRUE )
    
    .doGuiSimPlots( ctlPars )
  }          
  
  return( invisible() )
}     # END function .subSim

#------------------------------------------------------------------------------#
#-- Validity Checking for Simulation Control Parameters                      --#
#------------------------------------------------------------------------------#

.validHistoryFile <- function( historyType, tMP )
{
  valid <- TRUE
          
  # Check to make sure history file consistent with tMP.
  if ( historyType=="omFile" )
  {
    hstFile     <- file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE )
    historyVals <- read.csv( hstFile, header=T)
          
    if ( nrow(historyVals)!=(tMP-1) )
    {
      valid <- FALSE
      cat( "\nMSG (.validHistoryFile) History file inconsistent with tMP.\n" )
      alarm()
    }
  }
  valid
}     # END function .validHistoryFile

# .validDataPars (valid parameters for simulation GUI):
# Purpose:      Check whether the parameters supplied in the Data GUI for the
#               management procedure are valid. If invalid, display an error
#               message in the R console and clear the invalid field.
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid
# Parameters:   None
# GUI inputs:   Parameters taken directly from active guiSim.
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields
#               Corrects correctable fields (if any)
.validDataPars <- function( tMP=50, nT=100 )
{
  intVal <- function( x )
  {
    # Is the value of x an integer?  There must be a better way...
    result <- ifelse( (trunc(x)-x) == 0.0,TRUE,FALSE )
    result
  }

  # Get the GUI  values and make them local to this function.
  win <- .getWinName()         # Get the current window name
  getWinVal( scope="L", winName=win )

  isValid <- TRUE

  # This will be a list of the parameters that require changing.
  guiChanges <- list()

  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.

  
  if ( is.na(tauAge) || (tauAge < 0.0) )
  {
    cat( "\nMSG (.validOpModPars) Age proportion error must be > 0\n" )
    guiChanges$tauAge <- NA
    isValid <- FALSE
  }

  # Fishery ages constraints: 1<t1Ages<t2Ages<nT  AND 0<k1Ages<(nT-t1) & 0<k2Ages<(nT-t2)  

  if ( is.na(t1Ages) || (t1Ages < 1) || (t1Ages >= t2Ages) || (t1Ages > (nT-1)) || (!intVal(t1Ages)) )
  {
    cat( "\nMSG (.validDataPars) Ages start time in Period 1 must be integer and 0 < t1Ages < t2Ages < (nT-1).\n" )
    guiChanges$t1Ages <- NA
    isValid <- FALSE
  }

  if ( is.na(t2Ages) || (t2Ages < 2) || (t2Ages <= t1Ages) || (t2Ages > nT) || (!intVal(t2Ages)) )
  {
    cat( "\nMSG (.validDataPars) Ages start time in Period 2 must be integer and 1 < t1Ages < t2Ages < nT.\n" )
    guiChanges$t2Ages <- NA
    isValid <- FALSE
  }
  
  # Survey ages constraints: 1<t1Ages<t2Ages<nT  AND 0<k1Ages<(nT-t1) & 0<k2Ages<(nT-t2)  

  if ( is.na(t1AgesS) || (t1AgesS < 1) || (t1AgesS >= t2AgesS) || (t1AgesS > (nT-1)) || (!intVal(t1AgesS)) )
  {
    cat( "\nMSG (.validDataPars) Survey ages start time in Period 1 must be integer and 0 < t1AgesS < t2AgesS < (nT-1).\n" )
    guiChanges$t1AgesS <- NA
    isValid <- FALSE
  }

  if ( is.na(t2AgesS) || (t2AgesS < 2) || (t2AgesS <= t1AgesS) || (t2AgesS > nT) || (!intVal(t2AgesS)) )
  {
    cat( "\nMSG (.validDataPars) Survey ages start time in Period 2 must be integer and 1 < t1AgesS < t2AgesS < nT.\n" )
    guiChanges$t2AgesS <- NA
    isValid <- FALSE
  }

  if ( is.na(k1Ages) || (k1Ages < 0) || (!intVal(k1Ages)) )
  {
    cat( "\nMSG (.validDataPars) Frequency of Period 1 fishery ages must be integer and k1Ages >= 0.\n" )
    guiChanges$k1Ages <- NA
    isValid <- FALSE
  }

  if ( is.na(k2Ages) || (k2Ages < 0) || (!intVal(k2Ages)) )
  {
    cat( "\nMSG (.validDataPars) Frequency of Period 2 fishery ages must be integer and k2Ages >= 0.\n" )
    guiChanges$k2Ages <- NA
    isValid <- FALSE
  }
  
  if ( is.na(k1AgesS) || (k1AgesS < 0) || (!intVal(k1AgesS)) )
  {
    cat( "\nMSG (.validDataPars) Frequency of Period 1 survey ages must be integer and k1AgesS >= 0.\n" )
    guiChanges$k1AgesS <- NA
    isValid <- FALSE
  }

  if ( is.na(k2AgesS) || (k2AgesS < 0) || (!intVal(k2AgesS)) )
  {
    cat( "\nMSG (.validDataPars) Frequency of Period 2 survey ages must be integer and k2AgesS >= 0.\n" )
    guiChanges$k2AgesS <- NA
    isValid <- FALSE
  }  

  if ( is.na(tauSurvey1Mean) || (tauSurvey1Mean < 0.0) )
  {
    cat( "\nMSG (.validDataPars) Mean survey error for Period 1 must be 0 <= tauSurvey1Mean\n" )
    guiChanges$tauSurvey1Mean <- tauSurvey1Mean
    isValid <- FALSE
  }
  
  if ( is.na(tauSurvey1SD) || (tauSurvey1SD < 0.0) )
  {
    cat( "\nMSG (.validDataPars) Maximum survey error for Period 1 must be 0 <= tauSurvey1SD\n" )
    guiChanges$tauSurvey1SD <- tauSurvey1SD
    isValid <- FALSE
  }  

  if ( is.na(tauSurvey2Mean) || (tauSurvey2Mean < 0.0) )
  {
    cat( "\nMSG (.validDataPars) Minimum survey error for Period 2 must be 0 <= tauSurvey2Mean\n" )
    guiChanges$tauSurvey2Mean <- tauSurvey2Mean
    isValid <- FALSE
  }
  
  if ( is.na(tauSurvey2SD) || (tauSurvey2SD < 0.0) )
  {
    cat( "\nMSG (.validDataPars) Maximum survey error for Period 2 must be 0 <= tauSurvey2SD\n" )
    guiChanges$tauSurvey2SD <- tauSurvey2SD
    isValid <- FALSE
  }  

  # Survey constraints: 1 <t 1Survey < t2Survey < nT  AND
  #                     0 < k1Survey < (nT-t1) & 0 < k2Survey < (nT-t2)

  if ( is.na(t1Survey) || (t1Survey < 1) || (t1Survey >= t2Survey) || (t1Survey > (nT-1)) || (!intVal(t1Survey)) )
  {
    cat( "\nMSG (.validDataPars) Survey start time in Period 1 must be integer and 0 < t1Survey < t2Survey < (nT-1).\n" )
    guiChanges$t1Survey <- NA
    isValid <- FALSE
  }

  if ( is.na(t2Survey) || (t2Survey < 2) || (t2Survey <= t1Survey) || (t2Survey > nT) || (!intVal(t2Survey)) )
  {
    cat( "\nMSG (.validDataPars) Survey start time in Period 2 must be integer and 1 < t1Survey < t2Survey < nT.\n" )
    guiChanges$t2Survey <- NA
    isValid <- FALSE
  }

  if ( is.na(k1Survey) || (k1Survey < 1) || (!intVal(k1Survey)) )
  {
    cat( "\nMSG (.validDataPars) Frequency of Period 1 survey must be integer and k1Survey >= 1.\n" )
    guiChanges$k1Survey <- NA
    isValid <- FALSE
  }

  if ( is.na(k2Survey) || (k2Survey < 1) || (!intVal(k2Survey)) )
  {
    cat( "\nMSG (.validDataPars) Frequency of Period 2 survey must be integer and k2Survey >= 1.\n" )
    guiChanges$k2Survey <- NA
    isValid <- FALSE
  }

  #if ( is.na(nStations1) || (nStations1 < 2) || (!intVal(nStations1)) )
  #{
  #  cat( "\nMSG (.validDataPars) Number of stations in Period 1 must be integer and nStations1 >= 2.\n" )
  #  guiChanges$nStations1 <- NA
  #  isValid <- FALSE
  #}

  #if ( is.na(nStations2) || (nStations2 < 2) || (!intVal(nStations2)) )
  #{
  #  cat( "\nMSG (.validDataPars) Number of stations in Period 2 survey must be integer and nStations2 >= 2.\n" )
  #  guiChanges$nStations2 <- NA
  #  isValid <- FALSE
  #}

  # Now ensure text fields valid, i.e., no spaces.
  guiChanges$idxSurvey1 <- gsub( " ","",idxSurvey1 )
  guiChanges$idxSurvey2 <- gsub( " ","",idxSurvey2 )
  
  # If there are invalid parameters, bring the console to the top.
  if ( !isValid )
    bringToTop( -1 )

  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( guiChanges )
  return( isValid )
}     # END function .validDataPars


# .validHcrPars (valid parameters for simulation GUI):
# Purpose:      Check whether the parameters supplied in the "HCR"
#               GUI are valid. If invalid, display an error message in the R
#               console and clear the invalid field (can we color it?).
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid
# Parameters:   None
# GUI inputs:   Parameters taken directly from active guiSim.
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields
#               Corrects correctable fields (if any)
.validHcrPars <- function(tMP,Bexp)
{
  intVal <- function( x )
  {
    # Is the value of x an integer?  There must be a better way...
    result <- ifelse( (trunc(x)-x) == 0.0,TRUE,FALSE )
    result
  }

  win <- .getWinName()                 # Get the current window name.
  getWinVal( scope="L", winName=win )  # Get the GUI pars and make then local.

  isValid    <- TRUE
  guiChanges <- list()                 # List of parameters that require changes.

  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.


  # KRH: changed the following two if statements to take away requirement that lowerboundMult < upperBoundMult bc doesn't apply to historical ref points with different bases
  # 23-Aug-2013 
  if ( is.na(lowerBoundMult) || (lowerBoundMult < 0.0) || (lowerBoundMult > 1.0) )
  {
    cat( "\nMSG (.validHcrPars) Lower bound multiplier must be 0 < lowerBoundMult < 1 .\n" )
    guiChanges$lowerBoundMult <- NA
    isValid <- FALSE
  }
  
  if ( is.na(upperBoundMult) || (upperBoundMult < 0.0) || (upperBoundMult > 1) )
  {
    cat( "\nMSG (.validHcrPars) Upper bound multiplier must be 0 < upperBoundMult < 1.\n" )
    guiChanges$upperBoundMult <- NA
    isValid <- FALSE
  }  

  if ( is.na(inputF) || (inputF < 0.0) )
  {
    cat( "\nMSG (.validHcrPars) User input removal reference F must be inputF >= 0.0.\n" )
    guiChanges$inputF <- NA
    isValid <- FALSE
  }

  if ( is.na(lambda1) || (lambda1 < 0.0) || (lambda1 > 1.0) )
  {
    cat( "\nMSG (.validHcrPars) Fraction of previous TAC used must be 0 <= lambda1 <= 1.0.\n" )
    guiChanges$lambda1 <- NA
    isValid <- FALSE
  }
  
  #if ( is.na(constCatch) || (constCatch < 0.0) )
  #{
   # cat( "\nMSG (.validHcrPars) Constant catch TAC must be constCatch >= 0.0.\n" )
   # guiChanges$constCatch <- NA
   # isValid <- FALSE
  #}  
 
  if ( is.na(ctlPtFreq) || (ctlPtFreq < 0) || (!intVal(ctlPtFreq)) )
  {
    cat( "\nMSG (.validHcrPars) Frequency of HCR control point estimation must be integer and >= 0.\n" )
    guiChanges$ctlPtFreq <- NA
    isValid <- FALSE
  }
  
  if ( is.na(sprX) || (sprX < 1) || (sprX > 99) )
  {
    cat( "\nMSG (.validHcrPars) spr X% must be  1 < X% < 99\n" )
    guiChanges$sprX <- NA
    isValid <- FALSE
  }  
  
  # KRH Added; 22-Aug-2013 ======================
  if (histLowBase == "Bquant" & (lowBaseEndYr - lowBaseStartYr < 9.0))
  {
     cat( "\nMSG (.validHcrPars) Lower status base: End year must be > 9 years after start year for quantile method.\n" )
     guiChanges$lowBaseEndYr<-NA
     isValid <- FALSE
  }
  
  if (histUpperBase == "Bquant" & (upperBaseEndYr - upperBaseStartYr < 9.0))
  {
    cat( "\nMSG (.validHcrPars) Upper status base: End year must be > 9 years after start year for quantile method.\n" )
    guiChanges$upperBaseEndYr<-NA
    isValid <- FALSE
  }
  
  if (histFtBase == "Fquant" & (FtEndYr - FtStartYr < 9.0))
  {
    cat( "\nMSG (.validHcrPars) Removal rate base: End year must be > 9 years after start year for quantile method.\n" )
    guiChanges$FtEndYr<-NA
    isValid <- FALSE
  }
  
  if (statusBase == "statusBaseBt"  & (lowBaseEndYr >= tMP || lowBaseStartYr >= tMP))
    {
       cat( "\nMSG (.validHcrPars) Lower status base: Start and end year of historical time period must be < tMP.\n" )
       guiChanges$lowBaseStartYr<-NA
       guiChanges$lowBaseEndYr<-NA
       isValid <- FALSE
  }
  
  if (statusBase == "statusBaseBt"  & (upperBaseEndYr >= tMP || upperBaseStartYr >= tMP))
  {
        cat( "\nMSG (.validHcrPars) Upper status base: Start and end year of historical time period must be < tMP.\n" )
        guiChanges$upperBaseStartYr<-NA
        guiChanges$upperBaseEndYr<-NA
        isValid <- FALSE
  }
  
  if (remRefBase == "rrBaseFt"  & (FtEndYr >= tMP || FtStartYr >= tMP))
  {
        cat( "\nMSG (.validHcrPars) Removal rate base: Start and end year of historical time period must be < tMP.\n" )
        guiChanges$FtStartYr<-NA
        guiChanges$FtEndYr<-NA
        isValid <- FALSE
  }
    
  if ( is.na(lowBaseQuant) || (lowBaseQuant <= 0.0) || (lowBaseQuant > 1) )
  {
    cat( "\nMSG (.validHcrPars) Lower base quantile must be be > 0 and <=1.0.\n" )
    guiChanges$lowBaseQuant <- NA
    isValid <- FALSE
  }    
   
  if ( is.na(upperBaseQuant) || (upperBaseQuant <= 0.0) || (upperBaseQuant > 1) )
  {
    cat( "\nMSG (.validHcrPars) Upper base quantile must be be > 0 and <=1.0.\n" )
    guiChanges$upperBaseQuant <- NA
    isValid <- FALSE
  }  
   
  if ( is.na(histFtBaseQuant) || (histFtBaseQuant <= 0.0) || (histFtBaseQuant > 1) )
  {
    cat( "\nMSG (.validHcrPars) Removal rate quantile must be be > 0 and <=1.0.\n" )
    guiChanges$histFtBaseQuant <- NA
    isValid <- FALSE
  }  
 
 
  # KRH: added the following 2 if statements
  # 23-Aug-2013 ==============================================================
  
  if (statusBase == "statusBaseBt")
  {
       # Calculate lower base for historical reference points
       if (histLowBase=="Bmin")
       {
            lowerBaseB<-min(Bexp[lowBaseStartYr:lowBaseEndYr])
       }
       if (histLowBase=="Bmax")
       {
            lowerBaseB<-max(Bexp[lowBaseStartYr:lowBaseEndYr])
       }
       if (histLowBase=="Bmean")
       {
            lowerBaseB<-mean(Bexp[lowBaseStartYr:lowBaseEndYr])
       }
       if (histLowBase=="Bquant")
       {  
          lowerBaseB<-quantile(Bexp[lowBaseStartYr:lowBaseEndYr],lowBaseQuant)[[1]]
       }
       
       # Calculate upper base for historical reference points
       if (histUpperBase=="Bmin")
       {
            upperBaseB<-min(Bexp[upperBaseStartYr:upperBaseEndYr])
       }
       if (histUpperBase=="Bmax")
       {
            upperBaseB<-max(Bexp[upperBaseStartYr:upperBaseEndYr])
       }
       if (histUpperBase=="Bmean")
       {
            upperBaseB<-mean(Bexp[upperBaseStartYr:upperBaseEndYr])
       }
       if (histUpperBase=="Bquant")
       {  
          upperBaseB<-quantile(Bexp[upperBaseStartYr:upperBaseEndYr],upperBaseQuant)[[1]]
       }       
       
       if ((lowerBaseB*lowerBoundMult) >= (upperBaseB*upperBoundMult))
       {
       	  cat( "\nMSG (.validHcrPars) Upper bound will not be greater than lower bound based on operating model biomass .\n" )
          guiChanges$lowerBoundMult <- NA
          guiChanges$upperBoundMult <- NA
          isValid <- FALSE
        }
    }
  # ============================================================================
  
 
 
  # If there are invalid parameters, bring the console to the top.
  if ( !isValid )
    bringToTop( -1 )

  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( guiChanges )
  return( isValid )
}    # END function .validHcrPars


# .validMethodPars (valid parameters for simulation GUI):
# Purpose:      Check whether the parameters supplied in the Method
#               GUI are valid. If invalid, display an error message in the R
#               console and clear the invalid field (can we color it?).
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid
# Parameters:   None
# GUI inputs:   Parameters taken directly from active guiSim.
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields
#               Corrects correctable fields (if any)
.validMethodPars <- function( tMP=50, nT=100 )
{
  intVal <- function( x )
  {
    # Is the value of x an integer?  There must be a better way...
    result <- ifelse( (trunc(x)-x) == 0.0,TRUE,FALSE )
    result
  }

  win <- .getWinName()         # Get the current window name
  getWinVal( scope="L", winName=win )

  isValid <- TRUE

  # This will be a list of the parameters that require changing.
  guiChanges <- list()

  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.

  if ( is.na(avgPoints) || (avgPoints < 1) || (!intVal(avgPoints)) )
  {
    cat(  "\nMSG (.validMethodPars) Number of points to average must be integer and greater than 1.\n" )
    guiChanges$avgPoints <- NA
    isValid <- FALSE
  }

  if ( is.na(kfGain) || (kfGain <=0.0) || (kfGain >= 1.0) )
  {
    cat( "\nMSG (.validMethodPars) Kalman filter gain must be 0 < kfGain < 1.0.\n" )
    guiChanges$kfGain <- NA
    isValid <- FALSE
  }

  # Surplus production model parameters.
  
  if ( is.na(spPmFmsy) || (spPmFmsy<=0) )
  {
    cat( "\nMSG (.validMethodPars) Prior mean on Fmsy for fit must be > 0.\n" )
    guiChanges$spPmFmsy <- NA
    isValid <- FALSE
  }

  if (is.na(spPsdFmsy) || (spPsdFmsy<=0))
  {
   cat( "\nMSG (.validMethodPars) Prior standard deviation on Fmsy must be > 0.\n" )
    guiChanges$spPsdFmsy <- NA
    isValid<-FALSE
  }
  
  if ( is.na(spPmMsy) || (spPmMsy<=0) )
  {
    cat( "\nMSG (.validMethodPars) Prior mean on MSY must be > 0.\n" )
    guiChanges$spPmMsy <- NA
    isValid <- FALSE
  }

  if (is.na(spPsdMsy) || (spPsdMsy<=0))
  {
   cat( "\nMSG (.validMethodPars) Prior standard deviation on MSY must be > 0.\n" )
    guiChanges$spPsdMsy <- NA
    isValid<-FALSE
  }  

  if ( is.na(spMsy) || (spMsy<=0) )
  {
   cat( "\nMSG (.validMethodPars) Initial MSY value must be > 0.\n" )
    guiChanges$spMsy <- NA
    isValid<-FALSE
  }

  if ( is.na(spFmsy) || (spFmsy<=0) )
  {
   cat( "\nMSG (.validMethodPars) Initial Fmsy value must be > 0.\n" )
    guiChanges$spFmsy <- NA
    isValid <- FALSE
  }

  if (is.na(spRhoEiv) || (spRhoEiv<0) || (spRhoEiv>1))
  {
    cat( "\nMSG (.validMethodPars) EIV ratio must be 0 >= ratio <= 1.\n" )
    guiChanges$spRhoEiv <- NA
    isValid <- FALSE
  }

  # Delay-Difference model parameters.

  if ( is.na(ddPmM) || (ddPmM <= 0.0) )
  {
    cat( "\nMSG (.validMethodPars) Prior mean on natural mortality must be > 0.0\n" )
    guiChanges$ddPmM <- NA
    isValid <- FALSE
  }
  
  if ( is.na(ddPsdM) || (ddPsdM <= 0.0) )
  {
    cat( "\nMSG (.validMethodPars) Prior standard deviation on natural mortality must be > 0.0\n" )
    guiChanges$ddPsdM <- NA
    isValid <- FALSE
  }

  if ( is.na(ddPmSteep) || (ddPmSteep <= 0.21) || (ddPmSteep >= 0.99) )
  {
    cat( "\nMSG (.validMethodPars) Prior mean on steepness must be > 0.21 and < 0.99.\n" )
    guiChanges$ddPmSteep <- NA
    isValid <- FALSE
  }
  
  if ( is.na(ddPsdSteep) || (ddPsdSteep < 0.0) || (ddPsdSteep > 1.0) )
  {
    cat( "\nMSG (.validMethodPars) Prior standard deviation on steepness must be >= 0.0 and <=1.0.\n" )
    guiChanges$ddPsdSteep <- NA
    isValid <- FALSE
  }

  if (is.na(ddRhoEiv) || (ddRhoEiv<0) || (ddRhoEiv>1))
  {
    cat( "\nMSG (.validMethodPars) EIV ratio must be 0 >= ratio <= 1.\n" )
    guiChanges$ddRhoEiv <- NA
    isValid <- FALSE
  }
  
  if (is.na(ddkAge) || (ddkAge<2) )
  {
    cat( "\nMSG (.validMethodPars) Age of knife-edge recruitment must be >= 1.\n" )
    guiChanges$ddkAge <- NA
    isValid <- FALSE
  }      

  # Statistical Catch-At-Age model parameters.

  if ( is.na(caaPmM) || (caaPmM <= 0.0) )
  {
    cat( "\nMSG (.validMethodPars) Prior mean on natural mortality must be > 0.0\n" )
    guiChanges$caaPmM <- NA
    isValid <- FALSE
  }
  
  if ( is.na(caaPsdM) || (caaPsdM <= 0.0) )
  {
    cat( "\nMSG (.validMethodPars) Prior standard deviation on natural mortality must be > 0.0\n" )
    guiChanges$caaPsdM <- NA
    isValid <- FALSE
  }

  if ( is.na(caaPmSteep) || (caaPmSteep <= 0.21) || (caaPmSteep >= 0.99) )
  {
    cat( "\nMSG (.validMethodPars) Prior mean on steepness must be > 0.21 and < 0.99.\n" )
    guiChanges$caaPmSteep <- NA
    isValid <- FALSE
  }
  
  if ( is.na(caaPsdSteep) || (caaPsdSteep < 0.0) || (caaPsdSteep > 1.0) )
  {
    cat( "\nMSG (.validMethodPars) Prior standard deviation on steepness must be >= 0.0 and <=1.0.\n" )
    guiChanges$caaPsdSteep <- NA
    isValid <- FALSE
  }

  if (is.na(caaRhoEiv) || (caaRhoEiv<0) || (caaRhoEiv>1))
  {
    cat( "\nMSG (.validMethodPars) EIV ratio must be 0 >= ratio <= 1.\n" )
    guiChanges$caaRhoEiv <- NA
    isValid <- FALSE
  }
  
  if (is.na(nSamples) || (nSamples<1) )
  {
    cat( "\nMSG (.validMethodPars) Number of ages samples must be >= 1.\n" )
    guiChanges$nSamples <- NA
    isValid <- FALSE
  }

  if (is.na(nSamplesS) || (nSamplesS<1) )
  {
    cat( "\nMSG (.validMethodPars) Number of ages samples must be >= 1.\n" )
   guiChanges$nSamplesS <- NA
   isValid <- FALSE
  }
  
  # Method constraints:
  #
  # Period 1: tMP  = t1Method <= t2Method      AND integer.
  # Period 2: tMP <= t1Method <= t2Method < nT AND integer.

  if ( is.na(t1Method) || (t1Method != tMP) || (t1Method > t2Method) || (!intVal(t2Method)) )
  {
    cat( "\nMSG (.validMethodPars) Method start time in Period 1 must be integer and tMP=t1Method <= t2Method.\n" )
    guiChanges$t1Method <- tMP
    t1Method            <- tMP
    isValid <- FALSE
  }

  if ( is.na(t2Method) || (t1Method > t2Method) || (t2Method > nT) || (!intVal(t2Method)) )
  {
    cat( "\nMSG (.validMethodPars) Method start time in Period 2 must be integer and t1Method <= t2Method <= nT.\n" )
    guiChanges$t2Method <- NA
    isValid <- FALSE
  }

  if ( is.na(k1Method) || (k1Method < 1) || (!intVal(k1Method)) )
  {
    cat( "\nMSG (.validMethodPars) Frequency of Period 1 method must be integer and k1Method >= 1.\n" )
    guiChanges$k1Method <- NA
    isValid <- FALSE
  }

  if ( is.na(k2Method) || (k2Method < 1) || (!intVal(k2Method)) )
  {
    cat( "\nMSG (.validMethodPars) Frequency of Period 2 method must be integer and k2Method >= 1.\n" )
    guiChanges$k2Method <- NA
    isValid <- FALSE
  }

  # Now ensure text fields valid, i.e., no spaces.
  guiChanges$idxMethod1 <- gsub( " ","",idxMethod1 )
  guiChanges$idxMethod2 <- gsub( " ","",idxMethod2 )  

  # If there are invalid parameters, bring the console to the top.
  if ( !isValid )
    bringToTop( -1 )

  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( guiChanges )
  return( isValid )
}     # END function .validMethodPars

# .validOpModPars (valid parameters for simulation GUI):
# Purpose:      Check whether the parameters supplied in the "Operating model"
#               GUI are valid. If invalid, display an error message in the R
#               console and clear the invalid field (can we color it?).
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid
# Parameters:   None
# GUI inputs:   Parameters taken directly from active guiSim.
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields
#               Corrects correctable fields (if any)
.validOpModPars <- function()
{
  intVal <- function( x )
  {
    # Is the value of x an integer?  There must be a better way...
    result <- ifelse( (trunc(x)-x) == 0.0,TRUE,FALSE )
    result
  }

  # Get the GUI  values and make them local to this function.
  win <- .getWinName()         # Get the current window name
  getWinVal( scope="L", winName=win )

  isValid <- TRUE

  # This will be a list of the parameters that require changing.
  guiChanges <- list()

  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.

  # Check validity of each GUI variable.  Order similar to table rather than
  # GUI, which may be fluid and dynamic ....

  if ( is.na(nT) || (nT <= 10) || (nT > .MAXNT)  || (!intVal(nT)) )
  {
    cat( "\nMSG (.validOpModPars) Total number of years must be integer and 10 <= nT <= ",.MAXNT,".\n" )
    guiChanges$nT <- NA
    isValid <- FALSE
  }

  if ( is.na(tMP) || (tMP > .MAXNT) || (!intVal(tMP)) )
  {
    cat( "\nMSG (.validOpModPars) First year of management procedure must be integer and tMP <= ",.MAXNT,".\n" )
    guiChanges$tMP <- NA
    isValid <- FALSE
  }
  
  # Now check to ensure nT and tMP match other simulations in project.
  trackData <- .getTrackingData( .PRJFLD )
  nSims <- .getNumSims( trackData )

  if ( nSims > 0 )
  {
    nTtrack  <- unique( trackData$nT[1:nSims] )
    tMPtrack <- unique( trackData$tMP[1:nSims] )
  
    if ( nT != nTtrack )
    {
      cat( "\nMSG (.validOpModPars) Total number of years must match other simulations in project folder.\n" )
      guiChanges$nT <- nTtrack
      alarm()    
    }
    
    if ( tMP != tMPtrack )
    {
      cat( "\nMSG (.validOpModPars) First year of management procedure must match other simulations in project folder.\n" )
      guiChanges$tMP <- tMPtrack
      alarm()
    }    
  }
  
  # Now check if there is a clash with the historyFile.
  if (!.validHistoryFile( historyType, tMP ))
  {
    cat( "\nMSG (.validOpModPars) Last year of history must be tMP-1.\n" )
    guiChanges$historyType <- "omAuto" 
  }  

  if ( is.na(nAges) || (nAges < .MINAGES) || (nAges > .MAXAGES) || (!intVal(nAges)) )
  {
    cat( "\nMSG (.validOpModPars) Number of age classes must be integer and",.MINAGES,"<= nAges <=",.MAXAGES,".\n" )
    guiChanges$nAges <- NA
    isValid <- FALSE
  }

  if ( is.na(initDepTarg) || (initDepTarg<=0.0) )
  {
    cat( "\nMSG (.validOpModPars) Initial depletion target must be initDepTarg > 0.0.\n" )
    guiChanges$initDepTarg <- NA
    isValid <- FALSE
  }

  if ( is.na(B0) || ( B0 <= 0 ) )
  {
    cat( "\nMSG (.validOpModPars) B0 must be greater than 0 and in thousands of tonnes.\n" )
    guiChanges$B0 <- NA
    isValid <- FALSE
  }

  if ( is.na(rSteepness) || (rSteepness <= 0.2) || (rSteepness >= 1.0) )
  {
    cat( "\nMSG (.validOpModPars) Stock-recruitment steepness must be 0.2 < h < 1.0.\n" )
    guiChanges$rSteepness <- NA
    isValid <- FALSE
  }

  if ( is.na(M) || (M <= 0.0) )
  {
    cat( "\nMSG (.validOpModPars) Natural mortality must be M > 0.\n" )
    guiChanges$M <- NA
    isValid <- FALSE
  }
  
  if ( is.na(sigmaM) || (sigmaM < 0.0) )
  {
    cat( "\nMSG (.validOpModPars) Natural mortality standard error must be sigmaM >= 0.0.\n" )
    guiChanges$sigmaM <- NA
    isValid <- FALSE
  }  

  if ( is.na(Linf) || Linf <= 0.0 )
  {
    cat( "\nMSG (.validOpModPars) von Bertalannfy L infinity must be Linf > 0.0.\n" )
    guiChanges$Linf <- NA
    isValid <- FALSE
  }

  if ( is.na(L1) || (L1 <=0.0) || (L1 >= Linf) )
  {
    cat( "\nMSG (.validOpModPars) Length-at-age 1 must be 0 < L1 < Linf\n" )
    guiChanges$L1 <- NA
    isValid <- FALSE
  }

  if ( is.na(vonK) || (vonK <= 0.0) || (vonK >= 1.0) )
  {
    cat( "\nMSG (.validOpModPars) von Bertalannfy growth constant must be 0 < k < 1.0.\n" )
    guiChanges$vonK <- NA
    isValid <- FALSE
  }

  if ( is.na(aMat50) || is.na(aMat95) || (aMat50 < 1) || (aMat50 > aMat95) )
  {
    cat( "\nMSG (.validOpModPars) Age at 50% maturity must be aMat50 > 1.\n" )
    cat( "\nMSG (.validOpModPars) Age at 95% maturity must be greater than age at 50% maturity.\n")
    guiChanges$aMat50 <- NA
    guiChanges$aMat95 <- NA
    isValid <- FALSE
  }

  if ( is.na(aSel50) || is.na(aSel95) || (aSel50 < 1) || (aSel50 > aSel95) )
  {
    cat( "\nMSG (.validOpModPars) Fishery Age at 50% selectivity must be aSel50 > 1.\n" )
    cat( "\nMSG (.validOpModPars) Fishery Age at 95% selectivity must be greater than age at 50% selectivity.\n")
    guiChanges$aSel50 <- NA
    guiChanges$aSel95 <- NA
    isValid <- FALSE
  }

   if ( is.na(aSelS50) || is.na(aSelS95) || (aSelS50 < 1) || (aSelS50 > aSelS95) )
    {
      cat( "\nMSG (.validOpModPars) Survey Age at 50% selectivity must be aSel50 > 1.\n" )
      cat( "\nMSG (.validOpModPars) Survey Age at 95% selectivity must be greater than age at 50% selectivity.\n")
      guiChanges$aSelS50 <- NA
      guiChanges$aSelS95 <- NA
      isValid <- FALSE
  }

  if ( is.na(sigmaR) || (sigmaR < 0.0) )
  {
    cat( "\nMSG (.validOpModPars) Recruitment standard error must be sigmaR >= 0.0.\n" )
    guiChanges$sigmaR <- NA
    isValid <- FALSE
  }

  if ( is.na(sigmaL) || (sigmaL < 0.0) )
  {
    cat( "\nMSG (.validOpModPars) Length-at-age standard error must be sigmaL >= 0.0.\n" )
    guiChanges$sigmaL <- NA
    isValid <- FALSE
  }

  if ( is.na(gammaR) || (gammaR <= -1.0) || (gammaR >= 1.0) )
  {
    cat( "\nMSG (.validOpModPars) Lag-1 autocorrelation in log-recruitment deviations must be -1.0 < gammaR < 1.0.\n" )
    guiChanges$gammaR <- NA
    isValid <- FALSE
  }

  # ARK: Deprecated when spatial module introduced 24-Feb-09.
  #if ( is.na(gammaS) || (gammaS <= -1.0) || (gammaS >= 1.0) )
  #{
  #  cat( "Lag-1 autocorrelation in log-deviations in survey must be -1.0 <= gammaS <= 1.0.\n" )
  #  changes$gammaS <- NA
  #  isValid <- FALSE
  #}

  if ( is.na(qSurvey) || (qSurvey <= 0.0) )
  {
    cat( "\nMSG (.validOpModPars) Survey catchability coefficient must be qSurvey > 0.0.\n" )
    guiChanges$qSurvey <- NA
    isValid <- FALSE
  }
 
  #if ( is.na(avgPoints) || (avgPoints < 1) || (!intVal(avgPoints)) )
  #{
  #  cat( "Number of points to average must be integer and greater than 1.\n" )
  #  changes$avgPoints <- NA
  #  isValid <- FALSE
  #}

  #if ( is.na(kfGain) || (kfGain <=0.0) || (kfGain >= 1.0) )
  #{
  #  cat("Kalman filter gain must be 0 < kfGain < 1.0.\n" )
  #  changes$kfGain <- NA
  #  isValid <- FALSE
  #}

  #if (is.na(initBmsy) || (initBmsy<=0))
  #{
  # cat("Initial Bmsy for production model fit must be > 0.\n")
  #  changes$initBmsy<-NA
  #  isValid<-FALSE
  #}

  #if (is.na(initFmsy) || (initFmsy<=0))
  #{
  # cat("Initial Fmsy for production model fit must be > 0.\n")
  #  changes$initFmsy<-NA
  #  isValid<-FALSE
  #}
  
  #if (is.na(sprX) || (sprX <= 0) || (sprX > 100))
  #{
  # cat("Percentage for spr reference point must be > 0 and <= 100.\n")
  #  changes$initFmsy<-NA
  #  isValid<-FALSE
  #}

  #if (is.na(rhoEIV) || (rhoEIV<0) || (rhoEIV>1))
  #{
  # cat("EIV ratio for production model fit must be 0 >= ratio <= 1.\n")
  #  changes$rhoEIV<-NA
  #  isValid<-FALSE
  #}
  
  #if (is.na(likeWgt) || (likeWgt<0) || (likeWgt>1))
  #{
  # cat("Likelihood weight for production model fit must be 0 >= weight <= 1.\n")
  #  changes$likeWgt<-NA
  #  isValid<-FALSE
  #}
 
  #if (is.na(muPriorFmsy) || (muPriorFmsy<=0))
  #{
  # cat("Mean of prior distribution on Fmsy for production model fit must be > 0.\n")
  #  changes$muPriorFmsy<-NA
  #  isValid<-FALSE
  #}

  #if (is.na(sdPriorFmsy) || (sdPriorFmsy<=0))
  #{
  # cat("Standard deviation of prior distribution on Fmsy for production model fit must be > 0.\n")
  #  changes$sdPriorFmsy<-NA
  #  isValid<-FALSE
  #}

  #if ( is.na(pmSteepness) || (pmSteepness <= 0.21) || (pmSteepness >= 0.99) )
  #{
  #  cat( "Mean of prior distribution on steepness for CAA model must be > 0.21 and < 0.99.\n" )
  #  changes$pmSteepness <- NA
  #  isValid <- FALSE
  #}
  
  #if ( is.na(psdSteepness) || (psdSteepness < 0.0) || (psdSteepness > 1.0) )
  #{
  #  cat( "SD of prior distribution on steepness for CAA model must be >= 0.0 and <=1.0.\n" )
  #  changes$psdSteepness <- NA
  #  isValid <- FALSE
  #}

#  if ( is.na(limitBoundMult) || (limitBoundMult <= 0.0) || (limitBoundMult >= upperBoundMult) )
#  {
#    cat( "Limit bound multiplier for status zones must be 0 < limitBoundMult < upperBoundMult.\n" )
#    changes$limitBoundMult <- NA
#    isValid <- FALSE
#  }

#  if ( is.na(upperBoundMult) || (upperBoundMult <= 0.0) || (upperBoundMult <= limitBoundMult) )
#  {
#    cat( "Upper bound multplier for status zones must be 0 < limitBoundMult < upperBoundMult.\n" )
#    changes$upperBoundMult <- NA
#    isValid <- FALSE
#  }

  # If there are invalid parameters, bring the console to the top.
  if ( !isValid )
  {
    alarm()
    bringToTop( -1 )
  }

  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( guiChanges )
  return( isValid )
}     # END function .validOpModPars


# .validSimPars (valid parameters for simulation GUI):
# Purpose:      Check whether the parameters supplied in the Simulation GUI
#               are valid. If invalid, display an error message in the R
#               console and clear the invalid field (can we color it?).
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid
# Parameters:   None
# GUI inputs:   Parameters taken directly from active guiSim.
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields
#               Corrects correctable fields (if any)
.validSimPars <- function()
{
  intVal <- function( x )
  {
    # Is the value of x an integer?  There must be a better way...
    result <- ifelse( (trunc(x)-x) == 0.0,TRUE,FALSE )
    result
  }

  # Get the GUI  values and make them local to this function.
  win <- .getWinName()         # Get the current window name
  getWinVal( scope="L", winName=win )

  isValid <- TRUE

  # This will be a list of the parameters that require changing.
  guiChanges <- list()

  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.

  # Ensure text fields valid, i.e., no spaces.
  scenarioLabel <- gsub( " ","",scenarioLabel )
  mpLabel       <- gsub( " ","",mpLabel )

  # scenarioLabel and mpLabel must be a unique combination.
  simName <- paste(scenarioLabel,mpLabel,sep="-")
  
  trackData <- .getTrackingData( .PRJFLD )
  nSims     <- .getNumSims( trackData )
  simsDone  <- paste( trackData$scenario[1:nSims], trackData$mp[1:nSims], sep="-" )

  # Check if simulation name already used.
  if ( is.element( simName, simsDone ) )
  {
    # Check to find first available scenarioLabel that is not used.
    j <- 1
    tmpScenarioLabel <- scenarioLabel
    while( any( tmpScenarioLabel==trackData$scenario ) )
    {
      j <- j + 1
      tmpScenarioLabel <- paste( scenarioLabel,"-",j, sep="" )
    }
    guiChanges$scenarioLabel <- tmpScenarioLabel
    
    # Check to find first available mpLabel that is not used.
    j <- 1
    tmpMpLabel <- mpLabel
    while( any( tmpMpLabel==trackData$mp ) )
    {
      j <- j + 1
      tmpMpLabel <- paste( mpLabel,"-",j, sep="" )
    }
    guiChanges$mpLabel <- tmpMpLabel
    
    #stamp <- .getStamp()
    #guiChanges$scenarioLabel <- paste( "S",  stamp, sep="" )
    #guiChanges$mpLabel       <- paste( "MP", stamp, sep="" )

    alarm()
    if ( getYes( paste( "Scenario and procedure labels must be unique\n\n",
                        "Do you want to enter your own labels?" ) ) )
    {
      isValid <- FALSE
    }                        
    cat( "\nMSG (.validSimPars) Combination of scenarioLabel and mpLabel already taken.\n" )
    cat( "\nMSG (.validSimPars) Scenario and procedure labels changed so combination is unique.\n" )      
  }

  if ( is.na(nReps) || (nReps < .MINREP) || (nReps > .MAXREP)  || (!intVal(nReps)) )
  {
    cat( "\nMSG (.validSimPars) Total number of replicates must be integer and ",.MINREP,
                 "<= nReps <= ",.MAXREP,".\n" )
    guiChanges$nReps <- NA
    isValid <- FALSE
  }

  # If there are invalid parameters, bring the console to the top.
  if ( !isValid )
  {
    alarm()
    bringToTop( -1 )
  }

  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( guiChanges )
  return( isValid )
}     # END function .validSimPars

#------------------------------------------------------------------------------#
#-- Set the (conditional) State of a GUI                                     --#
#------------------------------------------------------------------------------#

# .setGuiDataState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiDataState <- function()
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  # Set options for FISHERY age data timing.
  #setWidgetState( varname="chkAges1", state="disabled" )  
  if ( !chkAges1 )
  {
    setWidgetState( varname="t1Ages",   state="normal" )
    setWidgetState( varname="k1Ages",   state="normal" )

    setWidgetState( varname="idxAges1", state="disabled" )
  }
  else
  {
    setWidgetState( varname="t1Ages",   state="disabled" )
    setWidgetState( varname="k1Ages",   state="disabled" )
    
    setWidgetState( varname="idxAges1", state="normal" )
  }
  
  #setWidgetState( varname="chkAges2", state="disabled" )  
  if ( !chkAges2 )
  {
    setWidgetState( varname="t2Ages",   state="normal" )
    setWidgetState( varname="k2Ages",   state="normal" )
    
    setWidgetState( varname="idxAges2", state="disabled" )  
  }
  else
  {
    setWidgetState( varname="t2Ages",   state="disabled" )
    setWidgetState( varname="k2Ages",   state="disabled" )
    
    setWidgetState( varname="idxAges2", state="normal" )   
  }
 
 #################################################
   # Set options for SURVEY age data timing.
   #setWidgetState( varname="chkAges1", state="disabled" )  
   if ( !chkAges1S )
   {
     setWidgetState( varname="t1AgesS",   state="normal" )
     setWidgetState( varname="k1AgesS",   state="normal" )
 
     setWidgetState( varname="idxAges1S", state="disabled" )
   }
   else
   {
     setWidgetState( varname="t1AgesS",   state="disabled" )
     setWidgetState( varname="k1AgesS",   state="disabled" )
     
     setWidgetState( varname="idxAges1S", state="normal" )
   }
   
   #setWidgetState( varname="chkAges2", state="disabled" )  
   if ( !chkAges2S )
   {
     setWidgetState( varname="t2AgesS",   state="normal" )
     setWidgetState( varname="k2AgesS",   state="normal" )
     
     setWidgetState( varname="idxAges2S", state="disabled" )  
   }
   else
   {
     setWidgetState( varname="t2AgesS",   state="disabled" )
     setWidgetState( varname="k2AgesS",   state="disabled" )
     
     setWidgetState( varname="idxAges2S", state="normal" )   
  }
 #####################################################

  # Set options for survey timing.
  #setWidgetState( varname="chkSurvey1", state="disabled" )  
  if ( !chkSurvey1 )
  {
    setWidgetState( varname="t1Survey",   state="normal" )
    setWidgetState( varname="k1Survey",   state="normal" )

    setWidgetState( varname="idxSurvey1", state="disabled" )
  }
  else
  {
    setWidgetState( varname="t1Survey",   state="disabled" )
    setWidgetState( varname="k1Survey",   state="disabled" )
    
    setWidgetState( varname="idxSurvey1", state="normal" )
  }
  
  #setWidgetState( varname="chkSurvey2", state="disabled" )  
  if ( !chkSurvey2 )
  {
    setWidgetState( varname="t2Survey",   state="normal" )
    setWidgetState( varname="k2Survey",   state="normal" )
    
    setWidgetState( varname="idxSurvey2", state="disabled" )  
  }
  else
  {
    setWidgetState( varname="t2Survey",   state="disabled" )
    setWidgetState( varname="k2Survey",   state="disabled" )
    
    setWidgetState( varname="idxSurvey2", state="normal" )   
  }
  
}     # END function .setGuiDataState

# .setGuiMethodState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiMethodState <- function()
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.
  
  # Set options for assessment method.
  #setWidgetState( varname="chkMethod1", state="disabled" )
  
  if ( chkMethod1 )
  {
    setWidgetState( varname="t1Method",   state="disabled" )
    setWidgetState( varname="k1Method",   state="disabled" )

    setWidgetState( varname="idxMethod1", state="normal" )
  }
  else
  {
    setWidgetState( varname="t1Method",   state="normal" )
    setWidgetState( varname="k1Method",   state="normal" )
    
    setWidgetState( varname="idxMethod1", state="disabled" )
  }
  
  if ( chkMethod2 )
  {
    setWidgetState( varname="t2Method",   state="disabled" )
    setWidgetState( varname="k2Method",   state="disabled" )

    setWidgetState( varname="idxMethod2", state="normal" )
  }
  else
  {
    setWidgetState( varname="t2Method",   state="normal" )
    setWidgetState( varname="k2Method",   state="normal" )
    
    setWidgetState( varname="idxMethod2", state="disabled" )
  }
  
  if ( caaAges )
  {
    setWidgetState( varname="caaFixFSel", state="normal" )
  }
  else
  {
    #setWidgetState( varname="caaFixFSel", state="disabled" )
    #guiChanges$caaFixFSel <- TRUE    
  }
  
  if ( caaAgesS )
  {
    setWidgetState( varname="caaFixSSel", state="normal" )
  }
  else
  {
    #setWidgetState( varname="caaFixSSel", state="disabled" )
    #guiChanges$caaFixSSel <- TRUE
  }  
  
  if ( caaFSelOM )
  {
    setWidgetState( varname="caaSelVal50", state="disabled" )  
    setWidgetState( varname="caaSelVal95", state="disabled" )
  }
  else
  {
    setWidgetState( varname="caaSelVal50", state="normal" )  
    setWidgetState( varname="caaSelVal95", state="normal" )
  }
  
  if ( caaSSelOM )
  {
    setWidgetState( varname="caaSelSVal50", state="disabled" )  
    setWidgetState( varname="caaSelSVal95", state="disabled" )
  }
  else
  {
    setWidgetState( varname="caaSelSVal50", state="normal" )  
    setWidgetState( varname="caaSelSVal95", state="normal" )
  }
  
  setWinVal( guiChanges )
  return( invisible() )    
}     # END function .setGuiMethodState    


# .setGuiHcrState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiHcrState <- function( methodId=.CAAMOD )
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.
  
  popDynModel <- FALSE
  if ( methodId != .MOVAVG & methodId != .KALMAN )
    popDynModel <- TRUE

  declineRiskOn <- FALSE
  if ( hcrType=="declineRisk" & popDynModel )
    declineRiskOn <- TRUE
  
  # Method dependent conditions.
  
  if ( popDynModel )
  {
    #setWidgetState( varname="statusSource", radiovalue="statusSrceEst", state="normal" )
    
    setWidgetState( varname="statusSource", state="normal" )
    setWidgetState( varname="remRefSource", state="normal" )
    
    if ( remRefBase == "rrBaseFspr" )
    {
      setWidgetState( varname="sprX", state="normal" )
    }
    else
    {
      setWidgetState( varname="sprX", state="disabled" )
    }
  
    if ( remRefBase == "rrBaseFinput" )
    {
      setWidgetState( varname="inputF", state="normal" )
    }
    else
    {
      setWidgetState( varname="inputF", state="disabled" )
    }    
    
    setWidgetState( varname="hcrType",   state="normal" )
    setWidgetState( varname="ctlPtFreq", state="normal" )
    setWidgetState( varname="useMLE",    state="normal" )
    setWidgetState( varname="nMCMC",     state="normal" )
    setWidgetState( varname="nThin",     state="normal" )
    setWidgetState( varname="forecast",  state="normal" )
    setWidgetState( varname="skipFails", state="normal" )
    setWidgetState( varname="useSnow",   state="normal" ) 
    
    # Estimation method - MLE vs. MCMC logic.
    if ( useMLE  )
    {
      setWidgetState( varname="nMCMC", state="disabled" )
      setWidgetState( varname="nThin", state="disabled" )
      setWidgetState( varname="paAdj", state="disabled" )
      guiChanges$paAdj <- FALSE    
    }
    else
    {
      setWidgetState( varname="nMCMC", state="normal" )
      setWidgetState( varname="nThin", state="normal" )
      setWidgetState( varname="paAdj", state="normal" )        
    }
    
    
    
  }
  else
  {
    # Moving average or Kalman filter.
    
    # Must use OM for Stock Status and Reference Removal Rate.
    # There is no MLE versus MCMC, forecast, fails, or parellel proc.
    
    setWidgetState( varname="statusSource", state="disabled" )
    guiChanges$statusSource <- "statusSrceEquil"
    
    setWidgetState( varname="remRefSource", state="disabled" )
    guiChanges$remRefSource <- "rrSrceEquil"
    
    setWidgetState( varname="hcrType",   radiovalue="declineRisk", state="disabled" )
    setWidgetState( varname="ctlPtFreq", state="disabled" )
    setWidgetState( varname="useMLE",    state="disabled" )
    setWidgetState( varname="nMCMC",     state="disabled" )
    setWidgetState( varname="nThin",     state="disabled" )
    setWidgetState( varname="forecast",  state="disabled" )
    setWidgetState( varname="skipFails", state="disabled" )
    setWidgetState( varname="useSnow",   state="disabled" )         
    
        
  }     # ENDIF popDynMod

  # Method independent conditions.


  # KRH added; Aug.16, 2013  ================================
      # Ghost historical reference point fields unless historical option selected:
  if (statusBase=="statusBaseBt")
  {
    setWidgetState( varname="histLowBase", state="normal" )
    setWidgetState( varname="lowBaseStartYr", state="normal" )
    setWidgetState( varname="lowBaseEndYr", state="normal" )
    setWidgetState( varname="histUpperBase", state="normal" )
    setWidgetState( varname="upperBaseStartYr", state="normal" )
    setWidgetState( varname="upperBaseEndYr", state="normal" )
    setWidgetState( varname="lowBaseQuant", state="normal" )
    setWidgetState( varname="upperBaseQuant", state="normal" )
    
    #  Only activate lower quantile box if histLowBase=quantile method
    if (histLowBase=="Bquant")
    {
      setWidgetState( varname="lowBaseQuant", state="normal" )
    }
    else
    {
      setWidgetState( varname="lowBaseQuant", state="disabled" )
    }
    # Only activate upper quantile box if histUpperBase=quantile method
    if (histUpperBase=="Bquant")
    {
      setWidgetState( varname="upperBaseQuant", state="normal" )
    }
    else
    {
      setWidgetState( varname="upperBaseQuant", state="disabled" )
    }
  }  
  else
  {
    setWidgetState( varname="histLowBase", state="disabled" )
    setWidgetState( varname="lowBaseQuant", state="disabled" )
    setWidgetState( varname="lowBaseStartYr", state="disabled" )
    setWidgetState( varname="lowBaseEndYr", state="disabled" )
    setWidgetState( varname="histUpperBase", state="disabled" )
    setWidgetState( varname="upperBaseQuant", state="disabled" )
    setWidgetState( varname="upperBaseStartYr", state="disabled" )
    setWidgetState( varname="upperBaseEndYr", state="disabled" )
  }
  
  if (remRefBase=="rrBaseFt")
  {
    setWidgetState( varname="histFtBase", state="normal" )
    setWidgetState( varname="FtStartYr",  state="normal" )
    setWidgetState( varname="FtEndYr",    state="normal" )
    
    
    #  Only activate lower quantile box if histFtBase=quantile method
    if ( histFtBase=="Fquant" )
      setWidgetState( varname="histFtBaseQuant", state="normal" )
    else
      setWidgetState( varname="histFtBaseQuant", state="disabled" )
  }  
  else
  {
    setWidgetState( varname="histFtBase",      state="disabled" )
    setWidgetState( varname="histFtBaseQuant", state="disabled" )
    setWidgetState( varname="FtStartYr",       state="disabled" )
    setWidgetState( varname="FtEndYr",         state="disabled" )
  }
  
 # ==================== End of KRH added ====================== 

  if ( hcrType == "constantF" )
  {
    setWidgetState( varname="lowerBoundMult", state="disabled" )
    setWidgetState( varname="upperBoundMult", state="disabled" )
    # KRH added the following items; 19-Aug-2013:
    setWidgetState( varname="statusBase", state="disabled" )
    setWidgetState( varname="histLowBase", state="disabled" )
    setWidgetState( varname="lowBaseQuant", state="disabled" )
    setWidgetState( varname="lowBaseStartYr", state="disabled" )
    setWidgetState( varname="lowBaseEndYr", state="disabled" )
    setWidgetState( varname="histUpperBase", state="disabled" )
    setWidgetState( varname="upperBaseQuant", state="disabled" )
    setWidgetState( varname="upperBaseStartYr", state="disabled" )
    setWidgetState( varname="upperBaseEndYr", state="disabled" )
    setWidgetState( varname="statusSource", state="disabled" )
  }
  else
  {
    setWidgetState( varname="lowerBoundMult", state="normal" )
    setWidgetState( varname="upperBoundMult", state="normal" ) 
    setWidgetState( varname="statusBase", state="normal" ) 
    # KRH added the following items; 19-Aug-2013:
    if (statusBase=="statusBaseBt") {
      setWidgetState( varname="histLowBase", state="normal" ) 
      setWidgetState( varname="lowBaseQuant", state="normal" ) 
      setWidgetState( varname="lowBaseStartYr", state="normal" ) 
      setWidgetState( varname="lowBaseEndYr", state="normal" ) 
      setWidgetState( varname="histUpperBase", state="normal" ) 
      setWidgetState( varname="upperBaseQuant", state="normal" ) 
      setWidgetState( varname="upperBaseStartYr", state="normal" ) 
      setWidgetState( varname="upperBaseEndYr", state="normal" )
      setWidgetState( varname="statusSource", state="normal" )
    }
    else
    {
      setWidgetState( varname="histLowBase", state="disabled" )
      setWidgetState( varname="lowBaseQuant", state="disabled" )
      setWidgetState( varname="lowBaseStartYr", state="disabled" )
      setWidgetState( varname="lowBaseEndYr", state="disabled" )
      setWidgetState( varname="histUpperBase", state="disabled" )
      setWidgetState( varname="upperBaseQuant", state="disabled" )
      setWidgetState( varname="upperBaseStartYr", state="disabled" )
      setWidgetState( varname="upperBaseEndYr", state="disabled" )
    }
    # End of KRH added
  }

  # Set options for control rule types.
  if ( declineRiskOn )
  {
    setWidgetState( varname="useMLE",        state="disabled" )
    guiChanges$useMLE <- FALSE
    
    setWidgetState( varname="paAdj",         state="disabled" )
    guiChanges$paAdj <- FALSE
    
    setWidgetState( varname="nMCMC",         state="normal" )
    setWidgetState( varname="nThin",         state="normal" )    
  
    setWidgetState( varname="Inc1a",         state="normal" )
    setWidgetState( varname="Inc1b",         state="normal" )
    setWidgetState( varname="Inc2a",         state="normal" )
    setWidgetState( varname="Inc2b",         state="normal" )
    setWidgetState( varname="Inc3a",         state="normal" )
    setWidgetState( varname="Inc3b",         state="normal" )
    setWidgetState( varname="Stable1a",      state="normal" )
    setWidgetState( varname="Stable1b",      state="normal" )
    setWidgetState( varname="Stable2a",      state="normal" )
    setWidgetState( varname="Stable2b",      state="normal" )
    setWidgetState( varname="Stable3a",      state="normal" )
    setWidgetState( varname="Stable3b",      state="normal" )
    setWidgetState( varname="Dec1a",         state="normal" )
    setWidgetState( varname="Dec1b",         state="normal" )
    setWidgetState( varname="Dec2a",         state="normal" )
    setWidgetState( varname="Dec2b",         state="normal" )
    setWidgetState( varname="Dec3a",         state="normal" )
    setWidgetState( varname="Dec3b",         state="normal" )
    
    setWidgetState( varname="trendYears",    state="normal" )
    setWidgetState( varname="lowerTrendPct", state="normal" )
    setWidgetState( varname="upperTrendPct", state="normal" )
    
    setWidgetState( varname="nProjYears",    state="normal" )
    setWidgetState( varname="lowerQuota",    state="normal" )
    setWidgetState( varname="upperQuota",    state="normal" )
    setWidgetState( varname="binQuota",      state="normal" )
    
    # There are NO F control points for decline risk.
    setWidgetState( varname="remRefBase",    state="disabled" )
    setWidgetState( varname="remRefSource",  state="disabled" )
    setWidgetState( varname="Fmult",         state="disabled" )                          
  }
  else
  {
    setWidgetState( varname="useMLE",        state="normal" )
    setWidgetState( varname="paAdj",         state="normal" )
    
    if ( useMLE )
    {
      setWidgetState( varname="nMCMC",       state="disabled" )
      setWidgetState( varname="nThin",       state="disabled" )
      setWidgetState( varname="paAdj",       state="disabled" )
      guiChanges$paAdj <- FALSE
    }
    else
    {
      guiChanges$paAdj <- TRUE
    }
      
    setWidgetState( varname="Inc1a",         state="disabled" )
    setWidgetState( varname="Inc1b",         state="disabled" )
    setWidgetState( varname="Inc2a",         state="disabled" )
    setWidgetState( varname="Inc2b",         state="disabled" )
    setWidgetState( varname="Inc3a",         state="disabled" )
    setWidgetState( varname="Inc3b",         state="disabled" )
    setWidgetState( varname="Stable1a",      state="disabled" )
    setWidgetState( varname="Stable1b",      state="disabled" )
    setWidgetState( varname="Stable2a",      state="disabled" )
    setWidgetState( varname="Stable2b",      state="disabled" )
    setWidgetState( varname="Stable3a",      state="disabled" )
    setWidgetState( varname="Stable3b",      state="disabled" )
    setWidgetState( varname="Dec1a",         state="disabled" )
    setWidgetState( varname="Dec1b",         state="disabled" )
    setWidgetState( varname="Dec2a",         state="disabled" )
    setWidgetState( varname="Dec2b",         state="disabled" )
    setWidgetState( varname="Dec3a",         state="disabled" )
    setWidgetState( varname="Dec3b",         state="disabled" )
      
    setWidgetState( varname="trendYears",    state="disabled" )
    setWidgetState( varname="lowerTrendPct", state="disabled" )
    setWidgetState( varname="upperTrendPct", state="disabled" )
    
    setWidgetState( varname="nProjYears",    state="disabled" )
    setWidgetState( varname="lowerQuota",    state="disabled" )
    setWidgetState( varname="upperQuota",    state="disabled" )
    setWidgetState( varname="binQuota",      state="disabled" )
    
    # Make sure F controls are ON.
    if ( popDynModel )
    {
      setWidgetState( varname="remRefBase",    state="normal" )
      setWidgetState( varname="remRefSource",  state="normal" )
    }
    setWidgetState( varname="Fmult",         state="normal" )                   
  }
  
  if ( hcrType == "hcrUser" )
  {
  }
  else
  {
  }
# KRH - decativated Aug 16, 2013
 # setWidgetState( varname="statusBase" ,  radiovalue="statusBaseBt", state="disabled" )
  setWidgetState( varname="hcrType",      radiovalue="hcrUser",        state="disabled" )  
  
  setWinVal( guiChanges )
  return( invisible() )
}     # END function .setGuiHcrState  


# .setGuiOpModState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiOpModState <- function()
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  # Where is the stock history coming from?
  if ( historyType == "omFile" )
  {
    hstFile <- file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE )
    if ( !file.exists( hstFile ) )
    {
      cat( "\nMSG (.setGuiOpModState) Stock history file not found: ", hstFile,"\n" )
      alarm()
      guiChanges$simHistoryType <- "omAuto"
    }
    setWidgetState( varname="initDepTarg", state="disabled", winname="mseRguiOpMod" )
    setWidgetState( varname="M",           state="disabled", winname="mseRguiOpMod" )
    cat( "\nMSG (.setGuiOpModState) Start M specified at tMP-1 by last Mt in history file.\n" )
  }
  else
  {
    setWidgetState( varname="initDepTarg", state="normal", winname="mseRguiOpMod" )
    setWidgetState( varname="M",           state="normal", winname="mseRguiOpMod" )
    cat( "\nMSG (.setGuiOpModState) Start M from GUI applies to t=1.\n" )
  }

  setWinVal( guiChanges, winName=win )
  
  return( invisible() )
}     # END function .setGuiOpModState

  
.getSimStateList <- function( ctlList )
{
  methodId <- ctlList$mp$assess$methodId

  simAge <- "Off"
  if ( methodId==5 && ctlList$mp$assess$caaAges)
    simAge <- "On"

  simAgeS <- "Off"
   if ( methodId==5 && ctlList$mp$assess$caaAgesS)
    simAgeS <- "On"
        
  simSurvey <- "Abs"

  if ( methodId==3 )
    if ( ctlList$mp$assess$spSurveyRel )
      simSurvey <- "Rel"
  if ( methodId==4 )
    if ( ctlList$mp$assess$ddSurveyRel )
      simSurvey <- "Rel"
  if ( methodId==5 )
    #if ( ctlList$mp$assess$caaSurveyRel )
      simSurvey <- NA
      
  stateList <- list( methodName=.METHODLAB[ctlList$mp$assess$methodId],
                     hcrName=ctlList$mp$hcr$hcrType,
                     simAge=simAge, simAgeS=simAgeS, simSurvey=simSurvey )
  stateList
}     #END function .getSimStateList

# .setGuiSimState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiSimState <- function( stateList )
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  methodName <- .METHODLAB[ stateList$methodId ]
  
  guiChanges$methodName <- stateList$methodName
  guiChanges$hcrName    <- stateList$hcrName
  guiChanges$simAge     <- stateList$simAge
  guiChanges$simAgeS     <- stateList$simAgeS
  guiChanges$simSurvey  <- stateList$simSurvey

  setWidgetState( varname="simSetXaxis", state="disabled", winname="mseRguiSim" )
  setWidgetState( varname="simSetYaxis", state="disabled", winname="mseRguiSim" )
  
  setWinVal( guiChanges, winName=win )
  
  return( invisible() )
}     # END function .setGuiSimState  

#-----------------------------------------------------------------------------##
#-- Simulation GUI Plotting Functions                                       --##
#-----------------------------------------------------------------------------##

# .doGuiSimPlots (Wrapper to get guiSim parameters and call the required plot)
# Purpose:       Calls the plot specified by simPlotType that is returned from
#                the Simulation GUI (simGUI).
# Parameters:    NONE.
# Returns:       NULL (invisibly).
# Source:        A.R. Kronlund
.doGuiSimPlots <- function( ctlPars )
{
  if ( is.data.frame( ctlPars ) )
    ctlList <- .createList( ctlPars )
  else
    ctlList <- ctlPars

  # Get simPlotType and plotting parameters from mseRguiSim.
  guiInfo <- getWinVal(scope="L", winName="mseRguiSim" ) # GUI information local scope

  checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
  names( checked ) <- c( "Fmsy",  "F0",  "F01",  "F40",  "Fmax",  "Fcra"  )
  
  # Save par settings that can be restored (some are readonly).
  oldpar <- par( no.readonly=TRUE )
  
  simAuto <- TRUE                    # This may be moved to GUI at some point.

  if ( .PLTMSG )
    cat( "\nMSG (.doGuiSimPlots) Calling ",simPlotType,"\n" )
  
  if ( simPlotType == "simSpecs" )
  {
     if ( simAuto )
       par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
       
    .plotCtlPars( ctlPars )
  }
  
  else if ( simPlotType == "simOM" )
  {
    if ( simAuto )
       par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
         
    .plotCtlPars( ctlPars, type="opMod" )
  }
  
  else if ( simPlotType == "simMP" )
  {
    if ( simAuto )
       par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    .plotCtlPars( ctlPars, type="mp" )
  }
  
  # Equilibrium reference points.
  
  else if ( simPlotType == "simYprF" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    obj <- list()
    obj$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
  
    .plotYprF( list(obj), gfx=list( annotate=simAnnotate, checked=checked,
                      doLegend=simLegend, xLim=NULL, yLim=NULL ) ) 
  }    
  
  else if ( simPlotType == "simRecSSB" )
  {
    if ( simAuto )
       par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    obj <- list()
    obj$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
  
    .plotRecSSB( list(obj), gfx=list( annotate=simAnnotate, checked=checked,
                 doLegend=simLegend, xLim=NULL, yLim=NULL ) ) 
  }
  
  else if ( simPlotType == "simSsbF" )
  {
    if ( simAuto )
       par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    obj <- list()
    obj$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
  
    .plotSsbF( list(obj), gfx=list( annotate=simAnnotate, checked=checked,
               doLegend=simLegend, xLim=NULL, yLim=NULL ) ) 
  }
 
  else if ( simPlotType == "simSsbRF" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  
    obj <- list()
    obj$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
  
    .plotSsbPerRecF( list(obj), gfx=list( annotate=simAnnotate, checked=checked,
                     doLegend=simLegend, xLim=NULL, yLim=NULL ) ) 
  }
  
  else if ( simPlotType == "simYieldF" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    obj <- list()
    obj$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
  
    .plotYieldF( list(obj), gfx=list( annotate=simAnnotate, checked=checked,
                      doLegend=simLegend, xLim=NULL, yLim=NULL ) ) 
  }  

  else if ( simPlotType == "simYieldSSB" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    obj <- list()
    obj$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
  
    .plotYieldSSB( list(obj), gfx=list( annotate=simAnnotate, checked=checked,
                      doLegend=simLegend, xLim=NULL, yLim=NULL ) ) 
  }
  
  # KRH added ability to plot multiple reference removal rates, or just one

  else if ( simPlotType == "simHCRopts" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    if ( ctlList$mp$hcr$hcrType=="variableF" )
      .plotHCRvariableF( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend, multRefRR=TRUE ) )

    if ( ctlList$mp$hcr$hcrType=="declineRisk" )
    {
      par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
      .plotHCRdeclineRisk( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend ) )
    }

    if ( ctlList$mp$hcr$hcrType=="constantF" )
      .plotHCRconstantF( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend, multRefRR=TRUE ) )

    if ( ctlList$mp$hcr$hcrType=="constantC" )
      .plotHCRconstantC( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend ) )
      
    if ( ctlList$mp$hcr$hcrType=="hcrUser" )
      .plotStatus( "MSG (.doSimPlots) No User Defined Harvest Control Rule Exists" )
  }    

  else if ( simPlotType == "simHCR" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    if ( ctlList$mp$hcr$hcrType=="variableF" )
      .plotHCRvariableF( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend, multRefRR=FALSE ) )

    if ( ctlList$mp$hcr$hcrType=="declineRisk" )
    {
      par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
      .plotHCRdeclineRisk( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend ) )
    }

    if ( ctlList$mp$hcr$hcrType=="constantF" )
      .plotHCRconstantF( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend, multRefRR=FALSE ) )

    if ( ctlList$mp$hcr$hcrType=="constantC" )
      .plotHCRconstantC( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend ) )
      
    if ( ctlList$mp$hcr$hcrType=="hcrUser" )
      .plotStatus( "MSG (.doSimPlots) No User Defined Harvest Control Rule Exists" )
  }
  
  # Life history plots.
  else if ( simPlotType=="simLifeHist" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    .plotLifeHist( ctlList$opMod, gfx=list( annotate=simAnnotate,
      doLegend=simLegend, xLim=NULL, yLim=NULL ) )
  }
    
  else if ( simPlotType=="simLenAtAge" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( simSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( simSetYaxis )
      yLim <- c( minLen,maxLen )
     
    .plotLenAtAge( .calcSchedules( ctlList$opMod ),
                   gfx=list( annotate=simAnnotate, xLim=xLim, yLim=yLim ) )
  }
  
  else if ( simPlotType=="simMatAtAge" )
  {
    if ( simAuto )
       par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    
    xLim <- NULL
    if ( simSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( simSetYaxis )
      yLim <- c( 0.0,1.0 )
    .plotMatAtAge( .calcSchedules( ctlList$opMod ),
                   gfx=list( annotate=simAnnotate, xLim=xLim, yLim=yLim ) )
  }
  
  else if ( simPlotType=="simNumAtAge" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    yLim <- NULL
    .plotNumAtAge( calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) ),
       gfx=list( annotate=simAnnotate, doLegend=simLegend, xLim=xLim, yLim=yLim ) )
  }
  
  else if ( simPlotType=="simWgtAtAge" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( simSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( simSetYaxis )
      yLim <- c( minWgt,maxWgt )
    .plotWgtAtAge( .calcSchedules( ctlList$opMod ), gfx=list( xLim=xLim, yLim=yLim ) )
  }
  
  else if ( simPlotType=="simWgtLen" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( simSetXaxis )
      xLim <- c( minLen,maxLen )
    yLim <- NULL
    if ( simSetYaxis )
      yLim <- c( minWgt,maxWgt )
    .plotWgtLen( .calcSchedules( ctlList$opMod ), gfx=list( xLim=xLim, yLim=yLim ) )
  }  

  else if ( simPlotType=="simSel" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( simSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( simSetYaxis )
      yLim <- c( 0.0,1.0 )
     
    result <- .plotSelAtAge( .calcSchedules( ctlList$opMod ), type="Fishery",
                 gfx=list( annotate=simAnnotate, xLim=xLim, yLim=yLim ) )
    
    cat( "\nFishery selectivity at age:\n" )                 
    print( result )    
  }

    else if ( simPlotType=="simSelS" )
    {
      if ( simAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
      xLim <- NULL
      if ( simSetXaxis )
        xLim <- c( minAge,maxAge )
      yLim <- NULL
      if ( simSetYaxis )
        yLim <- c( 0.0,1.0 )
       
      result <- .plotSelAtAge( .calcSchedules( ctlList$opMod ), type="Survey",
                   gfx=list( annotate=simAnnotate, xLim=xLim, yLim=yLim ) )
      cat( "\nSurvey selectivity at age:\n" )
      print( result )    
  }

  else if ( simPlotType=="simRefPts" )
  {
    if ( simAuto )
    {
      #par( oma=c(2,1.5,2,1), mar=c(3,4,1,1), mfrow=c(3,2) )
      myMar <- .MAR
      myMar[1] <- 3
      myMar[2] <- 3
      myMar[3] <- 2.5
      myMar[4] <- 3
      par( oma=.OMA, mar=myMar, mfrow=c(3,2) )
    }
    
    # The equilibrium ref pt plots are designed to take a list of blobs for
    # overlaying different simulations.  So when passing just one, the input
    # has to be rendered as the first element in a list.  Hence this stuff.
    # Avoids two separate sets of plotting functions.
    
    tmp <- list()
    tmp$refPtList <- calcRefPoints( ctlList$opMod, rpList=list( FALL=TRUE ) )
    
    .plotSimRefPoints( list(tmp), gfx=list( annotate=simAnnotate, checked=checked,
      doLegend=simLegend, setXaxis=simSetXaxis, setYaxis=simSetYaxis ) )  
  }

  else if ( simPlotType == "simIndexPars" )
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )    
    
    xLim <- NULL
    yLim <- NULL
    
    .plotMPindexPars( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend,
      xLim=xLim, yLim=yLim, useYears=simYrs ) )
  }
  
  else if ( simPlotType == "simDesign" )
  {
    if ( simAuto )
    {
      myMar <- .MAR
      myMar[2] <- 6
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
    
    xLim <- NULL
    yLim <- NULL
  
    .plotDesign( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend,
      xLim=xLim, yLim=yLim, useYears=simYrs ) ) 
  }

  else if ( simPlotType=="simHist" )
  {
    if ( simAuto )
    {
      myMar <- .MAR
      myMar[4] <- 2
      myMar[2] <- 3
      par( oma=.OMA, mar=myMar, mfrow=c(3,1) )
    }
    
    xLim <- NULL
    if ( simSetXaxis )
      xLim <- c( simMinYr, simMaxYr )
    
    yLim <- NULL
  
    if ( ctlList$opMod$historyType=="omFile" )
    {
      # Check if history file active, and consistent with tMP.
      if ( .validHistoryFile( ctlList$opMod$historyType, ctlList$opMod$tMP ) )
        .plotHistory( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend,
          xLim=xLim, yLim=yLim, useYears=simYrs ) )
      else
        .plotStatus( "(.doGuiSimPlots) History file inconsistent with tMP" )
    }
    else
    {
      cat( "\nMSG (.subSim) Calculating historic fishing mortalities for initial",
           "depletion target of ", ctlList$opMod$initDepTarg,"\n" )
      .plotHistory( ctlList, gfx=list( annotate=simAnnotate, doLegend=simLegend,
                      xLim=xLim, yLim=yLim, useYears=simYrs ) )
    }
  }
  
  else if ( simPlotType=="simUser1" )
  {
    .plotStatus( paste( simPlotType,"not implemented...\n" ) )
  }
  
  else if ( simPlotType=="simUser2" )
  {
    .plotStatus( paste( simPlotType,"not implemented...\n" ) )
  }
  
  else if ( simPlotType=="simUser3" )
  {
    .plotStatus( paste( simPlotType,"not implemented...\n" ) )
  }
  
  else if ( simPlotType=="simUser4" )
  {
    .plotStatus( paste( simPlotType,"not implemented...\n" ) )
  }
  
  else if ( simPlotType=="simUser5" )
  {
    .plotStatus( paste( simPlotType,"not implemented...\n" ) )
  }
    
  else
  {
    if ( simAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    .plotStatus( paste( simPlotType,"not implemented...\n" ) )
  }
  
  if ( simStamp )
  {
    .addStamp( iSim=-1, iRep=-1, nReps, simFolder="",
                 scenario=scenarioLabel,
                 procedure=mpLabel,
                 showFile=FALSE, outLine=NULL,
                 side=1, col=.COLSTAMP, cex=.CEXSTAMP )
  }

  # Restore the graphics settings.
  par( oldpar )
  return( invisible() )
}     # END function .doGuiSimPlots

#------------------------------------------------------------------------------#
#-- Helper Functions                                                         --#
#------------------------------------------------------------------------------#

.formatRefPts <- function( refPts, digits=NULL )
{
  result <- list()
  # Assign the reference points to the new GUI list.
  
  if ( !is.null(digits) )
  {
    result$ssbFmsy   <- round( refPts$ssbFmsy,   digits=4 )
    result$yieldFmsy <- round( refPts$yieldFmsy, digits=4 )
    result$F0        <- round( refPts$F0,        digits=4 )
    result$F01       <- round( refPts$F01,       digits=4 )
    result$F40       <- round( refPts$F40,       digits=4 )
    result$Fmsy      <- round( refPts$Fmsy,      digits=4 )
    result$Fcra      <- round( refPts$Fcra,      digits=4 )
    result$Fmax      <- round( refPts$Fmax,      digits=4 )
  }
  else
  {
    result$ssbFmsy   <- refPts$ssbFmsy
    result$yieldFmsy <- refPts$yieldFmsy
    result$F0        <- refPts$F0
    result$F01       <- refPts$F01
    result$F40       <- refPts$F40
    result$Fmsy      <- refPts$Fmsy
    result$Fcra      <- refPts$Fcra
    result$Fmax      <- refPts$Fmax
  }

  result
}     # END function .formatRefPoints


.getPdecline <- function( obj )
{
  # Assembles Probability of future decline matrix.
  
  pDecline <- matrix( c( obj$Inc1a, obj$Inc1b, obj$Inc2a,
                         obj$Inc2b, obj$Inc3a, obj$Inc3b,
                         obj$Stable1a, obj$Stable1b, obj$Stable2a,
                         obj$Stable2b, obj$Stable3a, obj$Stable3b,
                         obj$Dec1a, obj$Dec1b, obj$Dec2a,
                         obj$Dec2b, obj$Dec3a, obj$Dec3b ), nrow=3, ncol=6,
                         byrow=TRUE )
                           
  pDecline
}     # END function getPdecline


.calcIndexCV <- function( obj )
{
  nT         <- obj$opMod$nT
  #nStations1 <- obj$mp$data$nStations1
  #nStations2 <- obj$mp$data$nStations2
  tauSurvey  <- mean( c( obj$mp$data$tauSurvey1Min,obj$mp$data$tauSurvey1Max ) )
  t1Survey   <- obj$mp$data$t1Survey
  t2Survey   <- obj$mp$data$t2Survey
  
  # Generate coefficient of variation for Old and New surveys. 
  if( tauSurvey==0 )
  {
    surveyError1 <- 0.0
    surveyError2 <- 0.0
  }
  else
  {
    surveyError1  <- tauSurvey
    #sdev          <- tauSurvey * sqrt( nStations1 )
    #surveyError2  <- sdev      / sqrt( nStations2 )
  }

  # Concatenate the two vectors of standard errors.
  surveyError <- rep( NA, nT )
  surveyError[ 1:(t2Survey-1) ] <- surveyError1
  surveyError[ t2Survey:nT ]    <- surveyError2

  surveyError
}     # END function .calcIndexCV
