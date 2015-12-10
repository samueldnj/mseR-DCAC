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
#-- mseRguiOptFuns.r: An mseR module that provides graphical user interface  --#
#-- capabilities for changing mseR general options, mostly plotting.         --#
#--                                                                          --#
#-- Usage:        guiOpt()                                                   --#
#-- Side Effects: Changes mseR options.                                      --#
#--                                                                          --#
#-- Logic Flow: guiOpt -> .mseRsetupOpt -> .wdSetup -> createWin -> .subOpt  --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
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
# guiOpt       : Run mseR simulation Performance measures GUI.                 #
#                                                                              #
# GUI Error Checking/Validation:                                               #
#                                                                              #
# .validOptPars: Check if the Perf GUI parameters are valid.                   #
#                                                                              #
# GUI Submit Control Functions:                                                #
#                                                                              #
# .subOpt      : Processes guiPerf submit actions, e.g., buttons.              #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
# TASK LIST:                                                                   #
#                                                                              #
# 2. Add labels to status plot, i.e., .CtCEX, .CtLWD, etc.                     #
# 6. Add file for save using selectFile                                        #
# 9. Implement greyscale option.                                               #
# 12. Implement plot for test colors                                           #
# 18. Options get evaluated when any mseR module fires up, or just guiSim and  #
#     guiOpt?                                                                  #
#------------------------------------------------------------------------------#
#                                                                              #
# FUNCTIONS INCLUDED IN FILE                                                 --#
# (listed in order of occurence)                                             --#
#                                                                            --#
# --------------------------- GUI Setup Functions -----------------------------# 
#                                                                              #
# .mseRguiOptSetup                                                             #
# .subOpt            Processes Options page submit functions                   #
#                                                                              #
#------------------------------------------------------------------------------#

.DEFGENOPT  <- "defaultGeneralOptions.csv"
.DEFPRJOPT  <- "defaultProjectOptions.csv"
.FGENOPT    <- "mseRgenOptData.csv"
.FPRJOPT    <- "mseRprjOptData.csv"

.NGENOPT    <- 100
.NPRJOPT    <- 100

#------------------------------------------------------------------------------#
#-- Gui setup and execution functions                                        --#
#------------------------------------------------------------------------------#

guiOpt <- function()
{
  return( .mseRsetupOpt( "mseRguiOpt" ) )
}     # END function guiOpt.

# .mseRsetupOpt  (mseR GUI setup for Options GUI creation)
# Purpose:    Set up and run the specified GUI.
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Author:     A.R. Kronlund
# Source:     mseR(Sablefish) (modified)
.mseRsetupOpt <- function( win )
{
  # Get the required libraries.
  require( PBSmodelling )            # GUI functions
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  assign( gvar, list(), pos=1 )      # Hidden global list

  # Copy the GUI window description files and any .exe files to temp directory.
  wkDir <- .wdSetup()
  cat( "\nMSG (.mseRguiSetup) Working directory setup in ", wkDir, "\n" ) 

  closeWin()                         # Close all GUI windows to prevent crashes
  graphics.off()                     # Close all open R graphics windows

  if ( exists( ".guiOptPars" ) )
    rm( .guiOptPars, pos=1 )         # Remove the GUI parameters.

  .mseRinitProject( wkDir )          # Initialize mseR project, paths, options.

  # Initialize general mseR options.
  genOptFile <- file.path( .PRJFLD, .DEFOPTFLD, basename(.FGENOPT) )
  genOptData <- .readGenOptFile( genOptFile )
  
  # Initialize project specific mseR options.
  prjOptFile <- file.path( .PRJFLD, .DEFOPTFLD, basename(.FPRJOPT) )
  prjOptData <- .readPrjOptFile( prjOptFile )

  goMenu    <- TRUE                  # Can a menu be created?
  if ( is.null( genOptData ) | is.null( prjOptData ) )
    goMenu <- FALSE

  # Tracking object valid so go ahead with menus.
  if ( goMenu )
  {
    #--------------------------------------------------------------------------#
    # Initialize GUI from the description file using PBSmodelling createWin.   #
    #--------------------------------------------------------------------------#
    
    createWin( paste( wkDir, "/", win, "Win.txt", sep="" ) )

    win     <- .getWinName()            # Get the current window name
    act     <- getWinAct()[1]           # Get last menu window action
    guiInfo <- getWinVal( winName=win ) # Extract the GUI parameter list
    
    # If .guiOptPars exists, restore parameters.
    if ( exists( ".guiOptPars" ) )
      setWinVal( .guiOptPars, winName=win )
    
    guiChanges <- list()
    
    # Files.
    guiChanges$genOptFile <- genOptFile
    guiChanges$prjOptFile <- prjOptFile
    
    # Now set the values in the entry widgets for color, lty, lwd, etc.
    iVar <- .getIdx( prjOpts$select )
    guiChanges$maxVal   <- prjOptData$maxVal[iVar]
    guiChanges$minVal   <- prjOptData$minVal[iVar]    
    guiChanges$varDesc  <- prjOptData$description[iVar]
    guiChanges$varLab   <- prjOptData$label[iVar]
    guiChanges$varType  <- prjOptData$varType[iVar]
    guiChanges$varUnits <- prjOptData$units[iVar]
    
    setWinVal( guiChanges, win )
    .setGuiOptState( prjOpts[iVar,] )
    guiInfo <- getWinVal( winName=win )
    assign( ".guiOptPars",guiInfo,pos=1 )
 
    .doOptPlots( prjOptData, iVar=iVar, optPlotType="status" )
  }
  else
  {
    cat( "\nMSG (.mseRsetupOpt) Cannot create options settings, guiOpt exiting.\n" )
    stop()
  }
  return( invisible() )
}     # END function .mseRguiSetup

#------------------------------------------------------------------------------#
# GUI Submit Functions                                                         #
#                                                                              #
# .subOpt manages the calls to individual GUI Pages in notebook widget:        #
#------------------------------------------------------------------------------#

# .subOpt     (Main function to process GUI submit actions.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subOpt <- function()
{
  win        <- .getWinName()         # Get the current window name
  act        <- getWinAct()[1]        # Get last menu window action

  guiInfo    <- getWinVal( scope="L", winName=win )
  guiChanges <- list()                # List for any changes

  isExit      <- FALSE
  optPlotType <- "status"
  updateGUI   <- FALSE
  
  iVar <- .getIdx( prjOpts$select )
  
  #----------------------------------------------------------------------------#
  #--                          GUI Options Actions                           --#
  #----------------------------------------------------------------------------#    

  if ( act=="selPrj" )
  {
    initDir <- file.path( .PRJFLD, .DEFOPTFLD )
    tmpFile <- selectFile( initialfile="", initialdir=initDir,
                 filetype=list( c(".csv","Comma Seperated Value Files") ),
                 title="Select project options file", usewidget="prjOptFile" )

    if ( !is.null( tmpFile ) )
    {
      # Update the option file names.
      guiChanges$prjOptFile <- tmpFile
      
      # Read the option settings.
      prjOptData <- .readPrjOptFile( fileName=prjOptFile )

      # These were saved as hidden globals by .readGenOptFile and .readPrjOptFile.
      prjOpts              <- get( "prjOpts", pos=1 )
      guiChanges$prjOpts   <- prjOpts
      
      iVar <- 1
      guiChanges$maxVal   <- prjOptData$maxVal[iVar]
      guiChanges$minVal   <- prjOptData$minVal[iVar]
      guiChanges$varDesc  <- prjOptData$description[iVar]
      guiChanges$varLab   <- prjOptData$label[iVar]
      guiChanges$varType  <- prjOptData$varType[iVar]
      guiChanges$varUnits <- prjOptData$units[iVar]      

      # Now set the colours, lines, symbols.
      guiChanges$valBG    <- prjOptData$bg[iVar]
      guiChanges$valFG    <- prjOptData$fg[iVar]
      guiChanges$valCOL   <- prjOptData$col[iVar]
      guiChanges$valLTY   <- prjOptData$lty[iVar]
      guiChanges$valLWD   <- prjOptData$lwd[iVar]
      guiChanges$valPCH   <- prjOptData$pch[iVar]
      guiChanges$valCEX   <- prjOptData$cex[iVar]    
     
      # Save the new option settings and update.
      .saveOptionsToScript( genOptData, prjOptData )
      source( "mseRoptions.r" )
      
      updateGUI <- TRUE      
    }
    else
    {
      alarm()
      cat( "\nMSG (.subOpt) No file selected, projection options file unchanged.\n" )
    }
  }

  if ( act=="prjOpts" )
  {
    # New selection? if so, accept selection, set old one to FALSE.
    if ( length(iVar) > 1 )
    {
      prjOpts$select[.oldVar] <- FALSE
      assign( "prjOpts", prjOpts, pos=1 )

      iVar <- .getIdx( prjOpts$select )
      
      assign( ".oldVar",iVar,pos=1 )

      guiChanges$prjOpts  <- prjOpts
      
      # Because there was a selection, update description, label, units.
      guiChanges$maxVal   <- prjOptData$maxVal[iVar]
      guiChanges$minVal   <- prjOptData$minVal[iVar]      
      guiChanges$varDesc  <- prjOptData$description[iVar]
      guiChanges$varLab   <- prjOptData$label[iVar]
      guiChanges$varType  <- prjOptData$varType[iVar]
      guiChanges$varUnits <- prjOptData$units[iVar]
      
      # Now set the colours, lines, symbols.
      guiChanges$valBG    <- prjOptData$bg[iVar]
      guiChanges$valFG    <- prjOptData$fg[iVar]
      guiChanges$valCOL   <- prjOptData$col[iVar]      
      guiChanges$valLTY   <- prjOptData$lty[iVar]
      guiChanges$valLWD   <- prjOptData$lwd[iVar]
      guiChanges$valPCH   <- prjOptData$pch[iVar]
      guiChanges$valCEX   <- prjOptData$cex[iVar]
    }    
    else
    {
      prjOpts$select[.oldVar] <- TRUE
      assign( "prjOpts", prjOpts, pos=1 )
      guiChanges$prjOpts  <- prjOpts
      
      iVar <- .oldVar
                
      # Update prjOptData.
      colNames <- names( prjOpts )
      for ( i in 1:length(colNames) )
        prjOptData[ iVar,colNames[i] ] <- prjOpts[ iVar,colNames[i] ]
        
      prjOptData[ iVar, "description" ] <- varDesc
      prjOptData[ iVar, "label"       ] <- varLab
      prjOptData[ iVar, "units"       ] <- varUnits
      prjOptData[ iVar, "maxVal"      ] <- maxVal
      prjOptData[ iVar, "minVal"      ] <- minVal
      prjOptData[ iVar, "varType"     ] <- varType
    }
    updateGUI <- TRUE
  }
  
  if ( act=="selGen" )
  {
    initDir <- file.path( .PRJFLD, .DEFOPTFLD )
    tmpFile <- selectFile( initialfile="", initialdir=initDir,
                 filetype=list( c(".csv","Comma Seperated Value Files") ),
                 title="Select general options file", usewidget="genOptFile" )

    if ( !is.null( tmpFile ) )
    {
      # Update the option file names.
      guiChanges$genOptFile <- tmpFile
      
      # Read the option settings.
      genOptData <- .readGenOptFile( fileName=genOptFile )

      # These were saved as hidden globals by .readGenOptFile and .readPrjOptFile.
      genOpts              <- get( "genOpts", pos=1 )
      guiChanges$genOpts   <- genOpts

      .saveOptionsToScript( genOptData, prjOptData )
      source( "mseRoptions.r" )
      
      updateGUI <- TRUE      
    }
    else
    {
      alarm()
      cat( "\nMSG (.subOpt) No file selected, general options file unchanged.\n" )
    }
  }
  
  if ( act=="genOpts" )
  {
    assign( "genOpts", genOpts, pos=1 )
      
    guiChanges$genOpts  <- genOpts
    updateGUI <- TRUE
  }  
  
  if ( act=="optBG" || act=="valBG" )
  {
    if ( act=="optBG" )
    {
      tmpBG <- pickCol()               # hexadecimal
      if ( tmpBG!="" )
      {
        tmpBG <- toupper( tmpBG )        # Convert hex to upper case.
        valBG <- .hex2color( tmpBG )     # Convert to color string.
      }
    }
    
    if ( act=="valBG" )
    {
      # Does the color exist in colors()?
      if ( !.validColor( valBG ) )
      {
        valBG <- "white"
        cat( "\nMSG (.subOpt) Invalid color, try again...\n" )
        alarm()
      }
    }
  
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$bg[iVar] <- valBG
    assign( "prjOpts", prjOpts, pos=1 )
    
    guiChanges$prjOpts <- prjOpts
    guiChanges$valBG   <- valBG
    updateGUI <- TRUE
    
    optPlotType <- "bg"
  }    
  
  if ( act=="optCEX" || act=="valCEX" )
  {
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$cex[iVar] <- valCEX
    assign( "prjOpts",prjOpts,pos=1 )
    
    guiChanges$prjOpts <- prjOpts
    guiChanges$valCEX  <- valCEX
    updateGUI <- TRUE
    
    optPlotType <- "cex"
  }
  
  if ( act=="optCOL" || act=="valCOL" )
  {
    if ( act=="optCOL" )
    {
      tmpCOL <- pickCol()                # hexadecimal
      if ( tmpCOL!="" )
      {
        tmpCOL <- toupper( tmpCOL )        # Convert hex to upper case.
        valCOL <- .hex2color( tmpCOL )     # Convert to color string.
      }
    }

    if ( act=="valCOL" )
    {
      # Does the color exist in colors()?
      if ( !.validColor( valCOL ) )
      {
        valCOL <- "white"
        cat( "\nMSG (.subOpt) Invalid color, try again...\n" )
        alarm()
      }
    }
  
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$col[iVar] <- valCOL
    assign( "prjOpts",prjOpts,pos=1 )
    
    guiChanges$prjOpts <- prjOpts    
    guiChanges$valCOL  <- valCOL
    updateGUI <- TRUE
    
    optPlotType <- "col"
  }  

  if ( act=="optFG" || act=="valFG" )
  {
    if ( act=="optFG" )
    {
      tmpFG <- pickCol()               # hexadecimal
      if ( tmpFG!="" )
      {
        tmpFG <- toupper( tmpFG )        # Convert hex to upper case.
        valFG <- .hex2color( tmpFG )     # Convert to color string.
      }
    }

    if ( act=="valFG" )
    {
      # Does the color exist in colors()?
      if ( !.validColor( valFG ) )
      {
        valFG <- "white"
        cat( "\nMSG (.subOpt) Invalid color, try again...\n" )
        alarm()
      }
    }
  
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$fg[iVar] <- valFG
    assign( "prjOpts",prjOpts,pos=1 )
    
    guiChanges$prjOpts <- prjOpts
    guiChanges$valFG   <- valFG
    updateGUI <- TRUE
    
    optPlotType <- "fg"
  }    
  
  if ( act=="optLTY" || act=="valLTY" )
  {
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$lty[iVar] <- valLTY
    assign( "prjOpts",prjOpts,pos=1 )
    
    guiChanges$prjOpts <- prjOpts
    updateGUI <- TRUE
    
    optPlotType <- "lty"
  }
  
  if ( act=="optLWD" || act=="valLWD" )
  {
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$lwd[iVar] <- valLWD
    assign( "prjOpts",prjOpts,pos=1 )
    
    guiChanges$prjOpts <- prjOpts
    updateGUI <- TRUE
    
    optPlotType <- "lwd"
  }
  
  if ( act=="colorList" )
  {
     optPlotType <- "chart"
  
#    .getNvar <- function( obj )
#    {
#      idx <- c(1:nrow(obj))
#      result <- min( idx[ obj[,"varName"]=="" ] )
#      result
#    }
    
#    nVar <- .getNvar( prjOpts )
#    if ( nVar <= .NPRJOPT )
#    {
#      tmp <- prjOpts[ .NPRJOPT, ]
#      prjOpts[ 2:(nVar+1), ] <- prjOpts[1:nVar, ]
#      prjOpts[ 1, ] <- tmp

#      iVar <- 1
#      prjOpts$select[.oldVar] <- FALSE
#      assign( ".oldVar", iVar, pos=1 )
    
#      guiChanges$prjOpts <- prjOpts
#      updateGUI <- TRUE
#    }
#    else
#    {
#      cat( "\nMSG (.subGuiOpt) Maximum number of variables reached",.NVAR,
#           ". Increase .NPRJOPT.\n" )
#    }
  }
  
  if ( act=="optColKey" )
  {
    optPlotType <- "colorKey"
  }
  
  if ( act=="optPCH" || act=="valPCH" )
  {
    # Plot the current values on top panel, line widths on bottom panel.
    prjOpts$pch[iVar] <- valPCH
    assign( "prjOpts",prjOpts,pos=1 )
    
    guiChanges$prjOpts <- prjOpts
    updateGUI <- TRUE
    
    optPlotType <- "pch"
  }
  
  if ( act=="optReset" )
  {
    # Copy the existing options files to a backup.
    if ( getYes( "Restore default project and general options" ) )
    {
      if ( exists( ".guiOptPars" ) )
        rm( .guiOptPars, pos=1 )           # Remove the GUI parameters.
 
      # Copy the default options files.
      file.copy( file.path( getwd(), "mseRdefaults", .DEFGENOPT ),
                 file.path( .PRJFLD, .DEFOPTFLD, basename( genOptFile ) ),
                 overwrite=TRUE )
      
      file.copy( file.path( getwd(), "mseRdefaults", .DEFPRJOPT ),
                 file.path( .PRJFLD, .DEFOPTFLD, basename( prjOptFile ) ),
                 overwrite=TRUE )
      
      # Update the option file names.
      guiChanges$genOptFile <- genOptFile
      guiChanges$prjOptFile <- prjOptFile
      
      # Read the option settings.
      genOptData <- .readGenOptFile( fileName=genOptFile )
      prjOptData <- .readPrjOptFile( fileName=prjOptFile )

      # These were saved as hidden globals by .readGenOptFile and .readPrjOptFile.
      genOpts              <- get( "genOpts", pos=1 )
      prjOpts              <- get( "prjOpts", pos=1 )
      guiChanges$genOpts   <- genOpts
      guiChanges$prjOpts   <- prjOpts
      
      iVar <- 1
      guiChanges$maxVal   <- prjOptData$maxVal[iVar]
      guiChanges$minVal   <- prjOptData$minVal[iVar]
      guiChanges$varDesc  <- prjOptData$description[iVar]
      guiChanges$varLab   <- prjOptData$label[iVar]
      guiChanges$varType  <- prjOptData$varType[iVar]
      guiChanges$varUnits <- prjOptData$units[iVar]      

      # Now set the colours, lines, symbols.
      guiChanges$valBG    <- prjOptData$bg[iVar]
      guiChanges$valFG    <- prjOptData$fg[iVar]
      guiChanges$valCOL   <- prjOptData$col[iVar]
      guiChanges$valLTY   <- prjOptData$lty[iVar]
      guiChanges$valLWD   <- prjOptData$lwd[iVar]
      guiChanges$valPCH   <- prjOptData$pch[iVar]
      guiChanges$valCEX   <- prjOptData$cex[iVar]    
     
      # Save the new option settings and update.
      .saveOptionsToScript( genOptData, prjOptData )
      source( "mseRoptions.r" )
      
      updateGUI <- TRUE
    }
  }    
  
  if ( act=="optSave" )
  {
    .savePrjOpts( fileName=prjOptFile, prjOptData )
    #.setPrjOpts( prjOptData )
    
    .saveGenOpts( fileName=genOptFile, genOptData )
    #.setGenOpts( genOptData )
    
    .saveOptionsToScript( genOptData, prjOptData )
  }
  
  # BATCH generator.
  if ( act=="optBatch" )
  {
    guiInfo <- getWinVal( scope="L", winName=win )
    assign( ".guiOptPars", guiInfo, pos=1 )
    cat( "\nMSG (.subOpt) Exiting... saved GUI parameters to .guiOptPars.\n" )
    
    closeWin( .getWinName() )       
    on.exit( guiBatch() )
    isExit <- TRUE
  }  
  
  # PERF results.
  if ( act=="optPerf" )
  {
    guiInfo <- getWinVal( scope="L", winName=win )
    assign( ".guiOptPars", guiInfo, pos=1 )
    cat( "\nMSG (.subOpt) Exiting... saved GUI parameters to .guiOptPars.\n" )
    
    closeWin( .getWinName() )       
    on.exit( guiPerf() )
    isExit <- TRUE
  }    
  
  # VIEW results.
  if ( act=="optView" )
  {
    guiInfo <- getWinVal( scope="L", winName=win )
    assign( ".guiOptPars", guiInfo, pos=1 )
    cat( "\nMSG (.subOpt) Exiting... saved GUI parameters to .guiOptPars.\n" )
    
    closeWin( .getWinName() )       
    on.exit( guiView() )
    isExit <- TRUE
  }  

  # TRACKing GUI.
  if ( act=="optTrack" )
  {
    guiInfo <- getWinVal( scope="L", winName=win )
    assign( ".guiOptPars", guiInfo, pos=1 )
    cat( "\nMSG (.subOpt) Exiting... saved GUI parameters to .guiOptPars.\n" )
    
    closeWin( .getWinName() )       
    on.exit( guiTrack() )
    isExit <- TRUE
  }  
  
  # Simulation GUI.
  if ( act=="optSim" )
  {
    guiInfo <- getWinVal( scope="L", winName=win )
    assign( ".guiOptPars", guiInfo, pos=1 )
    cat( "\nMSG (.subOpt) Exiting... saved GUI parameters to .guiOptPars.\n" )

    closeWin( .getWinName() )       
    on.exit( guiSim() )
    isExit <- TRUE
  }

  if ( !isExit & updateGUI )
  {
    # Update prjOptData from prjOpts
    assign( "prjOptData", prjOptData, pos=1 )
    
    # Update genOptData from genOpts.
    genOptData <- cbind( genOptData$varName, genOpts )
    assign( "genOptData", genOptData, pos=1 )
    
    setWinVal( guiChanges, win )
    
    .setGuiOptState( prjOpts[iVar,] )
    
    cat( "\nMSG (.subOpt) Options updated...\n" )
  }
  
  # EXIT the GUI (leave graphics on).
  if ( act=="optExit" )
  {
    on.exit( closeWin() )     # Don't close window until .subGui exits.
    cat( "\nMSG (.subOpt) Exiting guiOpt on ", date(), "\n" )
  }     # if optExit
  
  if ( !isExit )
    .doOptPlots( prjOpts, iVar, optPlotType )

  return( invisible() )  
}     # END function .subOpt function.


# .setGuiOptState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiOptState <- function( obj )
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  # Example for setting a radiovalue.
  #      setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqYprF",     win )

  #setWidgetState( varname="optFileType", state="disabled" )

  setWidgetColor( "valBG",  entryfg=.setTextContrastColor( obj$bg ), entrybg=obj$bg, )
  setWidgetColor( "valCOL", entryfg=.setTextContrastColor( obj$col), entrybg=obj$col )  
  setWidgetColor( "valFG",  entryfg=.setTextContrastColor( obj$fg ), entrybg=obj$fg )
  
  return( invisible() )
}    # END .setGuiOptState function.


.getIdx <- function( select )
{
  idx <- c( 1:length(select) )[select]
  idx
}


.initGenOptData <- function( nRow=100 )
{
  result <- data.frame(
    varType=character(nRow),
    varName=character(nRow),
    value=character(nRow),
    description=character(nRow),    
    stringsAsFactors=FALSE
  )
  result  
}     # END function .initGenOptData


.initPrjOptData <- function( nRow=100 )
{
  result <- data.frame(
    varType=character(nRow),
    varName=character(nRow),    
    minVal=numeric(nRow),
    maxVal=numeric(nRow),
    description=character(nRow),
    label=character(nRow),
    units=character(nRow),
    prefix=character(nRow),
    bg=character(nRow),
    col=character(nRow),
    fg=character(nRow),
    lty=numeric(nRow),
    lwd=numeric(nRow),
    pch=numeric(nRow),
    cex=numeric(nRow),
    stringsAsFactors=FALSE
  )
  result  
}     # END function .initPrjOptData


.doOptPlots <- function( obj, iVar=1, optPlotType="none" )
{
  guiInfo <- getWinVal( scope="L" )
  
  if ( optPlotType=="bg" )
  {
    .plotVarState( obj[iVar,] )
  }

  if ( optPlotType=="cex" )
  {
    .plotCEX( obj[ iVar, ] )
  }
  
  if ( optPlotType=="chart" )
  {
    par( oma=c(0.5,0.5,2,0.5), mar=c(0,0,0,0), mfrow=c(1,1) )
    .plotColorChart()
  }
  
  if ( optPlotType=="colorKey" )
  {
    .plotColorKey( iPage=optColKey )
  }
  
  if ( optPlotType=="col" )
  {
    .plotVarState( obj[iVar,] )
  }  
  
  if ( optPlotType=="fg" )
  {
    .plotVarState( obj[iVar,] )
  }  
  
  if ( optPlotType=="pch" )
    .plotPCH( obj[ iVar, ] )
  
  if ( optPlotType=="status" )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotVarState( obj[ iVar, ] )
  }

  if ( optPlotType=="lty" )
    .plotLTY( obj[ iVar, ] )    
    
  if ( optPlotType=="lwd" )
    .plotLWD( obj[ iVar, ] )
  
  return( invisible() )  
}     # END function .doOptPlots


.plotVarState <- function( obj )
{
  plot( c(0,1),c(0,1), type="n", axes=FALSE, xlab="", ylab="" )
  

  text( 0.15, 0.5, cex=2, paste( obj$varName, obj$varType ) )
  segments( 0.5, 0.5, 0.75, 0.5, col=obj$col, lty=obj$lty, lwd=obj$lwd )
  points( 0.85, 0.5, cex=obj$cex, bg=obj$bg, fg=obj$fg, pch=obj$pch )
  
  box()
  
  return( invisible() )
}     # END function .plotVarState


.plotCEX <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE, xlim=NULL, yLim=NULL ) )
{
  par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
  .plotVarState( obj )

  cexVals <- seq( 0.1,3,0.1 )
  xLim <- range( cexVals )
  yLim <- c(0,1)
  
  nCex   <- length( cexVals )
  cols   <- c( "black" )
  colVec <- rep( cols, ceiling(nCex/length(cols)) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  for ( i in cexVals )
    points( i, 0.5, cex = i, col = colVec[i], pch=obj$pch )
  box()
  
  axis( side=1 )
  
  #mtext( as.character(cexVals), side = 1, line=1, at=cexVals )
  mtext( side=3, line=1, cex=1.2, "cex Values" )

  invisible(NULL)
}     # END function .plotLTY


.plotLTY <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE, xlim=NULL, yLim=NULL ) )
{
  par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
  .plotVarState( obj )

  ltyVals <- c(1:6)
  xLim <- range( ltyVals )
  yLim <- c(0,1)
  
  nLty   <- length( ltyVals )
  cols   <- c( "black" )
  colVec <- rep( cols, ceiling(nLty/length(cols)) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  for ( i in ltyVals )
    lines( c(i, i), c(0, 1), lty = i, lwd = 2, col = colVec[i] )
  box()
  
  mtext( as.character(ltyVals), side = 1, line=1, at=ltyVals )
  mtext( side=3, line=1, cex=1.2, "Line Types" )

  invisible(NULL)
}     # END function .plotLTY

.plotLWD <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE, xlim=NULL, yLim=NULL ) )
{
  par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
  .plotVarState( obj )

  lwdVals <- c(1:20)
  xLim <- range( lwdVals )
  yLim <- c(0,1)
  
  nLwd   <- length( lwdVals )
  cols   <- c( "black","blue" )
  colVec <- rep( cols, ceiling(nLwd/length(cols)) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  for ( i in lwdVals )
    lines( c(i, i), c(0, 1), lty = 1, lwd = i, col = colVec[i] )
  box()
  
  mtext( as.character(lwdVals), side = 1, line=1, at=lwdVals )
  mtext( side=3, line=1, cex=1.2, "Line Widths" )

  invisible(NULL)
}     # END function .plotLWD


.plotPCH <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE, xlim=NULL, yLim=NULL ) )
{
  par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
  
  .plotVarState( obj )

  pchVals <- c(0:25)
  xLim <- range( pchVals )
  yLim <- c(0,1)
  
  nPch   <- length( pchVals )
  bgCols <- obj$bg
  fgCols <- obj$fg
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  for ( i in pchVals )
    points( i, 0.5, bg=bgCols, fg=fgCols, cex=obj$cex, pch=i )
  box()
  
  #mtext( as.character(ltyVals), side = 1, line=1, at=ltyVals )
  axis( side=1 )
  mtext( side=3, line=1, cex=1.2, "Plotting Symbols" )

  invisible(NULL)
}     # END function .plotPCH


.readGenOptFile <- function( fileName )
{
  # Read graphics options.
  tmp <- read.table( file=fileName, as.is=TRUE, header=TRUE, sep=",",
                     stringsAsFactors=FALSE )

  # ARK (02-Jan-13)  Here I need to find that trick for allocating an empty data
  # frame with the same columns to an arbitrary number of rows.  For now, just
  # assume that all rows are there.
  
  genOptData <- .initGenOptData( nrow(tmp) )

  if ( nrow(tmp) <= nrow(genOptData) )
    genOptData[1:nrow(tmp),] <- tmp
  else
  {
    cat( "\nMSG (.mseRguiSetup) Reset general options limit .NGENOPT to at least",
          nrow(tmp),"\n" )
    alarm()
    genOptData <- NULL
  }
  
  if ( !is.null( genOptData ) )
  {
    assign( "genOptData", genOptData,       pos=1 )
    assign( "genOpts",    genOptData[ ,-1], pos=1 )    # Remove varType column.
    
    cat( "\nMSG (.readGenOptFile) Read and saved projection options file",fileName,"\n" )    
  }
    
  genOptData
}     # END function .readGenOptFile


.readPrjOptFile <- function( fileName )
{
  # Read project options.
  tmp <- read.table( file=fileName, as.is=TRUE, header=TRUE, sep=",",
                     stringsAsFactors=FALSE )

  prjOptData <- .initPrjOptData( nrow(tmp) )

  if ( nrow(tmp) <= nrow(prjOptData) )
    prjOptData[1:nrow(tmp),] <- tmp
  else
  {
    cat( "\nMSG (.mseRguiSetup) Reset project option limit .NPRJOPT to at least",nrow(tmp),"\n" )
    alarm()
    prjOptData <- NULL
  }

  if ( !is.null(prjOptData) )
  {
    varFields <- c( "varName","prefix","bg","col","fg","lty","lwd","pch","cex" )
    prjOpts   <- prjOptData[ ,varFields ]

    iVar         <- 1
    select       <- logical( nrow( prjOpts ) )
    select[iVar] <- TRUE

    prjOpts   <- cbind( prjOpts, select=select )
    assign( "prjOpts", prjOpts, pos=1 )
 
    .oldVar <- 1
    assign( ".oldVar", .oldVar, pos=1 )  

    assign( "prjOptData", prjOptData, pos=1 )
    assign( "prjOpts"   , prjOpts,    pos=1 )
    
    cat( "\nMSG (.readPrjOptFile) Read and saved projection options file",fileName,"\n" )
  }
  prjOptData
}     # END function .readPrjOptFile


.savePrjOpts <- function( fileName="mseRgenPrjData.csv", obj )
{
  # Write to *.CSV.
  write.csv( obj, file=fileName, row.names=FALSE )
  cat( "\nMSG (.savePrjOpts) General options written to ",fileName,"\n" )
}     # END function .savePrjOpts


.setPrjOpts <- function( obj )
{
  # Save the project options options to global environment and .CSV file.
  
  for ( i in 1:nrow(obj) )
  {
    prefix <- obj$prefix[i]
    
    bg  <- paste( prefix,"BG",  sep="" )
    assign ( bg, obj$bg[i], pos=1 )
    
    cex <- paste( prefix,"CEX", sep="" )
    assign( cex, obj$cex[i], pos=1 )
    
    col <- paste( prefix,"COL", sep="" )    
    assign( col, obj$col[i], pos=1 )
    
    fg  <- paste( prefix,"FG",  sep="" )
    assign( fg, obj$fg[i], pos=1 )
    
    lty <- paste( prefix,"LTY", sep="" )
    assign( lty, obj$lty[i], pos=1 )
    
    lwd <- paste( prefix,"LWD", sep="" )
    assign( lwd, obj$lwd[i], pos=1 )
  }
}   # END function .setPrjOpts


.saveGenOpts <- function( fileName="mseRgenOptData.csv", obj )
{
  # Write to *.CSV.
  write.csv( obj, file=fileName, row.names=FALSE )
  cat( "\nMSG (.saveGenOpts) General options written to ",fileName,"\n" )
  
  return( invisible() )
}     # END function .saveGenOpts

.saveOptionsToScript <- function( genOpts, prjOpts )
{
  # Write mseRoptions.r.
  optFile <- "mseRoptions.r"

  cat( file=optFile, "# mseR Options.r: Written: ",date(),"\n" )
    
  cat( file=optFile, "#\n# General options start here:\n#\n", append=TRUE ) 
  for ( i in 1:nrow( genOpts ) )
  {
    cat( file=optFile, paste( genOpts$varName[i], "<-", genOpts$value[i], "     # ",
                       genOpts$description[i] ),"\n", append=TRUE )
  }
  
  cat( file=optFile, "#\n# Project options start here:\n#\n", append=TRUE )
  for ( i in 1:nrow( prjOpts ) )
  {
    prefix <- prjOpts[ i,"prefix" ]
    cat( file=optFile, "#\n# Settings for",prefix,"\n#\n", append=TRUE )

    cat( file=optFile, paste( prefix,"BG", " <- ",dQuote(prjOpts[i,"bg" ]), sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"COL"," <- ",dQuote(prjOpts[i,"col"]), sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"FG", " <- ",dQuote(prjOpts[i,"fg" ]), sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"LTY"," <- ",       prjOpts[i,"lty"],  sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"LWD"," <- ",       prjOpts[i,"lwd"],  sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"PCH"," <- ",       prjOpts[i,"pch"],  sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"CEX"," <- ",       prjOpts[i,"cex"],  sep="" ), "\n", append=TRUE )
    
    cat( file=optFile, paste( prefix,"LAB","  <- ",dQuote(prjOpts[i,"label"]), sep="" ), "\n", append=TRUE )
    cat( file=optFile, paste( prefix,"UNIT"," <- ",dQuote(prjOpts[i,"units"]), sep="" ), "\n", append=TRUE )
  }
  
  source( optFile )
  
  return( invisible() )  
}     # .saveOptionsToScript

.setGenOpts <- function( obj )
{
  # Save the general options to global environment and CSV file.
  # These values are character, and must be evaluated.

  # Shut off whining, then coerce to numeric to let NA indicate non-numerics.
  options( warn=-1 )
  numericVal <- as.numeric( obj[,"value"] )
  options( warn=0 )
  
  result <- list()
  
  for ( i in 1:nrow(obj) )
  {
    varName <- obj$varName[i]
    value   <- obj$value[i]
    
    # Value is numeric, build the parse string.
    if ( !is.na(numericVal[i]) )
      listText <- paste( "result$",varName,"=",
                    value,sep="" )
    
    # Value is character, build the parse string.
    else
      listText <- paste( "result$",varName,"=",
                    value, sep="" )

    # Evaluate the parse string.
    eval( parse( text=listText ) )

    # Assign global variable, brutally learned trick here is using "get" to
    # obtain the actual value of the variable name stored as a text string.    
    assign( varName, get( varName ), pos=1 )
  }
}   # END function .setGenOpts


.hex2color <- function( x )
{
  rgbColors <- col2rgb( colors() )
  hexColors <- rgb( rgbColors[1,], rgbColors[2,], rgbColors[3,],
                    maxColorValue=255 )
  
  idx    <- c( 1:length(hexColors) )[ hexColors==x ]
  if ( length(idx)==0 )
  {
    # Can't find the color string, use the Hex string.
    result <- x
    cat( "\nMSG (.hex2color) Color string not found, using the hex string...\n" )
  }
  else
  {
    idx    <- min( idx )
    result <- colors()[idx]
  }
  result 
}     # END function .hex2color


.validColor <- function( colorStr )
{
  valid <- TRUE
  
  colorList <- colors()
  idx <- c(1:length(colorList))[ colorStr==colorList ]

  if ( length( idx )==0 )
    valid <- FALSE
  valid
}     # END function .validColor

# Define string vector of RGB hex and decimal constants for given color
# as a string, say: GetColorHexAndDecimal("yellow")
#     [1] "#FFFF00   255 255   0"
.getColorHexAndDecimal <- function( color )
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

.plotColorKey <- function( iPage=1 )
{
  par( oma=c(0,0,0,0), mar=c(0,0,1,0), mfrow=c(1,1) )
  cexKey <- 0.9
  
  # Define this array of text contrast colors that correponds to each
  # member of the colors() array.
  textContrastColor <- unlist( lapply(colors(), .setTextContrastColor ) )

  # Prepare text vectors to be displayed, in addition to color names.
  index     <- paste(1:length(colors()))
  HexAndDec <- unlist( lapply(colors(), .getColorHexAndDecimal) )

  perColumn <- 40
  perPage   <- 2 * perColumn

  # Plot a column of color rectangles at a time for each page.
  
  nPages <- trunc( ( length(colors() ) + (perPage-1)) / perPage )
  iPage <- min( c(iPage,nPages) )
  for ( page in 1:nPages  )
  {
    base <- perPage * ( page-1 )

    # Column 1
    remaining <- length(colors()) - base
    ColumnSize <- ifelse( remaining < perColumn, remaining, perColumn )

    if ( iPage==page )
    {
      plot( 0, type="n", ylab="", xlab="",
            axes=FALSE, ylim=c(perColumn,0), xlim=c(0,1))

      mtext( side=3, line=-1, cex=.CEXTITLE4, "R Color Key" )

      rect(0.00, 0:(ColumnSize-1),
           0.49, 1:ColumnSize,
           border="black",
           col=colors()[(base+1):(base+ColumnSize)])
      text(0.045, 0.45+(0:(ColumnSize-1)), adj=1,
           index[(base+1):(base+ColumnSize)], cex=cexKey,
           col=textContrastColor[(base+1):(base+ColumnSize)])
      text(0.06, 0.45+(0:(ColumnSize-1)), adj=0,
           colors()[(base+1):(base+ColumnSize)], cex=cexKey,
           col=textContrastColor[(base+1):(base+ColumnSize)])

      save <- par( family="mono" )  # use mono-spaced font with number columns
      text(0.25, 0.45+(0:(ColumnSize-1)), adj=0,
           HexAndDec[(base+1):(base+ColumnSize)], cex=cexKey,
           col=textContrastColor[(base+1):(base+ColumnSize)])
      par(save)

      # Column 2
      if ( remaining > perColumn )
      {
        remaining <- remaining - perColumn
        ColumnSize <- ifelse(remaining < perColumn, remaining, perColumn)
        rect(0.51, 0:(ColumnSize-1),
             1.00, 1:ColumnSize,
             border="black",
             col=colors()[(base+perColumn+1):(base+perColumn+ColumnSize)])
        text(0.545, 0.45+(0:(ColumnSize-1)), adj=1,
             index[(base+perColumn+1):(base+perColumn+ColumnSize)], cex=cexKey,
             col=textContrastColor[(base+perColumn+1):(base+perColumn+ColumnSize)])
        text(0.56, 0.45+(0:(ColumnSize-1)), adj=0,
             colors()[(base+perColumn+1):(base+perColumn+ColumnSize)], cex=cexKey,
             col=textContrastColor[(base+perColumn+1):(base+perColumn+ColumnSize)])
        save <- par(family="mono")
        text(0.75, 0.45+(0:(ColumnSize-1)), adj=0,
             HexAndDec[(base+perColumn+1):(base+perColumn+ColumnSize)], cex=cexKey,
             col=textContrastColor[(base+perColumn+1):(base+perColumn+ColumnSize)])
        par(save)
      }
    }     # if iPage==page
  }     # Page
}     # END function plotColorKey

