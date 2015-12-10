#------------------------------------------------------------------------------#
# (c) mseR-FinFish: Management Strategy Evaluation in R, Finfish Version 1.0   #
#                                                                              #
#     Copyright 2012-2013 by A.R. Kronlund and S.P. Cox.                       #
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
#                                                                              #
# mseRglobals.r: An mseR module that provides global variables, but not for    #
#                graphics options.  These globals intended for developers.     #
#                Changing these globals can cause mayhem - be careful.         #
                                                                               #
# Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)           #
#          S.P. Cox (Simon Fraser University, Burnaby, B.C.)                   #
#                                                                              #
# First Implementation: 07-Aug-12 from mseR V2, mseR-Prawn and mseR2011.       #
#------------------------------------------------------------------------------#

options(useFancyQuotes = FALSE)        # Required for parameter eval

# General global variables.
.GUIMSG         <- TRUE    # Control console whining re: a GUI message.
.PACKAGE        <- ""      # When mseR is an R package this will be needed.
.PLTMSG         <- TRUE    # Controls console whining re: a plot call.
.PLATFORM       <- .Platform$OS.type
.RESTORECONSOLE <- ifelse( .PLATFORM=="unix", FALSE, TRUE )
.WHINE          <- 3       # 1=Essential, 2=Info, 3=Debugging.
.NONSTATB0      <- FALSE    # Use Bt from NoFish MP as depletion reference
                           # save mseRstatistics.csv under alt name
if( .NONSTATB0 )
  cat(".NONSTATB0 is TRUE, so group and rank simulations using Track
      prior to computing Stats","\n")
.HISTLRP        <- FALSE
if( .HISTLRP )
  cat(".HISTLRP is TRUE, so all B0-based stats are relative to
       the historical minimum biomass","\n")
.USEMEDIANPROB <- FALSE

# mseR folder settings - do not alter unless you are a developer.
.FBACK <- "mseRbackup"                # Holds backups of project folders (i.e., reset).
.FCODE <- "mseRcode"                  # Copy of *.r and *.txt GUI files for safety.
.FDEFS <- "mseRdefaults"              # Folder containing default options.
.FDOCS <- "mseRdocs"                  # Folder containing documents.
.FHELP <- "mseRhelp"                  # Directory for help files.
.FTEMP <- "mseRtemp"                  # Folder containing temporary files.

# mseR project folder definitions - do not alter unless you are a developer.
.DEFBATFLD  <- "batch"                # For batch files.
.DEFHSTFLD  <- "history"              # For stock initialization (conditioning).
.DEFOPTFLD  <- "options"              # Folder for general and project options.
.DEFPLTFLD  <- "plots"                # Folder for guiSim and guiPerf plots.
.DEFPRJFLD  <- "mseRproject"          # Default project folder.
.PRJFLD     <- "mseRproject"
.DEFSTATFLD <- "statistics"           # Performance statistics folder.

# mseR default file names.
.DEFCTLFILE  <- "simCtlFile.txt"      # Default simulation control file.
.DEFHSTFILE  <- "simHistoryFile.csv"  # History file for conditioning.
.DEFSTATFILE <- "mseRstatistics.xls"  # Performance statistics file (Excel .xls).

# Batch file names, don't use simCtlFile.txt 'cause it gets overwritten.
.FBATDES  <- "mseRbatch.design"        # Design file (output)

# ADMB related globals.
.FADMBOPT            <- "mseRadmbOpt.txt"     # ADMB and C++ compiler options.
.ADMBOPTS            <- "-nox -iprint 1000"   # Default ADMB command line arguments.
.INVISIBLE           <- TRUE                  # Is ADMB reporting invisible?
.NOHESS              <- FALSE                 # Is hessian calculated?
.SHOWOUTPUTONCONSOLE <- FALSE                 # Windows: Show ADMB running on console?
.INTERN              <- FALSE                 # Mac: Show ADMB running on console? 

# Parallel processing globals.
.ISCLUSTER <- FALSE                           # Is snow cluster active?

# Constants to ID assessment method for management procedure.
.MOVAVG <- 1           # Moving average.
.KALMAN <- 2           # Kalman filter.
.PMOD   <- 3           # Surplus production model.
.DDMOD  <- 4           # Delay-Difference model.
.CAAMOD <- 5           # Catch-At-Age model.
.VPA    <- 6           # Virtual Population Analysis.
.USER   <- 7           # User specified method.

# Flag to indicate a fishery collapse
.DEADFLAG  <- FALSE       # All future catch==0 if TRUE

# Lables for each harvest control rule.
.HCRNAMES <- c( "Constant F", "Variable F", "Decline Risk", "User Rule" )

# Labels for each method.
.METHODLAB <- c( "Moving Average", "Kalman Filter", "Surplus Production",
                 "Delay-Diff",     "Catch-at-Age",  "VPA",              "User" )
                 
