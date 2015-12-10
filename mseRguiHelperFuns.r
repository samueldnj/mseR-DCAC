#------------------------------------------------------------------------------#
#-- mseR Helper Functions                                                    --#
#------------------------------------------------------------------------------#

.mseRinitProject <- function( wkDir=NULL )
{
  # Initializes mseR paths and files so that user can start with any module.
  
  # 1. Working directory path.
  if ( is.null( wkDir ) )
    wkDir <- .getwd()
  
  # 2. Does the .PRJFLD path variable exist?  If not initialize.
  if ( exists( ".PRJFLD" ) )
  {
    # 2a. Does the Project Folder path match the current folder?
    tmpPrjFld <- file.path( wkDir, basename( .PRJFLD ) )    
    
    if ( .PRJFLD != tmpPrjFld )
    {
      alarm()
      if ( getYes( paste( "mseRinitProject: Reset project folder to ", tmpPrjFld ) ) )
      {
        .PRJFLD <- tmpPrjFld
        assign( ".PRJFLD", .PRJFLD, pos=1 )
        cat( "\nMSG (.mseRinitProject) Set project folder to ", .PRJFLD, "\n" )
      }
    }
  }
  else
  {
    .PRJFLD <- file.path( wkDir, .DEFPRJFLD )
    assign( ".PRJFLD", .PRJFLD, pos=1 )
    cat( "\nMSG (.mseRinitProject) Set project folder path to ", .PRJFLD,"\n" )
  }

  # 3. Does the actual folde exist?
  if ( !file.exists( .PRJFLD ) )
  {
    # If .PRJFLD initialized, but file does not exist, then set to default.  
    dir.create( .PRJFLD )
    cat( "\nMSG (.mseRinitProject) Created new project folder ", .PRJFLD,"\n" )
    
    dir.create( file.path( .PRJFLD, .DEFBATFLD ) )
    dir.create( file.path( .PRJFLD, .DEFHSTFLD ) )
    dir.create( file.path( .PRJFLD, .DEFOPTFLD ) )
    dir.create( file.path( .PRJFLD, .DEFSTATFLD ) )
  }

  batFld  <- file.path( .PRJFLD, .DEFBATFLD )
  hstFld  <- file.path( .PRJFLD, .DEFHSTFLD )
  optFld  <- file.path( .PRJFLD, .DEFOPTFLD )
  pltFld  <- file.path( .PRJFLD, .DEFPLTFLD )
  statFld <- file.path( .PRJFLD, .DEFSTATFLD )

  # Make sure existing project folder has required sub-folders.
  
  if ( !file.exists( batFld ) )
    dir.create( file.path( .PRJFLD, .DEFBATFLD ) )  
  if ( !file.exists( hstFld ) )
    dir.create( file.path( .PRJFLD, .DEFHSTFLD ) )
  if ( !file.exists( optFld ) )
    dir.create( file.path( .PRJFLD, .DEFOPTFLD ) )    
  if ( !file.exists( pltFld ) )
    dir.create( file.path( .PRJFLD, .DEFPLTFLD ) )
  if ( !file.exists( statFld ) )
    dir.create( file.path( .PRJFLD, .DEFSTATFLD ) )
    
  # 4. Initialize general options.
  genOptFile <- file.path( .PRJFLD, .DEFOPTFLD, basename(.FGENOPT) )
  if ( !file.exists( genOptFile ) )
  {
    file.copy( file.path( wkDir, "mseRdefaults", .DEFGENOPT ), genOptFile )
    if ( .WHINE > 0 )
      cat( "\nMSG (.mseRinitProject) Copied default general options file to ",
           genOptFile,"\n" ) 
  }
  
  # 5. Initialize project options.
  prjOptFile <- file.path( .PRJFLD, .DEFOPTFLD, basename(.FPRJOPT) )
  if ( !file.exists( prjOptFile ) )
  {
    file.copy( file.path( wkDir, "mseRdefaults", .DEFPRJOPT ), prjOptFile )
    if ( .WHINE > 0 )
      cat( "\nMSG (.mseRsetupSim) Copied default project options file to ",
           prjOptFile,"\n" )
  }
  
  return( invisible() )
}     # END function .mseRinitProject

# .addStamp   (Adds a legend to a plot that identifies the simulation)
# Purpose:    Adds a plot legend that identifies the scenario, procedure,
#             current replicate (if applicable), and number of replicates in sim.
# Parameters: iSim, iRep, nReps, simFile, scenario, procedure, showFile.
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
.addStamp <- function( iSim=1,iRep=0,nReps=1, simFolder="", scenario="",
                       procedure="", outLine=NULL, showFile=FALSE,
                       xPos=NULL, yPos=NULL, ... )
{
  # Sim string.
  if ( iSim < 0 )
    simTxt <- ""
  else
    simTxt <- paste( "Sim",iSim, sep="" )

  # Rep string: if a replicate view, then indicate replicate ID and nReps.
  #             else just indicate the number of replicates in the simulation.
  if ( iRep < 0 )
    repTxt <- ""
  else if ( iRep > 0 )
    repTxt <- paste( " Rep ",iRep,"/",nReps, sep="" )
  else
    repTxt <- paste( " Reps: ",nReps, sep="" )

  # Scenario and procedure ID string: if procedure string supplied, then add it
  # to the idTxt string, else just add the scenario label.
  if ( procedure!="" )
    idTxt  <- paste( scenario,":",procedure,sep="" )
  else
    idTxt  <- paste( scenario,sep="" )

  # if showFile=T, then show the Rdata file name that holds the simulation.
  if ( showFile )
    txt <- paste( idTxt, collapse="" )
    #txt <- paste( simTxt,repTxt,idTxt,simFolder, collapse="" )
  else
    txt <- paste( idTxt, collapse="" )
    #txt <- paste( simTxt,repTxt,idTxt, collapse="" )

  # If a replicate view, then add the legend to the outer plot margin,
  # else add it to the plot area as in the case of guiPerf graphics.
  if ( iRep < 0 | iRep > 0 )
    if ( is.null( outLine ) )
    {
      oma <- par( "oma" )
      mtext( adj=1, line=oma[1]-1, outer=TRUE, txt,... )
    }
    else
      mtext( adj=1, outer=TRUE, txt,... )
  else
  {
    if ( is.null( xPos ) | is.null( yPos ) )
      panLab( 0.025,0.05, adj=0, txt )
    else
      panLab( xPos, yPos, adj=0, txt )
  }

  return( invisible() )
}     # END function .addStamp


.addTarget <- function( x, y, cexCenter=1, colCenter="red", colRing="red",... )
{
  points( x, y, bg=colCenter, col=colCenter,  fg=colCenter, cex=cexCenter, pch=21,... )
  points( x, y, col=colRing,  cex=cexCenter*2, lwd=cexCenter*2, pch=21,... )
  return( invisible() )
}

# .createList   (converts a data frame into a possiby nested list object)
# Purpose:      Converts a dataframe containing a column "parameter" and a
#               column "value" into a (possibly) nested list.
# Parameters:   obj is a dataframe with columns "parameter" and "value".
# Returns:      result, a list with (possibly) nested levels.
# Source:       A.R. Kronlund
.createList <- function( obj )
{
  # Input  a data frame with columns "parameter" and "value".
  # Output a list with elements named as parameter and values in "value".

  result <- list()

  # Shut off whining, then coerce to numeric to let NA indicate non-numerics.
  options( warn=-1 )
  numericVal <- as.numeric( obj[,"value"] )
  options( warn=0 )

  for ( i in 1:nrow(obj) )
  {
    # Value is numeric, build the parse string.
    if ( !is.na(numericVal[i]) )
      listText <- paste( "result$",obj[i,"parameter"],"=",
                    obj[i,"value"],sep="" )
    # Value is character, build the parse string.
    else
      listText <- paste( "result$",obj[i,"parameter"],"=",
                  obj[i,"value"], sep="" )

    # ARK: At one point I had this code, presumably to handle a different
    #      input format convention, perhaps assuming "value" was all character.
    #                   sQuote(obj[i,"value"]),sep="" )

    # Evaluate the parse string.
    eval( parse( text=listText ) )
  }
  result
}     # function .createList

# .parseGuiList (converts specified Sim GUI parameter list elements from
#               character to numeric as appropriate)
# Purpose:      Converts named elements of a list containing mixed numeric and
#               to numeric.
# Parameters:   obj is a list derived from a Simulation GUI (i.e., guiSim).
# Returns:      result, a list with specified elements converted to numeric.
# Source:       A.R. Kronlund
.parseGuiList <- function( obj )
{
  # This is where the result of parsing will live.
  parseObj <- list()

  nList <- length( obj )
  for ( i in 1:nList )
  {    
 
    parName <- names(obj)[i]
    parVal<-lapply(obj[i],unlist)
   
    # Deal with grayed-out menu items: c1, c2, aSel50, aSel95
    # Not user configurable, stored in GUI as text values so remove
    # the line feed that is inserted by PBSModelling GUI.

    #if ( (parName=="c1") || (parName=="c2") )
    #{
    #  parVal <- as.numeric( gsub( "\n", "", parVal ) )
    #}

    #if ( (parName=="aSel50") || (parName=="aSel95") )
    #{
    #  parVal <- as.numeric( gsub( "\n", "", parVal ) )
    #}

    if ( parName=="qSurvey" )
      parVal <- as.numeric( gsub( "\n","", parVal ) )
     
   parseObj[parName] <- parVal
      
  }
  parseObj
}     # END function .parseGuiList


# .unEvalList (converts possibly nested list to non-nested list)
# Purpose:    Converts a possibly nested list to a non-nested list.
# Parameters: obj is the possibly nested list to convert.
# Returns:    result, the non-nested list.
# Source:     A.R. Kronlund
.unEvalList <- function( obj ) 
{
  # Loop thru a (possibly) nested list, plucking out the name at the lowest
  # level and corresponding value.
  
  result   <- list()
  val      <- unlist( obj )
  valNames <-  names( val )
  
  for ( i in 1:length(val) )
  {
     tokenPos <- max(which(strsplit(valNames[i],'')[[1]]=='.')) + 1
     
     guiName <- substring( valNames[i], tokenPos,nchar(valNames[i]) )
     
     # Check for character or numeric.
     if ( is.character( val[i] ) )
       listText <- paste( "result$",guiName,"=\"",val[i],"\"",sep="" )
     else
       listText <- paste( "result$",guiName,"=",val[i],sep="" )
     eval( parse( text=listText ) )
  }
  result
}     # function .unEvalList      

# .viewFile   (view a file saved in the mseRcode directory)
# Purpose:    View a file that is stored in the mseR library directory in the
#             folder named "mseRtemp". This is the folder where copies of the R
#             code, the GUI description, the initial database, the ADMB
#             executable, and the documentation are kept.
# Parameters: fname is a character containing the name of the file to view 
#             (default is based on the last action performed by the current
#             GUI window)
# Returns:    NULL
# Source:     PBSref (gui_funs.r")
# Note: Why is viewFile Not Hidden?
.viewFile <- function(fname)
{
  # These two will be used when mseR is a proper R library.
  #pckg  <- "mseR"                       # the name of this package
  dname <- "mseRcode"                    # R directory where the file is located
  
  if( missing(fname) )
  {
    fname <- getWinAct(.getWinName())[1] # Name of the file to open
  }
  
  # This will be used when mseR is a proper R library.
	#rdir <- system.file(package = pckg)   # path to the R directory
	
	# Reference working directory.
	rdir <- getwd()
  fnam <- paste(rdir, dname, fname, sep = "/")  

  openFile(fnam)
  
  return()
}     # function .viewFile


# readParFile   (reads an ASCII file with 1 comment line, header, data frame)
# Purpose:      Reads an ASCII file: 1 comment, 1 header, space-delimited
#               data frame usually containing columns "parameter" and "value".
# Parameters:   parFile is a character string indicating the input file.
# Returns:      result, a data frame.
# Source:       A.R. Kronlund
readParFile <- function( parFile="inputFile.par" )
{
  # Read the file and store as a dataframe.

  result <- read.table( file=parFile, as.is=TRUE, header=TRUE, comment.char="#",
                        quote="",sep=" " )
  result
}     # function readPar File

# saveSimPars   (saves the Sim GUI parameters to a file for runMSE loop).
# Purpose:      Saves an ASCII file with simulation controle parameters.
# Parameters:   Output file name for ASCII file containing GUI parameters.
# Returns:      NULL
# Side effects: Writes an ASCII control file configured for read into a list.
# Source:       A.R. Kronlund
saveSimPars <- function( ctlPars, parFile="inputParameters.txt", overWrite=FALSE )
{
  if ( !overWrite )
  {
    tmpFile <- selectFile( initialfile=parFile,
                  filetype=list( c(".txt","txt files")), mode="save" )

    if ( is.null(tmpFile) )
      return( invisible() )
    else
      parFile <- tmpFile
  }
  else
  {
    # Force an overwrite of parFile for runMSE.
    if ( .WHINE > 1 )
      cat( "\nMSG (saveSimPars) Overwriting ",parFile,"\n" )
  }

  win     <- .getWinName()                       # Get the current window name
  guiInfo <- getWinVal( scope="L", winName=win ) # GUI information local scope

  cat( file=parFile, paste( "# mseRguiSim GUI parameters written",date(),"\n" ) )
  cat( file=parFile, "parameter value\n", append=TRUE )
  
  for ( i in 1:nrow(ctlPars) )
  {
    cat( file=parFile, ctlPars[i,"parameter"]," ",ctlPars[i,"value"],"\n",
         sep="", append=TRUE )
  }

  return( invisible() )
}     # END function saveSimPars


# readSimPars   (reads mseRguiSim GUI parameters from file written by saveSimPars)
# Purpose:      Reads an ASCII file with simulation (OM+MP) parameters.
# Parameters:   GUI parameters for mseRguiSim menu. Output GUI parameter list.
# Returns:      NULL (invisibly)
# Side effects: Reads an ASCII control file written by saveSimPars and resets
#               the GUI parameters.
# Source:       A.R. Kronlund
readSimPars <- function( parFile="inputParameters.par" )
{

  win <- .getWinName()

  tmpFile <- selectFile( initialfile=parFile,
                filetype=list( c(".par","par files") ), mode="open" )

  if ( is.null(tmpFile) )
    return( NULL )          # No such file exists, bale returning NULL.
  else
    parFile <- tmpFile      # Return the selected file name.

  val <- readParFile( parFile )
  guiList <- list()

  # Loop thru the rows, plucking out the parameter and value columns.
  # Find the name after the last "$" and assign it to the guiList.
  for ( i in 1:nrow(val) )
  {
     parName <- val[ i,"parameter" ]
     guiVal  <- val[ i,"value" ]

     tokenPos <- max(which(strsplit(parName,'')[[1]]=='$')) + 1

     guiName <- substring( parName, tokenPos,nchar(parName) )
     listText <- paste( "guiList$",guiName,"=",guiVal,sep="" )

     eval( parse( text=listText ) )
  }

  setWinVal( guiList, win ) # GUI information local scope.

  return( invisible() )
}     # function readSimPars

#-----------------------------------------------------------------------------##
#-- Helper Functions (some HIDDEN, e.g., .foo)                              --##
#-----------------------------------------------------------------------------##

# .closeActWin (close the active window)
# Purpose:     Closes the active window, say when the "exit" button is pressed.
# Parameters:  None
# Returns:     NULL (invisibly)
# Source:      A.R. Kronlund
.closeActWin <- function()
{
  closeWin( .getWinName() )
}

# .errorMessage (writes a generic error message to the console)
# Purpose:      Writes a generic error message to the console with optional value.
# Parameters:   msg, a character string error message and val, a value.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.errorMessage <- function( msg="ERROR: Program Ends", val=NULL )
{
  delimiter <- "#-------------------------------------------------------------#"

  cat( delimiter,"\n" )
  if ( !is.null( val ) )
    cat( "# ERROR: ", msg," ",val,"\n" )
  else
    cat( "# ERROR: ", msg, "\n" )
  cat( delimiter,"\n" )
  return( invisible() )
}


# .getWinName  (get the current winName)
# Purpose:     Determine which GUI is active (guiSim, guiView, guiPerf, etc.)
# Parameters:  None
# Returns:     A character containing the name of the current GUI window
# Source:      A.R. Kronlund, modified from PBSref (helper_funs.r)
.getWinName <- function()
{
  win <- tget( .PBSmod )$.activeWin
  
  # This is only required if PBSask is used, leave it for now.
  if( win == "PBSask" )
  {
    win <- getWinVal("win", winName="PBSask")[[1]]   # Hidden field in PBSask
    win <- gsub("\n", "", win)                       # Remove the linefeed \n
  }
  return(win)
}


.isGuiClosed <- function( winName="window" )
{
  # Thanks to Rowan Haigh for the code suggestions, modified from his version!
  # Returns TRUE if the named window is closed.
  
  isClosed <- FALSE
  
  # Step 1.  Is the requested GUI on the .PBSmod list?
  pbsModNames <- grep( "^[^\\.]", names(.PBSmod), value=TRUE )
  
  if ( !any(pbsModNames==winName) )
  {
    isClosed <- TRUE
  }
  # Step 2. If on .PBSmod list, can we set an widget value? If cannot, GUI is closed.
  else
  {
    # Get a widget value.
    guiList <- getWinVal( winName=winName )
    guiChanges <- guiList[ 1 ]
    
    err      <- class( try(setWinVal( guiChanges, winName=winName), silent=TRUE ) )
    isClosed <- ifelse( err=="try-error", TRUE, FALSE )
  }
  
  return( isClosed )
}

.subMenuClose <- function()
{
  assign( ".SUBMENUACTIVE", FALSE, pos=1 )
}


# .viewHelp   (view a help file or document)
# Purpose:    View a file that is stored in the mseR library directory in the
#             folder named "mseRcode". This is the folder where copies of the R
#             code, the GUI description, the initial database, the ADMB
#             executable, and the documentation are kept.
# Parameters: fname is a character containing the name of the file to view 
#             (default is based on the last action performed by the current
#             GUI window)
# Returns:    NULL
# Source:     PBSref (gui_funs.r")
.viewHelp <- function(fname)
{
  # These two will be used when mseR is a proper R library.
  #pckg  <- "mseR"                       # the name of this package
  dname <- "mseRhelp"                    # R directory where the file is located
  
  if( missing(fname) )
  {
    fname <- getWinAct(.getWinName())[1] # Name of the file to open
  }
  
  # This will be used when mseR is a proper R library.
	#rdir <- system.file(package = pckg)   # path to the R directory
	
	# Reference working directory.
	rdir <- getwd()
  fnam <- file.path( rdir, dname, fname )  

  openFile(fnam)
  
  return()
}     # function .viewHelp

# .wdSetup (Working directory set-up):
# Purpose:    Creates three (3) sub-directories if they do not exist:
#             (a) .FCODE - contains R code and PBSModelling GUI control files
#                 copied from working directory.
#             (b) .FHELP - contains help files copied from working directory.
#             (c) .FDOCS - contains code and model documentation.
# Notes:      In "shell", translate=TRUE turns forward slash / to backslash \.
# Parameters: None
# Returns:    The full path to the new directory
# Author:     A.R. Kronlund
# Source:     mseR V2.0, mseR2010, mseRsable, modified from PBSref
.wdSetup <- function()
{
  #pckg  <- .PACKAGE                      # Current package name.
  fdocs <- .FDOCS                        # Directory for documentation files.
  fhelp <- .FHELP                        # Directory for help files.
  fcode <- .FCODE                        # Directory for code files user can view.
  ftemp <- .FTEMP                        # Directory for temporary files.
  wkDir <- getwd()                       # Current R working directory.
  
  source( "mseRoptions.r" )
  
  # IMPORTANT - Set flag to catch first menu call on startup.
  .FIRSTMETHODGUI <- TRUE
  assign( ".FIRSTMETHODGUI",.FIRSTMETHODGUI,pos=1 )

  # These two will be used when mseR is a proper R library.
	#rdir <- system.file(package = pckg)          # Path to the R directory
  #fnam <- paste(rdir, dname, fils, sep = "/")  # the files you want to copy

	# Create .FDOC directory if needed, then copy any required files.
	docDir <- paste( wkDir, .FDOCS, sep="/" )
	if ( !file.exists(docDir) )
	  shell( paste( "mkdir", docDir ), translate=TRUE )

	# Create .FHELP directory if needed, then copy any required files.
	helpDir <- paste( wkDir, .FHELP, sep="/" )
	if ( !file.exists(helpDir) )
	  shell( paste( "mkdir", helpDir ), translate=TRUE )

	# Create .FCODE directory if needed, then copy R and PBSmodelling files.
  codeDir   <- paste( wkDir, .FCODE, sep="/" )
  if ( !file.exists(codeDir) )
    shell( paste("mkdir", codeDir), translate=TRUE )  # Create target directory.

	# Create .FCODE directory if needed, then copy R and PBSmodelling files.
  tempDir   <- paste( wkDir, .FTEMP, sep="/" )
  if ( !file.exists(tempDir) )
    shell( paste("mkdir", tempDir), translate=TRUE )  # Create target directory.

  codeFiles <- c(
              "mseRguiBatchWin.txt",
              "mseRguiCtlParsWin.txt",
              "mseRguiDataWin.txt",
              "mseRguiHcrWin.txt",
              "mseRguiMethodWin.txt",
              "mseRguiOpModWin.txt",              
              "mseRguiOptWin.txt",
              "mseRguiPerfWin.txt",
              "mseRguiSimWin.txt",
              "mseRguiTrackWin.txt",
              "mseRguiViewWin.txt",
              
              "mseRanimate_funs.r",
              "mseRguiBatchFuns.r",
              "mseRguiHelperFuns.r",
              "mseRguiOptFuns.r",                            
              "mseRguiPerfFuns.r",
              "mseRguiSimFuns.r",
              "mseRguiTrackFuns.r",                            
              "mseRguiViewFuns.r",

              "mseRoptions.r",
              "mseRplots.r",
              "mseRrefPoints.r",
              "mseRsimulation.r",
              
              "mseRAbout.txt",
              "testDoc.txt"
             )
 
  srcFiles <- paste( wkDir,   codeFiles, sep="/" )    # Source files.
  tarFiles <- paste( codeDir, codeFiles, sep="/" )    # Target files.
  file.copy( srcFiles, tarFiles, overwrite = TRUE)

  return(wkDir)
}     # END function .wdSetup


.ctlToGui <- function( ctlPars, prefix=NULL )
{
  # This function populates the GUI variables in the active guiInfo list with
  # values from ctlPars, where there is a match between the guiInfo variable
  # and the ctlPars "parameter" column.
  
  tmpwarn <- options( "warn" )
  options( warn=-1 )

  win        <- .getWinName()
  guiInfo    <- getWinVal( scope="L", winName=win )
  guiChanges <- guiInfo
  
  token <- names( guiInfo )
  if ( !is.null(prefix) )
    token <- paste( prefix, names(guiInfo), sep="$" )

  # For each item in guiInfo, look in ctlPars$parameter for a match.
  for ( i in 1:length(token) )
  {
    idx <- token[i]==ctlPars$parameter
    if ( sum(idx)== 1 )
    {
      # Take value from ctlPars$value and assign to guiChanges.
      value  <- ctlPars$value[ idx ]
      isChar <- is.na( as.numeric( value ) )
      
      # Check for character or numeric.
      if ( !isChar )
        value <- as.numeric( value )
     
      #guiChanges[ names(guiInfo)[i] ] <- value
      guiChanges[ names(guiInfo)[i] ] <- eval( parse( text=value ) )      
    }
    else if ( sum(idx)> 1 )
    {
      cat( "\nMSG (.ctlToGui) Duplicate matches, no changes made to guiInfo.\n" )
      print( token[i] )
      print( ctlPars[i,] )
    }
  }
  setWinVal( guiChanges, winName=win )
   
  options( tmpwarn )
  
  return( invisible() )
}     # END function .ctlToGui.

.guiToCtl <- function( ctlPars, prefix=NULL )
{
  # This function tranfers all variables from the active GUI guiInfo list
  # to the dataframe ctlPars where there is a match in the "parameter" column.
  # It ignores variables that are not matched.
  
  tmpwarn <- options( "warn" )
  options( warn=-1 )
  
  win     <- .getWinName()
  guiInfo <- getWinVal( scope="L", winName=win )
  
  # Make a vector of tokens from ctlPars$parameters.  
  # Transfer matching guiInfo variables to the value field of ctlPars.
  token <- names( guiInfo )
  if ( !is.null(prefix) )
    token <- paste( prefix, names(guiInfo), sep="$" )
  
  # For each item in the guiInfo list transfer the value into ctlPars$value.
  for ( i in 1:length(token) )
  {
    idx <- token[i]==ctlPars[,"parameter"]
    if ( sum(idx)== 1 )
    {
      # Take value from guiInfo, check to preserve quotes, vectors, etc.
      value  <- guiInfo[ i ]
      isChar <- is.na( as.numeric( value ) )
      
      # Check for character or numeric.
      if ( isChar )
        value <- dQuote( value )
     
      ctlPars[idx,"value"] <- value
    }
    else if ( sum(idx) > 1 )
    {
      cat( "\nMSG (.guiToCtl) Duplicate matches, no changes made to ctlPars.\n" )
      print( token[i] )
      print( guiInfo[i] )
    }
  }
  options( tmpwarn )  
  
  ctlPars
}     # END function .guiToCtl 


.getEstimationFails <- function( obj, keepFailRefPts=TRUE, keepFailConv=TRUE )
{
  # Input: obj - the runStatus dataframe from blob$mp$assess.
  #              applies to MPs with population dynamics models only.

  # ARK (29-Sep-13) Failed ref pts non-functional - need to return status from
  #                 refptsDD and refptsCA.
  
  # If keepFailRefPts=TRUE (from pfFailRefPts), INCLUDE replicates with failed reference points.
  
  
  # If failedConv=TRUE (from pfFailConv), INCLUDE replicates where convergence failed.

  if ( !is.null( obj ) )
  {  
    nReps     <- max( obj$iRep )
    nRepsFail <- 0
    
    nFailRefPts <- 0
    failRefPts  <- NULL
      
    if ( keepFailRefPts==FALSE )
    {
      # Get vector of replicate identifiers where reference points failed.
      # Since runStatus has more than one row for each replicate as progressive
      # years are estimated from tMP to nT.
  
      #failRefPts  <- unique( obj$iRep[ obj$EstRefPt=="DefaultUnRel" ] )
      failRefPts <- NULL
      nFailRefPts <- sum( failRefPts )
    }  
          
    nFailConv  <- 0
    failConv   <- NULL
      
    if ( keepFailConv==FALSE )
    {
      # Get vector of replicate identifiers where ADMB minmimizer failed to converge,
      # since runStatus has more than one row for each replicate as progressive years
      # are estimated from tMP to nT.
  
      # Failure is an exit code other than 1, or hessPosDef=FALSE.
      if ( any(names(obj)=="iExit") & any(names(obj)=="hessPosDef") )
      {
        idExit <- unique( obj$iRep[ obj$iExit!=1 ] )
        idHess <- unique( obj$iRep[ obj$hessPosDef==FALSE ] )
        failConv  <- unique( c(idExit,idHess) )
        nFailConv <- length( failConv )
      }
    }

    # Number of failed replicates is length of union of failed relplicate IDs.
    repsFail <- c( failRefPts, failConv )
    if ( !is.null( repsFail ) )
      repsFail <- sort( unique( repsFail ) )
    nRepsFail <- length( repsFail )
  }     # endif NOT null estPars.
  
  result <- list( failRefPts=failRefPts, nFailRefPts=nFailRefPts,
                  failConv=failConv, nFailConv=nFailConv,
                  repsFail=repsFail, nRepsFail=nRepsFail )
                  
  if ( .WHINE > 3 )
    print( result )
  result
}     # END function .getEstimationFails.  

#------------------------------------------------------------------------------#
#-- Deprecated, maybe, maybe not...                                          --#
#------------------------------------------------------------------------------#

# .cleanUp    (clean-up):
# Purpose:    Close any open database connections and close PBSask if open.
# Parameters: None
# Returns:    NULL (invisibly)
# Source:     PBSref
.cleanUp <- function(gvar)
{
  # close the DB connection if there was one
  conn <- get(gvar)$conn
  if(RODBC:::odbcValidChannel(conn))
  {
    odbcClose(conn)
  }
  # close PBSask if it was open
  if(!is.null(.PBSmod[["PBSask"]]$tkwindow))
  {
    closeWin("PBSask")
  }
  return(invisible())
}
