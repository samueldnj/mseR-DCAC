#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Finfish Version               #
#                                                                              #
#     Copyright 2010-2013 by A.R. Kronlund and K. Holt                         #
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
#------------------ mseRbatch.r: mseR Batch Job Generator ---------------------#
#--                                                                          --#
#-- mseRbatch.r: An mseR module that generates input parameter files for the --#
#--              mseR function "runMSE".  All cross-combinations of parameter--#
#--              levels specified by a batch job file are generated.         --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          K. Holt (Pacific Biological Station, Nanaimo, B.C.)             --#
#--                                                                          --#
#-- 22-Apr-2010 - (ARK) First implementation.                                --#
#-- 29-Apr-2010 - (ARK, KRH) Working version with a simple GUI completed.    --#
#-- 14-Jan-2013 - (ARK) Modified for mseR-Finfish architecture.              --#
#-- 09-Feb-2013 - (ARK) Finished implementing changes for mseR-Finfish.      --#
#                                                                              #
# Concept:                                                                     #
#                                                                              #
# A batch job control file containg a list of parameter names is formatted so  #
# that when read a list is created.  The list has two main nodes corresponding #
# to scenarios and management procedures.  The scenario node contains a list   #
# with nScenario elements, each of which contains named parameter values.  In  #
# the same way, the management procedure node contains nMP elements, each of   #
# which contains named parameter values.                                       #
#                                                                              #
# The following example batch job file creates 4 simulations by applying each  #
# of two management procedures to two scenarios:                               #
#                                                                              #
# Scenario_1 - steepness is 0.7, 10 replicates.                                #
# Scenario_2 - steepness is 0.9, 10 replicates.                                #
# Management_Procedure_2 - Kalman filter gain is 0.5                           %
# Management_Procedure_2 - Kalman filter gain is 0.7                           #
#                                                                              #
# Example Batch Job File (ignore first 3 columns):                             #
#
#  # File Begins <not run>.
#  parameter value
#
#  # Scenarios for reference set.
#  #
#  #  Scenario 1: S-R steepness is 0.7.
#  #
#  scenario$scenario1$gui$scenarioLabel "Scenario_1"
#  scenario$scenario1$opMod$rSteepness 0.7
#  scenario$scenario1$gui$nReps 10
#  #
#  # Scenario 2: S-R steepness is 0.9.
#  #
#  scenario$scenario2$gui$scenarioLabel "Scenario_2"
#  scenario$scenario2$opMod$rSteepness 0.9
#  scenario$scenario2$gui$nReps 10
#  #
#  # Management procedures to apply to each scenario.
#  #
#  # Management procedure 1 :
#  #
#  mp$mp1$gui$mpLabel "Management_Procedure_1"
#  mp$mp1$mp$assess$methodId 2
#  mp$mp1$mp$assess$kfGain 0.5
#  #
#  # Management procedure 2:
#  #
#  mp$mp2$gui$mpLabel "Management_Procedure_2"
#  mp$mp2$mp$assess$methodId 2
#  mp$mp2$mp$assess$kfGain 0.7
#  #
#  # File Ends <not run>.
#
# Questions:                                                                   #
#                                                                              #
# Q1. Where do the other parameters required for mseR come from because they   #
#     are not in the batch job file!!!                                         #
# A1. There is a base input parameters file that contains all parameters that  #
#     are required for mseR.  Any parameter value that occurs in the batch job #
#     control file OVERRIDES the value contained in the base parameter file.   #
#                                                                              #
# Q2. Can I have spaces in the scenarioLable or mpLabel strings?               #
# A2. No.  Well you can, but the code strips them out anyway so don't bother.  #
#                                                                              #
# Q3. Do I have to start scenarios with the string "scenario"?                 #
#     Yes.                                                                     #
#                                                                              #
# Q4. Do I have to start management procedures with the string "mp"?           #
# A4. Yes.                                                                     #
#                                                                              #
# Q5. Do I label each scenario uniquely after "scenario$" such as using        #
#     scenario$scenario5$pars$rSteepness ?                                     #
# A5. Yes. The same rule holds for management procedures after "mp$".          #
#                                                                              #
# Q6. How do I run the batch job?                                              #
# A6. A PBSmodelling GUI is used to manage the batch job, type "guiBatch()"    #
#     without the quotes at the R command console.                             #
#                                                                              #
# Q7. Are there any extra "add-ons" I can put in the batch control file?       #
# A7. Yes, these extras get passed to the design file for plotting purposes if #
#     they are found in the batch job control file:                            #
#                                                                              #
#	  mpLabel      - character string label for the management procedure.        #
#   methodName   - character string label for method.                          #
#   ruleName     - character string label for rule.                            #
#   mpRule       - number of rule.                                             #
#   mpCol        - color (character) for management procedure.                 #
#   mpLty        - line type for management procedure.                         #
#   mpLwd        - line width for managment procedure.                         #
#   mpSym        - symbol type for managment procedure.                        #
#   scCol        - color (character string) for scenario.                      #
#   scLty        - line type for scenario.                                     #
#   scLwd        - line width for scenario.                                    #
#   scSym        - symbol type for scenario.                                   #
#   join	       - join number for graphics.                                   #
#                                                                              #
#   Example use:                                                               #
#   mp$mp2$mpCol "red"                                                         #
#   scenario$scenario1$scLwd 2                                                 #
#                                                                              #
# guiBatch (to run type "guiBatch()" at the command conole without quotes)     #
#                                                                              #
# guiBatch allows the user to Select or Edit two input files:                  #
#  (a) Batch File - the batch job control file (see example above);            #
#  (b) Base File  - the mseR base file, same format as simCtlFile.txt.         #
# guiBatch allows the user to Select or View two output files:                 #
#  (c) Design File - lists the par files, scenario and management procedure    #
#                    labels and the values of tMP, nT, and nReps.              #
#                                                                              #
# guiBatch allows the user to select a text editor of their choice.            #
# guiBatch allows three batch job operations:                                  #
#  (f) Make - makes the nScenario * nMP parameter files required for the job,  #
#             and write the Design file.                                       #
#  (g) View - views the parameter files created by the Make step.              #
#  (h) Perf - performance statistics on the output.                            #
#  (h) Run  - runs the batch job by processing all parameter files specified   #
#             in the design using mseR's runMSE function.  In doing so, all    #
#             the simulations are added to the mseR project folder.            #
#                                                                              #
# WARNINGS:                                                                    #
#                                                                              #
# 1. The "Run" command will cause the simulations to run, adding simulation    #
#    folders to the project folder.  These can be moved to other project       #
#    folders, so that multiple batch jobs could be run at one time.            #
#                                                                              #
# USE INSTRUCTIONS:                                                            #
#                                                                              #
# 1. Select of edit a Batch File to control the generation of the simulation   #
#    design. Follow the format above with a default file extension of ".bch".  #
#                                                                              #
# 2. Select or create a Base File that contains the complete parameters list   #
#    required by mseR, i.e., the contents of simCtlFile.txt.                   #
#                                                                              #
# 3. Select a file name for the Design File (output).                          #
#                                                                              #
# 4. Select a Simulation File Prefix for naming the input parameter and the    #
#    corresponding simulation output files.  The latter contain the simulation #
#    results (e.g., blob) as an .Rdata file. For example, if the prefix is     #
#    "sim", then the input parameter files are named "sim1.txt", "sim2.txt"... #
#    and the simulation output folder and .Rdata files are named with a unique #
#    "simDateTime.Rdata",...  where  FateTime is a unique date/time stamp,     #
#    i.e., sim2904201021519.Rdata.                                             #
#                                                                              #
# 5. After the Input Files and Output Files are specified process the job:     #
#    (a) Make(s) the *.txt files specified in the Batch File.                  #
#    (b) View(s) the *.txt files to check they contain the desired parameters. #
#    (c) Run the batch job (press the button and walk away...)                 #
#                                                                              #
# 6. When the batch job completes, guiView or guiPerf can be selected to allow #
#    the results of the batch job to be reviewed.                              #
#                                                                              #
# NOTES:                                                                       #
#                                                                              #
# 1. To display hidden functions, e.g., .foo, enter command: ls(all.names=TRUE)#
# 2. Unique file names can be created by command:                              #
#                                                                              #
#    paste( "sim", format( Sys.time(), format="%d%m%Y%H%M%S" ), sep="" )       #
#                                                                              #
# Functions:                                                                   #
#                                                                              #
# .createBatchDesign   - Creates design dataframe and writes input par files.  #
# .createKeyList       - Creates a list from Batch File for .createBatchDesign.#
# .editTextFile        - Wrapper to call specified text editor.                #
# .getFileName         - Returns file dir name, filename, and file extension.  #
# .runBatchJob         - Runs the simulations specified in a design dataframe. #
# .writeDesignFile     - Writes the batch job design dataframe to a file.      #
#                                                                              #
# guiBatch             - Run the batch job GUI.                                #
# .guiBatchSetup       - Setup for guiBatch GUI creation.                      #
# .saveGuiBatchPars    - Saves the GUI parameters to global working directory. #
# .setFile             - Uses the PBSmodelling selectFile to set a filename.   #
# .subBatch            - Processes guiBatch submit actions, e.g., buttons.     #
# .validGuiBatchPars   - Checks for valid parameters for GUI.                  #
# 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# SECTION 1: Globals                                                           #
#------------------------------------------------------------------------------#

source( "mseRglobals.r" )
source( "mseRtools.r" )

# This options setting is required to get single ' and double " quotes.
options( "useFancyQuotes"=FALSE )

.subChar <- "_"

#------------------------------------------------------------------------------#
# SECTION 2: Helper Functions                                                  #
#------------------------------------------------------------------------------#

# .editTextFile (Wrapper to call specified text editor):
# Purpose:      Starts a text editor application to edit/view a text file.
# Parameters:   fName is a file name.
#               editApp is a text editor application such as notepad.exe.
# Returns:      TRUE if the file existed for editing, else FALSE.
# Source:       A.R. Kronlund (modified from PBSadmb function ....
.editTextFile <- function( fName, editApp="notepad.exe" )
{
  for ( i in 1:length(fName) )
  {
    # convSlashes is a PBSmodelling function that handles backslashes in
    # file paths.  dQuote adds double quotes, but options must be set to
    # options( "useFancyQuotes"=FALSE )
    
    options( "useFancyQuotes"=FALSE )
    
    fEdit <- paste( "start \"\"", dQuote(convSlashes(editApp)),
                    dQuote(convSlashes(fName[i])), sep=" " )
    fErr  <- paste( "WARNING (.editTextFile) File", fName[i],
               "does not exist.\n", sep=" " )
             
    if (file.exists(fName[i]))
    {
      shell( fEdit, intern = TRUE )
      cat( "\nMSG (.editTextFile)", fEdit, "\n" )
      fOut <- TRUE
    }
    else
    {
      cat( fErr )
      fOut <- FALSE
    }
  }
  return( fOut )
}     # END function .editTextFile

# .getFileName  (Returns file directory name, filename, and file extension)
# Purpose:      Given a filename, extracts the directory path, filename, and
#               the file extention and returns each as a list element.
# Parameters:   fName is a file name.
# Returns:      result is a list with elements dirName, fileName, fileExt.
# WARNING:      This function incomplete, file extension currently set to "".
# Source:       A.R. Kronlund, with an obtuse trick from CRAN.
.getFileName <- function( fName )
{
  # Returns path, file name and file extension.
  
  dirName <- dirname( fName )
 
  # ARK: I don't actually understand how this works, but it does.
  #      Obtained from CRAN mail archives.
  fileName <- sub( "[.][^.]*$","", fName, perl=TRUE )
  
  fileExt  <- ""
  
  result <- list( dirName=dirName, fileName=fileName, fileExt=fileExt )
  result
}     # END function .getFileName

#------------------------------------------------------------------------------#
# SECTION 3: Batch Job Management Functions                                    #
#------------------------------------------------------------------------------#

# .createBatchDesign  (Creates design dataframe and writes input par files)
# Purpose:      Given a batch control list, base parameter file, and simulation
#               file prefix, this function creates the batch job design by
#               crossing each management procedure node with each scenario node.
#               If the number of scenarios is nScenario and the number of
#               management procedures is nMP, then the result is a design
#               dataframe with nScenario*nMP rows and a *.par file corresponding
#               to each row (combination of scenario and management procedure).
#               The output design dataframe is passed to the .runBatchJob to
#               control completing the simulations using runMSE.
# Parameters:   ctlList is a list created by .createKeyList from the Batch File
#               basePars is a dataframe containing the base parameters that are
#                 modified by the ctlList values;
#               prefix is the Simulation File Prefix string concatenated to the
#               design filename, output parameter files, and simulation results
#               in .Rdata files (e.g., blobs).
# Returns:      result is the batch job design dataframe.
# Side Effects: The *.par files that specify each simulation are written.
# Source:       A.R. Kronlund, and probably under questionable conditions.
.createBatchDesign <- function( ctlList, basePars, prefix="job" )
{
  # Input a control list object, usually the output from .createList.

  scenario  <- ctlList$scenario              # List of scenarios.
  nScenario <- length( scenario )            # Number of scenarios.

  mp  <- ctlList$mp                          # List of management procedures.
  nMP <- length( mp )                        # Number of management procedures.

  nBatch <- nScenario * nMP                  # Number of mseR simulations.

  # Design dataframe - each row identifies the simulation and adds colors,
  # line types, line widths and symbols to be used for plotting outside of mseR.
  # This file is needed as input to runBatchJob to control the job, and for use
  # in plotting and performance calculations are to be done outside of mseR.
  
  parFile      <- character( nBatch )        # Name of each input parameter file.
  blobName     <- character( nBatch )        # Name of each blob.

  scenarioLabel <- character( nBatch )       # Name of each scenario.
  mpLabel       <- character( nBatch )       # Name of each management procedure.
  
  dataName     <- rep( "data",     nBatch )  # Name of data method.
  methodName   <- rep( "method",   nBatch )  # Name of assessment method.
  ruleName     <- rep( "rule",     nBatch )  # Name of HCR.
  scCol        <- rep( "black",    nBatch )  # Color for scenario.
  scLty        <- rep( 1,          nBatch )  # Line type for scenario.
  scLwd        <- rep( 1,          nBatch )  # Line width for scenario.
  scSym        <- rep( 1,          nBatch )  # Symbol for scenario.
  mpCol        <- rep( "black",    nBatch )  # Color for procedure.
  mpLty        <- rep( 1,          nBatch )  # Line type procedure.
  mpLwd        <- rep( 1,          nBatch )  # Line width for procedure.
  mpSym        <- rep( 1,          nBatch )  # Symbol for procedure.
  join         <- rep( 0,          nBatch )  # Join... ???

  iBatch <- 1

  # Loop over the scenarios.
  for ( i in 1:nScenario )
  {
    # Loop over the management procedures.
    for ( j in 1:nMP )
    {
      parFile[iBatch]   <- paste( prefix,iBatch,".txt",sep="" )
      
      # Create a unique blobname.
      # NOTE: This step is deferred until the simulation is launched to obtain
      #       a unique date-time stamp at time of execution.  For now simply
      #       provide a name indexed to the simulation number.
      blobName[iBatch] <- paste( "sim",prefix,iBatch,sep="" )
      
      # Set default scenarioLabel and mpLabel values in case not supplied.
      scenarioLabel[iBatch] <- paste( "scenario",i,sep="" )
      mpLabel[iBatch]       <- paste( "mp",j,sep="" )

      # Use the scenarioLabel if provided.
      #if ( !is.null(scenario[[i]]$scenarioLabel) )
      #  scenarioLabel[iBatch] <- scenario[[i]]$scenarioLabel

      # Scenario colour.
      if ( !is.null(scenario[[i]]$scCol ) )
        scCol[iBatch] <- scenario[[i]]$scCol

      # Scenario line type.
      if ( !is.null(scenario[[i]]$scLty ) )
        scLty[iBatch] <- scenario[[i]]$scLty

      # Scenario line width.
      if ( !is.null(scenario[[i]]$scLwd ) )
        scLwd[iBatch] <- scenario[[i]]$scLwd

      # Scenario symbol.
      if ( !is.null(scenario[[i]]$scSym ) )
        scSym[iBatch] <- scenario[[i]]$scSym

      # Use the mpLabel if provided.
      #if ( !is.null(mp[[j]]$mpLabel) )
      #  mpLabel[iBatch] <- mp[[j]]$mpLabel

      # Use the dataName if provided.
      if ( !is.null(mp[[j]]$dataName) )
        dataName[iBatch] <- mp[[j]]$dataName

      # Use the methodName if provided.
      if ( !is.null(mp[[j]]$methodName ) )
        methodName[iBatch] <- mp[[j]]$methodName

      # Use the ruleName if provided.
      if ( !is.null(mp[[j]]$ruleName) )
        ruleName[iBatch] <- mp[[j]]$ruleName

      # Management procedure colour.
      if ( !is.null(mp[[j]]$mpCol ) )
        mpCol[iBatch] <- mp[[j]]$mpCol

      # Management procedure line type.
      if ( !is.null(mp[[j]]$mpLty ) )
        mpLty[iBatch] <- mp[[j]]$mpLty

      # Management procedure line width.
      if ( !is.null(mp[[j]]$mpLwd ) )
        mpLwd[iBatch] <- mp[[j]]$mpLwd

      # Management procedure symbol.
      if ( !is.null(mp[[j]]$mpSym ) )
        mpSym[iBatch] <- mp[[j]]$mpSym

      # Join the procedures (groups share common integer value).
      if ( !is.null(mp[[j]]$join ) )
        join[iBatch] <- mp[[j]]$join

      # Replace values for any keywords that match those in the mseR input
      # parameters file:
      # 1. Find keywords shared between the batch job control list and the mseR
      #    parameter file.
      # 2. Replace the values in the mseR parameter file with those from the
      #    batch control list.
      
      newPars <- basePars
      
      # This step compares the names in the i-th scenario to the names in the
      # first column of newPars to find the common names using intersect.

      # Change all .subChar symbols to "$".
      names(scenario[[i]]) <- gsub( .subChar,"$",names(scenario[[i]]), fixed=TRUE )
       
      sharedNames <- intersect( names(scenario[[i]]),newPars[,1] )
 
      if ( length(sharedNames) > 0 )
        for ( k in 1:length(sharedNames) )
        {
          val <- scenario[[i]][[ sharedNames[k] ]]
          if ( is.character(val) )
            val <- dQuote( val )
          newPars[,2][ newPars[,1]==sharedNames[k] ] <- val
        }

      # Change all .subChar symbols to "$".
      names( mp[[j]] ) <- gsub( .subChar,"$",names(mp[[j]]), fixed=TRUE )
       
      sharedNames <- intersect( names(mp[[j]]),newPars[,1] )
 
      if ( length(sharedNames) > 0 )
        for ( k in 1:length(sharedNames) )
        {
          val <- mp[[j]][[ sharedNames[k] ]]
          if ( is.character(val) )
            val <- dQuote( val )
          newPars[,2][ newPars[,1]==sharedNames[k] ] <- val
        }
          
      # Check to see if scenarioLabel and mpLabel are updated.
      scenarioLabel[iBatch] <- newPars[,2][ newPars[,1]=="gui$scenarioLabel" ]
      mpLabel[iBatch] <- newPars[,2][ newPars[,1]=="gui$mpLabel" ]
      
      # Remove leading white space: gsub('^[[:space:]]+', '', " a test ")
      newPars[,2] <- gsub( '^[[:space:]]+','',newPars[,2] )
      
      # *** ARK: Why were there 3 columns in the original implementation?
      # newPars[,3] <- gsub( '^[[:space:]]+','',newPars[,3] )

      fName <- parFile[iBatch]          # mseR input parameters file.
      
      fName <- file.path( .PRJFLD, .DEFBATFLD, fName )
      batchDate <- date()               # Current date and time.      
      
      # Open a new parameter file and write the title and date/time stamp.
      cat( file=fName,
        "# ",parFile[iBatch],": mseR parameter file written ",batchDate,".\n", sep="" )

      # NOTE: write.table wants a matrix or data.frame, not a vector.
 
      # Write the header field names for the parameter file.
      
      colNames <- names( basePars )
      #write.table( file=fName, matrix( colNames, nrow=1 ), quote=FALSE,
      #             col.names=FALSE, row.names=FALSE,
      #             sep=" ", append=TRUE )
                   
      write.table( file=fName, newPars, quote=FALSE,
                   col.names=TRUE, row.names=FALSE,
                   sep=" ", append=TRUE )			     #RF changed append=FALSE to append=TRUE
   
      #options( warn=-1 )                        # Turn off whining.
      #for ( k in 1:nrow(newPars) )
      #{
      #  isNumericVal <- !is.na( as.numeric( newPars[k,2] ) )  # Coerce non-numeric to NA.
      #  if ( isNumericVal )
      #    cat( file=fName, newPars[k,1]," ",newPars[k,2],"\n", append=TRUE, sep="" )
      #  else
      #    cat( file=fName, newPars[k,1]," ",dQuote(newPars[k,2]),"\n", append=TRUE, sep="" )        
      #}
      #options( warn=0 )                 # Turn on whining.
      
      cat( "\nMSG(.createBatchDesign) mseR parameter file ",fName," written...\n" )

      iBatch <- iBatch + 1           # Increment the batch job counter.
    }
  }
  
  # Bind the vectors that make up the design dataframe.
  result <- data.frame( parFile, blobName, scenarioLabel,
              mpLabel, dataName, methodName, ruleName,
              scCol, scLty, scLwd, scSym,
              mpCol, mpLty, mpLwd, mpSym, join,
              stringsAsFactors=FALSE )
  result
}    # END function .createBatchDesign

# .createKeyList (Creates a list from the Batch File for .createBatchDesign):
# Purpose:      Function to convert the Batch File dataframe (loaded by the
#               function .readParFile) into a list that structures parameters
#               into "scenario" and "mp" nodes, each of each contains the
#               parameters for unique scenarios and management procedures,
#               respectively.
# Parameters:   obj is a dataframe read by .readParFile with columns "parameter"
#                 and "value".
# Returns:      result, a list with the batch control structure required to form
#               the desired cross-combinations of scenarios and procedures.
# Source:       A.R. Kronlund
.createKeyList <- function( obj )
{
  # Input  a data frame with columns "parameter" and "value".
  # Output a list with elements named as first level of parameter and the
  # balance as the key, with values in "value".

  result <- list()

  options( warn=-1 )                        # Turn off whining.
  numericVal <- as.numeric( obj[,"value"] ) # Coerce non-numeric to NA.
  options( warn=0 )                         # Turn on whining.

  for ( i in 1:nrow(obj) )
  {
    # Value is numeric, build the parse string.
    if ( !is.na(numericVal[i]) )
    {
      parName <- obj[i,"parameter"]
      # Replace all the "$" with "&" in parameter name.
      parName <- gsub( "$",.subChar,parName, fixed=TRUE )
      # Replace ONLY the first "&" with "$" in parameter name, TWICE.
      parName <- sub( .subChar,"$",parName, fixed=TRUE )
      parName <- sub( .subChar,"$",parName, fixed=TRUE )
      
      listText <- paste( "result$",parName,"=",
                    obj[i,"value"],sep="" )
    }
    # Value is character, build the parse string.
    else
    {
      parName <- obj[i,"parameter"]
      # Replace all the "$" with "&" in parameter name.
      parName <- gsub( "$",.subChar,parName, fixed=TRUE )
      # Replace ONLY the first "&" with "$" in parameter name.
      parName <- sub( .subChar,"$",parName, fixed=TRUE )
      parName <- sub( .subChar,"$",parName, fixed=TRUE )
      
      listText <- paste( "result$",parName,"=",
                    obj[i,"value"],sep="" )
    }

    # ARK: At one point I had this code, presumably to handle a different
    #      input format convention, perhaps assuming "value" was all character.
    #                   sQuote(obj[i,"value"]),sep="" )
    
    # Evaluate the parse string.
    eval( parse( text=listText ) )
  }
  result
}    # END function .createKeyList

# .runBatchJob  (Runs the simulations specified in a design dataframe)
# Purpose:      Loops through the rows of the design dataframe, each of which
#               specifies an input parameter file and labels for a simulation.
#               The mseR function runMSE is called for each row which generates
#               the simulation results (e.g., blob) and simulation folders.
# Parameters:   batchDesign is a batch design dataframe created by the function
#                 .createBatchDesign.
#               prefix is the Simulation File Prefix/
# Returns:      NULL (invisibly)
# Side Effects: A simulation folder containing *.info and *.Rdata file (blob) and
#               for each row of the design dataframe, i.e., for each simulation.
# Source:       A.R. Kronlund
.runBatchJob <- function( batchDesign, prefix="Job" )
{
  # Runs simulations from the design data.frame specified in batchDesign object.
  # 1. Does the mseR input parameter file exist? If YES then read the file.
  # 2. Run the simulation using runMSE().
  # 3. Update the design data.frame somehow...

  # Vector of simCtlFile names, like simCtlFile1.txt, simCtlFile2.txt, etc.
  batchParFile <- file.path( .PRJFLD, .DEFBATFLD, basename( batchDesign$parFile ) )
  blobName     <- batchDesign$blobName  # Vector of blobNames.
  nSims        <- length(blobName)      # Number of blobs (i.e., simulations).

  closeWin()                     # Close GUI to avoid TclTk ugliness.

  result <- data.frame( sim=character(nSims), scenarioLabel=character(nSims),
                        mpLabel=character(nSims), elapsed=numeric(nSims),
                        stringsAsFactors=FALSE )

  # This is part where shell script PPSS could be used to parallelize the batch job
  # Need to create a folder of inputParameters.par files to be processed via runMSE().
  # Might need to have the .par file be an argument to runMSE()...check what effects
  # that would have elsewhere.
  
  for ( i in 1:nSims )
  {
    if ( file.exists( batchParFile[i] ) )
    {
      fileName <- strsplit( batchParFile[i],"\\." )[[1]][1]
      cat( "\nRunning batch job: ",batchParFile[i],"...\n" )

      tBegin    <- proc.time()
      startDate <- date()

      cat( "\nMSG (.runBatchJob) Processing batchParFile = ", batchParFile[i], "\n" )
      
      # Overwrite the simCtlFile.txt file for mseR, the overwrite=TRUE is CRITICAL.
      
      cat( "\nWARNING (.runBatchJob) simCtlFile.txt overwritten by ",
           batchParFile[i],"\n" )
      file.copy(batchParFile[i],
                 "simCtlFile.txt", overwrite=TRUE )
     
      # runMSE assumes that input is inputParameters.par.
      runMSE()

      elapsed <- (proc.time() - tBegin)[ "elapsed" ]
      cat( "\nMSG (.runBatchJob): Elapsed time for simulation = ",
        round(elapsed/60.0,digits=2)," minutes.\n" )
    }
    
    result[ i,"sim" ]           <- batchDesign[ i,"parFile" ]
    result[ i,"scenarioLabel" ] <- batchDesign[ i,"scenarioLabel" ]
    result[ i,"mpLabel" ]       <- batchDesign[ i,"mpLabel" ]
    result[ i,"elapsed" ]       <- round( elapsed/60.0, digits=2)
    
    cat( "\nMSG (.runBatchJob) Progress as of ", date()," Simulation ",i," of ",nSims,"\n\n" )
    print( result[1:i,] )
  }

  cat( "\n" )
  invisible()
}     # END function .runBatchJob

# .writeDesignFile (Writes the batch job design dataframe to a file)
# Purpose:         Given a dataframe, assumed to contain a batch job design,
#                  created by .createBatchDesign, write to text file.
# Parameters:      obj is a dataframe.
#                  outFile is the desired filename.
# Returns:         NULL (invisibly)
# Side Effects:    Call to this function may generate a warning, not sure why.
# Source:          A.R. Kronlund
.writeDesignFile <- function( obj, outFile=.FBATDES )
{
  nRow <- nrow( obj )
  if ( is.null(nRow) )
    nRow <- 1

  cat( file=outFile, "# mseR batch design file, ",date(),"\n" )
  
  # This generates a warning, not sure why.
  write.table( obj, file=outFile,
               col.names=TRUE, row.names=FALSE, quote=TRUE, sep=",", append=TRUE )
  cat( "\nMSG (.writeDesignFile): Batch design file written to ",outFile,"\n" )
  invisible()
}     # END function .writeDesignFile

#------------------------------------------------------------------------------#
# SECTION 4: GUI Menu Functions                                                #
#------------------------------------------------------------------------------#

# guiBatch    (Run Batch GUI)
# Purpose:    Run the GUI.
# Parameters: None
# Returns:    NULL (invisibly)
guiBatch <- function()
{
  return( .guiBatchSetup("mseRguiBatch") )
}

#------------------------------------------------------------------------------#
#-- Gui Error Checking                                                       --#
#------------------------------------------------------------------------------#

# .validGuiBatchPars (Checks for valid parameters for GUI):
# Purpose:      Check whether the parameters supplied are valid.
#               If invalid, display an error message in the R console and
#               clear the invalid field.
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid.
# Parameters:   None
# GUI inputs:   Parameters taken directly from active myGui. 
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields, corrects correctable fields (if any)
.validGuiBatchPars <- function()
{
  # Get the GUI  values and make them local to this function.
  getWinVal( scope="L" )
                    
  isValid <- TRUE
  
  # This will be a list of the parameters that require changing.                                                     
  changes <- list()
  
  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.

  # Example for GUI parameter "x".  
  # if ( is.na(x) || (x <= 1) || (x > 10) )
  # {
  #   cat( "Parameter x must be 1 <= x <= 10".\n" )
  #   changes$x <- NA
  #   isValid <- FALSE
  # }

  # If there are invalid parameters, bring the console to the top.  
  if ( !isValid )
    bringToTop( -1 )
  
  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( changes )
  return( isValid )
}

# .subBatch (Processes guiBatch submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiBatch program flow:
#             - Attempts to check validity of the GUI parameters;
#             - processes any button actions;
#             - process any file open/save actions;
#             - processes any entry field actions;
#             - processes any batch job operations.
# Parameters: NONE
# Returns:    NULL (invisibly)
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subBatch <- function()
{
  win      <- .getWinName()                       # Get the current window name.
  gvar     <- paste( ".", win, sep="" )           # Global variable name.
  
  guiInfo  <- getWinVal( scope="L", winName=win ) # GUI information local scope.
  act      <- getWinAct()[1]                      # Get last menu window action.
  guiChanges <- list()                            # List for GUI changes.

  valid    <- TRUE                                # *** ARK: Add validity check.
  
  batBasePath <- file.path( .PRJFLD, .DEFBATFLD, basename( batBasePars ) )
  batDesPath  <- file.path( .PRJFLD, .DEFBATFLD, basename(batDesFile) )
  batJobPath  <- file.path( .PRJFLD, .DEFBATFLD, basename(batJobFile) )

  if ( valid )
  {
    #----------------------------------------------------------------#
    # Input Files                                                    #
    #----------------------------------------------------------------#
    
    # Edit the batch job file name.
    if ( act=="batJobFile" )
    {
      guiChanges$batJobFile <- batJobFile
      setWinVal( guiChanges, winName=win )
    }
    
    # Select the batch job input file.
    if ( act=="batJobSelect" )
    { 
      val <- .setFile( batJobPath, c( ".bch","bch files" ) )
      print( batJobPath )
      if ( !is.null(val) )
        guiChanges$batJobFile <- val
      setWinVal( guiChanges, winName=win )
      print( guiChanges )
    }
    
    # Edit the batch job input file.
    if ( act=="batJobEdit" )
    {
      val <- .editTextFile( batJobPath, batEditApp )
    }
    
    # Edit the base simCtlFile file name.
    if ( act=="batBasePars" )
    {
      guiChanges$batBasePars <- batBasePars
      setWinVal( guiChanges, winName=win )
    }
    
    # Select the base simCtlFile file.
    if ( act=="batBaseSelect" )
    {
      val <- .setFile( batBasePath, c( ".txt","txt files" ) )
      if ( !is.null(val) )
        guiChanges$batBasePars <- val
      setWinVal( guiChanges, winName=win )
    }
    
    # Edit the base parameter file.
    if ( act=="batBaseEdit" )
    {
      val <- .editTextFile( batBasePars, batEditApp )
    }    
    
    #----------------------------------------------------------------#
    # Output Files                                                   #
    #----------------------------------------------------------------#    
   
    # Edit the design file output file name.
    if ( act=="batDesFile" )
    {
      guiChanges$batDesFile <- batDesFile
      setWinVal( guiChanges, winName=win )
    }    
    
    # Select the batch design output file.
    if ( act=="batDesign" )
    {
      batDesFile <- file.path( .PRJFLD, .DEFBATFLD, basename(batDesFile) )
      val <- .setFile( batDesFile, c( ".design","design files" ) )
      if ( !is.null(val) )
        guiChanges$batDesFile <- val
      setWinVal( guiChanges, winName=win )
    }
    
    # View the batch design output file.
    if ( act=="batDesView" )
    {
      val <- .editTextFile( batDesPath, batEditApp )    
    }
    
    # Edit the batch job prefix for naming .Rdata files containing blobs.
    if ( act=="batPrefix" )
    {
      guiChanges$batPrefix <- batPrefix
      setWinVal( guiChanges, winName=win )
    }
       
    #----------------------------------------------------------------#
    # Options                                                        #
    #----------------------------------------------------------------#
    
    if ( act=="batSetEdit" )
    {
      val <- .setFile( batEditApp, c( ".exe","exe files" ) )
      if ( !is.null(val) )
        guiChanges$batEditApp <- val
      setWinVal( guiChanges, winName=win )      
    }
    
    #----------------------------------------------------------------#
    # Batch control buttons                                          #
    #----------------------------------------------------------------#
    
    # Make the simulation control files using .createBatchDesign.
    if ( act=="batMake" )
    {
      if ( exists( "batchDesign" ) )
        rm( batchDesign )
                                   
      # Load the batch control file.
      batchJobPars <- .readParFile( batJobPath )
      cat( "\nMSG (.subBatch): Batch job control file loaded from",
                    batJobPath, "\n" )
      print( batchJobPars )
        
      # Load the base input parameter file.
      baseInputPars <- .readParFile( batBasePars )
      cat( "\nMSG (.subBatch) Base simulation control parameters loaded from",
             batBasePars,"\n" )
      print( baseInputPars )

      # Create a list object from the batch job data frame.
      batchJobCtlList <- .createKeyList( batchJobPars )

      # Write the mseR simulation control files for the batch job.
      batchDesign <- .createBatchDesign( batchJobCtlList, baseInputPars,
                        prefix=batPrefix )
                        
      # Save a copy of the batch design to the global working directory.
      assign ( paste( batPrefix,"Design",sep="" ), batchDesign, pos=1 )
      cat( "\nMSG (.subBatch) Batch design matrix:\n\n ")
      print( batchDesign )
      
      # Issue a warning.
      cat( "\nMSG (.subBatch) Reference points in control file may be incorrect until run.\n" ) 
      
      # Write a copy of the batch design to file.
      .writeDesignFile( batchDesign, outFile=batDesPath )
    }
    
    # View the simulation control files generated by .createBatchDesign.
    if ( act=="batShow" )
    {
      if ( file.exists( batDesPath ) )
      {
        batchDesign <- read.table( batDesPath, as.is=TRUE, header=TRUE, sep="," )
        cat( "\nMSG (.subBatch): Viewing simulation control files for ",batDesPath,"\n" )
        print( batchDesign )
        val <-.editTextFile( file.path( .PRJFLD, .DEFBATFLD, batchDesign$parFile ),
                 batEditApp )     
      }
      else
        cat( "\nWARNING (.subBatch): Batch design file", batDesPath,
             " not found.\n" )
    }
    
    # Run the batch job - check to ensure required design data frame exists.
    # This action does NOT recreate the batch files to allow the user to edit.
    if ( act=="batRun" )
    {
      if ( file.exists( batDesPath ) )
      {
        batchDesign <- read.table( batDesPath, as.is=TRUE, header=TRUE, sep="," )
        cat( "\nMSG (.subBatch): Running batch job for ",batDesPath,"\n\n" )
        print( batchDesign )
        
        .runBatchJob( batchDesign )
        
        .writeDesignFile( batchDesign, batDesPath )
      }
      else
        cat( "WARNING (.subBatch): No job because batch design file",
             batchDesign,"does not exist.\n" )
    }
       
    #----------------------------------------------------------------#
    # Control buttons                                                #
    #----------------------------------------------------------------#
    
    # guiView requested.
    if ( act=="batSim" )
    {
      doGuiSim<- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subBatch has to exit first!!!
        .saveGuiBatchPars( win )
        
        closeWin()
        guiSim()
      }
      on.exit( doGuiSim() )
    }
    
    # guiOpt requested.
    if ( act=="batOpt" )
    {
      doGuiOpt <- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subBatch has to exit first!!!
        .saveGuiBatchPars( win )
        
        closeWin()
        guiOpt()
      }
      on.exit( doGuiOpt() )
    }
    
    # guiPerf requested.
    if ( act=="batPerf" )
    {
      doGuiPerf <- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subBatch has to exit first!!!
        .saveGuiBatchPars( win )
        
        closeWin()
        guiPerf()
      }
      on.exit( doGuiPerf() )
    }
    
    if ( act=="batReset" )
    {
      doReset <- function()
      {
        if ( exists( ".guiBatchPars" ) )
          rm( .guiBatchPars, pos=1 )
        
        cat( "\nMSG (subBatch) Reset guiBatch parameters to defaults.\n" )
          
        closeWin()
        guiBatch()
        
        .saveGuiBatchPars( win )
      }
      on.exit( doReset() )
    }
    
    # guiTrack requested.
    if ( act=="batTrack" )
    {
      doGuiTrack <- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subBatch has to exit first!!!
        .saveGuiBatchPars( win )
        
        closeWin()
        guiTrack()
      }
      on.exit( doGuiTrack() )
    }    

    # guiView requested.
    if ( act=="batView" )
    {
      doGuiView <- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subBatch has to exit first!!!
        .saveGuiBatchPars( win )
        
        closeWin()
        guiView()
      }
      on.exit( doGuiView() )
    }
      
    # EXIT the GUI (leave graphics on).
    if ( act=="batExit" )
    {
      # Don't close until .subBatch has terminated.
      .saveGuiBatchPars( win )
      on.exit( closeWin() )
    }      
      
  }     # if valid.
  else
  {
    # Invalid GUI parameters so bring R console to top.
    bringToTop(-1)
  }     # invalid.

  return( invisible() )  
}

#------------------------------------------------------------------------------#
#-- GUI Helper Functions                                                     --#
#------------------------------------------------------------------------------#

# .guiBatchSetup (Setup for guiBatch creation)
# Purpose:    Set up and run guiBatch
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.guiBatchSetup <- function( win )
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs.
  require( RODBC )                   # For the Excel and database SQL.
  require( tools )                   # For file_ext utilities.
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file.
  
  assign( gvar, list(), pos=1 )      # Hidden global list.
  
  wkDir <- .wdSetup()                # Setup working directory.

  .mseRinitProject( wkDir )          # Initialize mseR project, paths, options.

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
        cat( "\nMSG (.guiBatchSetup) Simulation control file",.CTLFILE,"copied to",
             ctlFile,"\n" )
      .CTLFILE <- ctlFile
      assign( ".CTLFILE",.CTLFILE, pos=1 )
    }    
  }
  
  # Close all windows that are currently open to prevent crashes.
  closeWin()
  
  # Can a menu be created?
  goMenu <- TRUE
  
  #*** Test here whether conditions are valid for menu creation. ***
  
  # Valid conditions exist for menu creation.
  if ( goMenu )
  {
    # Initialize the GUI from the description file using PBSmodelling createWin.
    createWin( paste( wkDir, "/", win, "Win.txt", sep="" ) )
    
    # Get the GUI parameters and make scope local to this function.
    win <- .getWinName()
    guiInfo <- getWinVal( scope="L", winName=win )
          
    # Initialize myGui action.
    if ( win=="mseRguiBatch" )
    {
      if ( exists( ".guiBatchPars" ) )
      {
        setWinVal( .guiBatchPars, winName=win )
        print( .guiBatchPars )
      
      }
    }
  }
  else
    cat( "\nERROR (.guiBatchSetup): GUI creation not possible:\n ")

  return( invisible() )
}     # END function .guiBatchSetup

# .saveGuiBatchPars  (Saves the GUI parameters to global working directory)
# Purpose:         Saves Batch GUI parameters obtained from getWinVal to
#                  the global working directory.
# Parameters:      win is the GUI window name.
# Returns:         NULL (invisibly)
# Source:          A.R. Kronlund
.saveGuiBatchPars <- function( win )
{
  guiInfo <- getWinVal( winName=win )
  assign( ".guiBatchPars",guiInfo, pos=1 )
  invisible()
}     # END function .saveGuiBatchPars

# .setFile    (Uses the PBSmodelling function selectFile to set a filename)
# Purpose:    Allows the user to set a filename via the GUI.
# Parameters: fName is a file name character string.
#             fileType is a file type as a character vector.
# Returns:    val the new file name.
# Source:          A.R. Kronlund
.setFile <- function( fName, fileType )
# fName is a file name.
# fileType is a character vector of file types.
# e.g., c( ".txt","txt files" )
{
  val <- NULL     # Returns NULL if no file name selected.
  tmpFile <- selectFile( initialfile=fName,
               filetype=list( fileType ), mode="open" )
               
  if ( !is.null(tmpFile) )
    val <- tmpFile   # Set new file name.
  return( val )
}     # END function .setFile
