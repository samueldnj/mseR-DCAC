#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Version 4.x                   #
#                                                                              #
#     Copyright 2008, 2009, 2010 by A.R. Kronlund and S.P. Cox.                #
#                                K. Holt.                                      #
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
#     "Anecdotal evidence is all you need to prove your point; scientific      #
#      is what you require of everybody else."                                 #
#                                                                              #
#--------------------- mseRplots.r: mseR Plotting Functions -------------------#
#--                                                                          --#
#-- mseRplot.r: An mseR support file that provides a plotting functions.     --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--          K.   Holt (Pacific Biological Station, Nanaimo, B.C.)           --#
#--                                                                          --#
#-- Contributors:                                                            --#
#--                                                                          --#
#--          A.F. Sinclair (contributions from PBSref)                       --#
#--          T.K. Deering  (contributions from PBSref)                       --#
#--                                                                          --#
#-- First Implementation: 09-Feb-10                                          --#
#                                                                              #
# NOTES:                                                                       #
#                                                                              #
# 1. To display hidden functions, e.g., .foo, enter command: ls(all.names=TRUE)#
#                                                                              #
# References:                                                                  #
#                                                                              #
# DFO, 2006. A Harvest Strategy Compliant with the Precautionary Approach.     #
#   DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2006/023.                       #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., and Egeli, A. and A.R. Kronlund. #
#   2010. PBS Modelling 2.00: users guide revised from Canadian Technical      #
#   Report of Fisheries and Aquatic Science 2674: v+146 p.                     #
#                                                                              #
# This script contains all the plotting functions for mseR-Finfish.  The plots #
# are grouped below by their calling GUI: guiSim, guiView, guiPerf.            #
                                                                               #
# (A) Shared plot functions (used by more than one GUI)                        #
#
# .plotCtlPars
# .plotDesign
# .plotRefPoints
# .plotStatus
#                                                                              #
# (B) Plot functions for guiSim                                                #
#                                                                              #
# .plotSimRefPoints                                                            #
#                                                                              #
# (C) Plot functions for guiView                                               #
#                                                                              #
# .getMethodLabel  : Get a text label for plotting for required assessMethod.  #
#                                                                              #
# .plotAgeBubblesOM: Plot bubble plots of operating model proportions at age.  #
# .plotAgeFreq     : Plot bar plots of the obs. and pred. age props.           #
# .plotAgeOM       : Plot the operating model true age props.                  #
# .plotBt          : Plot total and spawning biomass vs. time                  #
# .plotBtDtFt      : Plot spawning biomass, catch, & fishing mort. vs. time    #
# .plotBtDtRt      : Plot spawning biomass, catch, recruitment vs. time        #
# .plotDt          : Plot catch biomass vs. time                               #
# .plotFit         : Plot predicted biomass time series from all pr. mod fits  #
# .plotFt          : Plot fishing mortality vs. time                           #
# .plotItBt        : Plot the observed survey points against SSB               #
# .plotIt          : Plot the observed survey series vs. time                  #
# .plotMt          : Plot natural mortality vs. time.                          #
# .plotNt          : Plot total and spawning numbers vs. time                  #
# .plotRt          : Plot the recruitment numbers vs. time                     #
# .plotRtBt        : Plot the stock-recruitment points and curve               #
# .plotParEsts     : Plot timeseries of B0 and r estimates from pr. model fits #
# .plotRefPtSeries : Plot reference point time series against objectives.      #
# .plotYield       : Plot true and estimated yield curves                      #
#                                                                              #
# (D) Plot function for guiPerf                                                #
#                                                                              #
# -------- Mid-level (Level 2) functions that re-route to lower level -------- #
#                                                                              #
# .doBarPlots       : Sets up inputs for all lower level barplot functions     #
# .doQuantBoxPlots  : Sets up inputs for all lower level boxplot functions     #
# .doTulipPlots     : Sets up inputs for all lower level tulip plot functions  #
# .doConvPlots      : Sets up inputs for all lower level plots on conv. diag.  #
# .doOtherPlots     : Sets up inputs for all "other" functions                 #
#                                                                              #
# ---- Lower-level (Level 3) functions that produce plots ---------------------#
#
# .plotBarsByPeriod  : Barplots summarizing performance by time period         #                                                             
# .plotBarsByStat    : Barplots summarizing performance by performance stat    #
# .plotQboxDep       : Boxplots of depletion relative to status zones          #
# .plotQboxSSB       : Boxplots of spawning biomass relative to status zones   #
# .plotMaxGrad       : Maximum gradient for ADMB optimization solution         #
# .plotExitCodes     : Frequency of exit codes from ADMB optimizations         #
# .plotFuncCalls     : Number of function calls required by ADMB optimization  #
# .plotTimeConv      : Time required (in secs) to solve ADMB optimization      #
# .plotTulipCatch    : Tuilp (simulation envelope) plots of catch over time    #
# .plotTulipDepletion: Tulip (simulation envelope) plots of depltn over time   #
# .plotTulipF        : Tuilp (simulation envelope) plots of F over time        #
# .plotTulipBmsy     : Tuilp (simulation envelope) plots Bmsy ests over time   #
# .plotTulipFmsy     : Tuilp (simulation envelope) plots Fmsy ests over time   #
# .plotTulipF        : Tuilp (simulation envelope) plots of F over time        #
# .plotFvsSSB        : Fishing mortality vs. ssb pooled over all reps & years  #
#                                                                              #
# INSTRUCTIONS FOR ADDING NEW PLOTS TO GUIVIEW WINDOW                          #
#                                                                              #
#     (i) Create a new plot function (.plotXxx) that has a similar format as   #
#            the exist guiViewplot functions (e.g., .plotBiomass, .plotCatch)  #
#     (ii) Add a new if statement to .doViewPlots() function to call new       #
#             .plotXxx when required.                                          #
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#-- (A) Shared plot functions (used by more than one GUI)                    --#
#------------------------------------------------------------------------------#

.addXaxis <- function( xLim=xLim, initYear=.INITYEAR, side=1, years=FALSE, cexAxis=.CEXAXIS )
{         
  if ( years )
  {
    xPos <- seq( .INITYEAR,.INITYEAR+xLim[2]-1, 5 )
    xSeq <- xPos - .INITYEAR + 1
    xLabs <- paste( xPos )
    
    if ( side==1 )
    {  
      axis( side=1, at=xSeq, cex.axis=cexAxis, labels=xLabs )
      axis( side=3, at=xSeq, labels=FALSE )
    }
    if ( side==3 )
      axis( side=3, at=xSeq, cex.axis=cexAxis, labels=xLabs )
  }
  else
  {
    if ( side==1 )
    {
      axis( side=1, cex.axis=cexAxis )
      axis( side=3, labels=FALSE )
    }
    
    if ( side==3 )
    {
      axis( side=3, cex.axis=cexAxis )
      axis( side=1, labels=FALSE )    
    }
  }
}     # END function .addXaxis

.setTextContrastColor <- function( color )
{
  result <- ifelse( mean(col2rgb(color)) > 127, "black","white" )
  result
}

.plotColorChart <- function()
{
  colCount <- 25
  rowCount <- 27
  
  RGBColors <- col2rgb( colors()[1:length(colors())] )
  HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,], maxColorValue=255 )
  HueOrder  <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )
  
  textContrastColor <- unlist( lapply( colors(), .setTextContrastColor) )
  
  plot( c(1,colCount),c(0,rowCount), type="n", axes=FALSE, xlab="",
        ylab="", ylim=c(rowCount,0) )
  
  mtext( side=3, line=0, cex=.CEXTITLE4, "R Colors" )
  for ( j in 0:(rowCount-1) )
  {
    for ( i in 1:colCount )
    {
      k <- j * colCount + i
      if ( k <= length( colors() ) )
      {
        rect( i-0.5,j-0.5, i+0.5,j+0.5, border="black", col=colors()[ HueOrder[k] ] )
        text( i,j, paste( HueOrder[k] ), cex=0.7, col=textContrastColor[ HueOrder[k] ] )
      }
    }
  }
  return( invisible() )
}     # END function .plotColorChart

.plotCtlPars <- function( ctlPars, type=NULL )
{
  calcSettings <- function( x, nCol=4, byrow=TRUE )
  {
    result <- list()
    
    nGroups <- length( x )
    
    # How many rows to display all legends?
    nRows <- 0
    tmpCol <- nCol
    for ( i in 1:nGroups )
    {
      tmpCol <- nCol
      if ( names(x)[i]=="hcrPars" )
        tmpCol <- 3
      nRows <- nRows + round( length(x[[i]]) / tmpCol + 0.5 )
    }
    # Add a row for each group header.
    if ( byrow )
      nRows <- nRows + (nGroups * 2)
    else
      nRows <- max( sapply( x, length ) ) + 2
    
    # Calculate total height in user coordinates.
    usr <- par ("usr")
    yHeight  <- (usr[4] - usr[3]) * 0.9
    
    # Start with cex=1. 
    myCex <- 1.0
    
    # Character height then add 100% for interspace.
    charHgt  <- strheight( "X128", cex=myCex, font=1 ) * 1.25
    
    # Add space for legend headers.
    yTot <- (nRows * charHgt) + (nGroups * charHgt * 2)
    
    while ( yTot > yHeight )
    {
      myCex  <- myCex - 0.05
      # Character height then add 100% for interspace.
      charHgt  <- strheight( "X128", cex=myCex, font=1 ) * 1.25
      yTot <- (nRows * charHgt) + (nGroups * charHgt * 2)      
    }
    
    # Return vector of columns, cex.
    result$cex       <- myCex
    result$charHgt   <- charHgt
    result$nGroups   <- nGroups
    result$nRows     <- nRows
    result$yTot      <- yTot
    result
  }     # END internal function calcSettings.
  
  #----------------------------------------------------------------------------#
  
  # Open a graphics device.
  .plotStatus( "" )
  nCols <- 1
  titleText <- "Simulation Control Parameters"
  
  # Plot the control parameters specified by type.
  if ( is.null(type) )
  {
    nCols=4
    
    opModPars <- ctlPars[ substring( ctlPars$parameter,1,5 )=="opMod", ]
    opModPars <- paste( substring( opModPars$parameter,7),"=",opModPars$value, sep=" " )      
      
    refPtPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="refPts", ]
    refPtPars    <- paste( substring( refPtPars$parameter,8),"=",
                           round( as.numeric(refPtPars$value), digits=3), sep=" " )            

    dataPars   <- ctlPars[ substring( ctlPars$parameter,1,7 )=="mp$data", ]
    dataPars   <- paste( substring( dataPars$parameter,9 ),"=",dataPars$value, sep=" " )
    dataPars   <- substring( dataPars,1,30 )
      
    assessPars <- ctlPars[ substring( ctlPars$parameter,1,9 )=="mp$assess", ]
    assessPars <- paste( substring( assessPars$parameter,11 ),"=",assessPars$value, sep=" " )
      
    hcrPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="mp$hcr", ]
    hcrPars    <- paste( substring( hcrPars$parameter,8, ),"=",hcrPars$value, sep=" " )

    x <- list( opModPars=opModPars, refPtPars=refPtPars, dataPars=dataPars,
               assessPars=assessPars, hcrPars=hcrPars )
      
    subTitleText <- c( "OM","Ref Pts","Data","Method","HCR" )
    
    result <- calcSettings( x, byrow=TRUE )    
  }
  else if ( type=="opMod" )
  {
    nCols <- 1
      
    opModPars <- ctlPars[ substring( ctlPars$parameter,1,5 )=="opMod", ]
    opModPars <- paste( substring( opModPars$parameter,7),"=",opModPars$value, sep=" " )      
      
    refPtPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="refPts", ]
    refPtPars    <- paste( substring( refPtPars$parameter,8),"=",refPtPars$value, sep=" " )            
      
    x <- list( opModPars=opModPars, refPtPars=refPtPars )
      
    titleText <- "Operating Model Parameters"
    subTitleText <- c( "OM","Ref Pts" )
    
    result <- calcSettings( x, byrow=FALSE )    
  }
  else if ( type=="mp" )
  {
    nCols  <- 1
      
    dataPars   <- ctlPars[ substring( ctlPars$parameter,1,7 )=="mp$data", ]
    dataPars   <- paste( substring( dataPars$parameter,9 ),"=",dataPars$value, sep=" " )
    dataPars   <- substring( dataPars,1,30 )
          
    assessPars <- ctlPars[ substring( ctlPars$parameter,1,9 )=="mp$assess", ]
    assessPars <- paste( substring( assessPars$parameter,11 ),"=",assessPars$value, sep=" " )
      
    hcrPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="mp$hcr", ]
    hcrPars    <- paste( substring( hcrPars$parameter,8, ),"=",hcrPars$value, sep=" " )

    x <- list( dataPars=dataPars, assessPars=assessPars, hcrPars=hcrPars )
      
    titleText    <- "Management Procedure Parameters"
    subTitleText <- c( "Data","Assessement","HCR" )
    
    result <- calcSettings( x, byrow=FALSE )    
  }

  nGroups <- length( x )                  # Number of groups of parameters to show.
  xDelta  <- 1.0 / (nGroups + 0.75)       # Offset between groups.
  
  mtext( side=3, line=0, cex=1.2, titleText )        

  # Dumping all the simulation control parmaters.
  if ( is.null(type) )
  {
    xPos <- 0.5
    yPos <- 1.04

    for ( i in 1:nGroups )
    {
      if ( names(x)[i]=="hcrPars" )
        nCols <- 3
      else
        nCols <- 4
        
      nPars <- length( x[[i]] )
      nRows <- trunc( (nPars / nCols) + 0.5 )
      
      temp <- legend( x=xPos, y=yPos, xjust=0.5, yjust=1, ncol=nCols,
              legend=x[[i]], bty="n", title=subTitleText[i], adj=c(0,1),
              title.col="blue", text.font=1, cex=result$cex )
              
      yBottom <- temp$rect$top-temp$rect$h
    
      yPos <- yBottom
    }
  }
  else
  {
    xPos <- 0.0
    yPos <- 1.04  
  
    for ( i in 1:nGroups )
    {
      xPos <- xPos + xDelta
      legend( x=xPos, y=yPos, xjust=0.5, yjust=1, ncol=nCols,
              legend=x[[i]], bty="n", title=subTitleText[i], title.adj=0.5,
              title.col="blue", cex=result$cex )
    }
  }
}     # END function .plotCtlPars


.plotDesign <- function( obj, iObj=1, nObj=1, gfx=list( annotate=TRUE,
                         doLegend=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  addRects <- function( xl, yb, xr, yt, clr="black", dens=-1, offset=0.5 )
  {

    rect( xl-offset, yb, xr-offset, yt, border=TRUE, col=clr, density=dens )
  }
  
  symex <- 1
  tmp <- .calcTimes( obj )
  
  # Need to modify to loop over a list as an argument.
  nT  <- obj$opMod$nT
  tMP <- obj$opMod$tMP

  xPos    <- c( 1:nT )
  xOffset <- 0.1
  
  yLabel  <- paste( obj$gui$scenarioLabel,"-",obj$gui$mpLabel, sep="" )
  yLabel  <- substring( yLabel, 1, 15 )

  idxCatch  <- 1
  idxAges   <- idxCatch  + nObj + 1
  idxIndex  <- idxAges   + nObj + 1
  idxAgesS  <- idxIndex  + nObj + 1
  idxMethod <- idxAgesS  + nObj + 1
  idxCtlPts <- idxMethod + nObj + 1
  
  idx <- c( idxCatch,idxAges,idxIndex,idxAgesS,idxMethod,idxCtlPts )
  names( idx ) <- c( "Catch","Fishery Ages","Survey Index","Survey Ages","Method","Ctl Pts" )

  idx2 <- idx + iObj

  nCats  <- length( idx )
  
  nLines <- (nObj * nCats) + nCats

  xLim <- range( xPos )
  yLim <- c( 1,nLines )
  
  if ( iObj == 1 )
    plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="", ylim=rev(yLim) )

  cxy <- par( "cxy" )
  yOffset <- cxy[2] / 3.
  usr <- par( "usr" )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  # Plot catch indictor.
  x <- xPos
  y <- rep( idx2[ "Catch" ], nT )
  
  #points( x, y, cex=symex, pch=22, bg="black" )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 ) 

  text( usr[1], y[1], yLabel, pos=2, xpd=TRUE )

  # Plot symbol at times where index value is obtained for Period1 and Period2.

  x <- tmp$per1Survey
  if ( x[1] > 0 )
  {
    y <- tmp$surveyOn[ x ]
    y <- ifelse( y, idx2[ "Survey Index" ], NA )
  
    #points( x, y, cex=symex, pch=22, bg="gray" )
    addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )
  }

  x <- tmp$per2Survey
  
  if ( x[1] > 0 )
  {
    y <- tmp$surveyOn[ x ]
    y <- ifelse( y, idx2[ "Survey Index" ], NA )
    #points( x, y, cex=symex, pch=22, bg="green" )
    addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="blue", dens=-1 )

    text( usr[1], y[1], yLabel, pos=2, xpd=TRUE )
  }

  # Plot symbol at times where assessment is obtained for Period1 and Period2.

  x <- tmp$per1Method
  y <- tmp$methodOn[ x ]
  y <- ifelse( y, idx2[ "Method" ], NA )
  #points( x, y, cex=symex, pch=22, bg="gray" )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )  

  x <- tmp$per2Method
  y <- tmp$methodOn[ x ]
  y <- ifelse( y, idx2[ "Method" ], NA )
  #points( x, y, cex=symex, pch=22, bg="green" )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="green", dens=-1 )  

  if ( gfx$doLegend )
  {
    methodId <- obj$mp$assess$methodId
    label    <- .METHODLAB[ methodId ]
    
    if ( methodId == .CAAMOD )
    { 
      if ( obj$mp$assess$caaAges )
        label <- paste( label, "(Ages On" )
      else
        label <- paste( label, "(Ages Off" )
        
      if ( obj$mp$assess$caaSurveyRel )
        label <- paste( label,", Rel. Index)" )
      else
        label <- paste( label,", Abs. Index)" )
    }
    
    if ( methodId == .DDMOD )
    {
      if ( obj$mp$assess$ddSurveyRel )
        label <- paste( label,"(Rel. Index)" )
      else
        label <- paste( label,"(Abs. Index)" )    
    }    
    
    if ( methodId == .PMOD )
    {
      if ( obj$mp$assess$spSurveyRel )
        label <- paste( label,"(Rel. Index)" )
      else
        label <- paste( label,"(Abs. Index)" )    
    }
      
    hcrType <- obj$mp$hcr$hcrType
    label   <- paste( label, hcrType )
    
    if ( obj$mp$hcr$forecast )
      label <- paste( label, "(Interim catch=model)" )
    else
      label <- paste( label, "(Interim catch=constant)" )
    
    text( 0, y, adj=0, cex=0.8, label )
  }

  text( usr[1], y[1], yLabel, pos=2, xpd=TRUE )

  # Plot the fishery ageing data frequency.

  x <- tmp$per1Ages
  if ( x[1] > 0 )
  {
    y <- tmp$agesOn[ x ]
    y <- ifelse( y, idx2[ "Fishery Ages" ], NA )
    #points( x, y, cex=symex, pch=22, bg="gray" )
    addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )
  }

  x <- tmp$per2Ages
  
  if ( x[1] > 0 )
  {
    y <- tmp$agesOn[ x ]
    y <- ifelse( y, idx2[ "Fishery Ages" ], NA )
    #points( x, y, cex=symex, pch=22, bg="green" )
    addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="gray", dens=-1 )
    text( usr[1], y[1], yLabel, pos=2, xpd=TRUE )
  }
  
  # Plot the survey fishery ageing data frequency.

  x <- tmp$per1AgesS
  
  if ( x[1] > 0 )
  {
    y <- tmp$agesOnS[ x ]
    y <- ifelse( y, idx2[ "Survey Ages" ], NA )
    #points( x, y, cex=symex, pch=22, bg="gray" )
    addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )
  }

  x <- tmp$per2AgesS
  
  if ( x[1] > 0 )
  {
    y <- tmp$agesOnS[ x ]
    y <- ifelse( y, idx2[ "Survey Ages" ], NA )
    #points( x, y, cex=symex, pch=22, bg="green" )
    addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="gray", dens=-1 )
    text( usr[1], y[1], yLabel, pos=2, xpd=TRUE )      
  }

  # Plot the control point frequency.

  x <- xPos[ tmp$ctlPtsOn ]
  y <- rep( idx2["Ctl Pts"], length(xPos) )[ tmp$ctlPtsOn ]
  
  #addRects( min( x ), y-yOffset, max(x), y+yOffset, dens=tmp$ctlPtsOn*-1 )
  #addRects( x, y-yOffset, x, y+yOffset )
  
  #points( x, y, cex=symex, pch=22, bg="black" )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="black", dens=-1 )
  
  if ( gfx$doLegend )
  {
    if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
      label <- "Bmsy Base"
    else
      label <- "B0 Base"
      
    if ( obj$mp$hcr$hcrType == "variableF" )
    {
      label <- paste( label,
        paste( "(LB=", obj$mp$hcr$lowerBoundMult,
               " UB=",obj$mp$hcr$upperBoundMult,")", sep="" ) )
    }
    
    text( 0, y, adj=0, cex=0.8, label )    
  }

  text( usr[1], y[1], yLabel, pos=2, xpd=TRUE )

  if ( iObj == 1 )
  {  
    .addXaxis( xLim=xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS2, at=idx, labels=names(idx), las=.YAXISLAS )
    box()

    mtext( side=1, line=.OUTLINE, cex=1.2, outer=TRUE, "Year" )
  }
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=T, "Management Procedure Design" )
  }
  
  return( invisible() )
}     # END function .plotDesign


.plotStatus <- function( plotLabel="No Data Available" )
{
  par( oma=c(1,1,1,1), mar=c(0,0,0,0), mfrow=c(1,1) )
  plot( c(0,1),c(0,1), type="n", axes=FALSE, xlab="", ylab="" )
  box()

  panLab( 0.5,0.5, cex=1.5, plotLabel )
  return( invisible() )
}

.plotTrackOrder <- function( trackObj, nRow=4, nCol=2 )
{
  nSims   <- .getNumSims( trackObj )
  nPanels <- nRow * nCol

  for ( i in 1:min( c(nSims,nPanels) ) )
  {
    label <- paste( "Scenario: ",  trackObj$scenario[i], "\n",
                    "Mgmt Proc: ", trackObj$mp[i], sep="" )
    plot( c(0,1),c(0,1), type="n", axes=FALSE, xlab="", ylab="" )
    
    panLab( 0.05, 0.9, adj=0, paste( "Rank = ", trackObj$rank[i], sep="" ), cex=1.6 )
    panLab( 0.70, 0.9, adj=0, paste( "Group = ", trackObj$group[i], sep="" ), cex=1.6 )
    panLab( 0.5, 0.5, label, cex=1.4 )
    panLab( 0.5, 0.2, paste( "Folder:\n", trackObj$simFolder[i], sep="" ), cex=1.4 )
    box()
  }
}     # END function .plotTrackOrder

#------------------------------------------------------------------------------#
# Reference Point Plotting Functions for ALL GUIs                              #
#------------------------------------------------------------------------------#

.addRefPointsLegend <- function( x=0.5, y=0.5, checked )
{
  # Is there anything in the legend, or is nothing checked?
  if ( sum(checked)==0 )
  {
    cat( "\nmseRrefPoints (.addRefPointsLegend): Legend vector of length 0.\n" )
    return()
  }
  
  labels <- c( "F0","F0.1","Fmsy","Fspr40","Fmax","Fcrash" )
  names(labels) <- c("F0","F01","Fmsy","F40","Fmax","Fcra" )
  
  pchVec <- rep( 21,length(labels) )
  names(pchVec) <- names(labels)
  
  ptBg   <- c(.F0COL,.F01COL,.FmsyCOL,.FsprCOL,.FmaxCOL,.FcraCOL)
  names(ptBg) <- names(labels)

  # Now show only those reference points that are checked in the GUI.
  # This is tricky, we want the reference points where checked=TRUE, but
  # we have to get the names(checked) where checked=TRUE in case the order
  # of the reference points in the vectors differ.
  
  labels <- labels[ names(checked)[checked] ]
  pchVec <- pchVec[ names(checked)[checked] ]
  ptBg   <- ptBg[ names(checked)[checked] ]  
      
  # Add a legend.
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=.CEXSYM24,
             cex=.CEXLEG2, bty=.LEGBTY )
  
  return( invisible() )
}     # END function addRefPointsLegend

# .plotSimRefPoints    (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotSimRefPoints <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  # guiSim has no plotting limits at this time.
  
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )

  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Reference Points" )
}     # END function .plotSimRefPoints


.plotRecSSB <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$ssb) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$ssb)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$recruits) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$recruits)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList  
  
    lines( .posVal(refs$ssb), .posVal(refs$recruits), lwd=.LWD2 )

    # Adding steepness lines at B20=0.2*B0, R20, 
    # and steepness R20/B0.
    lines( c(refs$B20,refs$B20), c(0,        refs$R20), lty=.LWD2 )
    lines( c(0,       refs$B20), c(refs$R20, refs$R20), lty=.LWD2 )
  
    lines( c(refs$B0,refs$B0), c(0,       refs$R0), lty=.LWD2 )
    lines( c(0,      refs$B0), c(refs$R0, refs$R0), lty=.LWD2 )

    # Adding steepness label
    h <- round( refs$R20/refs$R0, digits=2 )
    xPos <- refs$B20
    yPos <- refs$R20 * 0.8
    text( xPos, yPos, cex=.CEXANNO2, pos=4, paste( "h=",refs$rSteepness,sep="") )
    
    if ( checked["F0"] )  
      points( refs$ssbF0,   refs$recruitsF0,   cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$ssbF01,  refs$recruitsF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$ssbFcra, refs$recruitsFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$ssbFmax, refs$recruitsFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
    if ( checked["F40"] )
      points( refs$ssbF40,  refs$recruitsF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
    if ( checked["Fmsy"] )
    {
      points( refs$ssbFmsy, refs$recruitsFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$ssbFmsy, refs$recruitsFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Spawning Biomass" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Recruits" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}     # END function .plotRecSSB    


# Spawning biomass as a function of F.
# Can pass a list of blobs.
# I think idNum is a label.
# gfx is graphics parameter list for plotting.
.plotSsbF <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$Fcra) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$ssb) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$ssb)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList
    
    lines( .posVal(refs$F), .posVal(refs$ssb), lwd=.LWD2 )
    
    if ( checked["F0"] )  
      points( refs$F0, refs$ssbF0,     cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$ssbF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$ssbFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$ssbFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
    if ( checked["F40"] )
      points( refs$F40, refs$ssbF40,   cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$ssbFmsy, cex=.CEXSYM24, bg=.FmsyCOL, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$ssbFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Spawning Biomass" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
  return( invisible() )
}    # END function .plotSsbF


.plotSsbPerRecF <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
 #   if ( n==1 )
 #     xLim <- c( 0, max(obj$refPtList$Fcra) )
 #   else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$ssbpr) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$ssbpr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList  
    
    lines( .posVal(refs$F), .posVal(refs$ssbpr), lwd=2 )
    
    if ( checked["F0"] )  
      points( refs$F0, refs$ssbprF0,     cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$ssbprF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$ssbprFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$ssbprFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
   if ( checked["F40"] )
      points( refs$F40,  refs$ssbprF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$ssbprFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$ssbprFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "SSBPR" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
    
  return( invisible() )
}     # END function .plotSsbPerRecF    


.plotYieldF <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  #refPts <- obj$ctlList$refPts
  refPts <- obj$refPtList
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$Fcra) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$yield) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$yield)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList    
  
    lines( .posVal(refs$F), .posVal(refs$yield), lwd=2 )
    
    if ( checked["F0"] )  
      points( refs$F0, refs$yieldF0,     cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$yieldF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$yieldFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$yieldFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )

    if ( checked["F40"] )
      points( refs$F40,  refs$yieldF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$yieldFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$yieldFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}     # END function .plotYieldF    


.plotYieldSSB <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$ssb) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$ssb)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
 #   if ( n==1 )
 #     yLim <- c( 0, max(obj$refPtList$yield) )
 #   else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$yield)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList
        
    lines( .posVal(refs$ssb), .posVal(refs$yield), lwd=2 )
    
    if ( checked["F0"] )  
      points( refs$ssbF0,   refs$yieldF0,   cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$ssbF01,  refs$yieldF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$ssbFcra, refs$yieldFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$ssbFmax, refs$yieldFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
    if ( checked["F40"] )
      points( refs$ssbF40,  refs$yieldF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$ssbFmsy, refs$yieldFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$ssbFmsy, refs$yieldFmsy, idNum[i], .CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "SSB" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}     # END function .plotYieldSSB      


.plotYprF <- function( obj, idNum=NULL, gfx )
{
  annotate <- gfx$annotate
  checked  <- gfx$checked
  
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$Fcra) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$ypr) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$ypr)) )
  }

  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList
        
    lines( .posVal(refs$F), .posVal(refs$ypr), lwd=2 )

    if ( checked["F0"] )  
      points( refs$F0,   refs$yprF0,   cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$yprF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$yprFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$yprFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )

    if ( checked["F40"] )
      points( refs$F40,  refs$yprF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$yprFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$yprFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "YPR" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.90, checked )
}     # END function .plotYprF    


# .plotRefPoints (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPoints <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=xLim,yLim=yLim ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSBR,maxSSBR) ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minYield,maxYield) ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSB,maxSSB) ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minRec,maxRec) ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minYield,maxYield) ) )

  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Reference Points" )
}     # END function .plotRefPoints

#------------------------------------------------------------------------------#
#-- (B) Plotting function for guiSim                                           #
#------------------------------------------------------------------------------#

# .plotLifeHist  (Plot the operating model life history parameters)
# Purpose:      Plot the life history schedules specified by the operating model.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotLifeHist <- function( obj, gfx=c( annotate=TRUE, doLegend=TRUE,
                           xLim=xlim, yLim=ylim ) )
{
  par( oma=c(2,1,2,1), mar=c(3,4,1,1), mfrow=c(3,2) )

  lh <- calcRefPoints( obj, rpList=list( FALL=TRUE ) )

  # Numbers at age in year 1.
  .plotNumAtAge( lh, gfx=gfx )

   # Weight versus length.  
  .plotWgtLen( lh, gfx=gfx )

  # Length-at-age.
  .plotLenAtAge( lh, gfx=gfx )
  
  # Maturity at age.
  .plotMatAtAge( lh, gfx=gfx )

  # Weight at age.
  .plotWgtAtAge( lh, gfx=gfx )

  # Selectivity at age.
  .plotSelAtAge( lh, type="Fishery", gfx=gfx )

  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Life History" )
}     # END function .plotLifeHist


.plotLenAtAge <- function( obj, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )
    yLim <- c( 0,max(obj$lenAge) )
  
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$lenAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, .AgeLAB )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( "Length-at-age", .LenUNIT ) )
}     # END function .plotLenAtAge


.plotMatAtAge <- function( obj, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(obj$matAge) )
    
  # Maturity at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$matAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )

  if ( gfx$annotate )
  {  
    A50 <- max( obj$ages[ obj$matAge <= 0.5001 ] )
    A95 <- max( obj$ages[ obj$matAge <= 0.9501 ] )
    segments( A50, 0.0,  A50, 0.5,  lty=2, lwd=2 )
    segments( 0.0, 0.5,  A50, 0.5,  lty=2, lwd=2 )
    segments( A95, 0.0,  A95, 0.95, lty=2, lwd=2 )
    segments( 0.0, 0.95, A95, 0.95, lty=2, lwd=2 )
  }
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, .AgeLAB )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Maturity-at-age" )
}     # END function .plotMatAtAge


.plotNumAtAge <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  yLim <- c( 0,max(obj$numAgeYr1 ) )
  yLim[2] <- round( yLim[2],digits=1 )
 
  # Numbers at age in year 1.
  barplot( obj$numAgeYr1,names.arg=obj$ages, axes=FALSE, xlab="", ylab="",
           ylim=yLim )
  axis (side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )  
  box()

  if ( gfx$doLegend )
  {    
    legend( "top",legend=c(paste("Max:",round(max(obj$numAgeYr1),digits=4)),
            paste("Min:", round(min(obj$numAgeYr1), digits=4))),
            cex=.CEXLEG2, bty="n")
  }
            
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, .AgeLAB )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Number at Age-1" )
}     # END function .plotNumAtAge


.plotWgtAtAge <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- c( 0,max(obj$ages) )
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(obj$wtAge) )
  
  # Weight at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$wtAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, paste( .AgeLAB, .AgeUNIT ) )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( "Weight-at-age", .WgtUNIT) )
}     # END function .plotWgtAtAge


.plotWgtLen <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0, max( obj$lenAge ) )

  if ( is.null(yLim) )
    yLim <- c( 0, max( obj$wtAge ) )
  
  # Weight against length.
  plot( obj$lenAge, obj$wtAge, type="n", axes=FALSE,
        xlab="",xlim=xLim,ylab="",ylim=yLim )
  lines( obj$lenAge, obj$wtAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, paste( .LenLAB, .LenUNIT ) )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( .WgtLAB, .WgtUNIT ) )    
}     # END function .plotWgtLen


# .plotSelAge  (Plot the operating model selectivity at age)
# Purpose:      Plot the life history schedules specified by the operating model.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotSelAtAge <- function( obj, type="Fishery", gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( type=="Fishery" )
    selAge <- obj$selAge
  else
    selAge <- obj$selAgeS
  
  if ( is.null(xLim) )
    xLim <- c( 0, max( obj$ages ) )

  if ( is.null(yLim) )
    yLim <- c( 0, max(selAge) )
    
  # Selectivity at age.
  plot( obj$ages, selAge, type="n", axes=FALSE,
    xlab="", ylab="", xlim=xLim, ylim=yLim )
  lines( obj$ages, selAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )

  #age50 <- max( obj$ages[ obj$selAge <= 0.5001 ] )
  #age95 <- max( obj$ages[ obj$selAge <= 0.9501 ] )
  
  #segments( age50, 0, age50, 0.5,  lty=2, lwd=2 )
  #segments( 0,   0.5, age50, 0.5,  lty=2, lwd=2 )
  #segments( age95, 0, age95, 0.95, lty=2, lwd=2 )
  #segments( 0,  0.95, age95, 0.95, lty=2, lwd=2 )
  
  abline( h=0.5,  lty=2, lwd=1 )
  abline( h=0.95, lty=2, lwd=1 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, "Age" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( type,"selectivity-at-age", sep=" " ) )

  result <- cbind( Age=obj$ages, Selectivity=selAge )
  result
}     # END function .plotSelAtAge

######################################################################
# .plotMPSelAge  (Plot the mp estimated selectivity at age)
# Purpose:      Plot the life history schedules specified by the operating model.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       R. FORREST (based on .plotSelAge)
.plotMPSelAtAge <- function(obj, type="Fishery", gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
   maxAge <- obj$ctlList$opMod$nAges
   nY <- length(obj$mp$assess$mpdPars$aSel50)
    
   if ( type=="Fishery" ){
   MPsel501 <- obj$mp$assess$mpdPars$aSel50[1]
    MPsel951 <- obj$mp$assess$mpdPars$aSel95[1]
   MPsel50t <- obj$mp$assess$mpdPars$aSel50[nY]
    MPsel95t <- obj$mp$assess$mpdPars$aSel95[nY]
    OMsel50 <- obj$ctlList$opMod$aSel50
    OMsel95 <- obj$ctlList$opMod$aSel95
    MPsel1 <- .calcLogistic(maxAge,MPsel501, MPsel951)#first projection year
    MPselt <- .calcLogistic(maxAge,MPsel50t, MPsel95t)#terminal projection year
    OMsel <- .calcLogistic(maxAge,OMsel50, OMsel95) }
  else{
     MPsel501 <- obj$mp$assess$mpdPars$aSelS50[1]
    MPsel951 <- obj$mp$assess$mpdPars$aSelS95[1]
    MPsel50t <- obj$mp$assess$mpdPars$aSelS50[nY]
    MPsel95t <- obj$mp$assess$mpdPars$aSelS95[nY]
    OMsel50 <- obj$ctlList$opMod$aSelS50
    OMsel95 <- obj$ctlList$opMod$aSelS95
     MPsel1 <- .calcLogistic(maxAge,MPsel501, MPsel951)#first projection year
     MPselt <- .calcLogistic(maxAge,MPsel50t, MPsel95t)#terminal projection year
    OMsel <- .calcLogistic(maxAge,OMsel50, OMsel95) }
  
  if ( is.null(xLim) )
    xLim <- c( 0, maxAge )

  if ( is.null(yLim) )
    yLim <- c( 0, 1 )
    
    # Selectivity at age.
  plot( 1:maxAge, OMsel, type="n", axes=FALSE, xlab="", ylab="", xlim=xLim, ylim=yLim )
  lines( 1:maxAge, OMsel, col="black", lty=2, lwd=2 )
  lines( 1:maxAge, MPsel1, col="red", lty=1, lwd=2 )
  lines( 1:maxAge, MPselt, col="blue", lty=1, lwd=2 )
  abline( h=0.5,  lty=2, lwd=1 )
  abline( h=0.95, lty=2, lwd=1 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, "Age" )
 mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( type,"selectivity-at-age", sep=" " ) )
 legend("bottomright", legend=c("Operating Model","Assessment: First projection year", " Assessment: Terminal projection year"), lty=c(2,1,1), lwd=2, col=c("black", "red", "blue"), cex=1.2, bty="n")

  result <- cbind( Age=1:maxAge, OMSelectivity=OMsel, EstSelectivityProj1=MPsel1, EstSelectivityProjt=MPselt)
  result
}     # END function .plotMPSelAtAge

######################################################################

.plotMPindexPars <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE,
                               xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  nT           <- obj$opMod$nT
  tMP          <- obj$opMod$tMP

  val <- .calcTimes( obj )
  idx1 <- val$per1Survey
  idx2 <- val$per2Survey  
  t1Survey <- val$t1Survey
  t2Survey <- val$t2Survey
  
  # Generate example observation error CVs for the survey periods
  surveyError <- rep( 0, nT )
  # Draw CVs from (inverse gamma?) statistical distribution between Min and Max CV.
  
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
  surveyError[t2Survey:nT] <- rinvgamma( n=(nT - t2Survey + 1), shape=alpha, scale=beta )
  
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
  
  # Normally-distributed survey errors ARK WHY THIS STEP?
  epsilont <- surveyError*rnorm(nT,0,1) - surveyError*surveyError/2.

  # Assign to "errors" object within operating model list.
  surveyCV <- surveyError
  
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c(0,nT)
    
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 0,max(surveyCV,na.rm=TRUE) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( v=tMP, col=.tMPCOL,, lty=.tMPLTY, lwd=.tMPLWD )
  
  segments( t1Survey, tauSurvey1Mean, t2Survey-1, tauSurvey1Mean, col="black", lty=1, lwd=3 )
  segments( t2Survey, tauSurvey2Mean, nT,         tauSurvey2Mean, col="black", lty=1, lwd=3 )
   
  usr <- par( "usr" )
  yDelta <- usr[4]-usr[3]
  
  points( idx1, surveyCV[ idx1 ], cex=.ItCEX, pch=.ItPCH, bg=.ItBG )
  points( idx2, surveyCV[ idx2 ], cex=.ItCEX, pch=.ItPCH, bg=.ItBG )
    
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE3, cex=.CEXLAB2, "Survey CV" )
  
  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$mp$data$tauSurvey2SD,   digits=2 )
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( paste( "Period 1 CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" )),
      col=c(.ItCOL, .ItCOL),
      cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }

}     # END function .plotMPindexPars

#------------------------------------------------------------------------------#
#-- (C) Plotting function for guiView                                          #
#------------------------------------------------------------------------------#

.plotAgeBubbles <- function( oat, pat, pltTitle="Proportions At Age", pwr=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 1,dim(oat)[2] )
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 1,( dim(oat)[1]+2 ) )

  par( oma=c(3.5,4,1,1), mar=c(2,2,1,1), mfrow=c(3,1 ) )

  # Observed age proportions.
  plotBubbles( oat, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n" )
  axis( side=1 )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Observed" )

  # Predicted age proportions.
  plotBubbles( pat, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n" )
  axis( side=1 )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Predicted" )

  # Residuals.
  ageResids <- oat - pat
  plotBubbles( ageResids, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n" )
  axis( side=1 )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Residuals" )

  mtext( side=1, line=0, cex=.CEXLAB2, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, outer=TRUE, "Age Class" )
}     # END function .plotAgeBubbles


.plotAgeBubblesOM <- function( uat, pltTitle="Proportions At Age", pwr=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 1,dim(uat)[2] )
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 1,( dim(uat)[1]+2 ) )

  # Observed age proportions.
  plotBubbles( uat, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n",
               cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=1, cex.axis=.CEXLAB2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Age Class" )
  
  if ( gfx$annotate )
    mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, pltTitle )
}     # END function .plotAgeBubblesOM


.plotAgeFreq <- function( patBars, patLines=NULL, pltTitle="Proportions At Age",
  gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE,
            mfCol=c(5,4), xLim=NULL, yLim=NULL, yrs=FALSE ) ) 
{
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( !is.null(patBars) )
    ageDim <- dim(patBars)
  yearClasses <- c(1:ageDim[1])

  if ( is.null(xLim) )
    xLim <- c( 1,ageDim[1] )

  if ( is.null(yLim) )
  {
    if ( is.null(patLines) )
      yLim <- c( 0,max(patBars,na.rm=TRUE) )
    else
      yLim <- c( 0,max(c(patBars,patLines), na.rm=TRUE ) )
  }
  
  nYears  <- dim( patBars )[2]
  years <- c(1:nYears)

  myMar <- c(0.5,0.5,1,0)
  myOma <- c(4,5,4,3)    

  if ( length(dev.list()) > 1 )
    graphics.off()
    
  par( oma=myOma, mar=myMar, mfcol=c(5,4) )

  for ( t in years )
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Observed age proportions - these are plotted as vertical lines..
    if ( !is.null(patBars) )
      lines( yearClasses, y=patBars[,t], type="h", lwd=.LWD2 )

    # Predicted age proportions - these are plotted as red a line.
    if ( !is.null(patLines) )
      lines( yearClasses, y=patLines[,t], type="l", lwd=.LWD2, col="red" )

    if ( gfx$yrs )
      panLab(0.2,0.9, cex=.CEXLAB4, t + .INITYEAR - 1 )
    else
      panLab( 0.1, 0.9, cex=.CEXLAB4, t )

    mfg <- par( "mfg" )

    # Row one.
    if ( mfg[1]==1 && mfg[2]%%2==0 )
      axis( side=3, labels=FALSE )

    if ( mfg[1]==1 && mfg[2]%%2!=0 )
      axis( side=3 )

    # Column one.
    if ( mfg[2]==1 && mfg[1]%%2==0 )
      axis( side=2, las=2 )

    if ( mfg[2]==1 && mfg[1]%%2!=0 )
      axis( side=2, labels=FALSE )

    # Last row.
    if ( mfg[1]==mfg[3] && mfg[2]%%2==0 )
      axis( side=1 )

    if ( mfg[1]==mfg[3] && mfg[2]%%2!=0 )
      axis( side=1, labels=FALSE )

    # Last column.
    if ( mfg[2]==mfg[4] && mfg[1]%%2==0 )
      axis( side=4, labels=FALSE )
  
    if ( mfg[2]==mfg[4] && mfg[1]%%2!=0 )
      axis( side=4, las=2 )

    box()
    
    mfg <- par( "mfg" )
    if ( mfg[1]*mfg[2]==20 )
    {
      mtext( side=1, line=.OUTLINE2, cex=.CEXLAB2, outer=TRUE, "Age Class" )
      mtext( side=2, line=.OUTLINE3, cex=.CEXLAB2, outer=TRUE, "Age Proportions" )
      
      if ( gfx$annotate )
        mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, pltTitle )

      dev.new()
      par( oma=myOma, mar=myMar, mfcol=c(5,4) )
    }
  }

  mtext( side=1, line=.OUTLINE2, cex=.CEXLAB2, outer=TRUE, "Age Class" )
  mtext( side=2, line=.OUTLINE3, cex=.CEXLAB2, outer=TRUE, "Age Proportions" )
   
  if ( gfx$annotate )
    mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, pltTitle )

}     # END function .plotAgeFreq


# .plotBt  (plots time trajectory of total biomass and spawning biomass)
# Purpose:      Plots both total biomass and spawning biomass over
#                   time for a single replicate, irep, from
#                   simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotBt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  #Bexp <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]
  Bexp <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]  
  Bt   <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  nT   <- length( Bt )
  tMP  <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max( c(Bexp,Bt) ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Bexp, lty=.BexpLTY, lwd=.BexpLWD, col=.BexpCOL )
  lines( c(1:nT), Bt,   lty=.BtLTY,   lwd=.BtLWD,   col=.BtCOL )
  abline( h=obj$refPtList$ssbFmsy, lty=.BmsyLTY, col=.BmsyCOL, lwd=.BmsyLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  .addXaxis( xLim, initYear=.INITYEAR, gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4,
         paste( .BtLAB," (",.BtUNIT,")", sep="" ) )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.2, legTxt=c( .BexpLAB, .BtLAB, .BmsyLAB ),
            lty=c(.BexpLTY,.BtLTY,.BmsyLTY),lwd=c(.BexpLWD,.BtLWD,.BmsyLWD),
            col=c(.BexpCOL,.BtCOL,.BmsyCOL), bg="white", bty=.LEGBTY, cex=.CEXLEG2 )
  }
  
  if ( gfx$annotate )
    mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, "Biomass" )
}     # END function .plotBt


#.plotBtDtFt    (plot spawning biomass, catch, & fishing mort. vs. time)
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual catch biomass, and the third
#                showing fishing mortality.  All values are from operating model.
# Notes:       Labelled "SSB Cat F" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotBtDtFt <- function( obj, iSim=1, iRep=1, gfx=list( animate=TRUE, annotate=TRUE,
                         doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
                         useYears=FALSE ) )
{
  # Stock status zone boundaries on SSB scale.
  #Bmsy      <- obj$pars$Fmsy
  #zoneLimit <- obj$pars$limitBoundMult * Bmsy
  #zoneUpper <- obj$pars$upperBoundMult * Bmsy

  Bt  <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Dt  <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  Ft  <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]
  It  <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  It  <- It / obj$ctlList$opMod$q
  nT  <- length( Bt )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim

  # X-axis limits (same for all three panels)
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  nTimes <- 1
  if ( gfx$animate )
  {
    ani.options( interval=0.05 )
    nTimes <- nT
  }

  idx <- c(1:nT)

  for ( i in seq_len(nTimes) )
  {
    if ( gfx$animate )
    {
      idx <- c(1:i)
      dev.hold()
    }

    # Panel 1: Plot biomass and survey index.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[1,]
    else
    {
      yLim <- c(0,max(c(Bt,It), na.rm=T))
    }
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
    lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
    points( idx, It[idx], bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
    points( idx, It[idx] )
  
    abline( h=obj$refPtList$ssbFmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=4, labels=FALSE )
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Biomass and Index" )
    mtext( side=1, line=.INLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Bmsy"), cex=.CEXLEG2,
              col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD, bg="white", bty=.LEGBTY  )
    }

    # Panel 2: Plot catch.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[2,]
    else
    {
      yLim <- c(0,max(Dt))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( idx, Dt[idx], col=.DtCOL, lty=.DtLTY,  lwd=.DtLWD )
    points( idx, Dt[idx], bg=.DtBG,    cex=.DtCEX, pch=.DtPCH )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )
    
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .DtLAB, " (",.DtUNIT,")", sep="" ) )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("MSY"), cex=.CEXLEG2,
        bg="white", col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD, bty=.LEGBTY )
    }

    # Panel 3: Plot fishing mortality.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[3,]
    else
    {
      yLim <- c(0,max(Ft))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( idx, Ft[idx], lty=.FtLTY,  lwd=.FtLWD, col=.FtCOL )
    abline( h=obj$refPtList$Fmsy, lty=.FmsyLTY, col=.FmsyCOL, lwd=.FmsyLWD )
    abline( h=obj$refPtList$Fcra, lty=.FcraLTY, col=.FcraCOL, lwd=.FcraLWD )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .FtLAB, .FtUNIT ) )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Fmsy","Fcrash"), cex=.CEXLEG2,
                col=c(.FmsyCOL,.FcraCOL), lty=c(.FmsyLTY,.FcraLTY),
                lwd=c( .FmsyLWD, .FcraLWD ), bg="white", bty=.LEGBTY )
    }
    
    if ( gfx$animate )
      ani.pause()
  }
}     # END function .plotBtDtFt


#.plotBtDtRt    (plot spawning biomass, catch, & recruitment. vs. time)
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual catch biomass, and the third
#                showing annual recruits.  All values are from operating model.
# Notes:       Labelled "SSB Cat R" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotBtDtRt <- function( obj, iSim=1, iRep=1, gfx=list( animate=FALSE, annotate=TRUE,
                         doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
                         useYears=FALSE ) )
{
  Bt  <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Dt  <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  Rt  <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  It  <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  It  <- It / obj$ctlList$opMod$q
  nT  <- length( Bt )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim

  # X-axis limits (same for all three panels)
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  nTimes <- 1
  if ( gfx$animate )
  {
    ani.options( interval=0.05 )
    nTimes <- nT
  }

  idx <- c(1:nT)

  for ( i in seq_len(nTimes) )
  {
    if ( gfx$animate )
    {
      idx <- c(1:i)
      dev.hold()
    }

    # Panel 1: Plot biomass and survey index.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[1,]
    else
    {
      yLim <- c(0,max(c(Bt,It), na.rm=T))
    }
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
    lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
    points( idx, It[idx], bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
    points( idx, It[idx] )
  
    abline( h=obj$refPtList$ssbFmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=4, labels=FALSE )
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Biomass and Survey Index" )
    mtext( side=1, line=.INLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Bmsy"), cex=.CEXLEG2,
              col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD, bg="white", bty=.LEGBTY  )
    }

    # Panel 2: Plot catch.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[2,]
    else
    {
      yLim <- c(0,max(Dt))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( idx, Dt[idx], col=.DtCOL, lty=.DtLTY,  lwd=.DtLWD )
    points( idx, Dt[idx], bg=.DtBG,    cex=.DtCEX, pch=.DtPCH )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )
    
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .DtLAB, " (",.DtUNIT,")", sep="" ) )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("MSY"), cex=.CEXLEG2,
        bg="white", col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD, bty=.LEGBTY )
    }

    # Panel 3: Plot recruitment.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[3,]
    else
    {
      yLim <- c(0,max(Rt))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    lines( idx, Rt[idx], lty=.RtLTY, lwd=.RtLWD )
    points( idx, Rt[idx], bg=.RtBG, cex=.RtCEX, pch=.RtPCH )  
    
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .RtLAB, .RtUNIT ) )

    if ( gfx$doLegend )
    {
    }
    
    if ( gfx$animate )
      ani.pause()
  }
}     # END function .plotBtDtRt


#.plotDt    (plot catch biomass vs. time)
# Purpose:     Plots true annual catch biomass from operating model
# Notes:       Labelled "Catch biomass" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotDt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  Dt  <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  nT  <- length( Dt )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null( xLim ) )
    xLim <- c( 1,nT )
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )
    
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,max(Dt) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Dt, col=.DtCOL, lty=.DtLTY, lwd=.DtLWD )
  points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX, pch=.DtPCH )
  
  abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=2.5, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=2.5, cex=.CEXLAB4, paste("Catch"," (",.DtUNIT,")", sep="") )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Catch Biomass" )
    
  if ( gfx$doLegend )
    panLegend( 0.05,0.95, legTxt=c( "MSY" ), cex=.CEXLEG2,
               col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD, bg="white", bty=.LEGBTY )
  return( invisible() )
}     # END function .plotDt


#.plotFt      (plot fishing mortality vs. time)
# Purpose:   Plots true annual fishing mortality from operating model over time
# Notes:       Labelled "Fishing mortality" in guiView
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotFt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
            showProj=FALSE, yLim=NULL, xLim=NULL, useYears=FALSE ) )
{
  if( .USEMt ){
      Ft <- obj$om$Mt[ iRep,(2:ncol(obj$om$Mt)) ]
      }else{
        Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]
      }
  
  
  nT <- length( Ft )
  tMP  <- obj$ctlList$opMod$tMP
    
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null( xLim ) )
    xLim <- c(1,nT)

  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0, max( obj$refPtList$Fmsy, obj$refPtList$Fcra, Ft ) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Ft, lty=.FtLTY,  lwd=.FtLWD,    col=.FtCOL )
  abline( h=obj$refPtList$Fmsy, lty=.FmsyLTY, col=.FmsyCOL, lwd=.FmsyLWD )
  abline( h=obj$refPtList$Fcra, lty=.FcraLTY, col=.FcraCOL, lwd=.FcraLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .FtLAB )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Fishing Mortality" )  

  if ( gfx$doLegend )
  {
   panLegend( 0.05,0.95, legTxt=c("Fmsy","Fcrash"), cex=.CEXLEG2,
              bg="white", lty=c(.FmsyLTY,.FcraLTY), col=c(.FmsyCOL,.FcraCOL),
              lwd=c( .FmsyLWD, .FcraLWD), bty=.LEGBTY )
  }
}     # END function .plotFt


.plotMt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  M   <- obj$ctlList$opMod$M
  Mt  <- obj$om$Mt[ iRep,(2:ncol(obj$om$Mt)) ]
  nT  <- length( Mt )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim
    
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max( Mt ) )
  }
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( h=M, col=.MCOL, lty=.MLTY, lwd=.MLWD )
   
  lines( c(1:nT), Mt, col=.MtCOL, lty=.MtLTY, lwd=.MtLWD )
  
  if ( any(names(obj$mp$assess$mpdPars)=="M" ) )
  {
    idx   <- obj$mp$assess$runStatus$iRep==iRep
    mpMt  <- obj$mp$assess$mpdPars$M[idx]
    tStep <- obj$mp$assess$mpdPars$tStep[idx]
    lines( tStep, mpMt, col="red", lty=2, lwd=.MtLWD )      
  }  

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
    
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Natural Mortality" )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Natural Mortality" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.05, 0.4, legTxt=c( .MtLAB," ","OM", "Est",
               paste( "Mean =", obj$ctlList$opMod$M ),
               paste( "Std. Dev. =", obj$ctlList$opMod$sigmaM ),
               paste( "Correlation =", obj$ctlList$opMod$gammaM ) ),
               lty=c(NA,NA,.MtLTY,2,NA,NA,NA),
               lwd=c(NA,NA,.MtLWD,.MtLWD,NA,NA,NA),
               col=c(NA,NA,.MtCOL,"red",NA,NA,NA),
               cex=.CEXLEG, bg="white", bty=.LEGBTY )  
    
    #panLegend( 0.05, 0.25, legTxt=c( .MtLAB," ",
    #           paste( "Mean =", obj$ctlList$opMod$M ),
    #           paste( "Std. Dev. =", obj$ctlList$opMod$sigmaM ),
    #           paste( "Correlation =", obj$ctlList$opMod$gammaM ) ),
    #           cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }
}     # END function .plotMt


# .plotNt (plot time trajectory of numbers of spanwing fish and total fish)
# Purpose:      Plots both total numbers and spawning numbers over
#                   time for a single replicate, irep, from
#                   simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotNt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
             showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  #Nexp <- obj$om$Nexp[ iRep,(2:ncol(obj$om$Nexp)) ]
  Nexp <- obj$om$Nexp[ iRep,(2:ncol(obj$om$Nexp)) ]  
  Nt   <- obj$om$Nt[ iRep,(2:ncol(obj$om$Nt)) ]
  nT   <- length( Nt )
  tMP  <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
    
  if ( is.null(yLim) )
  {
    #yAvg <- apply( obj$om$Nexp[,(2:ncol(obj$om$Nexp))],1,mean )
    #yLim <- c(0,mean(yAvg))
    yLim <- c( 0,max(c(Nt,Nexp)) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Nexp, lty=.NexpLTY, lwd=.NexpLWD, col=.NexpCOL )
  lines( c(1:nT), Nt,   lty=.NtLTY,   lwd=.NtLWD,   col=.NtCOL )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( .NtLAB, sep="" ) )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.2, legTxt=c( "Exploitable numbers",.NtLAB),
            lty=c(.NexpLTY, .NtLTY), lwd=c(.NexpLWD,.NtLWD), col=c(.NexpCOL,.NtCOL),
            bg="white", bty=.LEGBTY, cex=.CEXLEG2 )
  }
            
  if ( gfx$annotate )
    mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, "Numbers" )

}     # END function .plotNt


#.plotRt        Plot the recruitment numbers vs. time
# Purpose:      Plot the number of fish recruited into fishery in each year
#                   for a single replicate, irep, from
#                   simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  nT <- length( Rt )
  tMP  <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0, max( Rt ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Rt, lty=.RtLTY, lwd=.RtLWD )
  points( c(1:nT), Rt, bg=.RtBG, cex=.RtCEX, pch=.RtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( .RtLAB," (",.RtUNIT,")",sep="" ) )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Recruits" )
}     # END function .plotRt


#.plotRtBt  (plot the stock-recruitment points and curve)
# Purpose:  Plots annual spawner vs. recruits values as well as underlying
#                   spawner-recruitment relationship for a single replicate,
#                   irep, from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRtBt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                      doLegend=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]

  nT  <- length( Rt )
  tMP <- obj$ctlList$opMod$tMP
  
  rp <- calcRefPoints( obj$ctlList$opMod, rpList=list( FALL=TRUE ), application="OM" )

  pchVec <- rep( 21,nT )
  pchVec[ 1:(tMP-2) ] <- 22
  colVec <- rep( "gray85",nT )
  colVec[ 1:(tMP-2) ] <- "white"

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 0,max( Bt ) )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0,max(Rt) )
    
  # Lag the biomass (t) relative to the recruits (t+1).
  Rt <- Rt[ 2:length(Rt) ]
  Bt <- Bt[ 1:(length(Bt)-1) ]

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  points( Bt, Rt, cex=.CEXSYM4, bg=colVec, pch=pchVec )
  lines( rp$ssb[rp$ssb >= 0.0], rp$recruits[rp$ssb>=0.0], lty=1, lwd=.LWD2 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Spawning Biomass (year t-1)" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Recruits Numbers (year t)" )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Rec-Spawners" )  

  if ( gfx$doLegend )
    panLegend( 0.05,0.95, legTxt=c("History","Projection"), cex=.CEXLEG2,
      pt.cex=.CEXSYM4, pch=c(22,21), pt.bg=c("white","gray86"), bty=.LEGBTY )

  return( invisible() )
}     # END function .plotRtBt


#.plotRecSpawnMP  (plot the stock-recruitment points and curve from the MP, not OM)
# Purpose:      Plots (estimated) annual spawner vs. recruits values as well as underlying
#                   spawner-recruitment relationship for a single replicate,
#                   irep, from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRecSpawnMP <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL) )
{
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP
  nProj <- nT - tMP + 1

  # Extract the recruits and biomass from the management procedure assessment.
  # Since there are nT-tMP+1 attempts, we'll take the terminal estimates.

  idx <- obj$mp$assess$Rt[,"iRep"]==iRep & obj$mp$assess$Rt[,"tStep"]==nT
  Rt  <- obj$mp$assess$Rt[ idx,(3:ncol(obj$mp$assess$Rt)) ]

  #idx <- obj$mp$assess$spawnBt[,"iRep"]==iRep & obj$mp$assess$spawnBt[,"tStep"]==nT
  
  idx <- obj$mp$assess$retroSpawnBt[,"iRep"]==iRep & obj$mp$assess$retroSpawnBt[,"tStep"]==nT 
  Bt  <- obj$mp$assess$retroSpawnBt[ idx,(3:ncol(obj$mp$assess$retroSpawnBt)) ]

  rp <- calcRefPoints( obj$ctlList$opMod, rpList=list( FALL=TRUE ) )

  pchVec <- rep( 21,nT )
  pchVec[ 1:(tMP-2) ] <- 22
  colVec <- rep( "gray85",nT )
  colVec[ 1:(tMP-2) ] <- "white"

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
  {
    xLim <- c( 0, max(Bt) )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max(Rt,na.rm=TRUE) )
  }

  # Lag the biomass (t) relative to the recruits (t+1).
  Rt <- Rt[ 2:length(Rt) ]
  Bt <- Bt[ 1:(length(Bt)-1) ]

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  points( Bt, Rt, cex=.CEXSYM4, bg=colVec, pch=pchVec )
  
  # Add the ESTIMATED stock-recruit relationship for the terminal estimate.
  omB <- seq( 0,obj$ctlList$opMod$B0,length=100 )
  idx <- obj$mp$assess$mpdPars[,"iRep"]==iRep & obj$mp$assess$mpdPars[,"tStep"]==nT
  recA <- obj$mp$assess$mpdPars$rec.a[ idx ]
  recB <- obj$mp$assess$mpdPars$rec.b[ idx ]
  omR <- recA * omB / ( 1.0 + recB * omB )
  lines( omB,omR, col="red", lwd=2 )

  # Add the TRUE stock-recruit relationship.
  #lines( rp$ssb[rp$ssb >= 0.0], rp$recruits[rp$ssb>=0.0], col=.OmCOL, lty=.OmLTY, lwd=.OmLWD )
 lines( rp$ssb[rp$ssb >= 0.0], rp$recruits[rp$ssb>=0.0], col="black", lty=1, lwd=.LWD2 ) #RF hard wired lty, lwd and col to match hardwired legend below
 axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()

  mtext( side=1, line=.INLINE2, cex=.CEXLAB4, "Spawning Biomass (year t-1)" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Recruit Numbers (year t)" )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Rec-Spawners" )  

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95,legTxt=c("History","Projection"), pt.cex=.CEXSYM2,
      pch=c(22,21), pt.bg=c("white","gray86"), bty=.LEGBTY )
      
    panLegend( 0.05,0.85,legTxt=c("True","Estimated"),lty=c(1,1), lwd=c(.LWD2, .LWD2),
               col=c("black","red"), bty=.LEGBTY )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Rec-Spawners" )
  }
  return( invisible() )
}     # END function .plotRecSpawnMP


#.plotIt        Plot the survey index series vs. time
# Purpose:      Plot the number of fish recruited into fishery in each year
#                   for a single replicate, irep, from
#                   simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotIt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  ItOM <- obj$om$It[ iRep,(2:ncol(obj$om$It)) ]
  It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  nT <- length( It )
  tMP  <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0, max( It, na.rm=TRUE ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), ItOM, col=.ItCOL, lty=.ItLTY, lwd=.ItLWD )
  points( c(1:nT), It, bg=.ItBG, col=.ItCOL, fg=.ItFG, cex=.ItCEX, pch=.ItPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( .ItLAB ,sep="" ) )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c( paste( .ItLAB, "(OM)" ), paste( .ItLAB, "(Obs)" ) ),
               pt.bg=c(NA,.ItBG), col=c( .ItCOL, .ItCOL ), lty=c( .ItLTY,NA ),
               lwd=c(.ItLWD,NA), pch=c(NA,.ItPCH), pt.cex=c(NA,.ItCEX), bg="white",
               bty=.LEGBTY, cex=.CEXLEG2 )
  }
  
  # Now plot the CVs.
  
  surveyCV <- obj$om$surveyCV[ iRep,(2:ncol(obj$om$surveyCV)) ]
  plot( xLim, c(0,max(surveyCV,na.rm=TRUE)), type="n", axes=FALSE, xlab="", ylab="" )

  tmp <- .calcTimes( obj$ctlList )
  
  lines( tmp$per1Survey, surveyCV[ tmp$per1Survey ], type="h", col=.Per1COL, lwd=3 )  
  lines( tmp$per2Survey, surveyCV[ tmp$per2Survey ], type="h", col=.Per2COL, lwd=3 )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Survey CV" )
  
  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$ctlList$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$ctlList$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$ctlList$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$ctlList$mp$data$tauSurvey2SD,   digits=2 )
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( paste( "Period 1 CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" )),
      col=c(.Per1COL, .Per2COL),
      lwd=c(3,3), bg="white", bty=.LEGBTY, cex=.CEXLEG2 )  
  }

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Survey Index Series" )
}     # END function .plotIt

#.plotItBt  (plot the observed survey points against true SSB)
# Purpose:      Plots annual survey indices of spawning boimass against
#               true spawning biomass from the operating model for a single
#               replicate, irep, from simulation scenario isim.
# Notes:        Labelled "Survey vs Biomass" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotItBt <- function( obj, iSim=1, iRep=1, xAxis, yAxis, annotate=TRUE, showProj=FALSE,
                           yScale=FALSE, xScale=FALSE  )
{
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  
  # Scale It to spawning biomass.
  It <- It / obj$ctlList$opMod$q

  nT <- length( It )

  # X-axis limits.
  if ( xScale )
  {
    xAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
    xLim <- c( 0,max(Bt) )
  }
  if ( !xScale )
    xLim <- xAxis

  # Y-axis limits.
  if ( yScale )
  {
    yAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
    yLim <- c( 0,max(It) )
  }
  if ( !yScale )
    yLim <- yAxis

  lmFit <- lm( It~Bt )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( a=0.0, b=1.0, col="black", lty=1, lwd=.LWD2 )
  abline( lmFit,        col="red",   lty=2, lwd=.LWD2 )
  
  points( Bt, It, cex=.CEXSYM8, col="black", pch=21 )
  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, paste( .BtLAB, " (",.BtUNIT,")", sep="") )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .ItLAB )

  if ( annotate )
    mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, "Survey vs. SSB" )
}     # END function .plotItBt


#.plotObsSurvey  (plot observed biomass estimates vs. time)
# Purpose:      Plots annual estimates of spawning boimass in each estimation
#               year as well as true spawning biomass for a single replicate,
#               irep, from simulation scenario isim.
# Notes:        Labelled "SSB Survey Fit" in guiView
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotObsSurvey <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                    doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # What stock assessment method?
  methodId <- obj$ctlList$mp$assess$methodId

  assessMethod <- .METHODLAB[ methodId ]

  omBt      <- obj$om$Bt
  omBt      <- omBt[ iRep, (2:ncol(omBt)) ]

  omBexpS      <- obj$om$BexpS
  omBexpS      <- omBexpS[ iRep, (2:ncol(omBexpS)) ]    
  
  # ARK (23-Jan-13)  I think this is the PROJECTED (exploitable?) biomass from the
  #                  assessment method. Discuss with SPC.
  
  exploitBt  <- obj$mp$assess$exploitBt
  exploitBt  <- exploitBt[ iRep,(2:ncol(exploitBt)) ]
  
  It        <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  
  # Now correct It for last fitted q.
  if ( methodId >= .PMOD )
  {
    qMP <- obj$mp$assess$mpdPars$q
    qTemp <- qMP[ !is.na(qMP) ]
    It <- It / qTemp[ length(qTemp) ]
  }
  
  surveyCV1 <- obj$om$surveyCV[,"surveyCV1"]
  surveyCV2 <- obj$om$surveyCV[,"surveyCV2"]
  nT        <- obj$ctlList$opMod$nT
  tMP       <- obj$ctlList$opMod$tMP

  # X-axis limits.
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0,max(c(omBt,It),na.rm=TRUE) )
  }

  plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
  
  lines( c(1:nT), omBt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )  
  lines( c(1:nT), omBexpS, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )  
  
  points( c(1:nT), It, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
  
  # Plot terminal biomass estimates - these might be the one-year ahead projection.
  # This gets back to whether it is beginning of year or end of year biomass.
  
  lines( c(1:nT), exploitBt, col=.BtEstCOL, lty=.BtEstLTY, lwd=.BtEstLWD )
    
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .ItLAB )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Spawning and Estimated Biomass" )
    
  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$ctlList$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$ctlList$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$ctlList$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$ctlList$mp$data$tauSurvey2SD,   digits=2 )
    
    methodId <- obj$ctlList$mp$assess$methodId
    label <- paste( .METHODLAB[methodId]," (",obj$ctlList$mp$hcr$hcrType,")", sep="" )
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( label, paste( "Period 1 Survey CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 Survey CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" ),
              .BtLAB, "Survey Exp. Biomass", .BtEstLAB, "Observed survey biomass" ),
      col=c("white", "white","white",.BtCOL,.BexpCOL,.BtEstCOL,.ItFG),
      lty=c(NA,NA,NA,.BtLTY,.BexpLTY,.BtEstLTY,NA),
      pt.cex=c(NA,NA,NA,NA,NA,NA,.ItCEX), pt.bg=c(NA,NA,NA,NA,NA,NA,.ItBG),
      lwd=c(NA,NA,NA,.BtLWD,.BexpLWD,.BtEstLWD,NA), pch=c(NA,NA,NA,NA,NA,NA,.ItPCH),
      cex=.CEXLEG, bg="white", bty=.LEGBTY )  
  }
}     # END function .plotObsSurvey


#.plotModelBtFit  (plot observed biomass estimates vs. time)
# Purpose:      Plot all predicted biomass states from annual production model
#               fits for replicate irep from simulation scenario isim.
# Notes:        Labelled "SSB Stepwise Fits" in guiView
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (17-Aug-09)
.plotModelBtFit <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                            doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
                            useYears=FALSE ) )
{
  # What stock assessment method?
  assessMethod <- obj$ctlList$mp$methodId

  omBt      <- obj$om$Bt
  omBt      <- omBt[ iRep, (2:ncol(omBt)) ]

  omBexp    <- obj$om$Bexp
  omBexp    <- omBexp[ iRep, (2:ncol(omBexp)) ]
  
  It        <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  surveyCV1 <- obj$om$surveyCV[,"surveyCV1"]
  surveyCV2 <- obj$om$surveyCV[,"surveyCV2"]
  nT <- length( It )
  
  methodId <- obj$ctlList$mp$assess$methodId

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( obj$ctlList$opMod$tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0, max(It,na.rm=TRUE) )
  }

  plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )

  abline( v=obj$ctlList$opMod$tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.LWD )

  lines( c(1:nT), omBt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  lines( c(1:nT), omBexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )
  points( c(1:nT), It, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )  
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box( lwd=.LWD )
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4,  "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4 , paste( .BtLAB," (",.BtUNIT,")", sep="" ) )

  # Add lines for all retrospective exploitable biomass states
  retroExpBt <- obj$mp$assess$retroExpBt[ obj$mp$assess$retroExpBt[,1]==iRep, ]

  for ( j in 1:nrow(retroExpBt) )
  {
    tStep <- retroExpBt[ j,"tStep" ]
    lines( retroExpBt[ j, 3:ncol(retroExpBt) ], col=.BtStepCOL, lty=.BtStepLTY, lwd=.BtStepLWD )
  }

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE,
      paste( "Stepwise Model Fits: ", .METHODLAB[methodId], sep="" ) )  

  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$ctlList$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$ctlList$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$ctlList$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$ctlList$mp$data$tauSurvey2SD,   digits=2 )
    
    methodId <- obj$ctlList$mp$assess$methodId
    label <- paste( .METHODLAB[methodId]," (",obj$ctlList$mp$hcr$hcrType,")", sep="" )    
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( label, paste( "Period 1 Survey CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 Survey CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" ),
              .BtLAB, .BexpLAB, .BtEstLAB, "Observed survey biomass" ),
      pt.bg=c( NA,NA,NA,NA,NA,NA, .ItBG ), pt.cex=c(NA,NA,NA,NA,NA,NA, .ItCEX ),
      col=c("white","white","white",.BtCOL,.BexpCOL,.BtStepCOL,.ItCOL), lty=c(NA,NA,NA,.BtLTY,.BexpLTY,.BtStepLTY,NA),
      lwd=c(NA,NA,NA,.BtLWD,.BexpLWD,.BtStepLWD,NA), pch=c(NA,NA,NA,NA,NA,NA,.ItPCH),
      cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }
}     # END .plotModelBtFit


#.plotRefPtEsts   (plot annual parameter estimates)
# Purpose:      Plot annual parameter estimates from stepwise model
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09)
.plotRefPtEsts <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                          doLegend=TRUE, showProj=FALSE, yLim=NULL, xLim=NULL,
                          useYears=FALSE ) )
{
  iReplicate <- iRep
  nT         <- obj$ctlList$opMod$nT
  tMP        <- obj$ctlList$opMod$tMP

  # What stock assessment method?
  methodId <- obj$ctlList$mp$assess$methodId

  if ( methodId >= .PMOD  )
  {
    # Extract annual parameter estimates.
    mpdPars  <- obj$mp$assess$mpdPars

    iRepCol <- mpdPars[1] # extract this col only because iRep heading in mpdPars
                          # conflicts with function arguments

    BmsyEsts <- mpdPars$ssbFmsy[ iRepCol==iRep ]
    FmsyEsts <- mpdPars$Fmsy[ iRepCol==iRep ]

    BmsyUp <- 0
    BmsyLo <- 0
    FmsyUp <- 0
    FmsyLo <- 0

    if ( methodId == .PMOD )
    {
      # KRH - std not yet saved for caa model
      # Extract upper and lower error bars (+ 2 sd)
      #BmsyUp<- exp( log(BmsyEsts)+ 2*(stdBmsy[iRepCol==iRep]) )
      #BmsyLo<- exp( log(BmsyEsts)- 2*(stdBmsy[iRepCol==iRep]) )
      #FmsyUp<- exp( log(FmsyEsts)+ 2*(stdFmsy[iRepCol==iRep]) )
      #FmsyLo<- exp( log(FmsyEsts)- 2*(stdFmsy[iRepCol==iRep]) )
    }

    # Get OM equilibrium values for comparison.
    Bmsy <- obj$refPtList$ssbFmsy
    Fmsy <- obj$refPtList$Fmsy

    xLim <- gfx$xLim
    yLim <- gfx$yLim[1,]

    if ( is.null(xLim) )
      xLim <- c(1,nT)

    if ( gfx$showProj )
      xLim <- c( tMP - 1, nT )

    if ( is.null(yLim) )
    {
      yLim <- c(0, max( c( Bmsy, mpdPars$ssbFmsy ) ) )
    }
 
    plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
    
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    
    points( tMP:nT, BmsyEsts, bg=.BmsyBG, col=.BmsyCOL, cex=.BmsyCEX, pch=.BmsyPCH )

    # KRH - std not yet saved for caa model
    if ( methodId == .PMOD )
    {
      idx <- BmsyLo!=BmsyUp
      arrows( (tMP:nT)[idx], BmsyLo[idx], (tMP:nT)[idx], BmsyUp[idx],
        angle=0, col="gray50", lty=1, lwd=2 )
    }

    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    axis( side=4, labels=FALSE )
    box()
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .BmsyLAB )

    if ( gfx$annotate )
      mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Reference Points" )
    
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("MLE Estimate","2 SD error bar", "Bmsy"),
                 pt.bg=c(.BmsyBG,NA,NA), pt.cex=c(.BmsyCEX,NA,NA), lty=c(NA,1,.BmsyLTY),
                 pch=c(.BmsyPCH, NA, NA), col=c(.BmsyCOL, "gray50", .BmsyCOL ),
                 lwd=c(1,2,.BmsyLWD), cex=.CEXLEG2, bg="white", bty=.LEGBTY )
    }

    # Plot Fmsy estimates.
    yLim <- gfx$yLim[2,]
    if ( is.null(yLim) )
    {
      yLim <- c( 0,max( c( Fmsy, mpdPars$Fmsy ) ) )
    }

    plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    
    points( tMP:nT, FmsyEsts, bg=.FmsyBG, cex=.FmsyCEX, col=.FmsyCOL, pch=.FmsyPCH )

    # KRH - std not yet saved for caa model
    if ( methodId == .PMOD )
    {

    }

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    axis( side=4, labels=FALSE )
    abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )

    # K.Holt: temporary line to show prior mu:
    #abline(h=obj$guiPars$mp$assess$muPriorFmsy,lty=2, col="blue", lwd=1)
    box()
    
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("MLE Estimate","2 SD error bar", "Fmsy"),
                 pt.bg=c(.FmsyBG,NA,NA), pt.cex=c(.FmsyCEX,NA,NA), lty=c(NA,1,.FmsyLTY),
                 pch=c(.FmsyPCH, NA, NA), col=c(.FmsyCOL, "gray50", .FmsyCOL ),
                 lwd=c(1,2,.FmsyLWD), cex=.CEXLEG2, bg="white", bty=.LEGBTY )
    }
    
    
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .FmsyLAB )
  }
  return( invisible() )
}     # END function .plotParEsts

#.plotParCor   (plot correlation between Fmsy and Bmsy estimates)
# Purpose:      Plot annual parameter estimates of Fmsy vs. Bmsy from model
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09)
.plotParCor <- function( obj, iSim=1, iRep=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # What stock assessment method?
  methodId <- obj$ctlList$mp$assess$methodId

  if ( methodId >= .PMOD  )
  {
    # Extract annual parameter estimates.
    
    mpdPars  <- obj$mp$assess$mpdPars
    
    # Extract this column because iRep heading in esetPars conflicts with fun pars.
    iRepCol <- mpdPars[,"iRep" ]

    BmsyEsts <- mpdPars$ssbFmsy[ iRepCol==iRep ]
    FmsyEsts <- mpdPars$Fmsy[ iRepCol==iRep ]

    # Get equilibrium values for comparisson
    equilBmsy <- obj$refPtList$ssbFmsy
    equilFmsy <- obj$refPtList$Fmsy

    # Specify axes
    xLim <- gfx$xLim
    if ( is.null(xLim) )
    {
      xAvg <- mean( mpdPars$ssbFmsy )
      #xLim <- c(0,mean(xAvg))
      
      xLim <-c( 0, max( c( BmsyEsts, equilBmsy, na.rm=TRUE ) ) )
    }

    yLim <- gfx$yLim
    if ( is.null(yLim) )
    {
      yAvg <- mean( mpdPars$Fmsy )
      yLim <- c( 0, max( c( FmsyEsts, equilFmsy ), na.rm=TRUE ) )
    }

    plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
    
    points( BmsyEsts,  FmsyEsts,  cex=.CEXSYM4, pch=.BmsyPCH, col="black" )
    points( equilBmsy, equilFmsy, cex=.CEXSYM8, pch=.BmsyPCH, bg="red" )
    
    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    box()
    
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, .BmsyLAB )
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .FmsyLAB )

    if ( gfx$annotate )
    {
      mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Parameter Correlation" )
      panLegend( 0.05,0.95, legTxt=c("Estimates","Equilibrium"),
                 bg="white", cex=.CEXLEG2, pch=c(.BmsyPCH,.BmsyPCH),
                 pt.bg=c("white","red"), pt.cex=.CEXSYM8,
                 col=c("black", "red"), bty=.LEGBTY)
    }
  }
}     # END function .plotParCor


# plotFvsSSB   (plot the DFO target harvest control rule)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.  This plot shows the rule managers
#               were trying to implement.  In contrast, plotViewRealHCR shows
#               the pairs of F and SSB that actually occurred. In cases when
#               reference points are estimated, multiple lines are drawn to
#               show how HCR based on these points changes over time.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotFvsSSB <- function( obj, iSim=1, iRep=1, phase=FALSE, gfx=list( annotate=TRUE,
                          doGrid=FALSE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$refPtList$ssbFmsy
  Fmsy  <- obj$refPtList$Fmsy
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP

  # True spawning biomass and true fishing mortality
  idx   <- tMP:nT  
  omBt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  omFt <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ][idx]

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
  {
    if ( phase )
    {
      omBt <- omBt / Bmsy
      xLim <- c( 0, max( 1,omBt ) )
    }
    else
      xLim <- c( 0,max(c(Bmsy,omBt)) )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    if ( phase )
    {
      omFt <- omFt / Fmsy
      yLim <- c( 0,max( 1, omFt ) )
    }
    else
      yLim <- c( 0,max(c(Fmsy,omFt)) )
  }

  plot( xLim,yLim, type="n",axes=FALSE, xlab="",ylab="" )

  # Indicate lRef, uRef, and Bref for true case.
  if ( phase )
  {
    abline( v=1, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( h=1, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    .addTarget( 1, 1, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }
  else
  {
    abline( v=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }

  # NOW plot trajectory of target Ft by spawnBt for tMP forward...
  # (Assume that time order has not be changed)

  colVec <- rev( heat.colors( n=length(omBt) ) )

  points( omBt, omFt, cex=.CEXSYM6, bg=colVec, pch=21 )
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, label=FALSE )
  
  if ( !phase )
  {
    depletion <- seq( 0,B0,B0/10.0 )
    axis( side=3, cex.axis=.CEXAXIS4, at=depletion, labels=depletion/B0 )
    
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, paste( .BtLAB," (",.BtUNIT,")", sep="" ) )
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .FtLAB )    
    mtext( side=3, line=.INLINE3, cex=.CEXLAB4, "Depletion" )
    
    mainLab <- "Fishing Mortality vs. Spawning Biomass"    
  }
  else
  {
    axis( side=3, label=FALSE )
    mtext( side=1, line=.INLINE2, cex=.CEXLAB4, "Bt / Bmsy" )
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Ft / Fmsy" )
    
    mainLab <- "Phase Plot"
    if ( gfx$annotate )
      mtext( side=3, line=2, cex=.CEXTITLE4, "Phase Plot" )  
  }

  box()

  if ( gfx$doLegend )
  {
  }
  return( invisible() )
}     # END function .plotFvsSSB

# .plotHCRmpVariableF (plot a status-based rule)
# Purpose:      Plot a status-based harvest control rule, indicating the Lower
#               Bound, Upper Bound, and Removal Reference.
#               In cases when reference points are estimated, multiple lines are
#               drawn to show how HCR based on these points changes over time.
# Parameters:   obj - the blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotHCRmpVariableF <- function( obj, iSim=1, iRep=1, gfx=list(annotate=TRUE,
             doGrid=FALSE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL) )
{
  # Get zone reference points.
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$refPtList$ssbFmsy
  Fmsy  <- obj$refPtList$Fmsy
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP
  # Indexing vector for projection years.     
  idx   <- tMP:nT

  # Determine base for stock status.whether objectives are based on Bmsy or B0.
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    Btarget <- Bmsy
    
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    Btarget <- B0

  # Determine HCR type and get properties.
  hcrType <- obj$mp$hcr$specs$hcrType

  # OM biomass and realized fishing mortality
  omBt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  omFt <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ][idx]

  # Extract target control rules that were used in a subset of years:
  # Select every k-th year to plot HCR for in the middle of MP time series
  start  <- tMP
  end    <- nT
  len    <- trunc( (end - start)/5. )
  #len <- 1
  midYrs <- seq( start, end, length=len )

  # In addition to these years, always plot first and last year
  #plotYrs <- c( tMP, midYrs ,nT )
  plotYrs <- c(tMP:nT)

  # MPD values.
  lowerBound <- obj$mp$hcr$lowerBound[ iRep,idx ]
  upperBound <- obj$mp$hcr$upperBound[ iRep,idx ]
  
  # MPD values.
  Fmult      <- obj$mp$hcr$specs$Fmult
  refRemRate <- Fmult*obj$mp$hcr$Fref[ iRep,idx ]

  # Estimated projected spawning biomass and intended target fishing mortality from rule.
  spawnBt <- obj$mp$assess$spawnBt[ iRep,(2:ncol(obj$mp$assess$spawnBt)) ][idx]
    
  mpFt <- obj$mp$hcr$targetFt[ iRep,(2:ncol(obj$mp$hcr$targetFt)) ][idx]
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 0,max( c(B0,spawnBt,omBt) ) )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0,max(c(mpFt,omFt), na.rm=TRUE) )

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  usr <- par( "usr" )

  # Add HCR for selected years to plot (first, last years have diffnt line types)
  for (j in 1:length(spawnBt))
  {
    lty.tmp <- 1
    lwd.tmp <- 1
    col.tmp <- "grey30"
    
    if ( j==1 )    # first year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "blue"
    }
    
    if ( j==length(plotYrs))   # last year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "red"
    }

    if ( (hcrType=="variableF") | (hcrType=="variableFopMod") )
    {
      segments(           0,               0, lowerBound[j],             0,
                col=col.tmp, lty=lty.tmp, lwd=lwd.tmp )
      segments( lowerBound[j],             0, upperBound[j], refRemRate[j],
                col=col.tmp, lty=lty.tmp, lwd=lwd.tmp )
      segments( upperBound[j], refRemRate[j],        usr[2], refRemRate[j],
                col=col.tmp, lty=lty.tmp, lwd=lwd.tmp )
    }
  }

  # Plot trajectory of target Ft by assessBt for tMP forward, assume time order.
  colVec <- rev( heat.colors( n=length(spawnBt) ) )

  # Test of whiskers... x0, y0, x1, y1
  if ( gfx$annotate )
  {
    segments( spawnBt,mpFt, omBt,omFt )
    points( omBt,omFt, cex=.CEXSYM8, bg=colVec, pch=22 )
  }
  
  points( spawnBt, mpFt, cex=.CEXSYM8, bg=colVec, pch=21 )

  # Axes.
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  #depletion <- seq( 0,B0,B0/10.0 )
  #axis( side=3, at=depletion, labels=depletion/B0 )
  
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Estimated Stock Status" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB4, "Intended Removal Rate (F)" )

  if ( gfx$doLegend )
  {
    panLegend( 0.8, 0.95, legTxt=c( "OM","MP" ), cex=.CEXLEG2, pch=c(22,21),
               pt.cex=c(.CEXSYM8, .CEXSYM8), bty=.LEGBTY )
  }
  
  if ( gfx$annotate )
    mtext( side=3, line=1, cex=.CEXTITLE4, outer=TRUE, "Harvest Control Rule" )
    
}     # END function .plotHCRmpVariableF

# .plotHCRmpConstantF (plot a status-based rule)
# Purpose:      Plot a status-based harvest control rule, indicating the Lower
#               Bound, Upper Bound, and Removal Reference.
#               In cases when reference points are estimated, multiple lines are
#               drawn to show how HCR based on these points changes over time.
# Parameters:   obj - the blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotHCRmpConstantF <- function( obj, iSim=1, iRep=1, gfx=list(annotate=TRUE,
             doGrid=FALSE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL) )
{
  # Get OM reference points.
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$refPtList$ssbFmsy
  Fmsy  <- obj$refPtList$Fmsy
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP

  # Indexing vector for projection years.     
  idx   <- tMP:nT

  # Determine base for stock status whether control points are based on Bmsy or B0.
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    Btarget <- Bmsy
    
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    Btarget <- B0

  # Determine HCR type and get properties.
  hcrType <- obj$mp$hcr$specs$hcrType

  # True spawning biomass and realized fishing mortality (on exploitable?).
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ][idx]

  # Extract target control rules that were used in a subset of years:
  # Select every k-th year to plot HCR for in the middle of MP time series
  start  <- tMP
  end    <- nT
  len    <- trunc( (end - start)/5. )
  midYrs <- seq( start, end, length=len )

  # In addition to these years, always plot first and last year
  plotYrs <- c( tMP, midYrs ,nT )

  lowerBound <- obj$mp$hcr$lowerBound[ iRep,idx ]
  upperBound <- obj$mp$hcr$upperBound[ iRep,idx ]
  refRemRate <- obj$mp$hcr$Fref[ iRep,idx ]

  # Estimated spawning biomass and intended target fishing mortality from rule.
  spawnBt   <- obj$mp$assess$spawnBt[ iRep,(2:ncol(obj$mp$assess$spawnBt)) ][idx]
  exploitBt <- obj$mp$assess$exploitBt[ iRep,(2:ncol(obj$mp$assess$exploitBt)) ][idx]
  targetFt  <- obj$mp$hcr$targetFt[ iRep,(2:ncol(obj$mp$hcr$targetFt)) ][idx]

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 0,B0 )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0,max(c(targetFt,Ft)) )

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  usr <- par( "usr" )

  # Add HCR for selected years to plot (first, last years have diffnt line types)
  for (j in 1:length(spawnBt))
  {
    lty.tmp <- 1
    lwd.tmp <- 1
    col.tmp <- "grey30"
    
    if ( j==1 )    # first year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "grey30"
    }
    if ( j==length(plotYrs))   # last year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "grey30"
    }

    segments( 0, refRemRate[j], usr[2], refRemRate[j], lty=lty.tmp, lwd=lwd.tmp )    
  }

  # Plot trajectory of target Ft by assessBt for tMP forward, assume time order.
  colVec <- rev( heat.colors( n=length(spawnBt) ) )

  # Test of whiskers... x0, y0, x1, y1
  segments( spawnBt,targetFt, Bt,Ft )
  points( spawnBt, targetFt, cex=.CEXSYM4, bg=colVec, pch=21 )

  # Axes.
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  
  #depletion <- seq( 0,B0,B0/10.0 )
  #axis( side=3, at=depletion, labels=depletion/B0 )
  
  box()
  
  mtext( side=1, line=2.5, cex=.CEXLAB4, "Estimated Stock Status" )
  mtext( side=2, line=3.0, cex=.CEXLAB4, "Intended Removal Rate (F)" )

  if ( gfx$doLegend )
  {
  }
  
  if ( gfx$annotate )
    mtext( side=3, line=1, cex=.CEXTITLE4, outer=TRUE, "Harvest Control Rule" )
    
}     # END function .plotHCRmpConstantF


# .plotRefPtSeries (plot the times series of reference points against objectives.
# Purpose:      This plot shows trajectories of Operating Model and MP estimates
#               of biomass and fishing mortality.
# Parameters:   obj - the blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund	(Aug 2013 R. Forrest added code for plotting historical ref points)
.plotRefPtSeries <- function( obj, iSim=1, iRep=1,
  gfx=list( annotate=TRUE,colorZones=TRUE,doLegend=TRUE,xLim=NULL,yLim=NULL,
            yLim2=NULL, useYears=FALSE ) )
{
  # Extract plot parameters.
  xLim       <- gfx$xLim
  yLim       <- gfx$yLim
  yLim2      <- gfx$yLim2

  nT         <- obj$ctlList$opMod$nT
  tMP        <- obj$ctlList$opMod$tMP

  # Extract relevant reference point bases: SPC edited 4June
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$refPtList$ssbFmsy
  Fmsy  <- obj$refPtList$Fmsy
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    base <- "Bmsy"
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    base <- "B0"
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBt" )
    base <- "HistB"

   #for historical case, get true upper and lower ctl points
   if ( base=="HistB")
   {
     lowerSyr <-obj$mp$hcr$specs$lowBaseStartYr
	   lowerNyr <-obj$mp$hcr$specs$lowBaseEndYr
     upperSyr <-obj$mp$hcr$specs$upperBaseStartYr
     upperNyr <-obj$mp$hcr$specs$upperBaseEndYr
	   
     # Calculate lower base for historical reference points	      #RF has changed om$Bexp to om$Bt - ctl points based on spawning biomass
     if (obj$mp$hcr$specs$histLowBase=="Bmin")
	   {
	     trueLowerBref <- min(obj$om$Bt[lowerSyr:lowerNyr])    #RF Bt in OM is spawning biomass
	   }
	   if (obj$mp$hcr$specs$histLowBase=="Bmax")
	   {
	    trueLowerBref <- max(obj$om$Bt[lowerSyr:lowerNyr])   
	   }
	   if (obj$mp$hcr$specs$histLowBase=="Bmean")
	   {
	    trueLowerBref <- mean(obj$om$Bt[lowerSyr:lowerNyr])
	   }

     if (obj$mp$hcr$specs$histLowBase=="Bquant")
     {
       trueQuant     <- obj$mp$hcr$specs$lowBaseQuant
       trueLowerBref <- quantile(obj$om$Bt[lowerSyr:lowerNyr], trueQuant)[[1]]
	   }
	   
     # Calculate upper base for historical reference points
	   if (obj$mp$hcr$specs$histUpperBase=="Bmin")
	   {
	     trueUpperBref <- min(obj$om$Bt[upperSyr:upperNyr])   
	   }
	   
	   if (obj$mp$hcr$specs$histUpperBase=="Bmax")
	   {
	     trueUpperBref <- max(obj$om$Bt[upperSyr:upperNyr])  
	   }
	   
	   if (obj$mp$hcr$specs$histUpperBase=="Bmean")
	   {
	     trueUpperBref <- mean(obj$om$Bt[upperSyr:upperNyr])
	   }
	   
	   if (obj$mp$hcr$specs$histUpperBase=="Bquant")
	   {
	     trueQuant     <- obj$mp$hcr$specs$upperBaseQuant
	     trueUpperBref <- quantile(obj$om$Bt[upperSyr:upperNyr],trueQuant)[[1]]
     }

	   trueUpperBound <-  trueUpperBref*obj$ctlList$mp$hcr$upperBoundMult
	   trueLowerBound <-  trueLowerBref*obj$ctlList$mp$hcr$lowerBoundMult
   }

   if ( obj$mp$hcr$specs$remRefBase=="rrBaseFmsy" & obj$mp$hcr$specs$remRefSource=="rrSrceEst" )
     remRate <- obj$refPtList$Fmsy
	 
   if ( obj$mp$hcr$specs$remRefBase != "rrBaseFmsy" )
	  remRate <- obj$mp$hcr$specs$remRateInput

   # HCR control point multipliers.
   lowerBoundMult <- obj$ctlList$mp$hcr$lowerBoundMult
   upperBoundMult <- obj$ctlList$mp$hcr$upperBoundMult   

   # Lower and upper control points for HCR, plus base. These are time series of estimated bounds
   lowerB  <- obj$mp$hcr$lowerBound
   upperB  <- obj$mp$hcr$upperBound
   #RF added conditional statement
   if ( base != "HistB" )
     Bref  <- obj$mp$hcr$Bref

   # Operating model Bt and Ft.
   Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]		      #assume this is SBt? RF - yes in the OM but not MP
   Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]

   # Management procedure estimated spawning Bt and Ft.
   BtEst <- obj$mp$assess$spawnBt[ iRep, c(2:ncol(obj$mp$assess$spawnBt)) ]	 #RF changed this to spawnBt - Bt in the assessment model is exploitable biomass
   idx <- obj$mp$assess$retroFt[,"iRep"]==iRep
   FtEst <- obj$mp$assess$retroFt[ idx, c(3:ncol(obj$mp$assess$retroFt)) ]	 #RF_added this:   terminal year estimates of fishing mortality
   FtEst <- FtEst[ nT-tMP+1, ]
   
   #ARK: Corrected (these are retrospective Fs - i.e. the F you thought you had in the year of the assessment)  now they are! ARK.

   # Panel 1: Stock status - spawning biomass.

   # X-axis limits (Year).
   if ( is.null(xLim) )
   {
     xLim <- c( tMP-1,nT+1 )
   }

   # Y-axis limits (Spawning biomass).
   if ( is.null(yLim) )
   {
     yLim <- c( 0, max( c( Bt,BtEst ), na.rm=TRUE ) )
   }

   plot( xLim,yLim, type="n",axes=FALSE, xlab="", ylab="" )

   usr <- par( "usr" )   # Get the plot region user coordinate limits.

   if ( base=="Bmsy" )
     abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    
   if ( base=="B0" )
     abline( h=B0, col=.B0COL, lty=.B0LTY, lwd=.B0LWD )

   if ( base=="HistB" )
   {
	   abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )      #show performance of historical ctl points relative to true bmsy
	   if(gfx$annotate){
		   abline( h=trueUpperBound, col=.HCRUBBG, lty=.BmsyLTY, lwd=.BmsyLWD )      #show performance of historical ctl points relative to true values
		   abline( h=trueLowerBound, col=.HCRLBBG,  lty=.BmsyLTY, lwd=.BmsyLWD )      #show performance of historical ctl points relative to true values
   }
  }

  abline( v=tMP,    col=.tMPCOL,   lty=.tMPLTY,    lwd=.tMPLWD )

  # Operating model spawning stock biomass.
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )

  # Estimated spawning stock biomass from method.
  lines( c(1:nT), BtEst, col=.BtEstCOL, lty=.BtEstLTY, lwd=.BtEstLWD )

  # Lower control bounds.
  idx <- lowerB[,"iRep"] == iRep
  points( c(1:nT), lowerB[idx,-1], bg=.HCRLBBG, col=.HCRLBCOL, cex=.HCRLBCEX,
          fg=.HCRLBFG, lty=.HCRLBLTY, lwd=.HCRLBLWD, pch=.HCRLBPCH )

  # Upper control bounds.
  idx       <- upperB[,"iRep"] == iRep
  points( c(1:nT), upperB[idx,-1], bg=.HCRUBBG, col=.HCRUBCOL, cex=.HCRUBCEX,
          fg=.HCRUBBG, lty=.HCRUBLTY, lwd=.HCRUBLWD, pch=.HCRUBPCH )

  # Estimated HCR base control point.	  RF added if statement
  if( base != "HistB"){ idx <- Bref[,"iRep"] == iRep
  points( c(1:nT), Bref[idx,-1], bg=.BrefCOL, cex=.BrefCEX, fg=.BrefFG,
          lty=.BrefLTY, lwd=.BrefLWD, pch=.BrefPCH )
  }
  
  .addXaxis( xLim, initYear=.INITYEAR, cexAxis=.CEXAXIS2, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()

  mtext( side=2, line=.INLINE4, cex=.CEXLAB4, "Spawning Stock Status" )

  if ( gfx$doLegend )
  {
    if( base != "HistB") 
	  { 
  		panLegend( 0.5,0.95, legTxt=c( "OM Bmsy", .BtLAB, .BtEstLAB,
				paste( "HCR",.BrefLAB ), paste( "HCR",.HCRUBLAB ), paste( "HCR",.HCRLBLAB ) ),
		    col=c( .BmsyCOL, .BtCOL, .BtEstCOL, .BrefFG, .HCRUBFG, .HCRLBFG ),
		    pch=c(NA,NA,NA,.BrefPCH,.HCRUBPCH,.HCRLBPCH),
		    pt.cex=c(NA,NA,NA,.BrefCEX,.HCRUBCEX,.HCRLBCEX),
		    pt.bg=c(NA,NA,NA,.BrefBG,.HCRUBBG,.HCRLBBG ),
		    lwd=c( .BmsyLWD, .BtLWD, .BtEstLWD,NA,NA,NA),
		    lty=c(.BmsyLTY,.BtLTY,.BtEstLTY,NA,NA,NA),
		    bg="white", bty="o" )
    }
    else
    {
      if ( gfx$annotate )
      {
        # no Bref for historical case with true control points
   		  panLegend( 0.5,0.95,
	        legTxt=c( "OM Bmsy", paste( "OM ",.HCRUBLAB ),paste( "OM ",.HCRLBLAB ),.BtLAB,
                    .BtEstLAB, paste( "HCR",.HCRUBLAB ), paste( "HCR",.HCRLBLAB ) ),
			    col=c( .BmsyCOL,.HCRUBBG, .HCRLBBG, .BtCOL, .BtEstCOL,.HCRUBFG, .HCRLBFG ),	  #RF not sure why these cols don't work for lines .HCRUBFG, .HCRLBFG  
			    pch=c(NA,NA,NA,NA,NA,.HCRUBPCH,.HCRLBPCH),
			    pt.cex=c(NA,NA,NA,NA,NA,.HCRUBCEX,.HCRLBCEX),
			    pt.bg=c(NA,NA,NA,NA,NA,.HCRUBBG,.HCRLBBG ),
			    lwd=c( .BmsyLWD,.BmsyLWD, .BmsyLWD, .BtLWD, .BtEstLWD,NA,NA),
			    lty=c(.BmsyLTY,.BmsyLTY,.BmsyLTY,.BtLTY,.BtEstLTY,NA,NA),
			    bg="white", bty="o" )
			}
      else
      {	     
        # no Bref for historical case without true control points
				panLegend( 0.5,0.95, legTxt=c( "OM Bmsy", .BtLAB, .BtEstLAB,
						paste( "HCR",.HCRUBLAB ), paste( "HCR",.HCRLBLAB ) ),
				    col=c( .BmsyCOL, .BtCOL, .BtEstCOL,.HCRUBFG, .HCRLBFG ),	  #RF not sure why these cols don't work for lines .HCRUBFG, .HCRLBFG  
				    pch=c(NA,NA,NA,.HCRUBPCH,.HCRLBPCH),
				    pt.cex=c(NA,NA,NA,.HCRUBCEX,.HCRLBCEX),
				    pt.bg=c(NA,NA,NA,.HCRUBBG,.HCRLBBG ),
				    lwd=c( .BmsyLWD, .BtLWD, .BtEstLWD,NA,NA),
				    lty=c(.BmsyLTY,.BtLTY,.BtEstLTY,NA,NA),
			      bg="white", bty="o" )	
      }
    }
  }

  # Panel 2: Fishing mortality.
  
  # Y-axis limits (Spawning biomass).
  if ( is.null(yLim2) )
  {
    yLim2 <- c( 0, max( c(Ft,FtEst,Fmsy), na.rm=TRUE ) )
  }

  plot( xLim,yLim2, type="n",axes=FALSE, xlab="", ylab="" )

  abline( h=obj$refPtList$Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
  abline( v=tMP,                col=.tMPCOL,  lty=.tMPLTY,  lwd=.tMPLWD )

  # Plot the OM fishing mortality.
  lines( c(1:nT), Ft, col=.FtCOL, lty=.FtLTY, lwd=.FtLWD )

  #Plot the terminal year estimates of fishing mortality (these are retrospective Fs - i.e. the F you thought you had in the year of the assessment)
  lines( c(1:nT), FtEst, col=.BtEstCOL, lty=.BtEstLTY, lwd=.BtEstLWD )
  
  # Extract the reference and target (post rule) fishing mortality values.
  idx <- obj$mp$hcr$Fref[,"iRep"] == iRep
  Fref <- obj$mp$hcr$Fref[ idx, c(2:ncol(obj$mp$hcr$Fref)) ]

  idx <- obj$mp$hcr$targetFt[,"iRep"] == iRep
  Fhcr <- obj$mp$hcr$targetFt[ idx, c(2:ncol(obj$mp$hcr$targetFt)) ]

  idx <- Fref!=Fhcr
  arrows( c(1:nT)[idx], Fref[idx], c(1:nT)[idx], Fhcr[idx], col="gray", lwd=2, angle=15, length=0.15 )
  points( c(1:nT), Fref, bg=.FrefBG, cex=.FrefCEX*1.25, fg=.FrefFG, pch=.FrefPCH )
  points( c(1:nT), Fhcr, bg="yellow", cex=.FhcrCEX, fg=.FhcrFG, pch=.FhcrPCH )

  .addXaxis( xLim, initYear=.INITYEAR, cexAxis=.CEXAXIS2, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()

  mtext( side=1, line=0.0,      cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB4,             "Removal Rate" )

  if ( gfx$doLegend )
  {
    panLegend( 0.5,0.95, legTxt=c( "OM Fmsy", "OM Fishing mortality", "Est Fishing mortality",
               "Ref Removal Rate", "Adj Removal Rate"  ),
      col=c( .FmsyCOL, .FtCOL, .BtEstCOL, .FrefFG, .FhcrFG ),
      pch=c(NA,NA,NA,.FrefPCH,.FhcrPCH),
      pt.cex=c( NA,NA,NA,.FhcrCEX,.FhcrCEX ),
      pt.bg=c( NA,NA,NA, .FrefBG, "yellow" ),
      lwd=c( .FmsyLWD, .FtLWD, .BtEstLWD, NA, NA ),
      lty=c(.FmsyLTY,.BtEstLTY,.FtLTY,NA,NA),
      bg="white", bty="o" )
  }
  return( invisible() )
}     # END function .plotRefPtSeries


#.plotRetroFits (Plot OM exploitable, spawning, and retrospective fits)
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual survey indices and stock
#                assessment model fits, and the third showing annual estimates.
# Notes:       Labelled "SSB Fit Assmt" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       K.R. Holt

.plotRetroFits <- function( obj, iSim, iRep, varName="exploitBt", 
   gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
             useYears=FALSE ) )
{
  # There are 3 options: varName=c( "exploitBt","exploitBtS", "spawnBt" )
  
  nT   <- obj$ctlList$opMod$nT
  tMP  <- obj$ctlList$opMod$tMP

  # Operating model.
  Bt    <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Bexp  <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]
  BexpS <- obj$om$BexpS[ iRep,(2:ncol(obj$om$BexpS)) ]  
  Dt    <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  qOM   <- obj$ctlList$opMod$qSurvey
  
  idx   <- obj$mp$assess$mpdPars$iRep==iRep
  tStep <- obj$mp$assess$mpdPars$tStep[ idx ]
  qMP   <- obj$mp$assess$mpdPars$q[ idx ]

  Bmsy <- obj$refPtList$ssbFmsy
  
  # Management procedure.
  It   <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  
  if ( varName=="exploitBt" )
  {
    retroBt <- obj$mp$assess$retroExpBt
    bioCOL <- .BexpRetroCOL
    bioLAB <- .BexpRetroLAB
    bioLTY <- .BexpRetroLTY
    bioLWD <- .BexpRetroLWD
    yLabel <- .BexpRetroLAB
  }
  
  if ( varName=="exploitBtS" )
  {
    retroBt <- obj$mp$assess$retroExpBtS
    bioCOL <- .BexpSRetroCOL
    bioLAB <- .BexpSRetroLAB
    bioLTY <- .BexpSRetroLTY
    bioLWD <- .BexpSRetroLWD
    yLabel <- .BexpSRetroLAB
  }  
  
  if ( varName=="spawnBt" )
  {
    retroBt <- obj$mp$assess$retroSpawnBt
    bioCOL <- .BspawnRetroCOL
    bioLAB <- .BspawnRetroLAB
    bioLTY <- .BspawnRetroLTY
    bioLWD <- .BspawnRetroLWD
    yLabel <- .BspawnRetroLAB
  }
  
  runStatus <- obj$mp$assess$runStatus

  # X-axis limits (same for both panels)
  xLim <- gfx$xLim
  yLim <- gfx$yLim[1,]
  
  if ( is.null(xLim) )
    xLim <- c(1,nT)

  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Panel 1: Plot OM spawning, survey and fishery exploitable biomass and survey index.
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    idx <- xLim[1]:xLim[2]
    yLim <- c(0, max(c( Bt[idx], Bmsy, It[idx]/qOM, Bexp[idx], BexpS[idx] ), na.rm=TRUE ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  abline( v=tMP,  col=.tMPCOL,  lty=.tMPLTY,  lwd=.tMPLWD )  
  
  lines( c(1:nT), Bt,    col=.BtCOL,    lty=.BtLTY,    lwd=.BtLWD+1 )
  lines( c(1:nT), Bexp,  col=.BexpCOL,  lty=.BexpLTY,  lwd=.BexpLWD )
  lines( c(1:nT), BexpS, col=.BexpSCOL, lty=.BexpSLTY, lwd=.BexpSLWD )
    
  points( c(1:nT), It/qOM, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=0.5, cex=.CEXLAB4, outer=TRUE, "Year" )

  if ( gfx$doLegend )
  {
    panLegend( 0.1,0.975, legTxt=c(.BtLAB,.BexpLAB, .BexpSLAB, .BmsyLAB, .ItLAB),
            lty=c(.BtLTY,.BexpLTY, .BexpSLTY, .BmsyLTY, NA ),
            lwd=c(.BtLWD+1,.BexpLWD, .BexpSLWD, .BmsyLWD, NA),
            col=c(.BtCOL,.BexpCOL, .BexpSCOL, .BmsyCOL, .ItCOL ),
            pch=c(NA,NA,NA,NA,.ItPCH), pt.cex=c(NA,NA,NA,NA,.ItCEX),
            pt.bg=c(NA,NA,NA,NA,.ItBG), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
  }

  # Panel 2: Plot retrospective "stepwise" estimates.

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0, max(retroBt) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  
  if ( varName=="exploitBt" )
    lines( c(1:nT), Bexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )

  if ( varName=="exploitBtS" )
    lines( c(1:nT), BexpS, col=.BexpSCOL, lty=.BexpSLTY, lwd=.BexpSLWD )
    
  if ( varName=="spawnBt" )
    lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  # Add lines for all retrospective biomass states
  retroBt   <- retroBt[ retroBt[,1]==iRep, ]
  runStatus <- runStatus[ runStatus[,1]==iRep, ]
  
  for ( j in 1:nrow(retroBt) )
  {
    lines( retroBt[ j, 3:ncol(retroBt) ], col=.BtStepCOL, lty=.BtStepLTY, lwd=.BtStepLWD )  
    
    if ( runStatus$hessPosDef[j]==FALSE )
      lines( retroBt[ j, 3:ncol(retroBt) ], col="magenta", lty=.BtStepLTY, lwd=.BtStepLWD )
  
    if ( gfx$showProj )
    {
      points( tMP+j-1, retroBt[ j,tMP+j+2-1 ], bg="yellow", cex=.CEXSYM4, pch=21 )
      points( tMP+j-2, retroBt[ j,tMP+j+2-2 ], bg="black",  cex=.CEXSYM4, pch=21 )
    }
  }
  
  # Plot index scaled by terminal estimated q, there might not be a q in last element.
  qTemp <- qMP[ !is.na(qMP) ]
  points( c(1:nT), It/qTemp[length(qTemp)], bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )

  if ( gfx$doLegend )
  {
    if ( varName=="exploitBtS" )
    {
      panLegend( 0.1,0.975, legTxt=c(.BtLAB, .BexpSLAB, bioLAB ),
                 lty=c(.BtLTY,.BexpSLTY, bioLTY ), lwd=c(.BtLWD,.BexpSLWD,bioLWD),
                 col=c(.BtCOL,.BexpSCOL, bioCOL), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }
    
    if ( varName=="exploitBt" )
    {
      panLegend( 0.1,0.975, legTxt=c(.BtLAB, .BexpLAB, bioLAB ),
                 lty=c(.BtLTY,.BexpLTY, bioLTY ), lwd=c(.BtLWD,.BexpLWD,bioLWD),
                 col=c(.BtCOL,.BexpCOL, bioCOL), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }    
    
    if ( varName=="spawnBt" )
    {
      panLegend( 0.1,0.975, legTxt=c(.BtLAB, bioLAB ),
                 lty=c(.BtLTY, bioLTY ), lwd=c(.BtLWD, bioLWD),
                 col=c(.BtCOL, bioCOL), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }
            
    if ( gfx$showProj )
    {
      panLegend( 0.7, 0.9, legTxt=c("Terminal Estimate","Projection"),
                 pt.bg=c("black","yellow"), pt.cex=.CEXSYM4, pch=21, bg="white", cex=.CEXLEG, bty=.LEGBTY ) 
    }            
  }
  
  # Panel 3: Plot first and last retrospective "stepwise" fits

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0, max(retroBt) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  
  if ( varName=="exploitBt" )
    lines( c(1:nT), Bexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )

  if ( varName=="exploitBtS" )
    lines( c(1:nT), BexpS, col=.BexpSCOL, lty=.BexpSLTY, lwd=.BexpSLWD )  

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  # Add lines for all retrospective biomass states
  retroBt   <- retroBt[ retroBt[,1]==iRep, ]
  runStatus <- runStatus[ runStatus[,1]==iRep, ]
  
  iCount <- 3
  for ( j in c(1,nrow(retroBt)) )
  {
    iCount <- iCount - 1
    lines( retroBt[ j, 3:ncol(retroBt) ], col=.BtStepCOL, lty=iCount, lwd=.BtStepLWD )  
    
    if ( runStatus$hessPosDef[j]==FALSE )
      lines( retroBt[ j, 3:ncol(retroBt) ], col="magenta", lty=iCount, lwd=.BtStepLWD )
  
    if ( gfx$showProj )
    {
      points( tMP+j-1, retroBt[ j,tMP+j+2-1 ], bg="yellow", cex=.CEXSYM4, pch=21 )
      points( tMP+j-2, retroBt[ j,tMP+j+2-2 ], bg="black",  cex=.CEXSYM4, pch=21 )
    }
  }
  
  # Plot index scaled by terminal estimated q, there might not be a q in last element.
  qTemp <- qMP[ !is.na(qMP) ]
  points( c(1:nT), It/qTemp[length(qTemp)], bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )

  if ( gfx$doLegend )
  {
    if ( varName=="exploitBtS" )
    {
      panLegend( 0.1,0.975, legTxt=c(.BtLAB, .BexpSLAB,paste( "First",bioLAB),paste("Last",bioLAB) ),
                 lty=c(.BtLTY,.BexpSLTY, 2, bioLTY ),
                 lwd=c(.BtLWD,.BexpSLWD,bioLWD,bioLWD),
                 col=c(.BtCOL,.BexpSCOL, bioCOL,bioCOL), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }
    
    if ( varName=="exploitBt" )
    {
      panLegend( 0.1,0.975, legTxt=c(.BtLAB, .BexpLAB, paste( "First",bioLAB),paste("Last",bioLAB) ),
                 lty=c(.BtLTY,.BexpLTY, 2, bioLTY ),
                 lwd=c(.BtLWD,.BexpLWD,bioLWD,bioLWD),
                 col=c(.BtCOL,.BexpCOL, bioCOL,bioCOL), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }
    
    if ( varName=="spawnBt" )
    {
      panLegend( 0.1,0.975, legTxt=c(.BtLAB, paste( "First",bioLAB),paste("Last",bioLAB) ),
                 lty=c(.BtLTY, 2, bioLTY ),
                 lwd=c(.BtLWD, bioLWD, bioLWD),
                 col=c(.BtCOL, bioCOL, bioCOL), bg="white", cex=.CEXLEG, bty=.LEGBTY  )    
    }    
            
    if ( gfx$showProj )
    {
      panLegend( 0.7, 0.9, legTxt=c("Terminal Estimate","Projection"),
                 pt.bg=c("black","yellow"), pt.cex=.CEXSYM4, pch=21, bg="white", cex=.CEXLEG, bty=.LEGBTY ) 
    }            
  }      
  
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE,
         paste( "Biomass"," (",.BtUNIT,")", sep="" ) )
  
  mtext( side=3, line=0, cex=.CEXTITLE2, outer=TRUE, "Retrospective Patterns" )
    
}     # END function .plotRetroFits

#------------------------------------------------------------------------------#
#--  Convergence and ADMB plots.                                             --#
#------------------------------------------------------------------------------#

#.plotDiagnostics (plot annual parameter estimates from production model)                   
# Purpose:      Plot annual parameter estimates from stepwise model 
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09), Revised A.R. Kronlund (02-Aug-13)
.plotDiagnostics <- function( obj, iSim=1, iRep=1,
                      gfx=list( annotate=TRUE, doLegend=FALSE,
                      xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  runStatus <- obj$mp$assess$runStatus
  idx       <- runStatus$iRep == iRep
  nT        <- max( as.numeric( runStatus$tStep ) )
  tMP       <- obj$ctlList$opMod$tMP
  
  # Extract time step, maxGrad and iExit code for replicate.
  tStep   <- runStatus$tStep[ idx ]
  convT   <- as.numeric(runStatus$convT[ idx ])
  iExit   <- runStatus$iExit[ idx ]  
  maxGrad <- as.numeric(runStatus$maxGrad[ idx ])
  nEval   <- as.numeric(runStatus$nEval[ idx ])
  objFun  <- as.numeric( runStatus$objFun[ idx ] )
  
  deadFlag   <- as.logical(runStatus$deadFlag)
  fisheryClosed <- as.logical(runStatus$fisheryClosed)
  hessPosDef <- as.logical(runStatus$hessPosDef)
  
  tColor <- rep( "white", length(tStep) )
  tColor <- ifelse( iExit==0, .EXIT0COL, tColor )
  tColor <- ifelse( iExit==1, .EXIT1COL, tColor )
  tColor <- ifelse( iExit==2, .EXIT2COL, tColor )
  tColor <- ifelse( iExit==3, .EXIT3COL, tColor )
  
  # Plot maximum gradient, coloring the lines by iExit code.
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( tMP,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( c(0,max( maxGrad ),.MAXGRADCRIT ) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, maxGrad, col=tColor, type="h", lwd=3 )
  
  usr <- par( "usr" )
  yMid <- rep( (usr[4]-usr[3])/3.0, length(tStep) )
  
  if ( any(fisheryClosed==TRUE) )
  {
    points( tStep[fisheryClosed], yMid[fisheryClosed], bg="white", cex=4, pch=21 )  
    text( tStep[fisheryClosed],   yMid[fisheryClosed], "C", col="red", cex=1.5 )
  }

  if ( any(deadFlag)==TRUE )
  {
    points( tStep[deadFlag], yMid[deadFlag], bg="white", cex=4, pch=21 )  
    text( tStep[deadFlag],   yMid[deadFlag], "D", col="red", cex=1.5 )
  }
  
  yMid <- yMid * 2
  if ( any(!hessPosDef) )
  {
    points( tStep[!hessPosDef], yMid[!hessPosDef], bg="white", cex=4, pch=21 )
    text( tStep[!hessPosDef], yMid[!hessPosDef], "H", col="red", cex=1.5 )
  }
  
  abline( h=0, col="black", lty=3, lwd=2 )  
  abline( h=.MAXGRADCRIT, col="black", lty=2, lwd=2 )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS3, las=0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Max Gradient" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               pt.cex=c(.EXIT0COL,.EXIT1CEX,.EXIT2CEX,.EXIT3CEX), lwd=c(3,3,3,3),
               bg="white", bty=.LEGBTY )
  }
  
  # Plot function calls, coloring by Exit Code.
  
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( 0,max( c(.MAXFUNCALLS,nEval) ) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, nEval, col=tColor, type="h", lwd=3 )
  
  abline( h=0, col="black", lty=3, lwd=2 )
  abline( h=.MAXFUNCALLS, col="black", lty=2, lwd=2 )  

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS3, las=0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4,             "Fun Calls" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               lwd=c(3,3,3,3), bg="white", bty=.LEGBTY )
  }
  
  # Plot convergence time, coloring by Exit Code.
  
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( 0,max(convT) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, convT, col=tColor, type="h", lwd=3 )
  
  abline( h=0, col="black", lty=3, lwd=2 )  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=0 )
  box()

  mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3,  cex=.CEXLAB4,             "Converge Time" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               lwd=c(3,3,3,3), bg="white", bty=.LEGBTY )
  }
  
  # Plot objective function, coloring by Exit Code.
   
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( 0,min(objFun) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, objFun, col=tColor, type="h", lwd=3 )

  abline( h=0, col="black", lty=3, lwd=2 )
    
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS4, las=0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4,             "Objective Fun" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               lwd=c(3,3,3,3), bg="white", bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotDiagnostics


.plotDiagSim <-function( obj, gfx=list( annotate=TRUE, doLegend=FALSE,
                                        xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
#             code 0 = solution equals initial value
#             code 1 = normal exit, all derivatives satisfy conditions,
#             code 2 = error in the derivative
#             code 3 = maximum number of function calls exceeded)

  runStatus <- obj$mp$assess$runStatus
  nT        <- max( as.numeric(runStatus$tStep) )
  nReps     <- max( as.numeric(runStatus$iRep) )
  tMP       <- min( as.numeric(runStatus$tStep) )
  
  # Extract time step, maxGrad and iExit code for replicate.
  iRep    <- runStatus$iRep
  tStep   <- runStatus$tStep
  convT   <- runStatus$convT
  iExit   <- as.numeric(runStatus$iExit)  
  maxGrad <- as.numeric(runStatus$maxGrad)
  nEval   <- as.numeric(runStatus$nEval)
  objFun  <- runStatus$objFun

  fisheryClosed <- as.logical( runStatus$fisheryClosed )
  deadFlag      <- as.logical( runStatus$deadFlag )
  hessPosDef    <- as.logical( runStatus$hessPosDef )

  nSteps <- nrow( runStatus )

  cexVec <- rep( .EXIT1CEX, nSteps )
  symVec <- rep( 21, nSteps )
  
  nSteps <- nrow( runStatus )
  
  # Exit code status.
  exitColor <- rep( .EXIT1COL, nSteps )
  exitColor <- ifelse( iExit==0, .EXIT0COL, exitColor )
  exitColor <- ifelse( iExit==2, .EXIT2COL, exitColor )
  exitColor <- ifelse( iExit==3, .EXIT3COL, exitColor )

  bgCol  <- rep( .EXIT1COL, nSteps )
  bgCol <- ifelse( iExit==0, .EXIT0COL, bgCol )  
  bgCol <- ifelse( iExit==2, .EXIT2COL, bgCol )
  bgCol <- ifelse( iExit==3, .EXIT3COL, bgCol )

  colVec <- bgCol
  fgVec  <- bgCol
 
  # Gradient maximum exceeded?
  gradProblem <- maxGrad > .MAXGRADCRIT
  cexVec[ gradProblem ] <- .CEXANNO2  
  colVec[ gradProblem ] <- "red"
  symVec[ gradProblem ] <- 71     # ASCII number 71 is "G".
  
  cexVec[ fisheryClosed ] <- .CEXANNO2
  symVec[ fisheryClosed ] <- 67     # ASCII number 68 is "D", 67 id "C".

  cexVec[ deadFlag ] <- .CEXANNO2
  symVec[ deadFlag ] <- 68     # ASCII number 68 is "D", 67 id "C".
  
  cexVec[ !hessPosDef ] <- .CEXANNO2
  symVec[ !hessPosDef ] <- 72     # ASCII number 72 is "H".
  
  idx <- fisheryClosed * !hessPosDef 
  cexVec[ idx ] <- .CEXANNO2
  symVec[ idx ] <- 88    # ASCII number 88 is "X".
  
  idx <- deadFlag * !hessPosDef
  cexVec[ idx ] <- .CEXANNO2
  symVec[ idx ] <- 89    # ASCII number 89 is "Y".
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( tMP,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c(0.9,nReps+0.1 )

  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  
  # (1) If there are no problems, then plot a green.
  # (2) If there are Exit code problems plot symbol in color corresponding to
  #     the exit code.
  # (3) If there are gradient problems plot "G" - use exit code color.
  # (4) If model parameter threshold problems plot "F" for failure in exit color.
  
  # tStep and iRep are vectors so that all combinations of tStep and iRep are
  # plotted at once.

  points( tStep, iRep, bg=bgCol, pch=symVec, cex=cexVec, col=fgVec )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, at=c(1:nReps), cex.axis=.CEXAXIS2, labels=c(1:nReps), las=.YAXISLAS )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Replicate" )

  #gfx$doLegend <- FALSE
  if ( gfx$doLegend )
  {
    panLegend( 0.5,0.5, xjust=0.5, ncol=2,
               legTxt=c( "E0: Stalled", "E1: Clean", "E2: Bad derivative",
                         "E3: Max funcalls",
                         paste(": maxGrad >",.MAXGRADCRIT), ": CLOSED",
                         ": Hessian NPD", ": CLOSED and HESSIAN",
                         ": DEAD", ": DEAD and HESSIAN"
                        ),
               pt.bg=c( .EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL,"red","blue","black","black", "black"),
               pch=c( 21, 21, 21, 21, 71, 67, 72, 88, 68,89 ),
               pt.cex=c( rep(.EXIT1CEX,4), rep(.CEXANNO2,6) ),
               bg="white", cex=.CEXLEG, bty=.LEGBTY )
  }
  return( invisible() )  
}     # END function .plotDiagSim

#------------------------------------------------------------------------------#
#-- Perf Plotting Functions:                                                 --#
#------------------------------------------------------------------------------#

.plotObjectives <- function( obj,
                             depCEX=1,  depCOL="red",    depLTY=2,  depLWD=2,
                             probCEX=1, probCOL="black", probLTY=2, probLWD=2,
                             yrCEX=1,   yrCOL="blue",    yrLTY=2,   yrLWD=2 )
{
  usr <- par( "usr" )
  
  if ( obj$outcome=="dep" )
  {
    segments( obj$objYear, usr[3], obj$objYear, obj$objDep,
              col=depCOL, lty=depLTY, lwd=depLWD )
    .addTarget( obj$objYear, obj$objDep, cexCenter=depCEX, colCenter=depCOL,
                colRing=depCOL )
     text( obj$objYear+0.5, usr[3]+0.065, adj=0, cex=.CEXANNO, col=depCOL,
          paste( "D:",round(obj$objDep,digits=3),sep="" ) )        
  }
  else if ( obj$outcome=="year" )
  {
    abline( h=obj$objDep, lty=depLTY, lwd=depLWD )
    segments( obj$objYear, usr[3], obj$objYear, obj$objDep,
              col=yrCOL, lty=yrLTY, lwd=yrLWD )
    .addTarget( obj$objYear, obj$objDep, cexCenter=yrCEX, colCenter=yrCOL,
                colRing=yrCOL )
    text( obj$objYear+0.5, usr[3]+0.025, adj=0, cex=.CEXANNO, col=yrCOL,
          paste( "Y:",obj$objYear,sep="" ) )
  }
  else if ( obj$outcome=="prob" )
  {
    .addTarget( obj$objYear, obj$objDep, cexCenter=probCEX, colCenter=probCOL,
                colRing=probCOL )
    text( obj$objYear+0.5, usr[3]+0.105, adj=0, cex=.CEXANNO, col=probCOL,
          paste( "C:",round(obj$objProb,digits=3),sep="" ) )
  }
  return( invisible() )
}     # END function .plotObjectives


.plotStrategy <- function( obj, base="Bmsy", phase=FALSE,
   mults=list( pfLimitMultBmsy=0.4, pfUpperMultBmsy=0.8, pfTargetMultBmsy=1.0,
               pfLimitMultB0=0.2, pfUpperMultB0=0.35, pfTargetMultB0=0.4 ),
               gfx=list( annotate=TRUE, doColors=TRUE, doLegend=TRUE, grids=FALSE,
                         image=FALSE, xLim=NULL, yLim=NULL ),... )
{
  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tProj <- c(tMP:nT)

  # Get spawning biomass and fishing mortality for all replicates.
  Bt <- obj$om$Bt[ ,(2:ncol(obj$om$Bt)), drop=FALSE ]
  Ft <- obj$om$Ft[ ,(2:ncol(obj$om$Ft)), drop=FALSE ]
    
  B0   <- obj$refPtList$B0
  Bmsy <- obj$refPtList$ssbFmsy
  Fmsy <- obj$refPtList$Fmsy
  
  # Get the status-based reference points on the operating model scale.
  if ( base=="Bmsy" )
  {
    baseVal <- Bmsy
    LRP <- mults$pfLimitMultBmsy  * Bmsy
    USR <- mults$pfUpperMultBmsy  * Bmsy
    TRP <- mults$pfTargetMultBmsy * Bmsy
  }
  
  if (  base=="B0" )
  {
    baseVal <- B0
    LRP <- mults$pfLimitMultB0  * B0
    USR <- mults$pfUpperMultB0  * B0
    TRP <- mults$pfTargetMultB0 * B0
  }

  # Get the Fishing mortality at MSY from the operating model.
  Fmsy <- obj$refPtList$Fmsy

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # Subset the SSB and F from the projection period.
  Bt <- Bt[ ,tProj, drop=FALSE ]
  Ft <- Ft[ ,tProj, drop=FALSE ]
  
  if ( phase )
  {
    Bt  <- Bt / baseVal
    Ft  <- Ft / baseVal
    LRP <- LRP / baseVal
    USR <- USR / baseVal
    TRP <- TRP / baseVal
    
    if ( is.null(xLim) )
      xLim <- range( c(0,Bt,1) )

    if ( is.null(yLim) )
      yLim <- range( c(0,Ft,1) )    
  }
  else
  {
    if ( is.null(xLim) )
      xLim <- range( c(0,Bt,Bmsy) )

    if ( is.null(yLim) )
      yLim <- range( c(0,Ft,Fmsy) )
  }
  
  xLim[2] <- xLim[2]*1.1
  yLim[2] <- yLim[2]*1.1

  plot( xLim, yLim, type="n", axes=FALSE, xaxs="i", xlab="", yaxs="i", ylab="")

  if ( !gfx$doColors )
  {
    # Image the points.
    xBrks <- seq( 0,100,1 )
    yBrks <- seq( 0,5,0.005 )
    
    if ( phase )
    {
      xBrks <- seq( 0, 3, 0.05 )
      yBrks <- seq( 0, 3, 0.05 )
    }
  
    xCuts  <- cut( Bt, breaks=xBrks )
    yCuts  <- cut( Ft, breaks=yBrks )
    counts <- table( xCuts, yCuts )  
    #image(  xBrks, yBrks, counts, col=rev(heat.colors(64)), add=TRUE,axes = FALSE)
    #contour( xBrks, yBrks, counts, add = TRUE )
    
    x <- cbind( Bt, Ft )
    #est <- bkde2D( x, bandwidth=c(1,0.01), range.x=list( c(0,max(Bt)),c(0,max(Ft)) ) )
    #est <- bkde2D( x, bandwidth=c(20,0.5) )    
    #contour( est$x1, est$x2, est$fhat, levels=c(0.1,0.25,0.5,0.75,0.9) )
  
    #for ( i in 1:nrow(Bt) )
    #  points( Bt[i,],Ft[i,], pch=21, bg="black", col="black", cex=0.2 )
  }
  else
  {
    # Add DFO zones and points.
    usr <- par( "usr" )
    rect(   0, 0,    LRP, usr[4], border=NA, col=.CriticalCOL )
    rect( LRP, 0,    USR, usr[4], border=NA, col=.CautiousCOL )
    rect( USR, 0, usr[2], usr[4], border=NA, col=.HealthyCOL )    

    for ( i in 1:nrow(Bt) )
      points( Bt[i,],Ft[i,], pch=21, bg="lightgray", cex=0.8 )
  }
  
  # Add points at quantiles of Bt and Ft.
  quantsBt <- quantile( Bt, probs=c(0.10,0.25,0.5,0.75,0.90) )
  quantsFt <- quantile( Ft, probs=c(0.10,0.25,0.5,0.75,0.90) )
  #points( quantsBt, quantsFt, bg="red", col="red", cex=.CEXSYM20, pch=21 )

  segments( quantsBt[1],quantsFt[3],quantsBt[5],quantsFt[3], col="red", lwd=3 ) 
  segments( quantsBt[3],quantsFt[1],quantsBt[3],quantsFt[5], col="red", lwd=3 )
  points( quantsBt[3], quantsFt[3], bg="red", col="red", cex=.CEXSYM20, pch=21 )

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=2, labels=FALSE, cex.axis=.CEXAXIS2, las=.YAXISLAS )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=4, labels=FALSE, cex.axis=.CEXAXIS2, las=.YAXISLAS )

  if ( phase )
  {
    abline( h=1, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD  )
    abline( v=1, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD  )
    .addTarget( 1, 1, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )
  }
  else
  {
    abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD  )
    abline( v=TRP,  col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD  )
    .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }
  
  abline( v=LRP, col=.LrpCOL, lty=.LrpLTY, lwd=.LrpLWD )
  abline( v=USR, col=.UsrCOL, lty=.UsrLTY, lwd=.UsrLWD )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
    abline( h=seq(yLim[1], yLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
  }

  if ( gfx$annotate )
  {
    usr <- par( "usr" )
    zoneMidPts <-c( LRP/2.0, ((USR-LRP)/2.0)+LRP, ((usr[2]-USR)/2.0)+USR )
    xPos <- zoneMidPts / usr[2]
    
    fMidPts <- c( Fmsy/2.0, (usr[4]-Fmsy)/2.0 + Fmsy )
    yPos    <- fMidPts / usr[4]

    # Note that Bt and Ft were restricted to projection period  using tProj.
    
    # Stock Status: Calculate number and proportion in each zone from tMP:nT.
    
    nZone <- table( cut( Bt, breaks=c(0,LRP,USR,max(Bt) ) ) )
    pZone <- nZone / sum( nZone )

    zoneLabels <- c( .CriticalLAB, .CautiousLAB, .HealthyLAB )
    panLab( xPos, 0.8, adj=0.5, zoneLabels )
    panLab( xPos, 0.875, adj=0.5, cex=.CEXANNO2, col="blue",
            format( round( pZone,2 ), nsmall=2 ) )

    # Overfishing vs. Not overfishing.
    nZone <- table( cut( Ft, breaks=c(0,Fmsy,max(Ft)) ) )
    pZone <- nZone / sum( nZone )
    pZone <- format( round( pZone,2 ), nsmall=2 )
    zoneLabels <- c( "Not Overfishing","Overfishing" )
    panLab( 0.9, Fmsy/usr[4], cex=.CEXANNO2, col="red", pos=1, paste( pZone[1] ) )
    panLab( 0.9, Fmsy/usr[4], cex=.CEXANNO2, col="red", pos=3, paste( pZone[2] ) )
    
    # Overfished vs. not overfished.
    nZone <- table( cut( Bt, breaks=c(0,TRP,max(Bt)) ) )
    pZone <- nZone / sum( nZone )
    pZone <- format( round( pZone,2 ), nsmall=2 )
    zoneLabels <- c( "Overfished","Not Overfished" )
    panLab( TRP/usr[2], 0.1, cex=.CEXANNO2, col="magenta", pos=2, paste( pZone[1] ) )
    panLab( TRP/usr[2], 0.1, cex=.CEXANNO2, col="magenta", pos=4, paste( pZone[2] ) )    
    
    # Each of the 6 zones created by LRP, USR, and Reference Removal Rate.
    nZone <- table( cut( Bt, breaks=c(0,LRP,USR,max(Bt) ) ),
                    cut( Ft, breaks=c(0,Fmsy,max(Ft)) ) )
    pZone <- nZone / sum( nZone )
    pZone <- format( round( pZone,2 ), nsmall=2 )
    panLab( xPos, yPos[1], pZone[,1], cex=.CEXANNO2, col="darkgreen" )
    panLab( xPos, yPos[2], pZone[,2], cex=.CEXANNO2, col="darkgreen" )
  }

  if ( gfx$doLegend )
  {
  }

  usr <- par( "usr" )

  box()
  return( invisible() )
}     # END function .plotStrategy

#-----------------------------------------------------------------------------##
#-- Level 3 mseR Peformance GUI Plotting Functions                          --##
#-----------------------------------------------------------------------------##

# .plotBarsByPeriod  (barplots showing distribution of specified stat by period)                   
# Purpose:      Produces a bar plot showing the median and quanitile values for 
#                the specified performance statistic (depletion, catch or AAV) 
#               for each of three different time periods (short, medium, long)
# Parameters:   A stats dataframe that inlcudes previously calculated median 
#                and quanitile values for performance statistics to be plotted.  
#                This dataframe in compiled in ".doBarplots" function. 
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBarsByPeriod <- function( stats, xvars=c("depletion","catch","aav"), 
                        refPoints=NULL, gfx=list( xLim=NULL, yLim=NULL ) )
{
  # This plot treats the periods as columns, variables as rows so that we have
  # to plot the simulations by variable within period.

  simList    <- unique( stats$simLabel )
  periodList <- unique( stats$period   )

  # Loop over the summary periods.
  for ( i in 1:length(periodList) )
  {
    # Extract the statistics for period i.
    idx <- stats$period==periodList[i]
    simStats <- stats[ idx, ]
    
    # Loop over the x-variables.
    for ( j in 1:length(xvars) )
    {
      # Get the y-values for the plot, which are categorical, then reverse them.
      yVals <- c( 1:nrow(simStats) )
      yVals <- rev(yVals)
      y.Lim <- range( yVals )
      
      if ( length(xvars)==1 )
        x.Lim <- gfx$xLim
      else
        x.Lim <- gfx$xLim[j,]
        
      if ( is.null(x.Lim) )
      {
        # Determine the limits of the x-axis.
        if ( xvars[j]=="depletion" )
         x.Lim <- range( stats[,c("Q1finalDep","Q2finalDep")] )
        else if ( xvars[j]=="catch" )
          x.Lim <- range( stats[,c("Q1AvgCatch","Q2AvgCatch")] )
        else if ( xvars[j]=="AAV" )
          x.Lim <- range( stats[,c("Q1AAV","Q2AAV")] )
      }

      # Set up the plot panel region.
      plot( x.Lim,y.Lim, type="n", axes=F, xlab="", ylab="" )

      # Plot the relevant points and line segments.
      if ( xvars[j]=="depletion" )
      {
        # ARK: Aggregate statistics removed at request of Cox 02-Mar-09.
        #points( simStats$medAvgDep, yVals, cex=.PERFCEX, pch=.PERFSYM )
        #segments( simStats$Q1AvgDep, yVals, simStats$Q2AvgDep, yVals )
        #xLabel <- "Depletion"

        points( simStats$medFinalDep, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1finalDep, yVals, simStats$Q2finalDep, yVals )
        xLabel <- "Final Depletion"
      }
      else if ( xvars[j]=="catch" )
      {
        points( simStats$medAvgCatch, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AvgCatch, yVals, simStats$Q2AvgCatch, yVals )
        xLabel <- "Catch"
      }
      else if ( xvars[j]=="AAV" )
      {
        points( simStats$medAAV, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AAV, yVals, simStats$Q2AAV, yVals )
        xLabel <- "AAV"
      }

      mfg <- par( "mfg" )
      if ( mfg[2]==mfg[4] )
        if ( length(xvars)==1 )
          mtext( side=1, line=.INLINE, cex=.CEXLAB4, outer=TRUE, xLabel )
        else
          mtext( side=4, line=.INLINE, cex=.CEXLAB4, xLabel )

      if ( mfg[2]==1 )
        axis( side=2, at=yVals, cex.axis=.CEXAXIS4, las=.YAXISLAS, substring(simList,1,12) )

      axis( side=1, cex.axis=.CEXAXIS4 )
      axis( side=3, labels=FALSE )
      box()

      # Add the period label.
      t1 <- unique( simStats[,"t1" ] )
      t2 <- unique( simStats[,"t2" ] )
      mtext( side=3, line=.INLINE, cex=.CEXLAB2,
        paste( periodList[i]," (",t1,",",t2,")", sep="") )

    }     # Loop over j x-variables.
  }     # Loop over i periods.
   
  return( invisible() )
}     # END function .plotBarsByPeriod


# .plotBarsByStats  (barplots showing all 3 perf statistics in specified period)                   
# Purpose:      Produces a bar plot showing the median and quanitile values for 
#                three performance statistics (depletion, catch and AAV) 
#                calculated over the specified time period
# Parameters:   A stats dataframe that inlcudes previously calculated median 
#                and quanitile values for performance statistics to be plotted.
#                This dataframe in compiled in ".doBarplots" function.  
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBarsByStats <- function( stats, periodList=c("Short","Medium","Long"), 
        refPoints=NULL, gfx=list( xLim=NULL, yLim=NULL ) )
{
  # This plot treats the periods as rows, variables as columns so that we have
  # to plot the simulations by period within variable.

  simList <- unique( stats$simLabel )
  xvars   <- c( "depletion","catch","AAV" )

  # Loop over the summary periods.
  for ( i in 1:length(periodList) )
  {
    # Extract the statistics for period i.
    idx <- stats$period==periodList[i]
    simStats <- stats[ idx, ]

    # Loop over the x-variables.
    for ( j in 1:length(xvars) )
    {
      # Get the y-values for the plot, which are categorical, then reverse them.
      yVals <- c( 1:nrow(simStats) )
      yVals <- rev(yVals)
      y.Lim  <- range( yVals )

      if ( length(xvars)==1 )
        x.Lim <- gfx$xLim
      else
        x.Lim <- gfx$xLim[j,]
        
      if ( is.null(x.Lim) )
      {
        # Determine the limits of the x-axis.
        if ( xvars[j]=="depletion" )
          x.Lim <- range( stats[,c("Q1finalDep","Q2finalDep")] )
        else if ( xvars[j]=="catch" )
          x.Lim <- range( stats[,c("Q1AvgCatch","Q2AvgCatch")] )
        else if ( xvars[j]=="AAV" )
          x.Lim <- range( stats[,c("Q1AAV","Q2AAV")] )
      }

      # Set up the plot panel region.
      plot( x.Lim,y.Lim, type="n", axes=FALSE, xlab="", ylab="" )

      # Plot the relevant points and line segments.
      if ( xvars[j]=="depletion" )
      {
        # ARK: Aggregate by period removed at request of Cox 02-Mar-09.
        #points( simStats$medAvgDep, yVals, cex=.PERFCEX, pch=.PERFSYM )
        #segments( simStats$Q1AvgDep, yVals, simStats$Q2AvgDep, yVals )
        #xLabel <- "Depletion"

        # Final depletion at end of period.
        points( simStats$medFinalDep, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1finalDep, yVals, simStats$Q2finalDep, yVals )
        xLabel <- "Final Depletion"
      }
      else if ( xvars[j]=="catch" )
      {
        points( simStats$medAvgCatch, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AvgCatch, yVals, simStats$Q2AvgCatch, yVals )
        xLabel <- "Catch"
      }
      else if ( xvars[j]=="AAV" )
      {
        points( simStats$medAAV, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AAV, yVals, simStats$Q2AAV, yVals )
        xLabel <- "AAV"
      }

      mfg <- par( "mfg" )
      if ( mfg[1]==mfg[3] )
        mtext( side=1, line=3, cex=.CEXLAB4, xLabel )

      if ( mfg[2]==1 )
        axis( side=2, at=yVals, cex.axis=.CEXAXIS4, las=.YAXISLAS, substring(simList,1,12) )

      axis( side=1, cex.axis=.CEXAXIS4 )
      axis( side=3, labels=FALSE )
      box()

      # Add the period label.
      if ( mfg[2]==mfg[4] )
      {
        t1 <- unique( simStats[,"t1" ] )
        t2 <- unique( simStats[,"t2" ] )
        if ( length( periodList)==1 )
          mtext( side=3, line=.OUTLINE-1, cex=.CEXLAB4, outer=TRUE,
            paste( periodList[i]," (",t1,",",t2,")", sep="") )
        else
          mtext( side=4, line=.INLINE,  cex=.CEXLAB4,
            paste( periodList[i]," (",t1,",",t2,")", sep="") )
      }
    }     # Loop over j x-variables.
  }     # Loop over i periods.
  return( invisible() )
}     # END function .plotBarsByStats

# .plotBxpStatus  (boxplots of depletion relative to status zones)                   
# Purpose:      Produces boxplots of depletion values from operating model (reps
#                 and years combined) for each of three time periods, and shows
#                  status zones  
# Parameters:  An object containing the nRep by 1:nT matrix of depletions 
#                from the OM. 
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBxpStatus <- function( obj, period, statusBase="statusBaseBmsy", 
                    quantProbs=c(0.05,0.1,0.5,0.9,0.95),
                    yLabel="",
                    gfx=list( annotate=TRUE, doColors=TRUE, doLegend=TRUE,
                              grids=FALSE, xLim=NULL, yLim=NULL ) )
{
  nSim     <- length(obj)
  simLabel <- character( nSim )
  
  xLim <- c( (1-0.5),nSim+0.5 )
  
  yLim <- gfx$yLim   
  if ( is.null(yLim) )
  {
    yLim <- c(0,0.01)
    for ( i in 1:nSim )
      yLim[2] <- max( yLim[2], max(obj[[i]][,c(8:ncol(obj[[i]]))] ) )
    yLim[2] <- yLim[2] * 1.1
  }

  iCol <- 15
  # Loop over the periods.
  for ( i in 1:nrow(period) )
  {
    plot(xLim, yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="", yaxs="i")

    # Get the plot region user coordinate limits.
    usr <- par( "usr" )

    # Loop over simList...
    for ( j in c(1:length(obj)) )
    {
      tmp <- obj[[j]][ period==period$pName[i], ]
      simLabel[j] <- tmp$simLabel

      if ( statusBase == "statusBaseBmsy" )
      {  
        limitRef  <- tmp$lrpBmsy
        upperRef  <- tmp$usrBmsy
        targetRef <- tmp$trpBmsy
      }
      
      if ( statusBase == "statusBaseB0" )
      {
        limitRef  <- tmp$lrpB0
        upperRef  <- tmp$usrB0
        targetRef <- tmp$trpB0
      }

      # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
      x1 <- j - 0.5
      x2 <- j + 0.5

      # Colour the Critical, Cautious and Healthy Zones.
      # Note these are done based on the Status Zones, not the HCR ref points.
      if ( gfx$doColors )
      {
        rect( x1,        0, x2, limitRef, border=NA, col=.CriticalCOL )
        rect( x1, limitRef, x2, upperRef, border=NA, col=.CautiousCOL )
        rect( x1, upperRef, x2,   usr[4], border=NA, col=.HealthyCOL )
      }

      yVals <- as.numeric( tmp[ c(iCol:ncol(tmp) ) ] )

      # Now lay down the median, 25th, and 75th quantiles.
      delta <- 0.25
      
      quantVals <- quantile( yVals, probs=quantProbs, na.rm=TRUE )
      rect( j-delta, quantVals[2], j+delta,quantVals[4] )
      medVal <- median(yVals)
 
      segments( j-delta, medVal, j+delta, medVal, lty=1, lwd=2 )

      points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
               pch=21, bg="white", cex=.CEXSYM24 )
      #points( j, targetRef, pch=21, bg="red", cex=.CEXSYM24 )
      .addTarget( j, targetRef, cexCenter=1.4, colCenter="red" )

      mtext( side=3, line=.INLINE, cex=.CEXLAB2,
             paste( tmp$period," (",tmp$t1,",",tmp$t2,")", sep="" ) )

    }     # Loop j over simulations.
    
    axis( side=1, at=c(1:nSim), cex.axis=.CEXAXIS4, labels=simLabel )
    axis( side=2, las=.YAXISLAS, cex.axis=.CEXAXIS4 )
    axis( side=3, labels=F )
    axis( side=4, las=.YAXISLAS, cex.axis=.CEXAXIS4 )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, yLabel )
    box()
  }     # Loop i over periods.
}     # END function .plotBxpStatus

# .plotBxpFmort  (boxplots of depletion relative to status zones)                   
# Purpose:      Produces boxplots of depletion values from operating model (reps
#                 and years combined) for each of three time periods, and shows
#                  status zones  
# Parameters:  An object containing the nRep by 1:nT matrix of depletions 
#                from the OM. 
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBxpFmort <- function( obj, period, quantProbs=c(0.05,0.1,0.5,0.9,0.95),
                    yLabel="",
                    gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                              xLim=NULL, yLim=NULL ) )
{
  nSim     <- length(obj)
  simLabel <- character( nSim )
  
  xLim <- c( (1-0.5),nSim+0.5 )
  
  yLim <- gfx$yLim   
  if ( is.null(yLim) )
  {
    yLim <- c(0,0.1)
    for ( i in 1:nSim )
      yLim[2] <- max( yLim[2], max(obj[[i]][,c(14:ncol(obj[[i]]))] ) )
    yLim[2] <- yLim[2] * 1.1
  }

  iCol <- 15

  # Loop over the periods.
  for ( i in 1:nrow(period) )
  {
    plot(xLim, yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="", yaxs="i")

    # Get the plot region user coordinate limits.
    usr <- par( "usr" )

    # Loop over simList...
    for ( j in c(1:length(obj)) )
    {
      tmp <- obj[[j]][ period==period$pName[i], ]
      simLabel[j] <- tmp$simLabel

      # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
      x1 <- j - 0.5
      x2 <- j + 0.5

      Fmsy  <- tmp$Fmsy
      segments( x1,Fmsy,x2,Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )

      # Now lay down the median, 25th, and 75th quantiles.
      delta <- 0.25

      yVals <- as.numeric( tmp[ c(iCol:ncol(tmp) ) ] )     
      quantVals <- quantile( yVals, probs=quantProbs, na.rm=TRUE )

      rect( j-delta, quantVals[2], j+delta,quantVals[4] )
      medVal <- median(yVals)
 
      segments( j-delta, medVal, j+delta, medVal, lty=1, lwd=2 )

      points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
               pch=21, bg="white", cex=.CEXSYM24 )
      #points( j, Fmsy, pch=21, bg="red", cex=.CEXSYM24 )
      .addTarget( j, Fmsy, cexCenter=1.4, colCenter="red" )

      if ( gfx$annotate )
      {
        # To JAC from ARK (22-Oct-13).
        abline( h=0.22, col="black", lty=3, lwd=1 )
      }

      mtext( side=3, line=.INLINE, cex=.CEXLAB2,
             paste( tmp$period," (",tmp$t1,",",tmp$t2,")", sep="" ) )  
 
    }     # Loop j over simulations.

    axis( side=1, at=c(1:nSim), cex.axis=.CEXAXIS4, labels=simLabel )
    axis( side=2, las=.YAXISLAS,     cex.axis=.CEXAXIS4 )
    axis( side=3, labels=F )
    axis( side=4, las=.YAXISLAS,     cex.axis=.CEXAXIS4 )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, yLabel )
    box()
  }     # Loop i over periods.
  
  return( invisible() )
}     # END function .plotBxpFmort

                 
# .plotQboxDep  (boxplots of spawning biomass relative to status zones)                   
# Purpose:      Produces boxplots of ssb values from operating model (reps
#                 and years combined) for each of three time periods, and shows
#                  status zones  
# Parameters:  An object containing the nRep by 1:nT matrix of SSB  from the OM.
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
# Note:        Not currently an option in guiSim window (K. Holt, 25-Nov-09)
.plotQboxSSB <- function( ssbObj, obj, period,
                    quantProbs=c(0.05,0.1,0.5,0.9,0.95), yLim, fixY )
{
     # ssbObj: the nRep by 1:nT matrix of SSB from the OM.
    xLim <- c( (1-0.5),(length(unique(obj$simLabel) )+0.5) )

    if (fixY == TRUE) 
       yLim <- c( 0,max(ssbObj) )
    if (fixY == FALSE)
      yLim <- yLim
      

     par( oma=c(2,3,2,2), mar=c(3,2,2,2), mfrow=c(nrow(period),1) )

     nReps <- nrow( ssbObj )

     # Loop over the periods.
     for ( i in 1:nrow(period) )
     {
       tmp <- obj[ obj$period==period$pName[i], ]
       t1  <- period$t1[ i ]
       t2  <- period$t2[ i ]
       tdx <- c( t1:t2 )

       plot( xLim,yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="" )

       # Get the plot region user coordinate limits.
       usr <- par( "usr" )

       for ( j in 1:nrow(tmp) )
       {
         Dmsy      <- tmp$Dmsy[ j ]
         zoneLimit <- tmp$zoneLimit[ j ]
         zoneUpper <- tmp$zoneUpper[ j ]

         # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
         x1 <- j - 0.5
         x2 <- j + 0.5

         # Colour the Critical, Cautious and Healthy Zones.
         # Note these are done based on the Status Zones, not the HCR ref points.
         colorZones <- TRUE
         if ( colorZones )
         {
           rect( x1,         0, x2, zoneLimit, border=NA, col=.CriticalBG )
           rect( x1, zoneLimit, x2, zoneUpper, border=NA, col=.CautiousBG )
           rect( x1, zoneUpper, x2,    usr[4], border=NA, col=.HealthyBG )
         }

         yVals <- ssbObj[ ,c(t1:t2) ]

         # Now lay down the median, 25th, and 75th quantiles.
         delta <- 0.25
         quantVals <- quantile( yVals, probs=quantProbs )
         rect( j-delta, quantVals[2], j+delta,quantVals[4] )
         medVal <- median(yVals)
         segments( j-delta, medVal, j+delta, medVal, lty=1, lwd=2 )

         points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
                  pch=21, bg="white", cex=1 )
         points( j, Dmsy, pch=21, bg="red", cex=2.0 )

         mtext( side=3, line=1, cex=1.0,
           paste( tmp$period[j]," (",tmp$t1[j],",",tmp$t2[j],")", sep="" ) )

       }     # Loop j over simulations.

       axis( side=1, at=c(1:nrow(tmp)), labels=tmp$simLabel )
       axis( side=2, las=2, cex.axis=1 )
       axis( side=3, labels=F )
       axis( side=4, las=2, cex.axis=1 )
       mtext( side=2, line=1, cex=1.2, outer=TRUE, "Depletion" )
       box()
       
     }     # Loop i over periods.
}     # .plotQboxSSB


# .plotTulipCatch (tulip simulation envelope for catch)
# Purpose:        Display tulip (simulation envelope) catch for one simulation.
# Parameters:     obj is the object containing the Rdata blob list.
#                 Allow percentiles to be specified.
# Returns:        NULL (invisibly)
# Source:         A.R. Kronlund
.plotTulipCatch <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                             xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                             allQuants=TRUE,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                                 showProj=pfProj, xLim=xLim, yLim=yLim, useYears=FALSE ),... )
{
  nReps <- obj$ctlList$gui$nReps
  Dt    <- obj$om$Dt[ ,(2:ncol(obj$om$Dt)) ]
  MSY   <- obj$refPtList$yieldFmsy

  # Time indices (could use obj$par$nT).
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tVec  <- c(1:ncol(Dt))

  # Specify axes for plot
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Dt[ ,tdx ], na.rm=TRUE )
  }
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
 # else
  #  #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
   # .addXaxis( xLim, initYear=.INITYEAR, side=3, years=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=3, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#  else
#    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

  axis( side=4, labels=FALSE )
  
  if ( gfx$grids )
  {
    abline( v=seq( xLim[1],  xLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
    abline( h=seq(yLim[1], yLim[2], length=10),   col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  quants <- apply( Dt, 2, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], lwd=.TULQLWD, col=.TULQCOL, lty=.TULQLTY )
    lines( tVec,quants[4,], lwd=.TULQLWD, col=.TULQCOL, lty=.TULQLTY )
  }
 
  usr <- par( "usr" )

  if ( !is.null(MSY) && refPts )
  {
    urs <- par( "usr" )
    points( c(usr[1],usr[2]), c(MSY,MSY), xpd=T, bg=.MSYBG, col=.MSYCOL, cex=.MSYCEX, pch=.MSYPCH )
  }
  
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Dt[i,], col="black", lty=1, lwd=1 )
  }
  box()
  return( invisible() )
}     # END function .plotTulipCatch


# .plotTulipDepletion (tulip simulation envelope for depletion)
# Purpose:            Display tulip (envelope) depletion for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund
.plotTulipDepletion <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                                 xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                                 allQuants=FALSE,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                                 showProj=pfProj, xLim=xLim, yLim=yLim, useYears=FALSE ),... )                                 
{
  # Get the spawning biomass and number of replicates.
  Bt    <- obj$om$Bt[ ,c(2:ncol(obj$om$Bt)), drop=FALSE ]
  nReps <- nrow( Bt )
  
  # Extract B0 and Bmsy for depletion calculations.
  B0     <- obj$ctlList$opMod$B0
  Bmsy   <- obj$refPtList$ssbFmsy

  # Convert MSY and Bt to depletion scale.
  depMSY <- Bmsy / B0
  Dept   <- Bt   / B0

  # Time indices.
  tMP  <- obj$ctlList$opMod$tMP
  nT   <- obj$ctlList$opMod$nT
  tVec <- c(1:ncol(Dept))

  # Specify axis limits for plotting
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Dept[ ,tdx ], na.rm=TRUE )
  }
  
  # Build the plot area.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
 # else
  #  #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
   # .addXaxis( xLim, initYear=.INITYEAR, side=3, years=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=3, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#  else
#    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

  # Y-axis (left).
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

  axis( side=4, labels=FALSE )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  quants <- apply( Dept, 2, quantile, probs=qProbs )

  # Plot the shaded envelope.
  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }

  if( .NONSTATB0 )
  {
    lines( tVec[tMP:nT], 0.25*quants[3,tMP:nT], lwd=2, lty=1, col="blue" )
  }
  
  box()

  usr <- par( "usr" )
  if ( !is.null(depMSY) && refPts )
  {
    urs <- par( "usr" )
    points( c(usr[1],usr[2]), c(depMSY,depMSY), xpd=T, bg=.BmsyBG, cex=.BmsyCEX, col=.BmsyCOL, pch=.BmsyPCH )
  }

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Dept[i,] )
  }

  return( invisible() )
}     # END function .plotTulipDepletion


# .plotTulipF        (tulip simulation envelope for fishing mortality)
# Purpose:            Display tulip (envelope) F for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund
.plotTulipF <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                         xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                         allQuants=TRUE,
                         gfx=list( annotate=TRUE, grids=FALSE, doLegend=TRUE,
                         showProj=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ),... )
{
  Ft    <- obj$om$Ft[ ,(2:ncol(obj$om$Ft)), drop=FALSE ]
  if( .USEMt )
    Ft    <- obj$om$Mt[ ,(2:ncol(obj$om$Ft)), drop=FALSE ]

  Fmsy  <- obj$refPtList$Fmsy
  nReps <- obj$ctlList$gui$nReps

  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tVec  <- c(1:ncol(Ft))

  # Specify axis limits for plots
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Ft[ ,tdx ], na.rm=TRUE )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
 # else
  #  #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
   # .addXaxis( xLim, initYear=.INITYEAR, side=3, years=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=3, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#  else
#    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }
  
  axis( side=4, labels=FALSE )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  quants <- apply( Ft, 2, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=3, lty=3 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }

  usr <- par( "usr" )

  box()

  if ( !is.null(Fmsy) && refPts )
  {
    urs <- par( "usr" )
    points( c(usr[1],usr[2]), c(Fmsy,Fmsy), xpd=T, bg=.FmsyBG, col=.FmsyCOL, cex=.FmsyCEX, pch=.FmsyPCH )
  }

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Ft[i,], col="black", lty=1, lwd=1 )
  }
  return( invisible() )
}     # END function .plotTulipF

# .plotTulipBmsy        (tulip simulation envelope for fishing mortality)
# Purpose:            Display tulip (envelope) Bmsy estimates for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund  / K. Holt
.plotTulipBmsy <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                         refPts=TRUE, allQuants=TRUE,
                         gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                         showProj=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ), ... )
{
  # Extract time indices.
  tMP  <- obj$ctlList$opMod$tMP
  nT   <- obj$ctlList$opMod$nT
  
  mpdPars <- obj$mp$assess$mpdPars
  nReps <- length( unique( mpdPars$iRep ) )
  
  ssbFmsy <- obj$refPtList$ssbFmsy
  ssbF01  <- obj$refPtList$ssbF01
     
  # Specify axis limits for plotting
  xLim <- gfx$xLim
  yLim <- gfx$yLim
   
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )
   
  if ( gfx$showProj )
    xLim <- c( tMP,nT )
      
  # Create an x-axis vector   
  xSeq <- seq( xLim[1],xLim[2], 5 )  

  # Time indices
  tVec  <- sort( unique( mpdPars$tStep ) )

  # Set the y-axis limits.
  yLim <- gfx$yLim
  
  if ( is.null(yLim) )
  {
    tdx <- max( xLim[1], tMP):(xLim[2] ) - tMP + 1
    
    yLim <- range( mpdPars$ssbFmsy, na.rm=TRUE )
    if ( refPts )
      yLim <- range( c(mpdPars$ssbFmsy,ssbFmsy), na.rm=TRUE )
  }

  # Create plot
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
  mfg <- par( "mfg" )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
 # else
  #  #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
   # .addXaxis( xLim, initYear=.INITYEAR, side=3, years=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=3, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#  else
#    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  
  axis( side=4, labels=FALSE )
  
  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  val <- split( mpdPars$ssbFmsy, mpdPars$tStep )
    
  quants <- sapply( val, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )
  
  # Plot the quantiles.
  lines( tVec,quants[3,], col="black", lwd=.LWD2, lty=1 )     # Median.
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }

  box()

  usr <- par( "usr" )

  if ( refPts )
  {
   urs <- par( "usr" )
   points( c(usr[1],usr[2]), c(ssbFmsy,ssbFmsy), xpd=T, bg=.BmsyBG, col=.BmsyCOL,
           cex=.BmsyCEX, pch=.BmsyPCH )
           
   abline(h=ssbFmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD ) 
     
   #points( usr[2], ssbF01, xpd=T, bg=.F01BG, col=.F01COL,
   #        cex=.CEXSYM4, pch=.F01PCH )
   #abline(h=obj$pars$ssbF01, col=.F01COL, lty=.F01LTY, lwd=.F01LWD ) 
  }
    
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    val <- split( mpdPars$ssbFmsy, mpdPars$iRep )
    for ( i in traces )
    {
     lines( tVec, val[[i]], col="black", lty=1, lwd=1 )
     points( tVec, val[[i]], bg="white", fg="black", pch=21 , cex=2 )
    }
  }
  return( invisible() )
}     # END function .plotTulipBmsy
   

# .plotTulipFmsy        (tulip simulation envelope for fishing mortality)
# Purpose:            Display tulip (envelope) Bmsy estimates for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund  / K. Holt
.plotTulipFmsy <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                         refPts=TRUE, allQuants=TRUE,
                         gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                         showProj=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ), ... )
{
  # Extract time indices.
  tMP  <- obj$ctlList$opMod$tMP
  nT   <- obj$ctlList$opMod$nT
  
  mpdPars <- obj$mp$assess$mpdPars
  nReps <- length( unique( mpdPars$iRep ) )

  Fmsy <- obj$refPtList$Fmsy
  F01  <- obj$refPtList$F01
     
  # Specify axis limits for plotting
  xLim <- gfx$xLim
  yLim <- gfx$yLim
   
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )
   
  if ( gfx$showProj )
    xLim <- c( tMP,nT )
      
  # Create an x-axis vector   
  xSeq <- seq( xLim[1],xLim[2], 5 )  

  # Time indices
  tVec  <- sort( unique( mpdPars$tStep ) )

  # Set the y-axis limits.
  yLim <- gfx$yLim
  
  if ( is.null(yLim) )
  {
    tdx <- max( xLim[1], tMP):(xLim[2] ) - tMP + 1      

    yLim <- range( mpdPars$Fmsy, na.rm=TRUE )
    if ( refPts )
      yLim <- range( c(mpdPars$Fmsy,Fmsy), na.rm=TRUE )
  }

  # Create plot
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
  mfg <- par( "mfg" )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
 # else
  #  #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
   # .addXaxis( xLim, initYear=.INITYEAR, side=3, years=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=3, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#  else
#    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  
  axis( side=4, labels=FALSE )
  
  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  val <- split( mpdPars$Fmsy, mpdPars$tStep )
    
  quants <- sapply( val, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )
  
  # Plot the quantiles.
  lines( tVec,quants[3,], col="black", lwd=.LWD2, lty=1 )     # Median.
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }

  box()

  usr <- par( "usr" )

  if ( refPts )
  {
   urs <- par( "usr" )
   points( c(usr[1],usr[2]), c(Fmsy,Fmsy), xpd=T, bg=.FmsyBG, col=.FmsyCOL,
           cex=.FmsyCEX, pch=.FmsyPCH )
   abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
     
   #points( usr[2], F01, xpd=T, bg=.F01BG, col=.F01COL,
   #        cex=.CEXSYM4, pch=.F01PCH )
  }
    
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    val <- split( mpdPars$Fmsy, mpdPars$iRep )
    for ( i in traces )
    {
     lines( tVec, val[[i]], col="black", lty=1, lwd=1 )
     points( tVec, val[[i]], bg="white", fg="black", pch=21 , cex=2 )
    }
  }
    
  return( invisible() )
}     # END function .plotTulipBmsy


#------------------------------------------------------------------------------#
# ADMB Diagnostic plots for guiPerf.                                           #
#------------------------------------------------------------------------------#

# Function: .plotConvTime
# Purpose: Produces boxplots showing the time required to reach convergence 
#           (in seconds) by ADMB optimization for all years within a given replicate
#           (if plot typ = convCallsRep) or all replicates within a given year
#           (if plot typ = convCallsYr). 
# Author: K.Holt (29-Jan-10)
.plotConvTime <- function( obj, typ="convTimeYr", gfx=list( annotate=TRUE, doLegend=TRUE,
                           xLim=NULL, yLim=NULL ) )
{
  # input obj is runStatus.
  
  nReps <- length( unique( obj$iRep ) )
  nT    <- max( obj$tStep )
  tMP   <- min( obj$tStep )
  
  xLim <- gfx$xLim
  yLim <- c(0,60) #gfx$yLim
  
  if ( is.null(yLim) )
    yLim <- c( 0, max( obj$convT ) )
      
  if ( typ == "convTimeYr" )
  {
    boxplot( obj$convT~obj$tStep, axes=FALSE, col="grey", ylim=yLim )
    xLabel <- seq( tMP, nT, by=10 )
    xPos   <- seq( 1,(nT-tMP+1), by=10 )

    # Add points where convergence failed...
    idx <- obj$iExit != 1
    points( jitter( obj$tStep[idx]-tMP+1 ), obj$convT[idx], bg="red", cex=.CEXSYM8, pch=21 )     
  }
    
  if ( typ == "convTimeRep" )
  {
    boxplot( obj$convT~obj$iRep, axes=FALSE, col="grey", ylim=yLim )
    xLabel <- seq( 1, nReps, by=1 )
    xPos   <- seq( 1, nReps, by=1 )
    
    # Add points where convergence failed...
    idx <- obj$iExit != 1
    points( jitter( obj$iRep[idx] ), obj$convT[idx], bg="red", cex=.CEXSYM8, pch=21 ) 
  }
  box()

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS4, at=xPos, labels=xLabel )
  else
    axis( side=1, at=xPos, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  else
      axis( side=2, labels=FALSE )

  if ( mfg[1]==1 )
    axis( side=3, at=xPos, labels=xLabel, cex.axis=.CEXAXIS4 )
  else
    axis( side=3, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  else
    axis( side=4, labels=FALSE )

  return( invisible() )
}     # END function .plotConvTime


# Function: .plotMaxGrad
# Purpose: Boxplot of maximum gradients (log scale) among years within each 
#           replicate (if plot typ = convGradRep)or among replicates for each year
#           (if plot typ = convGradYr) relative to the threshold level of 10-4 
#           that is used to define a sucessful convergence in teh optimization 
#           (note: a large maximum gradient from optimization indicates 
#            convergence failure).
# Input:   The "runStatus" data frame from blob$mp$assess
# Author: K.Holt (26-Aug-09), modified A.R. Kronlund (07-Dec-12).
.plotMaxGrad <- function( obj, typ, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  nReps <- length( unique( obj$iRep ) )
  nT    <- max( obj$tStep )
  tMP   <- min( obj$tStep )
  
  if ( is.null( gfx$xLim ) )
    xLim <- c( 1, max( obj$iRep ) )
    
  if ( is.null( gfx$yLim ) )
    yLim <- c( 0, mean( obj$maxGrad ) )

  if ( typ == "convGradRep" )
  {
    boxplot( obj$maxGrad~obj$iRep, axes=FALSE, log="y", ylim=c(1.e-7,1.e2) )
    abline( h=.MAXGRADCRIT, col="red", lty=1, lwd=2 )
    xLabel <- seq( 1, nReps, 1 )
    xPos   <- seq( 1, nReps, 1 )
    box()
  }

  if ( typ=="convGradYr" )
  {
    boxplot( obj$maxGrad~obj$tStep, axes=FALSE, log="y", ylim=c(1.e-7,1.e2)  )
    abline( h=.MAXGRADCRIT, col="red", lty=1, lwd=2 )
    xLabel <- seq( tMP, nT, by=5 )
    xPos   <- seq( 1, ( nT-tMP+1), by=5 )
    box()
  }

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
   axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabel )
  else
   axis( side=1, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  else
    axis( side=2, labels=FALSE )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2, at=xPos, labels=xLabel )
  else
    axis( side=3, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  else
    axis( side=4, labels=FALSE )

  return( invisible() )
}     # END function .plotMaxGrad


# .plotFtBt   (plot realized fishing mortality vs. SSB in relation to HCR)
# Purpose:     Shows the pairs of F and SSB that actually occurred
#              (i.e.,realized) pooled over all years and all repitions.  The 
#              harvest control rule used is also show, as are the Critical, 
#              Cautious, and Healthy zones.  The portion of points falling 
#              in each zone are shown in the legend.
# Parameters:   obj - the saved blob for simulation, colorZones=TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotFtBt <- function( obj, qProbs=c(0.05,0.1,0.5,0.9,0.95),
               refPts=pfRefPts, allQuants=TRUE, phase=FALSE,
               gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                         image=FALSE, xLim=NULL, yLim=NULL ),... )
{       
  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tProj <- c(tMP:nT)

  # Get spawning biomass and fishing mortality for all replicates.
  Bt <- obj$om$Bt[ ,(2:ncol(obj$om$Bt)), drop=FALSE ]
  Ft <- obj$om$Ft[ ,(2:ncol(obj$om$Ft)), drop=FALSE ]
    
  B0   <- obj$ctlList$opMod$B0
  Bmsy <- obj$refPtList$ssbFmsy
  Fmsy <- obj$refPtList$Fmsy

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # Subset the SSB and F from the projection period.
  Bt <- Bt[ ,tProj, drop=FALSE ]
  Ft <- Ft[ ,tProj, drop=FALSE ]
  
  if ( phase )
  {
    Bt <- Bt / Bmsy
    Ft <- Ft / Fmsy
  }
   
  if ( is.null(xLim) )
    xLim <- range( c(0,Bt) )
  
  if ( is.null(yLim) )
    yLim <- range( c(0,Ft) )

  plot( xLim, yLim, type="n", axes=FALSE, xaxs="i", xlab="", yaxs="i", ylab="")

  if ( gfx$image )
  {
    # Image the points.
    xBrks <- seq( 0,100,1 )
    yBrks <- seq( 0,5,0.005 )
    
    if ( phase )
    {
      xBrks <- seq( 0, 3, 0.05 )
      yBrks <- seq( 0, 3, 0.05 )
    }
  
    xCuts  <- cut( Bt, breaks=xBrks )
    yCuts  <- cut( Ft, breaks=yBrks )
    counts <- table( xCuts, yCuts )  
    image(  xBrks, yBrks, counts, col=rev(heat.colors(64)), add=TRUE,axes = FALSE)
    #contour( xBrks, yBrks, counts, add = TRUE )
    
    for ( i in 1:nrow(Bt) )
      points( Bt[i,],Ft[i,], bg="white", col="black", cex=0.8, pch=21 )
  }
  else
  {
    for ( i in 1:nrow(Bt) )
    {
      x <- cbind( x1=Bt[i,], x2=Ft[i,] )
      points( Bt[i,],Ft[i,], pch=20, col=densCols( x, nbin=64, colramp=heat.colors ) )
    }
  }

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=2, labels=FALSE, las=.YAXISLAS )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=4, labels=FALSE, las=.YAXISLAS )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
    abline( h=seq(yLim[1], yLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
  }

  if ( phase )
  {
    abline( h=1, col=.FmsyCOL, lwd=.FmsyLWD, lty=.FmsyLTY )
    abline( v=1, col=.BmsyCOL, lwd=.BmsyLWD, lty=.BmsyLTY )
    .addTarget( 1, 1, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }
  else
  {
    abline( h=Fmsy, col=.FmsyCOL, lwd=.FmsyLWD, lty=.FmsyLTY )
    abline( v=Bmsy, col=.BmsyCOL, lwd=.BmsyLWD, lty=.BmsyLTY )
    .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }

  if ( gfx$annotate )
  {
    ## Calculate number and proportion in each zone from tMP:nT.
    #nZone <- table( cut( Bt, breaks=c(0,zoneLimit,zoneUpper,max(Bt) ) ) )
    #pZone <- nZone / length(Bt)

    #zoneLabels <- c( paste( format(round(pZone[1],2),digits=2,nsmall=2)," Critical",sep=" " ),
    #                 paste( format(round(pZone[2],2),digits=2,nsmall=2)," Cautious",sep=" " ),
    #                 paste( format(round(pZone[3],2),digits=2,nsmall=2)," Healthy",sep=" " ) )
    
    mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, "Fishing mortality vs. Spawning Biomass" )
  }

  if ( gfx$doLegend )
  {
  }

  usr <- par( "usr" )

  box()
  return( invisible() )
}     # END function .plotFtBt.

#------------------------------------------------------------------------------#
#-- Sim Plotting Functions:                                                  --#
#------------------------------------------------------------------------------#

#-- Harvest Control Rule plotting functions.                                 --#

# .plotHCRvariableF (Plot a status-based control rule)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
# Modified: KRH - Added ability to choose between plotting multiple reference
#           removal rates or just the one selected using multRefRR (10-Sept-2013)
  
.plotHCRvariableF <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE, multRefRR=FALSE) )
{

  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  Fmult  <- obj$mp$hcr$Fmult
  nameF  <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF   <- Fmult*c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0
  if ( obj$mp$hcr$statusBase == "statusBaseBt" )
    {
        # Get historical biomass trajectory
        mpObj   <- .createMP( obj )
        initPop <- .initPop( mpObj )
        Bexp <- initPop$om$Bexp
        
        # Extract start and end years for upper and lower bases
        lowerSyr<-obj$mp$hcr$lowBaseStartYr
        lowerNyr<-obj$mp$hcr$lowBaseEndYr
          
        upperSyr<-obj$mp$hcr$upperBaseStartYr
        upperNyr<-obj$mp$hcr$upperBaseEndYr
          
        # Calculate lower base for historical reference points
        if (obj$mp$hcr$histLowBase=="Bmin")
         {
             lowerBaseB<-min(Bexp[lowerSyr:lowerNyr])
         }
        if (obj$mp$hcr$histLowBase=="Bmax")
         {
             lowerBaseB<-max(Bexp[lowerSyr:lowerNyr])
         } 
        if (obj$mp$hcr$histLowBase=="Bmean")
         {
            lowerBaseB<-mean(Bexp[lowerSyr:lowerNyr])
         }
        if (obj$mp$hcr$histLowBase=="Bquant")
         {
            quant<-obj$mp$hcr$lowBaseQuant
            lowerBaseB<-quantile(Bexp[lowerSyr:lowerNyr],quant)[[1]]
         }
         # Calculate upper base for historical reference points
        if (obj$mp$hcr$histUpperBase=="Bmin")
         {
             upperBaseB<-min(Bexp[upperSyr:upperNyr])
         }
        if (obj$mp$hcr$histUpperBase=="Bmax")
         {
             upperBaseB<-max(Bexp[upperSyr:upperNyr])
         } 
        if (obj$mp$hcr$histUpperBase=="Bmean")
         {
            upperBaseB<-mean(Bexp[upperSyr:upperNyr])
         }
        if (obj$mp$hcr$histUpperBase=="Bquant")
         {
            quant<-obj$mp$hcr$upperBaseQuant
            upperBaseB<-quantile(Bexp[upperSyr:upperNyr],quant)[[1]]
         }  
    }  

  idxRemRate <- 0
  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
  {
    remRefRate <- Fmsy
    idxRemRate <- 1
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
  {
    remRefRate <- F01
    idxRemRate <= 2
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- FsprX
    idxRemRate <- 3
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
  {
    remRefRate <- Finput
    idxRemRate <- 4
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFt" )
  {
    # Get historical Ft trajectory
    mpObj   <- .createMP( obj )
    initPop <- .initPop( mpObj )
    Ft <- initPop$om$Ft
            
    # Extract start and end years for historical period
    syr<-obj$mp$hcr$FtStartYr
    nyr<-obj$mp$hcr$FtEndYr
                     
    # Calculate reference removal rate for historical reference points
    if (obj$mp$hcr$histFtBase=="Fmin")
    {
      remRefRate<-min(Ft[syr:nyr])
    }
    if (obj$mp$hcr$histFtBase=="Fmax")
    {
      remRefRate<-max(Ft[syr:nyr])
    }
    if (obj$mp$hcr$histFtBase=="Fmean")
    {
      remRefRate<-mean(Ft[syr:nyr])
    }
    if (obj$mp$hcr$histFtBase=="Fquant")
    {
      quant<-obj$mp$hcr$histFtBaseQuant
      remRefRate<-quantile(Ft[syr:nyr],quant)[[1]]
    }
  
    idxRemRate <- 5
  }
  remRefRate <- remRefRate*obj$mp$hcr$Fmult
  
  if ( gfx$multRefRR==T ) yMax <- 1.1 * max(valF,na.rm=TRUE)
  if ( gfx$multRefRR==F ) yMax <- 1.3 * remRefRate
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,yMax), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )
  
  # Status-based rule control points.
  if (obj$mp$hcr$statusBase=="statusBaseBt")
  {
     upperCtlPt<-obj$mp$hcr$upperBoundMult * upperBaseB
     lowerCtlPt<-obj$mp$hcr$lowerBoundMult * lowerBaseB
  }
  else
  {
     upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
     lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB
  }

  if ( gfx$multRefRR == TRUE )
  {                                                                                           
    # Draw the options for HCRs.
    for ( i in 1:length(valF) )
    {
      rrr <- valF[ i ]
      segments(          0,   0, lowerCtlPt,   0, col="black", lty=1, lwd=1 )
      segments( lowerCtlPt,   0, upperCtlPt, rrr, col="black", lty=1, lwd=1 )
      segments( upperCtlPt, rrr,     usr[2], rrr, col="black", lty=1, lwd=1 )
    }
    
    # Draw the selected intended removal rate curves.
    segments(          0,          0, lowerCtlPt,          0, col="blue", lty=1, lwd=4 )
    segments( lowerCtlPt,          0, upperCtlPt, remRefRate, col="blue", lty=1, lwd=4 )
    segments( upperCtlPt, remRefRate,     usr[2], remRefRate, col="blue", lty=1, lwd=4 )

    # Plot Fmsy~Bmsy.
    points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
    points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
    points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
    points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 ) 
  }

  if ( gfx$multRefRR == FALSE )
  {                                                                                           
    # Draw the selected intended removal rate curves.
    segments(          0,          0, lowerCtlPt,          0, col="blue", lty=1, lwd=4 )
    segments( lowerCtlPt,          0, upperCtlPt, remRefRate, col="blue", lty=1, lwd=4 )
    segments( upperCtlPt, remRefRate,     usr[2], remRefRate, col="blue", lty=1, lwd=4 )

  }

  # Indicate lRef, uRef, and Bmsy.
  abline( v=lowerCtlPt, col=.HCRLBCOL, lty=.HCRLBLTY, lwd=.HCRLBLWD )
  abline( v=upperCtlPt, col=.HCRUBCOL, lty=.HCRUBLTY, lwd=.HCRUBLWD )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Stock Status" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Removal Rate" )

  if ( gfx$annotate )
  {
    lowerB <- round( lowerCtlPt,digits=2 )
    upperB <- round( upperCtlPt,digits=2 )
    
    #panLab( 0.8, 0.55, bquote( "B"["lower"]==.(lowerB) ) )
    #panLab( 0.8, 0.50, bquote( "B"["upper"]==.(upperB) ) )
    
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    
    if (gfx$multRefRR) {
      labels <- nameF
      pchVec <- rep( 21,length(labels) )
      names(pchVec) <- labels
  
      ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
      names(ptBg) <- labels
     
      # Add a legend.
      colVec <- rep( "white", 4 )
      colVec[ idxRemRate ] <- "blue"
      ltyVec <- rep( 1, 4 )
      lwdVec <- rep( 4, 4 )
      panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2, cex=1,
               title=paste( "LB =",lowerB," UB =",upperB ),
               col=colVec, lty=ltyVec, lwd=lwdVec, bty=.LEGBTY )          
    }
    else
    {
      panLegend( 0.6, 0.3, legTxt=c(paste( "LB =",lowerB), paste(" UB =",upperB)),
        bty=.LEGBTY)
    }  
  }     # if gfx$annotate

  return( invisible() )
}     # END function .plotHCRvariableF

# .plotHCRvariableF (Plot a status-based control rule)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
   
.plotHCRvariableFold <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF  <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF   <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0
  if ( obj$mp$hcr$statusBase == "statusBaseBt" )
    {
        # Get historical biomass trajectory
        mpObj   <- .createMP( obj )
        initPop <- .initPop( mpObj )
        Bexp <- initPop$om$Bexp
        
        # Extract start and end years for upper and lower bases
        lowerSyr<-obj$mp$hcr$lowBaseStartYr
        lowerNyr<-obj$mp$hcr$lowBaseEndYr
          
        upperSyr<-obj$mp$hcr$upperBaseStartYr
        upperNyr<-obj$mp$hcr$upperBaseEndYr
          
        # Calculate lower base for historical reference points
        if (obj$mp$hcr$histLowBase=="Bmin")
         {
             lowerBaseB<-min(Bexp[lowerSyr:lowerNyr])
         }
        if (obj$mp$hcr$histLowBase=="Bmax")
         {
             lowerBaseB<-max(Bexp[lowerSyr:lowerNyr])
         } 
        if (obj$mp$hcr$histLowBase=="Bmean")
         {
            lowerBaseB<-mean(Bexp[lowerSyr:lowerNyr])
         }
        if (obj$mp$hcr$histLowBase=="Bquant")
         {
            quant<-obj$mp$hcr$lowBaseQuant
            lowerBaseB<-quantile(Bexp[lowerSyr:lowerNyr],quant)[[1]]
         }
         # Calculate upper base for historical reference points
        if (obj$mp$hcr$histUpperBase=="Bmin")
         {
             upperBaseB<-min(Bexp[upperSyr:upperNyr])
         }
        if (obj$mp$hcr$histUpperBase=="Bmax")
         {
             upperBaseB<-max(Bexp[upperSyr:upperNyr])
         } 
        if (obj$mp$hcr$histUpperBase=="Bmean")
         {
            upperBaseB<-mean(Bexp[upperSyr:upperNyr])
         }
        if (obj$mp$hcr$histUpperBase=="Bquant")
         {
            quant<-obj$mp$hcr$upperBaseQuant
            upperBaseB<-quantile(Bexp[upperSyr:upperNyr],quant)[[1]]
         }  
    }  
    

  idxRemRate <- 0
  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
  {
    remRefRate <- Fmsy
    idxRemRate <- 1
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
  {
    remRefRate <- F01
    idxRemRate <= 2
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- FsprX
    idxRemRate <- 3
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
  {
    remRefRate <- Finput
    idxRemRate <- 4
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFt" )
  {
     # Get historical Ft trajectory
     mpObj   <- .createMP( obj )
     initPop <- .initPop( mpObj )
     Ft <- initPop$om$Ft
          
            
      # Extract start and end years for historical period
       syr<-obj$mp$hcr$FtStartYr
       nyr<-obj$mp$hcr$FtEndYr
                     
       # Calculate reference removal rate for historical reference points
       if (obj$mp$hcr$histFtBase=="Fmin")
       {
           remRefRate<-min(Ft[syr:nyr])
       }
       if (obj$mp$hcr$histFtBase=="Fmax")
       {
           remRefRate<-max(Ft[syr:nyr])
       }
       if (obj$mp$hcr$histFtBase=="Fmean")
       {
           remRefRate<-mean(Ft[syr:nyr])
       }
       if (obj$mp$hcr$histFtBase=="Fquant")
       {
            quant<-obj$mp$hcr$histFtBaseQuant
            remRefRate<-quantile(Ft[syr:nyr],quant)[[1]]
       }
  
    idxRemRate <- 5
  }

  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )
  
  # Status-based rule control points.
  if (obj$mp$hcr$statusBase=="statusBaseBt")
  {
     upperCtlPt<-obj$mp$hcr$upperBoundMult * upperBaseB
     lowerCtlPt<-obj$mp$hcr$lowerBoundMult * lowerBaseB
  }
  else
  {
     upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
     lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB
  }


  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    segments(          0,   0, lowerCtlPt,   0, col="black", lty=1, lwd=1 )
    segments( lowerCtlPt,   0, upperCtlPt, rrr, col="black", lty=1, lwd=1 )
    segments( upperCtlPt, rrr,     usr[2], rrr, col="black", lty=1, lwd=1 )
  }

  # Draw the selected intended removal rate curves.
  segments(          0,          0, lowerCtlPt,          0, col="blue", lty=1, lwd=4 )
  segments( lowerCtlPt,          0, upperCtlPt, remRefRate, col="blue", lty=1, lwd=4 )
  segments( upperCtlPt, remRefRate,     usr[2], remRefRate, col="blue", lty=1, lwd=4 )

  # Indicate lRef, uRef, and Bmsy.
  abline( v=lowerCtlPt, col=.HCRLBCOL, lty=.HCRLBLTY, lwd=.HCRLBLWD )
  abline( v=upperCtlPt, col=.HCRUBCOL, lty=.HCRUBLTY, lwd=.HCRUBLWD )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )      

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    lowerB <- round( lowerCtlPt,digits=2 )
    upperB <- round( upperCtlPt,digits=2 )
    
    #panLab( 0.8, 0.55, bquote( "B"["lower"]==.(lowerB) ) )
    #panLab( 0.8, 0.50, bquote( "B"["upper"]==.(upperB) ) )
    
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    colVec <- rep( "white", 4 )
    colVec[ idxRemRate ] <- "blue"
    ltyVec <- rep( 1, 4 )
    lwdVec <- rep( 4, 4 )
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2, cex=1,
               title=paste( "LB =",lowerB," UB =",upperB ),
               col=colVec, lty=ltyVec, lwd=lwdVec, bty=.LEGBTY )                
  }

  return( invisible() )
}     # END function .plotHCRvariableF


# .plotHCRconstantF       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
# Modified: KRH - Added ability to choose between plotting multiple reference
#           removal rates or just the one selected using multRefRR (10-Sept-2013)
.plotHCRconstantF <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE, multRefRR=FALSE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
    remRefRate <- Fmsy
  else if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
    remRefRate <- F01
  else if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- FsprX
  }
  else if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
    remRefRate <- Finput
  
  # Plot harvest control rule.
  
  if (gfx$multRefRR==T) yMax<-1.1*max(valF,na.rm=TRUE)
  if (gfx$multRefRR==F) yMax<-1.3*remRefRate
  
  plot( c(0,B0),c(0,yMax), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )

  # Status-based rule control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB
  
   if (gfx$multRefRR == TRUE) {      
  
      # Draw the options for HCRs.
      for ( i in 1:length(valF) )
      {
        rrr <- valF[ i ]
        abline( h=rrr, col="black", lty=1, lwd=1 )
      }

      # Draw the selected intended removal rate curves.
      abline( h=remRefRate, col="blue", lty=1, lwd=4 )

      # Plot Fmsy~Bmsy.
      points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
      points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
      points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
      points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 ) 

      if ( gfx$annotate )
      {
        #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
        #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
        #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
        #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
        #if ( obj$paAdj )
        #{
        #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
        #          sep="" ) )
        #}
        labels <- nameF
        pchVec <- rep( 21,length(labels) )
        names(pchVec) <- labels
  
        ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
        names(ptBg) <- labels
     
        # Add a legend.
        panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
      }

   }

    if (gfx$multRefRR == FALSE) { 
      # Draw the selected intended removal rate curves.
      abline( h=remRefRate, col="blue", lty=1, lwd=4 )
    }

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "Removal Rate" )

  
  return( invisible() )
}     # END function .plotHCRconstantF

# .plotHCRconstantC       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
.plotHCRconstantCold <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  constCatch <- obj$mp$hcr$constCatch
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )
  
  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    abline( h=rrr, col="black", lty=1, lwd=1 )
  }

  # Annual harvest rate.
  Bt <- seq( 0,B0, B0/10 )
  Ut <- constCatch / Bt
  
  # Annual instantaneous rate.
  Ft <- 1-exp(-Ut)
  lines( Bt, Ft, col="blue", lwd=3 )

  # Draw the selected intended removal rate curves.
  #abline( h=remRefRate, col="blue", lty=1, lwd=4 )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )    

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotHCRconstantC

# .plotHCRconstantF       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
.plotHCRconstantF <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
    remRefRate <- Fmsy
  else if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
    remRefRate <- F01
  else if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- FsprX
  }
  else if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
    remRefRate <- Finput
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )

  # Status-based rule control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB
  
  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    abline( h=rrr, col="black", lty=1, lwd=1 )
  }

  # Draw the selected intended removal rate curves.
  abline( h=remRefRate, col="blue", lty=1, lwd=4 )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )   

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotHCRconstantF


# .plotHCRdeclineRisk  (Plot the "9-zone" harvest control rule described in Table 1 
#                         of DFO (2009) Precautionary Harvest Strategy)
# Purpose:      Plot the harvest control rule, which is expressed as the maximum  
#               acceptable probability of future stock decline as a function of 
#               current stock status and recent trends in stock status. 
#               Boundaries of Critical, Cautious, and Healthy zones are shown.
#                Three different plots will be produced for three different 
#               recent trends in stock status: increasing, stable, decreasing.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR  
.plotHCRdeclineRisk <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  # Three plot panels showing the increasing, stable, and decreasing acceptable
  # probability of decline curves.
 
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Harvest control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB

  # Get the acceptable probability of decline values.
  pDecline <- .getPdecline( obj$mp$hcr )
  cat( "\n(.plotHCRdeclineRisk) Acceptable probabiity of decline:\n\n" )
  print( pDecline )

  #----------------------------------------------------------------------------#  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy)                     #
  #----------------------------------------------------------------------------#

  trendLabel <- c( "Increasing Trend","Stable Trend","Decreasing Trend" )

  for ( i in 1:length(trendLabel) )
  {
    plot( c(0,B0),c(0,1), type="n", axes=FALSE, xlab="",ylab="", xaxs="i",yaxs="i")
  
    usr <- par( "usr" )
    
    for ( j in 1:3 )
      lines( x=c(usr[1],lowerCtlPt, lowerCtlPt,upperCtlPt, upperCtlPt,usr[2] ),
             y=pDecline[i,], lty=1,lwd=2 )
  
    # Indicate lower and upper harvest control rule points.
    abline( v=lowerCtlPt, col=.HCRLBCOL, lty=.HCRLBLTY, lwd=.HCRLBLWD )
    abline( v=upperCtlPt, col=.HCRUBCOL, lty=.HCRUBLTY, lwd=.HCRUBLWD )

    if ( gfx$annotate )
    {
      panLab( 0.8,0.5,  cex=1.4, trendLabel[i] )
      panLab( 0.8,0.4, cex=1.4, paste( "Trend Years = ", obj$mp$hcr$trendYears,
        " (",obj$mp$hcr$lowerTrendPct,", ",obj$mp$hcr$upperTrendPct,")", sep="" ) )
      panLab( 0.8,0.3, cex=1.4, paste( "Proj Years = ",  obj$mp$hcr$nProjYears,sep="" ) )
#      panLab( 0.8,0.2, cex=1.4,
#        paste( "MCMC (thin) = ",  obj$nMCMC, " (",obj$nThin,")",sep="" ) )
      
      # Acceptable probability of decline levels.
      offset <- 0.01
      panLab( offset + 0.0,            0.8, adj=0, pDecline[i,1] )
      panLab( lowerCtlPt/B0 - offset,  0.8, adj=1, pDecline[i,2] )      
      panLab( offset+lowerCtlPt/B0,    0.8, adj=0, pDecline[i,3] )
      panLab( upperCtlPt/B0 - offset,  0.8, adj=1, pDecline[i,4] )
      panLab( offset+upperCtlPt/B0,    0.8, adj=0, pDecline[i,5] )
      panLab( 1 - offset,              0.8, adj=1, pDecline[i,6] )
    }

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    depletion <- seq( 0,B0, B0/10.0 )
    axis( side=3, at=depletion, labels=depletion/B0 )
    
    if ( i==2 )
      mtext( side=2, line=3, cex=.CEXLAB4, "Acceptable Probability of Decline" )
    
    box()
  }
  mtext( side=1, line=0.5, cex=.CEXTITLE4, outer=TRUE, "Biomass" )
   
  return( invisible() )
}     # END function .plotHCRdeclineRisk

# .plotHCRviewDeclineRisk  (Plot the "9-zone" harvest control rule described in Table 1 
#                         of DFO (2009) Precautionary Harvest Strategy)
# Purpose:      Plot the harvest control rule, which is expressed as the maximum  
#               acceptable probability of future stock decline as a function of 
#               current stock status and recent trends in stock status. 
#               Boundaries of Critical, Cautious, and Healthy zones are shown.
#                Three different plots will be produced for three different 
#               recent trends in stock status: increasing, stable, decreasing.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR  
.plotHCRviewDeclineRisk <- function( obj, iSim=1, iRep=1,
   gfx=list( annotate=TRUE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Six plot panels, with left column showing acceptable probability of decline
  # and right panels showing SSB_t for increasing, stable and decreasing recent
  # stock trends.
    
  #layout( matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(1.5,2,2,2) )

  B0   <- obj$ctlList$opMod$B0
  Bmsy <- obj$refPtList$ssbFmsy
  
  # Always default to showing HCR based on "true" biomass ref points, regardless
  # of whether Bmsy or B0 is used as a base (K.Holt):
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    base <- Bmsy
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    base <- B0
  
  # If Fmsy is used for reference removal rate, show HCR based on equilibrium value:
  #if ( obj$mp$hcr$guiPars$remRefTyp=="Fmsy" )
  #  remRate <- obj$mp$specespars$Fmsy     # K.Holt
  
  # Rule control points on operating model scale.
  lowerB <- obj$mp$hcr$specs$lowerBoundMult * base  
  upperB <- obj$mp$hcr$specs$upperBoundMult * base

  # Get the acceptable probability of decline values.
  pDecline <- .getPdecline( obj$mp$hcr$specs )
  cat( "\n(.plotHCRviewDeclineRisk) Acceptable probabiity of decline:\n\n" )
  print( pDecline )

  #----------------------------------------------------------------------------#  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy)                     #
  #----------------------------------------------------------------------------#

  trendLabel <- c( "Increasing Trend","Stable Trend","Decreasing Trend" )
  nT  <- obj$mp$hcr$specs$nT
  tMP <- obj$mp$hcr$specs$tMP

  colVec <- rev( heat.colors( nT-tMP+1 ) )

  for ( i in 1:length(trendLabel) )
  {
    plot( c(0,B0),c(0,1), type="n", axes=FALSE, xlab="",ylab="", xaxs="i",yaxs="i")
  
    mfg <- par( "mfg" )
    usr <- par( "usr" )
    
    # So called "true" line, may wish to omit.
    lines( x=c(usr[1],lowerB, lowerB,upperB, upperB,usr[2] ),
           y=pDecline[i,], lty=1,lwd=2 )
  
    lowerBound <- obj$mp$hcr$lowerBound
    lowerBound <- lowerBound[ iRep,c(2:ncol(lowerBound)) ]
    upperBound <- obj$mp$hcr$upperBound
    upperBound <- upperBound[ iRep,c(2:ncol(upperBound)) ]
    
    # Is the year in the current category of increasing, stable, decreasing?
    idx   <- obj$mp$assess$trendVal[ iRep,2:(nT+1) ]==i
    idx[ is.na(idx) ] <- FALSE    
    
    # These are the lines for each update of control points.  But we should only
    # plot the lines corresponding to the increasing, decreasing, stable.
    for ( j in c(tMP:nT) )
    {
      # Is the trend in the current category?
      if ( idx[j] )
      {
        lines( x=c( usr[1], lowerBound[j], lowerBound[j],
                    upperBound[j], upperBound[j], usr[2] ),
               y=pDecline[i,], col="gray", lty=1, lwd=1 )
      }
    }
  
    # Indicate acceptability probability of decline control bounds.
    abline( v=lowerB, col=.HCRLBCOL, lty=.HCRLBLTY,  lwd=.HCRLBLWD )
    abline( v=upperB, col=.HCRLBCOL, lty=.HCRUBLTY,  lwd=.HCRUBLWD )
    abline( v=Bmsy,   col=.BmsyCOL,  lty=.BmsyLTY,   lwd=.BmsyLWD )    

    # Add points.
    ssb   <- obj$mp$assess$ssb[ iRep,2:(nT+1) ]
    pStar <- obj$mp$hcr$pStar[ iRep,2:(nT+1) ]
    
    points( ssb[idx], pStar[idx], bg=colVec[idx[tMP:nT]], cex=.CEXSYM20, pch=21 )

    panLab( 0.7,0.1, cex=.CEXANNO2, trendLabel[i] )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    depletion <- seq( 0,B0,B0/10.0 )
    
    if ( mfg[1]==1 )
      axis( side=3, at=depletion, cex.axis=.CEXAXIS4, labels=depletion/B0 )
    else
      axis( side=3, at=depletion, labels=FALSE )
    
    if ( i==2 )
      mtext( side=2, line=3, cex=.CEXLAB2, "Acceptable Probability of Decline" )
    
    box()
  }
  mtext( side=1, line=3, cex=.CEXTITLE2, "Estimated Stock Status" )

  Bexp <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]
  Bt   <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  nT   <- length( Bt )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max( c(Bexp,Bt) ) )
  }

  for ( i in 1:length(trendLabel) )
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
    lines( c(1:nT), Bexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )
    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    # Add slopes and points.
    idx <- obj$mp$assess$trendVal[ iRep,2:(nT+1) ]==i
    idx[ is.na(idx) ] <- FALSE

    ssb   <- obj$mp$assess$ssb[ iRep,2:(nT+1) ]
    trendBio <- obj$mp$assess$trendBio[ iRep, 2:(nT+1) ]
    maxSlope <- max( abs( trendBio ), na.rm=T )
    
    # Add slope lines.
    usr <- par( "usr" )
    yDelta <- usr[4] - usr[3]
    mult <- (0.4 * yDelta) / maxSlope

    # ARK (24-Mar-13) Removed "-1" multiplcation.
    #segments( c(1:nT)[idx],ssb[idx],c(1:nT)[idx],ssb[idx]+(trendBio[idx]*mult*-1.0) ) 
    segments( c(1:nT)[idx],ssb[idx],c(1:nT)[idx],ssb[idx]+(trendBio[idx]*mult) )
    
    x <- c(1:nT)[idx]
    y <- ssb[idx]
    points( x, y, bg=colVec[idx[tMP:nT]], cex=.CEXSYM20, pch=21 )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis (side=4, labels=FALSE )
    box()

    if ( gfx$doLegend )
    {
    }
  }
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Year" )
  mtext( side=4, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Spawning Stock Biomass" )

  if ( gfx$annotate )
  {
    methodLab     <- .METHODLAB[obj$ctlList$mp$assess$methodId]  
    nProjYears    <- obj$mp$hcr$specs$nProjYears
    trendYears    <- obj$mp$hcr$specs$trendYears
    upperTrendPct <- obj$mp$hcr$specs$upperTrendPct
    lowerTrendPct <- obj$mp$hcr$specs$lowerTrendPct
    
    label <- paste( "Decline Risk: ", methodLab,", Proj=",nProjYears,
                    " Trend=",trendYears," (",lowerTrendPct,",",upperTrendPct,
                    ")", sep="" )
    mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, label )
  }
     
  return( invisible() )
}     # END function .plotHCRviewDeclineRisk


# .plotYield    (plot true and estimated yield curves)
# Purpose:      Plot the true yield curve (based on equilibrium analsyis)
#               and the estimated curve from the surplus production model fit
#               in the final year of the management procedure.  Dots on each
#               curve show the spawning stock biomass level associated with MSY.
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       K.R. Holt
.plotYield <- function(obj, iSim=1, iRep=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Plot yield against spawning stock biomass.
  rp <- calcRefPoints( obj$pars, rpList=list( FALL=TRUE ) )

  assessMethod<-obj$guiPars$assess
  if ( assessMethod=="pMod" )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )

    # Extract parameter estimates in last year:
    mpdPars <-obj$mp$assess$mpdPars
    B0Est<-mpdPars[7][mpdPars[1]==iRep & mpdPars[2]==obj$pars$nT] *2
    rEst<- mpdPars[8][mpdPars[1]==iRep& mpdPars[2]==obj$pars$nT ] *2
    msyEst<-rEst*B0Est/4
    BmsyEst<-B0Est/2

    # Calculate estimated yied curve:
    X<-seq(0,(B0Est+1),by=1)
    Y<-rEst*X*(1-(X/B0Est))

    # Specify axis limits
    # x-axis limits.
    if ( xScale )
    {
      xAvg <- mean(mpdPars[7])
      xLim<-c(0,max((1.2*xAvg),obj$guiPars$pars$B0))
    }
    else
      xLim <- xAxis
    # y-axis limits.
    if ( yScale )
    {
      yAvg <- mean(mpdPars[8]*mpdPars[7]/4)
      yLim <- c(0,max(1.2*mean(yAvg), obj$guiPars$other$yieldFmsy))
    }
    else
      yLim<-yAxis

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Add true curve
    lines( rp$ssb, rp$yield, lwd=2 )
    points( rp$ssbFmsy, rp$yieldFmsy, cex=.CEXSYM4, bg=.BmsyBG, pch=.BmsyPCH )

    # Add estimated curve
    lines(X,Y, lwd=2, col="red")
    points( BmsyEst, msyEst, cex=.CEXSYM4, bg=.FmsyBG, pch=21 )

    # Make it look pretty
    axis( side=1 )
    axis( side=2, las=.YAXISLAS )
    box()
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Spawning Biomass" )
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Yield" )

    if ( annotate )
      {
        panLegend( 0.05,0.95, legTxt=c("Equilibrium Analysis","Production Model Estimate"),
            lty=c(1,1), col=c("black", "red"), lwd=c(2,2), bg="white"  )
        mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, "Yield Curve", bty=.LEGBTY )
      }
  }
  else
  {
    plot( c(0,1),c(0,1),type="n", axes=FALSE, xlab="", ylab="" )
    panLab( 0.5, 0.5, cex=1.4, "Plot not Implemented for this Procedure" )
  }
}

.plotAcceptProbDecline <- function(
  pDecline=matrix( c( 0.01,0.01,0.01, 0.05,0.05,0.025, 0.05,0.05,0.025,
                      0.95,0.50,0.25, 0.95,0.50,0.25,  0.95,0.95,0.95 ),
                      nrow=3, ncol=6 ),
                      lowerLimit=0.4, upperLimit=0.8 )
{
  cat( "\nMSG (.plotAcceptProbDecline) Acceptable Probability of Decline:\n\n" )
  print( pDecline )
  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy).    
  xLim <- c(0,1)
  trendLabels <- c( "Increasing","Stable","Decreasing" )
  for ( i in 1:nrow(pDecline) )
  {
    plot( xLim,c(0,1), type="n", axes=FALSE, xlab="", xlim=xLim, ylab="" )

    for ( j in 1:3 )
      lines( x=c(xLim[1],lowerLimit, lowerLimit,upperLimit, upperLimit,xLim[2]),
             y=pDecline[i,],
             lty=1,lwd=2 )
               
    abline( v=lowerLimit, lty=2 )
    abline( v=upperLimit, lty=2 )

    text( 0.01, 0.9, adj=0, trendLabels[i] )

    axis( side=1 )
    axis( side=2, las=2 )
    box()

    mtext( side=1, line=1, cex=1.0, outer=TRUE, "Stock Status (Depletion)" )
    mtext( side=2, line=1, cex=1.0, outer=TRUE,
           "Acceptable Probability of Decline" )
  }
}     # END function .plotAcceptProbDecline.

#.plotPaAdj    (plot spawning biomass, HCR, catch showing AP Adjustment.
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual perceived HCR, and third
#                showing catch.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotPaAdj <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
                useYears=FALSE ) )
{
  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  
  # Stock status zone boundaries on SSB scale.
  Bmsy   <- obj$refPtList$ssbFmsy
  Fmsy   <- obj$refPtList$Fmsy
  omBt   <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  omBexp <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]
  
  # Get projected exploitable biomass trajectory.
  exploitBt <- obj$mp$assess$exploitBt[ iRep, (2:ncol(obj$mp$assess$exploitBt)) ]

  # Factor to adjust exploitable biomass for error.  
  eBioAdj <- obj$mp$hcr$eBioAdj[ iRep,2:ncol(obj$mp$hcr$eBioAdj) ]
  
  Dt <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]
  
  targetF <- obj$mp$hcr$targetF[ iRep,c(2:ncol(obj$mp$hcr$targetF)) ]
  
  It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]

  xLim <- gfx$xLim

  # X-axis limits (same for all three panels)
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Panel 1: Plot biomass and adjustment.
  # Y-axis limits.
  if ( !is.null(gfx$yLim) )
    yLim <- gfx$yLim[1,]
  else
  {
    yLim <- c(0,max(omBt))
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  # OM spawning biomass.
  lines( c(1:nT), omBt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  
  # OM exploitable biomass.
  lines( c(1:nT), omBexp, col="gray", lty=.BtLTY, lwd=.BtLWD )
  
  # Adjustment.
  adjExploitBt <- eBioAdj * exploitBt
  segments( c(tMP:nT), exploitBt[tMP:nT], c(tMP:nT), adjExploitBt[tMP:nT] )

  #points( c(tMP:nT), adjB[tMP:nT], bg="black", cex=.CEXSYM20, pch=21 )
  colVec <- rev( heat.colors(nT-tMP+1) )  
  points( c(tMP:nT), exploitBt[tMP:nT], bg=colVec, cex=.CEXSYM20, pch=21 )
    
  abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  abline( v=tMP,  col=.tMPCOL,  lty=.tMPLTY,  lwd=.tMPLWD )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS3 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "OM and MP Biomass" )
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Year" )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c(.BmsyLAB),lty=.BmsyLTY,col=.BmsyCOL, lwd=.BmsyLWD,
               cex=.CEXLEG2, bg="white", bty=.LEGBTY )
  }

  # Panel 2: Plot F vs. SSB.
  
  # Determine HCR type and get properties.
  hcrType <- obj$ctlList$mp$hcr$hcrType
  if ( hcrType=="constantF" | hcrType=="variableF" )
  {
    if( obj$mp$hcr$specs$remRefBase == "rrBaseFmsy" )
      remRate <- obj$refPtList$Fmsy
  	if ( obj$mp$hcr$specs$remRefBase == "rrBaseF01" )
      remRate <- obj$refPtList$F01
    if ( obj$mp$hcr$specs$remRefBase == "rrBaseFspr" )
      remRate <- obj$refPtList$FsprX
  }

  # Calculate targeted (intended) HCR bound points.
  lowerBoundMult <- obj$ctlList$mp$hcr$lowerBoundMult
  upperBoundMult <- obj$ctlList$mp$hcr$upperBoundMult
  
  # Panel 1: Plot biomass and adjustment.
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(gfx$xLim) )
    xLim <- c(0,max( c(Bmsy,exploitBt[tMP:nT]), na.rm=TRUE))
  
  # Y-axis limits.
  if ( !is.null(gfx$yLim) )
    yLim <- gfx$yLim[2,]
  else
    yLim <- c(0,max(c(Fmsy,targetF),na.rm=TRUE))
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  arrows( exploitBt[tMP:nT], targetF[tMP:nT], adjExploitBt[tMP:nT], targetF[tMP:nT],
          length=0.10, angle=20 )
  points( exploitBt[tMP:nT],   targetF[tMP:nT], bg=colVec, cex=.FtCEX*1.4, pch=.FtPCH )

  abline( h=Fmsy, lty=.FmsyLTY, col=.FmsyCOL, lwd=.FmsyLWD )
  abline( h=obj$refPtList$Fcra, lty=.FcraLTY, col=.FcraCOL )
  abline( v=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  
  .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS3 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Fishing Mortality" )
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Projected Exploitable Biomass" )  

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c(.FmsyLAB),lty=.FmsyLTY,col=.FmsyCOL, lwd=.FmsyLWD,
               cex=.CEXLEG2, bg="white", bty=.LEGBTY )
  }

  # X-axis limits (same for all three panels)
  if ( is.null(gfx$xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Panel 3: Plot catch.
  # Y-axis limits.
  if ( !is.null(gfx$yLim) )
    yLim <- gfx$yLim[3,]
  else
  {
    yLim <- c(0,max(Dt))
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )

  lines( c(1:nT), Dt, lty=.DtLTY, lwd=.DtLWD, col=.DtCOL )
  points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX*1.5, pch=.DtPCH )
  points( c(tMP:nT), Dt[ tMP:nT ], bg=colVec, cex=.DtCEX*1.5, pch=.DtPCH )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS3 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Year"  )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Catch" )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c(.MSYLAB),lty=.MSYLTY,col=.MSYCOL, lwd=.MSYLWD,
               cex=.CEXLEG2, bg="white", bty=.LEGBTY )
  }
}     # END function .plotPaAdj

.plotDeclineRisk <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  drawTrend <- function( y2, slope, xDelta, t )
  {
    x2 <- t
    x1 <- x2 - xDelta + 1
    #y1 <- -1 * (xDelta * slope) + y2
    y1 <- y2 * exp( xDelta * slope*-1 )
    segments( x1, y1, x2, y2 )
  }

  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  idx <- tMP:nT
  
  lowerQuota <- obj$ctlList$mp$hcr$lowerQuota
  upperQuota <- obj$ctlList$mp$hcr$upperQuota
  nQlevels   <- obj$ctlList$mp$hcr$nQlevels

  pStar      <- obj$mp$hcr$pStar
  pStar      <- pStar[ pStar[,1]==iRep,c(2:ncol(pStar)) ]
  
  trendYears <- obj$ctlList$mp$hcr$trendYears

  trendBio   <- obj$mp$assess$trendBio
  trendBio   <- trendBio[ trendBio[,1]==iRep,c(2:ncol(trendBio)) ]
  
  trendVal   <- obj$mp$assess$trendVal
  trendVal   <- trendVal[ trendVal[,1]==iRep,c(2:ncol(trendVal)) ]  
  
  Bt         <- obj$om$Bt
  Bt         <- Bt[ Bt[,"iRep"]==iRep, c(2:ncol(Bt)) ]
  
  retroSpawnBt <- obj$mp$assess$retroSpawnBt
  retroSpawnBt <- retroSpawnBt[ retroSpawnBt[,"iRep"]==iRep, c(2:ncol(retroSpawnBt)) ]
  
  # Now the last element in retroSpawnBt is the projSpawnBio, so we need the
  # second to last element for spawnBT in any retrospective year.  This is what
  # is used in the trend calculation, not the projected spawning biomass.
  
  trendSpawnBT <- rep( NA,nrow(retroSpawnBt) )
  for ( i in 1:length(trendSpawnBT) )
    trendSpawnBT[i] <- retroSpawnBt[ i, retroSpawnBt[i,"tStep"]-1 ]
  print( trendSpawnBT )
 
  Dt         <- obj$om$Dt
  Dt         <- Dt[ Dt[,"iRep"]==iRep, c(2:ncol(Dt)) ]
  
  MSY        <- obj$refPtList$yieldFmsy
  Bmsy       <- obj$refPtList$ssbFmsy

  # Plotting attributes.
  trendCol <- rep( "white", length(trendBio) )
  trendCol <- ifelse( trendVal==1, "green", trendCol )
  trendCol <- ifelse( trendVal==3, "red", trendCol )
  
  trendSym <- rep( 21, length(trendBio) )
  trendSym <- ifelse( trendVal==1, 24, trendSym )
  trendSym <- ifelse( trendVal==3, 25, trendSym )
  
  xLim <- gfx$xLim
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  
  if ( gfx$showProj )
    xLim <- c( (tMP - trendYears-1),nT )
  
  # Plot OM spawning biomass and terminal spawning biomass for each
  # time step.  Add trend slope, and color symbols as increasing, stable, decreasing.

  yLim <- gfx$yLim[,1]  
  if ( is.null(yLim) )
    yLim <- c( 0,max(Bt[xLim[1]:xLim[2]]))
  
  plot( c(1:nT), Bt, type="n", axes=FALSE, xlim=xLim, ylim=yLim )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  
  drawTrend( trendSpawnBT, trendBio[idx], trendYears, idx )

  points( idx, trendSpawnBT, bg=trendCol[idx], cex=.BtCEX, pch=.BtPCH )
    
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.4,
      legTxt=c( .BmsyLAB, .BtLAB, paste( "Trend (",trendYears," year)",sep="" ),
                "Est. SSB (increasing)","Est. SSB (stable)","Est. SSB (decreasing)" ),
      col=c( .BmsyCOL, .BtCOL, "black", "black","black","black" ),
      pch=c(NA,NA,NA, .BtPCH, .BtPCH, .BtPCH ),
      pt.cex=c(NA,NA,NA,.BtCEX, .BtCEX, .BtCEX),
      pt.bg=c(NA,NA,NA, "green","white","red" ),
      lwd=c(.BmsyLWD, .BtLWD,1, NA,NA,NA ),
      lty=c(.BmsyLTY,.BtLTY,1, NA,NA,NA ),
      bg="white", bty=.LEGBTY )  
  }

  mtext( side=2, line=3, cex=.CEXLAB4, paste( "Biomass"," (",.BexpUNIT,")", sep="") )
  
  # Plot pStar and catch over time.
  yLim <- gfx$yLim[,2]  
  if ( is.null(yLim) )
    yLim <- c(0,max(Dt))  
   
  plot( c(1:nT), Dt, type="n", axes=FALSE, xlim=xLim, ylim=yLim )
  abline( h=MSY, col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD )
  lines( c(1:nT), Dt, type="h", col="gray", lty=1, lwd=4 )
  axis( side=4, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  
  par( new=TRUE )
  yLim <- c(0,1)
  plot( c(1:nT), pStar, type="n", axes=FALSE, xlim=xLim, ylim=yLim )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    
  points( c(1:nT), pStar, bg=trendCol, cex=.CEXSYM8, pch=trendSym )
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
    
  box()
  
  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95,
      legTxt=c( .MSYLAB, .DtLAB, "pStar (increasing)", "pStar (stable)",
                "pStar (decreasing)" ),
      col=c( .MSYCOL, "gray", "black","black","black" ),
      pch=c(NA,NA,24,21,25), pt.cex=c(NA,NA,.CEXSYM8,.CEXSYM8,.CEXSYM8),
      pt.bg=c(NA,NA,"green","white","red"),
      lwd=c( .MSYLWD, 4,NA,NA,NA),
      lty=c(.MSYLTY, 1,NA,NA,NA),
      bg="white", bty=.LEGBTY )  
  }  
  
  mtext( side=2, line=3, cex=.CEXLAB4, "pStar" )
  mtext( side=4, line=3, cex=.CEXLAB4, paste( .DtLAB," (",.DtUNIT,")", sep="" ) )
  
  mtext( side=1, line=0, cex=.CEXLAB4, outer=TRUE, "Year" )
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=-1, cex=.CEXTITLE4, outer=TRUE,
      paste( "Decline Risk Rule:", .METHODLAB[obj$ctlList$mp$assess$methodId] ) )
  }
   
}     # END function .plotDeclineRisk


.plotRetroStat1 <- function( obj, iSim=1, iRep=1, statName="SumRelErr",
  gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  retroStats <- obj$mp$assess$retroStats
  retroVals  <- obj$mp$assess$retroVals[,,statName]
  
  nReps      <- nrow( retroStats )
  nT         <- obj$ctlList$opMod$nT
  retroYears <- obj$ctlList$mp$assess$retroYears
  tMP        <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c(1,nT)
  }
  
  if ( gfx$showProj )
  {
    xLim <- c( tMP-retroYears-1, nT )
  }
  
  yLim <- range( retroVals[ ,c(3:ncol(retroVals)) ], na.rm=TRUE ) 
  
  # Plot the retrospective values.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
  
  vals <- retroVals[ retroVals[,"iRep"]==iRep, ]
  idx  <- c( 3:(nT+2) )
  nSteps <- nrow(vals)
  colVec <- rev( heat.colors(nrow(vals)) )
  
  for ( i in 1:nSteps )
  {
    lines( c(1:nT), vals[ i, idx ] )
    tStep <- vals[ i,"tStep" ]
    points( tStep, vals[ i, tStep+2 ], bg=colVec[i], cex=1.4, pch=21 )
  }
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE2, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Retrospective Values" ) 
  
  # Plot the retrospective statistics.
  xLim <- c(1,nReps)
  yLim <- range( retroStats[ ,statName ], na.rm=T )
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( h=mean( retroStats[ ,statName ] ), lty=3, lwd=2 )
  
  points( c(1:nReps), retroStats[ ,statName ], bg="white", cex=1.5, pch=21 )
  points( iRep, retroStats[ iRep,statName ], bg="black", cex=1.5, pch=21 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Replicate" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( "Statistics: ", statName,sep="" ) )   
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, statName )
  }
  
}     # END function .plotRetroStat1

.plotHistory <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE,
                          xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  tMP        <- obj$opMod$tMP

  # Get the history file.
  hstFile <- file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE )
  if ( !file.exists( hstFile ) )
  {
    cat( "\nMSG (.plotHistory) Stock history file not found: ", hstFile,"\n" )
    alarm()
  }
  else
  {
    tStep      <- NULL
    histOmegat <- NULL
    histFt     <- NULL
    histMt     <- NULL
    if ( obj$opMod$historyType=="omFile" )
    {
      histPars <- read.table( hstFile, as.is=TRUE, header=TRUE, sep="," )
    
      tStep      <- histPars$Year
      histOmegat <- histPars$omegat
      histFt     <- histPars$Ft
      
      histMt <- NULL
      if ( any( names(histPars)=="Mt" ) )
        histMt <- histPars$Mt
    }
    
    # This is trick, yay!
    mpObj   <- .createMP( obj )
    initPop <- .initPop( mpObj )

    tStep <- c(1:(tMP-1))

    Bt    <- initPop$om$Bt[1:(tMP-1)]
    Bexp  <- initPop$om$Bexp[1:(tMP-1)]
    BexpS <- initPop$om$BexpS[1:(tMP-1)]
    Dt    <- initPop$om$Dt[1:(tMP-1)]
    Ft    <- initPop$om$Ft[1:(tMP-1)]
    Mt    <- initPop$om$Mt[1:(tMP-1)]
    It    <- initPop$mp$data$It[1:(tMP-1)]
    q     <- obj$opMod$q
    
    Rt   <- initPop$om$Rt[1:(tMP-1)]
    
    Bmsy <- obj$refPts$ssbFmsy
    MSY  <- obj$refPts$yieldFmsy
    
    xLim <- gfx$xLim
    if ( is.null( xLim ) )
    {
      xLim <- range( tStep )
    }
    
    yLim <- c( 0,max(Rt) )
  
    # Plot the recruitments.
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
  
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )    
  
    points( tStep,Rt, bg=.RtBG, cex=.RtCEX, col=.RtCOL, pch=.RtPCH )
  
    xTicks <- axTicks( side=1 )
    xLabs  <- as.character( xTicks )
    
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS3 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    
    par( new=T )
    yLim <- c(0,max(Mt, na.rm=TRUE))
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
    
    lines( tStep, Mt, col=.MtCOL, lwd=.MtLWD )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS )   
    box()
  
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Recruitment" )
    mtext( side=4, line=.INLINE3, cex=.CEXLAB2, "Natural mortality" ) 

     # Plot the catches and fishing mortalities.
    yLim <- c(0,max(Dt,na.rm=T))
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
    
    abline( h=MSY, col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD )    
    lines( tStep, Dt, type="h", col=.DtCOL, lwd=4 )
    
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS3 )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS )

    par( new=T )
    yLim <- c(0,max(Ft, na.rm=TRUE))
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
    
    points( tStep, Ft, bg=.FtBG, cex=.FtCEX+0.4, col=.FtCOL, pch=.FtPCH )
    if ( !is.null( histFt ) )
      points( tStep, histFt, bg="black", cex=.FtCEX, col=.FtCOL, pch=.FtPCH )  

    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    box()
  
    if ( gfx$doLegend )
    {
      panLegend( 0.7,0.98, legTxt=c(.FtLAB, "History File Ft",.DtLAB, .MSYLAB ),
            lty=c(NA, NA, .DtLTY, .MSYLTY), lwd=c(NA,NA,4,.MSYLWD),
            col=c(.FtCOL,.FtCOL,.DtCOL, .MSYCOL), pch=c(.FtPCH,.FtPCH,NA,NA),
            pt.cex=c(.FtCEX,.FtCEX,NA,NA),
            pt.bg=c(.FtBG,"black",NA,NA), bg="white", cex=.CEXLEG2, bty=.LEGBTY  )
    }  
  
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, .FtLAB )
    mtext( side=4, line=.INLINE3, cex=.CEXLAB2, .DtLAB )
    
    # Plot the reconstructed biomass trajectories.
    yLim <- c( 0,max(Bt) )
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )

    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  
  
    lines( c(1:(tMP-1)), Bt,    col=.BtCOL,    lty=.BtLTY,    lwd=.BtLWD )
    lines( c(1:(tMP-1)), Bexp,  col=.BexpCOL,  lty=.BexpLTY,  lwd=.BexpLWD )
    lines( c(1:(tMP-1)), BexpS, col=.BexpSCOL, lty=.BexpSLTY, lwd=.BexpSLWD )
    
    # Scale the index to the spawning biomass.  
    points( c(1:(tMP-1)), It/q,   bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears, cexAxis=.CEXAXIS3 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
 
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()

    if ( gfx$doLegend )
    {
      panLegend( 0.025,0.6, legTxt=c(.BtLAB, .BexpLAB, .BexpSLAB, .BmsyLAB, .ItLAB),
            lty=c(.BtLTY,.BexpLTY,.BexpSLTY,.BmsyLTY, NA ), lwd=c(.BtLWD,.BexpLWD, .BexpSLWD, .BmsyLWD, NA),
            col=c(.BtCOL,.BexpCOL, .BexpSCOL, .BmsyCOL, .ItCOL ), pch=c(NA,NA,NA,NA,.ItPCH),
            pt.cex=c(NA,NA,NA,NA,.ItCEX),
            pt.bg=c(NA,NA,NA,.ItBG), bg="beige", cex=.CEXLEG2, bty="o"  )
    }
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Biomass" )  
  }  
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Year" )
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Initialization History" )
  }
}     # END function .plotHistory


#function to plot estimated and OM values of M, h, Bo and q -- to help with diagnostics -- added by RF 28 Sep 2013
.plotParEsts <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
                          showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  # get index lengths
  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  nMP <- nT-tMP+1 #number of assessment years
  
  # Get stationary OM values
  omM  <- obj$ctlList$opMod$M
  omQ  <- obj$ctlList$opMod$q
  omH  <- obj$ctlList$opMod$rSteepness
  omB0 <- obj$ctlList$opMod$B0
  
  omMt  <- obj$om$Mt[ iRep,(2:ncol(obj$om$Mt)) ] #m can be time varying  
    
  #Get MP values
  mpdPars <- obj$mp$assess$mpdPars
  idx <- mpdPars$iRep==iRep
  
  mpM <- NA
  if ( any(names(mpdPars)=="M" ) )
    mpM <- mpdPars$M[idx]
    
  mpQ <- NA
  if ( any(names(mpdPars)=="q" ) )
    mpQ <- mpdPars$q[idx]
    
  mpH <- NA
  if ( any(names(mpdPars)=="rSteepness" ) )
    mpH <- mpdPars$rSteepness[idx]
    
  mpB0 <- NA
  if ( any(names(mpdPars)=="B0" ) )
  mpB0 <- mpdPars$B0[idx]
     
  xLim <- gfx$xLim
  yLim <- gfx$yLim
    
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLimM <- c( 0,1.1*max(c( omMt,mpM), na.rm=TRUE )) 
    yLimQ <- c( 0,1.1*max(c( omQ,mpQ),  na.rm=TRUE )) 
    yLimH <- c( 0,1.1*max(c( omH,mpH),  na.rm=TRUE )) 
    yLimB <- c( 0,1.1*max(c( omB0,mpB0),  na.rm=TRUE )) 
  }
    
  # M
  plot( xLim, yLimM, type="n", axes=FALSE, xlab="", ylab="" )
  abline( h=omM, col="darkblue", lty=.MLTY, lwd=.MLWD )
  lines( c(1:nT), omMt, col=.MtCOL, lty=.MtLTY, lwd=.MtLWD )
  lines( c(tMP:nT), mpM, col=.MtCOL, lty=2, lwd=.MtLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, expression(italic(M)))

  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXTITLE, expression(italic(M)) )
    
  if ( gfx$doLegend )
  {
    panLegend( 0.5, 0.3, legTxt=c("OM", "Est"),
               lty=c(.MtLTY,2), 
               lwd=c(.MtLWD,.MtLWD),
               col=c("darkblue",2),
               cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }
  
  #Survey q
  plot( xLim, yLimQ, type="n", axes=FALSE, xlab="", ylab="" )
  abline( h=omQ, col="darkblue", lty=.MtLTY, lwd=.MLWD )
  lines( c(tMP:nT), mpQ, col=2, lty=2, lwd=.MtLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
      
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, expression(italic(q)))

  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXTITLE, expression(italic(q)) )
   
  #S-R Steepness
  plot( xLim, yLimH, type="n", axes=FALSE, xlab="", ylab="" )
  abline( h=omH, col="darkblue", lty=.MtLTY, lwd=.MLWD )
  lines( c(tMP:nT), mpH, col=2, lty=2, lwd=.MtLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
      
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Steepness" )

  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXTITLE, "Steepness" )
          
  #Bo
  plot( xLim, yLimB, type="n", axes=FALSE, xlab="", ylab="" )
  abline( h=omB0, col="darkblue", lty=.MtLTY, lwd=.MLWD )
  lines( c(tMP:nT), mpB0, col=2, lty=2, lwd=.MtLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
      
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, expression(italic(B)[0]) ) 
  
  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXTITLE, expression(italic(B)[0]) )
     
  mtext( side=1, line=0, cex=.CEXLAB2, outer=TRUE, "Year" )
  
  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXLAB2, outer=TRUE, "Operating Model vs Estimated Parameters" )

  return( invisible() )  
}     # END function .plotParEsts