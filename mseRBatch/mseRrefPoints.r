#------------------------------------------------------------------------------#
#------ mseRrefPoints.r: life history & reference point calculations ----------#
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
#                                                                              #
#--                                                                          --#
#-- mseRrefPoints.r: An mseR module that provides life history and           --#
#                     refernce point calculations.                           --#
#--                                                                          --#
#-- Authors: S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--          A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--                                                                          --#
#-- First Implementation: 29-Jan-09.                                         --#
#--                                                                          --#                                                                              
#                                                                              #
# ------------------- Main Reference Point Function -------------------------- #
#                                                                              #
# calcRefPoints    :  Primary function; returns ref.points based on input pars #
#                  : SPC modified 1-May-2010 added list of F options to return #
#                  : now has additional arg with syntax                        #
#                  : rpList=list( F01=TRUE, F40=FALSE, FALL=TRUE )
#                                                                              #
# ------------- Reference Point Helper Functions (HIDDEN): ------------------- #
# .calcEquil      : Equilibrium yield calculations.                            #
# .calcPerRecruit : Calculate equilibrium per-recruit for input fishing mort.  #
# .calcRefCurves  : Calculate all equilibrium relationships to fishing mort.   #
# .calcSchedules  : Calculate length-, weight-, and maturity-at-age vectors    #
# .getF01         : Calculate F0.1 by fitting spline to F vs.YPR relationship  #
# .getFmax        : Calculate Fmax by fitting spline to F vs. YPR relationship #
# .getFmsy        : Calculate Fmsy by fitting spline to F vs. yield            #
# .getF40         : Calculate F40 by fitting spline to F vs. SSB-per-recruit   #
# .getFx          : Calculate Fx%, where x is a percentile input by user       #
# .getFcra        : Calculate Fcrash by fitting spline to F vs. SSB            #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#-- Main Reference Point Function (PUBLIC)                                   --#
#------------------------------------------------------------------------------#

# calcRefPoints
# Purpose:     Primary function to input population parameters, compute life 
#              history schedules, equilibrium relationships to F, and equilibrium
#              reference points. 
# Parameters:  obj=list of all operating model parameters, input from gui via createMP,
#              rpList=a list of ref pts to compute (see below for names), application=
#              calling function.
# Returns:     a list with all life history schedules (vectors), equilibrium relationships 
#              to F (vectors), and equilibrium points (scalars)
#              reference points.
# Source:      S.P. Cox
calcRefPoints <- function( obj, rpList, application="OM" )
{
 
  # Add life history schedules to parameters
  obj <- .calcSchedules( obj )
  
  # Add spr and ypr
  obj <- .calcPerRecruit( f=0, obj=obj )
 
  # Extract unfished biomass and steepness
  B0         <- obj$B0
  rSteepness <- obj$rSteepness
 
  # If calcRefPoints function called from operating model (application = "OM" 
  # in passed arguments, default case), calculate recruitment parameters based 
  # on user-specified steepness from guiSim window
  if (application=="OM" ) 
  {
    # unfished recruitment
    obj$R0  <- B0/obj$ssbpr
    # Calculate Beverton-Holt stock-recruitment parameters
    obj$rec.a  <- 4.*rSteepness*obj$R0/( B0*(1.-rSteepness) )
    obj$rec.b  <- (5.*rSteepness-1.)/( B0*(1.-rSteepness) )
  }
  
  # If calcRefPoints function called from management procedure as part of the 
  # assessment step (application = "MP" specified in passed arguements), no 
  # need to calculate recruitment parameters.  The estimated parameters should 
  # already be included in the passed obj list. 
   
  B20  <- 0.2*B0
  R20  <- obj$rec.a*B20/( 1.+obj$rec.b*B20 )

  # Initialise population at equilibrium
  A <- obj$nAges
  a <- obj$ages[c(-1,-A)]
  obj$numAgeYr1    <- numeric( length=A )
  obj$numAgeYr1[1] <- obj$R0  
  obj$numAgeYr1[a] <- obj$R0*exp( -obj$M*(a-1) )
  obj$numAgeYr1[A] <- obj$R0*exp( -obj$M*(A-1) )/(1.-exp(-obj$M))

  # Calculate reference curves
  obj <- .calcRefCurves( obj )
  
  # Calculate reference points

  # Recruitment calcs for ref pts/steepness plot
  obj$B20  <- B20
  obj$R20  <- R20

  # Unfished F0
  if( !is.null(rpList$F0) | rpList$FALL ){
    tmp            <- .calcEquil( f=0, obj )
    obj$F0         <- 0
    obj$yprF0      <- tmp$ypr
    obj$ssbprF0    <- tmp$ssbpr
    obj$yieldF0    <- tmp$yield
    obj$ssbF0      <- tmp$ssb
    obj$expbF0     <- tmp$expb
    obj$recruitsF0 <- tmp$recruits
  }
  # F0.1
  if( !is.null(rpList$F01) | rpList$FALL ){
    tmp             <- .getF01( obj )
    obj$F01         <- tmp$F01
    obj$yprF01      <- tmp$yprF01
    obj$ssbprF01    <- tmp$ssbprF01
    obj$yieldF01    <- tmp$yieldF01
    obj$ssbF01      <- tmp$ssbF01
    obj$expbF01     <- tmp$expbF01
    obj$recruitsF01 <- tmp$recruitsF01
  }  
  # Fmsy
  if( !is.null(rpList$Fmsy) | rpList$FALL ){
    tmp              <- .getFmsy( obj )
    obj$Fmsy         <- tmp$Fmsy
    obj$yprFmsy      <- tmp$yprFmsy
    obj$ssbprFmsy    <- tmp$ssbprFmsy
    obj$yieldFmsy    <- tmp$yieldFmsy
    obj$ssbFmsy      <- tmp$ssbFmsy
    obj$expbFmsy     <- tmp$expbFmsy
    obj$recruitsFmsy <- tmp$recruitsFmsy
  }
  # Fx%
	if( !is.null(rpList$Fx)  ){
    tmp             <- .getFx( obj, x=rpList$x )
    obj$Fx         <- tmp$Fx
    obj$yprFx      <- tmp$yprFx
    obj$ssbprFx    <- tmp$ssbprFx
    obj$yieldFx    <- tmp$yieldFx
    obj$ssbFx      <- tmp$ssbFx
    obj$expbFx     <- tmp$expbFx
    obj$recruitsFx <- tmp$recruitsFx
  }
  # F40%
  if( !is.null(rpList$F40) | rpList$FALL ){
    tmp             <- .getF40( obj )
    obj$F40         <- tmp$F40
    obj$yprF40      <- tmp$yprF40
    obj$ssbprF40    <- tmp$ssbprF40
    obj$yieldF40    <- tmp$yieldF40
    obj$ssbF40      <- tmp$ssbF40
    obj$expbF40     <- tmp$expbF40
    obj$recruitsF40 <- tmp$recruitsF40
  }
  # Fmax 
  if( !is.null(rpList$Fmax) | rpList$FALL ){
    tmp              <- .getFmax( obj )
    obj$Fmax         <- tmp$Fmax
    obj$yprFmax      <- tmp$yprFmax
    obj$ssbprFmax    <- tmp$ssbprFmax
    obj$yieldFmax    <- tmp$yieldFmax
    obj$ssbFmax      <- tmp$ssbFmax
    obj$expbFmax     <- tmp$expbFmax
    obj$recruitsFmax <- tmp$recruitsFmax
  }
  # Fcrash
  if( !is.null(rpList$Fcra) | rpList$FALL ){
    tmp              <- .getFcra( obj )
    obj$Fcra         <- tmp$Fcra
    obj$yprFcra      <- tmp$yprFcra
    obj$ssbprFcra    <- tmp$ssbprFcra
    obj$yieldFcra    <- tmp$yieldFcra
    obj$ssbFcra      <- tmp$ssbFcra
    obj$expbFcra     <- tmp$expbFcra
    obj$recruitsFcra <- tmp$recruitsFcra
  }
  
  obj
}     # END function calcRefPoints

#------------------------------------------------------------------------------#
#-- Reference Point Helper Functions (HIDDEN)                                --#
#------------------------------------------------------------------------------#

# .calcEquil   
# Purpose:     Calculate all equilibrium quantities of interest for an
#              input fishing mortality.
# Parameters:  f=scalar input fishing mortality rate; obj=list of all operating 
#              model parameters.
# Returns:     a list with equilibrium quantities - (i) total recruits,spawning
#              biomass (ssb) and yield; (ii) spawning stock biomass-per-recruit
#              (ssbpr), and (iii) yield-per-recruit (ypr) 
# Source:      S.P. Cox
.calcEquil <- function( f=0, obj )
{
  # Compute yield and biomass per recruit function values
  tmp <- .calcPerRecruit( f=f,obj=obj )

  # Beverton-Holt sr model parameters
  rec.a <- obj$rec.a
  rec.b <- obj$rec.b
  
  # Compute equilibrium recruitment, biomass and yield
  recruits <- (rec.a*tmp$ssbpr - 1.0) / (rec.b*tmp$ssbpr)# Eq(E3.4)
  ssb      <- recruits * tmp$ssbpr                       # Eq(E3.5)
  exb      <- recruits * tmp$exbpr
  yield    <- exb * ( 1.-exp(-obj$M-f) ) * f / (obj$M+f) # Eq(E3.6)

  equil <- list()
    equil$recruits <- recruits
    equil$ssbpr    <- tmp$ssbpr
    equil$ssb      <- ssb
    equil$exb      <- exb
    equil$ypr      <- tmp$ypr
    equil$yield    <- yield
  equil
}     # END function calcRefPoints

# .calcPerRecruit   
# Purpose:     Calculate all equilibrium per-recruit quantities of interest for an
#              input fishing mortality.
# Parameters:  f=scalar input fishing mortality rate; obj=list of all operating 
#              model parameters.
# Returns:     a list with equilibrium quantities - (i) spawning stock biomass-per-recruit
#              and (ii) yield-per-recruit (ypr) 
# Source:      S.P. Cox (SPC modified to use "sel" if provided: 1 May 2010)
.calcPerRecruit <- function( f=0, obj )
{
  # Compute equilibrium spawning biomass per recruit given f and parameters.

  A <- obj$nAges
  M <- obj$M

  mat <- obj$matAge
  len <- obj$lenAge
  wt  <- obj$wtAge
  sel <- obj$selAge

  # Survivorship-at-age Eq(E3.1)
  l <- numeric( length=A )
  # age-1
  l[1] <- 1.0
  # age-2 to age-(A-1)
  for( a in 2:(A-1) )
    l[a] <- l[a-1]*exp( -M - sel[a-1]*f )
  # age-A+
  l[A] <- l[A-1]*exp(-M - sel[A-1]*f)/(1.0-exp(-M - sel[A]*f))
  
  # ypr function Eq(3.2)
  ypr   <- sum( l*sel*wt*f*(1.0 - exp(-M-sel*f))/(M+sel*f) ) 
  
  # spr function Eq(E3.3)
  ssbpr <- sum( l*mat*wt )    

  # exbpr function for 1 =< ages =< A.
  exbpr <- sum( l*sel*wt)
  
  phi <- obj
    phi$ssbpr <- ssbpr
    phi$ypr   <- ypr
    phi$exbpr <- exbpr
  phi
}     # END function .calcPerRecruit

# .calcRefCurves   
# Purpose:     Calculate all equilibrium relationships to fishing mortality.
# Parameters:  obj=list of all operating model parameters; nFs=the number of 
#              fishing mortality points over which to compute the functions
# Returns:     a list with vectors of fishing mortality (f) and equilibrium 
#              functions (these are later fitted with splines for finding 
#              references points via root-finding algorithms)
# Source:      S.P. Cox
.calcRefCurves <- function( obj, nFs=1000 )
{
  maxF <- obj$maxF

  f <- seq( from=0.0, to=maxF, length=nFs )

  recruits <- rep( NA, length=nFs )
  ssbpr    <- rep( NA, length=nFs )
  ssb      <- rep( NA, length=nFs )
  exb      <- rep( NA, length=nFs )
  ypr      <- rep( NA, length=nFs )
  yield    <- rep( NA, length=nFs )

  for( i in 1:length(f) )
  {
    tmp        <- .calcEquil( f=f[i],obj=obj )
    recruits[i]<- tmp$recruits
    ssbpr[i]   <- tmp$ssbpr
    ssb[i]     <- tmp$ssb
    exb[i]     <- tmp$exb
    ypr[i]     <- tmp$ypr
    yield[i]   <- tmp$yield
  }
  
  refCurves <- obj
    refCurves$F        <- f
    refCurves$ypr      <- ypr
    refCurves$ssbpr    <- ssbpr
    refCurves$yield    <- yield
    refCurves$ssb      <- ssb
    refCurves$exb      <- exb
    refCurves$recruits <- recruits
  refCurves  
}     # END function .calcRefCurves

# .calcSchedules
# Purpose:     Calculate length-, weight-, and maturity-at-age vectors
# Parameters:  obj=list of all operating model parameters; nFs=the number of 
#              fishing mortality points over which to compute the functions
# Returns:     a list with vectors for each life history schedule
# Source:      S.P. Cox
.calcSchedules <- function( obj )
{
  # Extract constants for later dimensioning.
  A       <- obj$nAges
  obj$ages <- c(1:A)

  # Extract parameters for setting up the population dynamics model
  # life history pars
  aMat50 <- obj$aMat50
  aMat95 <- obj$aMat95
  
  # Fishery selectivity.
  aSel50 <- obj$aSel50
  aSel95 <- obj$aSel95
  
  # Survey selectivity.
  aSelS50 <- obj$aSelS50
  aSelS95 <- obj$aSelS95
  
  M      <- obj$M
  Linf   <- obj$Linf
  L1     <- obj$L1
  vonK   <- obj$vonK
  sigmaL <- obj$sigmaL
  c1     <- obj$c1
  c2     <- obj$c2
  alpha  <- obj$alpha_g
  rho.int    <- obj$rho.int
  rho.slope  <- obj$rho.slope
  rho        <- rho.int + rho.slope*alpha

  # compute life history schedules
  lifeScheds           <- obj
    lifeScheds$lenAge  <- .calcLenAge( Linf=alpha/(1.-rho), vonK=-log(rho), L1=alpha, A=A )
    lifeScheds$wtAge   <- .calcWtAge( c1=c1,c2=c2,lenAge=lifeScheds$lenAge, sigmaL=sigmaL )
    lifeScheds$matAge  <- .calcLogistic( a50=aMat50,a95=aMat95,A=A )
    lifeScheds$selAge  <- .calcLogistic( a50=aSel50,a95=aSel95,A=A )
    lifeScheds$selAgeS <- .calcLogistic( a50=aSelS50, a95=aSelS95, A=A )
  lifeScheds
}     # END function .calcSchedules

# .getF01
# Purpose:     Fit a spline function to f vs ypr, then use a root finder to get F0.1. Note
#              this function can be easily modified to get any F0.X by changing target
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for F0.1
# Source:      S.P. Cox
.getF01 <- function( obj )
{
  maxF <- max( obj$F )
  # create the spline function: this is not a number, it is a function
  # that can be called like any other function, except it only has one
  # argument, in this case F. Spline functions below are similar
  fyprSplineFun <- splinefun( x=obj$F,y=obj$ypr )
  slopeAtOrigin <- fyprSplineFun( x=0, deriv=1 )
  yprRatio <- function( fin ){
    f2     <- fyprSplineFun( x=fin, deriv=1 )
    ratio  <- f2/slopeAtOrigin
    target <- 0.1
    return(ratio - target)
  }
  if( fyprSplineFun( x=maxF,deriv=1 ) > 0 )
    obj$F01 <- maxF
  else
    obj$F01 <- uniroot( f=yprRatio,interval=c(0,maxF) )$root
  
  tmp             <- .calcEquil( f=obj$F01, obj=obj  )
  obj$yprF01      <- tmp$ypr
  obj$ssbprF01    <- tmp$ssbpr
  obj$yieldF01    <- tmp$yield
  obj$ssbF01      <- tmp$ssb
  obj$expbF01     <- tmp$expb
  obj$recruitsF01 <- tmp$recruits
  obj
}     # END function .getF01

# .getFmsy     ()
# Purpose:     fit a spline function to f vs yield, then use a root finder to get Fmsy. 
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for Fmsy
# Source:      S.P. Cox
.getFmsy <- function( obj )
{
  maxF <- max( obj$F )
  fySplineFun <- splinefun( x=obj$F,y=obj$yield )

  #cat("maxF=",maxF,"\n")
  #cat("fySplineFun=",fySplineFun(x=maxF),"\n")
  #cat("dfySplineFun=",fySplineFun(x=maxF,deriv=1),"\n")

  Fmsy <- uniroot( f=fySplineFun,interval=c(0,maxF), deriv=1 )$root
  obj$Fmsy <- min( Fmsy, maxF )

  tmp              <- .calcEquil( f=obj$Fmsy, obj=obj )
  obj$yprFmsy      <- tmp$ypr
  obj$ssbprFmsy    <- tmp$ssbpr
  obj$yieldFmsy    <- tmp$yield
  obj$ssbFmsy      <- tmp$ssb
  obj$expbFmsy     <- tmp$expb
  obj$recruitsFmsy <- tmp$recruits
  obj
}     # END function .getFmsy

# .getF40     ()
# Purpose:     fit a spline function to f vs ssbpr, then use a root finder to get F40%. Can
#              get any FX% by changing the value of "target"
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for F40%
# Source:      S.P. Cox
.getF40 <- function( obj )
{
  maxF <- max( obj$F )
  fssbprSplineFun <- splinefun( x=obj$F,y=obj$ssbpr )
  ssbprAtOrigin   <- fssbprSplineFun( x=0 )
  ssbprRatio <- function( fin ){
    f2 <- fssbprSplineFun( fin )
    ratio <- f2/ssbprAtOrigin
    target <- 0.4
    return(ratio - target)
  }
  F40 <- uniroot( f=ssbprRatio,interval=c(0,maxF) )$root
  obj$F40 <- min( F40, maxF )
  
  tmp             <- .calcEquil( f=obj$F40, obj=obj )
  obj$yprF40      <- tmp$ypr
  obj$ssbprF40    <- tmp$ssbpr
  obj$yieldF40    <- tmp$yield
  obj$ssbF40      <- tmp$ssb
  obj$expbF40     <- tmp$expb
  obj$recruitsF40 <- tmp$recruits
  obj
}     # END function .getF40


# .getFx     ()
# Purpose:     fit a spline function to f vs ssbpr, then use a root finder to get FX%. Can
#              get any FX% by changing the value of "target"
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for F40%
# Source:      S.P. Cox (modified from .getF40 by K.Holt on 11-Feb-2010) 
.getFx <- function( obj, x=40 )
{  
  maxF <- max( obj$F )
  fssbprSplineFun <- splinefun( x=obj$F,y=obj$ssbpr )
  ssbprAtOrigin   <- fssbprSplineFun( x=0 )
  ssbprRatio <- function( fin ){
    f2 <- fssbprSplineFun( fin )
    ratio <- f2/ssbprAtOrigin
    target <- x/100
    return(ratio - target)
  }
  
  Fx <- uniroot( f=ssbprRatio,interval=c(0,maxF) )$root
  obj$Fx <- min( Fx, maxF )
  
  tmp             <- .calcEquil( f=obj$Fx, obj=obj )
  obj$yprFx      <- tmp$ypr
  obj$ssbprFx    <- tmp$ssbpr
  obj$yieldFx    <- tmp$yield
  obj$ssbFx      <- tmp$ssb
  obj$expbFx     <- tmp$expb
  obj$recruitsFx <- tmp$recruits
  obj
}     # END function .getFx

# .getFmax     ()
# Purpose:     fit a spline function to f vs ypr, then use a root finder to get Fmax. 
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for Fmax
# Source:      S.P. Cox
.getFmax <- function( obj )
{
  maxF          <- max( obj$F )
  fyprSplineFun <- splinefun( x=obj$F,y=obj$ypr )

  if( fyprSplineFun( x=maxF,deriv=1 ) > 0 )
    obj$Fmax <- maxF
  else
    obj$Fmax <- uniroot( f=fyprSplineFun,interval=c(0,maxF),deriv=1 )$root

  tmp              <- .calcEquil( f=obj$Fmax, obj=obj )
  obj$yprFmax      <- tmp$ypr
  obj$ssbprFmax    <- tmp$ssbpr
  obj$yieldFmax    <- tmp$yield
  obj$ssbFmax      <- tmp$ssb
  obj$expbFmax     <- tmp$expb
  obj$recruitsFmax <- tmp$recruits
  obj
}     # END .getFmax


# .getFcra
# Purpose:     fit a spline function to f vs ssb, then use a root finder to get Fcra(sh). 
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for Fcra(sh)...only really matters
#              for per-recruit quantities
# Source:      S.P. Cox
.getFcra <- function( obj )
{
  maxF          <- max( obj$F )
  fssbSplineFun <- splinefun( x=obj$F,y=obj$ssb )

  if( fssbSplineFun( x=maxF ) > 0 )
    obj$Fcra <- maxF
  else
    obj$Fcra <- uniroot( f=fssbSplineFun,interval=c(0,maxF) )$root
  
  tmp              <- .calcEquil( f=obj$Fcra, obj=obj )
  obj$yprFcra      <- tmp$ypr
  obj$ssbprFcra    <- tmp$ssbpr
  obj$yieldFcra    <- tmp$yield
  obj$ssbFcra      <- tmp$ssb
  obj$expbFcra     <- tmp$expb
  obj$recruitsFcra <- tmp$recruits
  obj
}     # END .getFcra


# .getFxRefPtCalcs ()
# Purpose:     Primary function to input population parameters, compute life 
#              history schedules, and calculate equilibrium values for Fx%
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a single value for Fx%
# Source:      S.P. Cox (modified from .getF40 by K.Holt on 11-Feb-2010) 
.getFxRefPtCalcs <- function( obj, x )
{
  # add life history schedules to parameters
  obj <- .calcSchedules( obj )
 
  # add spr and ypr
  obj <- .calcPerRecruit( f=0, obj=obj )
    
  # Extract unfished biomass and steepness
  B0         <- obj$B0
  rSteepness <- obj$rSteepness
  
  # unfished recruitment
  obj$R0  <- B0/obj$ssbpr
  
  # Beverton-Holt stock-recruitment parameters
  obj$rec.a  <- 4.*rSteepness*obj$R0/( B0*(1.-rSteepness) )
  obj$rec.b  <- (5.*rSteepness-1.)/( B0*(1.-rSteepness) )

  B20  <- 0.2*B0
  R20  <- obj$rec.a*B20/( 1.+obj$rec.b*B20 )

  # Initialise population at equilibrium
  A <- obj$nAges
  a <- obj$ages[c(-1,-A)]
  obj$numAgeYr1    <- numeric( length=A )
  obj$numAgeYr1[1] <- obj$R0  
  obj$numAgeYr1[a] <- obj$R0*exp( -obj$M*(a-1) )
  obj$numAgeYr1[A] <- obj$R0*exp( -obj$M*(A-1) )/(1.-exp(-obj$M))

  # Calculate reference curves
  obj <- .calcRefCurves( obj )
  
  # Calculate reference points

  # Recruitment calcs for ref pts/steepness plot
  obj$B20  <- B20
  obj$R20  <- R20
  
  Fsprx<-.getFx(obj,x)$Fx
 
  return(Fsprx)
}     # END function .getFxRefPtCalcs
