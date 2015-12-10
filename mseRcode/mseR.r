# Globals.
source( "mseRglobals.r" )
source( "mseRoptions.r" )
source( "mseRtools.r" )

# Batch code.
source( "mseRguiBatchFuns.r" )

# GUI source code.
source( "mseRguiSimFuns.r" )
source( "mseRguiViewFuns.r" )
source( "mseRguiPerfFuns.r" )
source( "mseRguiOptFuns.r" )
source( "mseRguiTrackFuns.r" )
source( "mseRguiHelperFuns.r" )

# Parallel processing and mcmc packages
#require(parallel)  # for detectCores()
#.NPROC <<- detectCores()-1
#detach( package:parallel )
library(snow)      # for actual p.p. work
library(MCMCpack)  # for inverse gamma function

# Feedback simulation code.
source( "mseRrefPoints.r" )
source( "mseRsimulation.r" )
source( "mseRstats.r" )

# Plotting code.
source( "mseRplots.r" )
