# mseR Options.r: Written:  Wed Mar 12 15:40:12 2014 
#
# General options start here:
#
.CEXANNO <- 1      #  Annotation character expansion 
.CEXANNO2 <- 1.2      #  Annotation character expansion 
.CEXAXIS <- 1      #  Axis character expansion 
.CEXAXIS2 <- 1.2      #  Axis character expansion 
.CEXAXIS3 <- 1.3      #  Axis character expansion 
.CEXAXIS4 <- 1.4      #  Axis character expansion 
.CEXAXIS5 <- 1.5      #  Axis character expansion 
.CEXLAB <- 1      #  Label character expansion 
.CEXLAB2 <- 1.2      #  Label character expansion 
.CEXLAB4 <- 1.4      #  Label character expansion 
.CEXLEG <- 1      #  Legend character expansion 
.CEXLEG2 <- 1.2      #  Legend character expansion 
.CEXSTAMP <- 0.8      #  Stamp character expansion 
.CEXSYM <- 1      #  Symbol character expansion 
.CEXSYM2 <- 1.2      #  Symbol character expansion 
.CEXSYM4 <- 1.4      #  Symbol character expansion 
.CEXSYM6 <- 1.6      #  Symbol character expansion 
.CEXSYM8 <- 1.8      #  Symbol character expansion 
.CEXSYM20 <- 2      #  Symbol character expansion 
.CEXSYM24 <- 2.4      #  Symbol character expansion 
.COLSTAMP <- "gray"      #  Color of stamp text 
.INLINE <- 1      #  Plot margin text line 
.INLINE2 <- 2      #  Plot margin text line 
.INLINE3 <- 3      #  Plot margin text line 
.INLINE4 <- 4      #  Plot margin text line 
.MAR <- c(2,2,1,1)      #  par option for mar 
.OMA <- c(3,3,3,3)      #  par option for oma 
.OUTLINE <- 1      #  Outer margin text line 
.OUTLINE2 <- 2      #  Outer margin text line 
.OUTLINE3 <- 3      #  Outer margin text line 
.OUTLINE4 <- 4      #  Outer margin text line 
.YAXISLAS <- 2      #  y-axis label orientation 
.CEXTITLE <- 1      #  Title character expansion 
.CEXTITLE2 <- 1.2      #  Title character expansion 
.CEXTITLE4 <- 1.4      #  Title character expansion 
.LWD <- 1      #  Line width 
.LWD2 <- 2      #  Line width 
.LWD3 <- 3      #  Line width 
.LWD4 <- 4      #  Line width 
.GRIDCOL <- "lightblue"      #  Grid color 
.GRIDLWD <- 2      #  Grid line width 
.GRIDLTY <- 1      #  Grid line type 
.LEGBTY <- "n"      #  Box around legend 
.BMSYCRIT <- 70      #  Maximum value of Bmsy 
.FMSYCRIT <- 5      #  Maximum value of Fmsy 
.TULQCOL <- 24      #  Tulip plot quantile color 
.TULQLWD <- 1      #  Tulip plot quantile line width 
.TULQLTY <- 1      #  Tulip plot quantile line type 
.TULENVCOL <- 416      #  Tulip plot envelope color 
.EXIT0COL <- "black"      #  Exit code 0 color - solution equal intial value 
.EXIT1COL <- "green3"      #  Exit code 1 color - normal exit all derivatives satisfy conditions 
.EXIT2COL <- "red"      #  Exit code 2 color - error in the derivative 
.EXIT3COL <- "blue"      #  Exit code 3 color - maximum funcalls exceeded 
.EXIT0CEX <- 1.8      #  Character expansion for Exit code 0 
.EXIT1CEX <- 1.8      #  Character expansion for Exit code 1 
.EXIT2CEX <- 1.8      #  Character expansion for Exit code 2 
.EXIT3CEX <- 1.8      #  Character expansion for Exit code 3 
.MAXGRADCRIT <- 0.0001      #  Maximum gradient value for convergence 
.MAXFUNCALLS <- 500      #  Maximum function calls (plot value only) 
.INITYEAR <- 0      #  Initial calendar year for time axes 
.MAXREP <- 200      #  Maximum replications 
.MINREP <- 1      #  Minimum replications 
.MAXSIM <- 50      #  Maximum number of simulations 
.MAXNT <- 100      #  Maximum years in simulation 
.MINAGES <- 4      #  Minimum age 
.MAXAGES <- 50      #  Maximum age (value would be plus group) 
.ANIREPS <- 0      #  Number of replications to replay each simulation 
.ANIREPADV <- 0      #  Should the animation replicate counter be advanced? 
.ANIWAIT <- 3      #  Wait time before return from animation to R console. 
.ANIVIEWCEX <- 1      #  Character expansion for axis labels in animation 
.ANIVIEWFONT <- 2      #  Font type for axis labels in animation 
.NRETROSTATS <- 5      #  Number of retrospective statistics 
.NCORES <- 1      #  Number of CPU cores to use (e.g., i7 has 4 physical, 8 virtual) 
#
# Project options start here:
#
#
# Settings for .Dt 
#
.DtBG <- "cyan" 
.DtCOL <- "cyan" 
.DtFG <- "black" 
.DtLTY <- 1 
.DtLWD <- 2 
.DtPCH <- 21 
.DtCEX <- 1.4 
.DtLAB  <- "Catch" 
.DtUNIT <- "metric tons" 
#
# Settings for .Bt 
#
.BtBG <- "white" 
.BtCOL <- "red" 
.BtFG <- "black" 
.BtLTY <- 1 
.BtLWD <- 3 
.BtPCH <- 21 
.BtCEX <- 1.4 
.BtLAB  <- "Spawning biomass" 
.BtUNIT <- "metric tons" 
#
# Settings for .Bexp 
#
.BexpBG <- "white" 
.BexpCOL <- "gray" 
.BexpFG <- "gray" 
.BexpLTY <- 1 
.BexpLWD <- 2 
.BexpPCH <- 21 
.BexpCEX <- 1.4 
.BexpLAB  <- "Fishery exploitable biomass" 
.BexpUNIT <- "metric tons" 
#
# Settings for .BexpS 
#
.BexpSBG <- "white" 
.BexpSCOL <- "black" 
.BexpSFG <- "black" 
.BexpSLTY <- 1 
.BexpSLWD <- 2 
.BexpSPCH <- 21 
.BexpSCEX <- 1.4 
.BexpSLAB  <- "Survey exploitable biomass" 
.BexpSUNIT <- "metric tons" 
#
# Settings for .Nt 
#
.NtBG <- "white" 
.NtCOL <- "red" 
.NtFG <- "black" 
.NtLTY <- 1 
.NtLWD <- 3 
.NtPCH <- 21 
.NtCEX <- 1.4 
.NtLAB  <- "Spawning numbers" 
.NtUNIT <- "000s" 
#
# Settings for .Nexp 
#
.NexpBG <- "white" 
.NexpCOL <- "gray" 
.NexpFG <- "gray" 
.NexpLTY <- 1 
.NexpLWD <- 3 
.NexpPCH <- 21 
.NexpCEX <- 1.4 
.NexpLAB  <- "Exploitable numbers" 
.NexpUNIT <- "" 
#
# Settings for .Ft 
#
.FtBG  <- "white" 
.FtCOL <- "black" 
.FtFG  <- "black" 
.FtLTY <- 1 
.FtLWD <- .5 
.FtPCH <- 21 
.FtCEX <- 1.4 
.FtLAB  <- "Fishing mortality" 
.FtUNIT <- "" 
#
# Settings for .M 
#
.MBG <- "purple" 
.MCOL <- "purple" 
.MFG <- "black" 
.MLTY <- 4 
.MLWD <- 2 
.MPCH <- 21 
.MCEX <- 1.4 
.MLAB  <- "Mean natural mortality" 
.MUNIT <- "" 
#
# Settings for .Mt 
#
.MtBG <- "purple" 
.MtCOL <- "purple" 
.MtFG <- "black" 
.MtLTY <- 1 
.MtLWD <- 2 
.MtPCH <- 21 
.MtCEX <- 1.4 
.MtLAB  <- "Natural mortality" 
.MtUNIT <- "" 
#
# Settings for .Rt 
#
.RtBG <- "darkorange" 
.RtCOL <- "black" 
.RtFG <- "black" 
.RtLTY <- 1 
.RtLWD <- 2 
.RtPCH <- 21 
.RtCEX <- 1.4 
.RtLAB  <- "Recruits" 
.RtUNIT <- "000s" 
#
# Settings for .tMP 
#
.tMPBG <- "white" 
.tMPCOL <- "black" 
.tMPFG <- "black" 
.tMPLTY <- 3 
.tMPLWD <- 2 
.tMPPCH <- 21 
.tMPCEX <- 1.4 
.tMPLAB  <- "tMP" 
.tMPUNIT <- "years" 
#
# Settings for .Dept 
#
.DeptBG <- "white" 
.DeptCOL <- "black" 
.DeptFG <- "black" 
.DeptLTY <- 1 
.DeptLWD <- 1 
.DeptPCH <- 21 
.DeptCEX <- 1.4 
.DeptLAB  <- "Depletion" 
.DeptUNIT <- "" 
#
# Settings for .It 
#
.ItBG <- "aquamarine" 
.ItCOL <- "black" 
.ItFG <- "black" 
.ItLTY <- 1 
.ItLWD <- 2 
.ItPCH <- 21 
.ItCEX <- 1.4 
.ItLAB  <- "Survey index" 
.ItUNIT <- "metric tons" 
#
# Settings for .BtEst 
#
.BtEstBG <- "white" 
.BtEstCOL <- "green4" 
.BtEstFG <- "black" 
.BtEstLTY <- 1 
.BtEstLWD <- 3 
.BtEstPCH <- 21 
.BtEstCEX <- 1.4 
.BtEstLAB  <- "Estimated biomass" 
.BtEstUNIT <- "metric tons" 
#
# Settings for .BtStep 
#
.BtStepBG <- "white" 
.BtStepCOL <- "blue" 
.BtStepFG <- "black" 
.BtStepLTY <- 1 
.BtStepLWD <- 2 
.BtStepPCH <- 21 
.BtStepCEX <- 1.4 
.BtStepLAB  <- "Estimated biomass" 
.BtStepUNIT <- "metric tons" 
#
# Settings for .BmsyEst 
#
.BmsyEstBG <- "white" 
.BmsyEstCOL <- "black" 
.BmsyEstFG <- "black" 
.BmsyEstLTY <- 2 
.BmsyEstLWD <- 2 
.BmsyEstPCH <- 22 
.BmsyEstCEX <- 1.4 
.BmsyEstLAB  <- "Estimated Bmsy" 
.BmsyEstUNIT <- "metric tons" 
#
# Settings for .FmsyEst 
#
.FmsyEstBG <- "cyan" 
.FmsyEstCOL <- "cyan" 
.FmsyEstFG <- "black" 
.FmsyEstLTY <- 2 
.FmsyEstLWD <- 2 
.FmsyEstPCH <- 22 
.FmsyEstCEX <- 1.4 
.FmsyEstLAB  <- "Estimated Fmsy" 
.FmsyEstUNIT <- "" 
#
# Settings for .MsyEst 
#
.MsyEstBG <- "cyan" 
.MsyEstCOL <- "cyan" 
.MsyEstFG <- "black" 
.MsyEstLTY <- 2 
.MsyEstLWD <- 2 
.MsyEstPCH <- 22 
.MsyEstCEX <- 1.4 
.MsyEstLAB  <- "Estimated MSY" 
.MsyEstUNIT <- "metric tons" 
#
# Settings for .BexpRetro 
#
.BexpRetroBG <- "white" 
.BexpRetroCOL <- "blue" 
.BexpRetroFG <- "black" 
.BexpRetroLTY <- 1 
.BexpRetroLWD <- 2 
.BexpRetroPCH <- 21 
.BexpRetroCEX <- 1.4 
.BexpRetroLAB  <- "Estimated fishery exploitable biomass" 
.BexpRetroUNIT <- "metric tons" 
#
# Settings for .BexpSRetro 
#
.BexpSRetroBG <- "white" 
.BexpSRetroCOL <- "blue" 
.BexpSRetroFG <- "black" 
.BexpSRetroLTY <- 1 
.BexpSRetroLWD <- 2 
.BexpSRetroPCH <- 21 
.BexpSRetroCEX <- 1.4 
.BexpSRetroLAB  <- "Estimated survey exploitable biomass" 
.BexpSRetroUNIT <- "metric tons" 
#
# Settings for .BspawnRetro 
#
.BspawnRetroBG <- "white" 
.BspawnRetroCOL <- "blue" 
.BspawnRetroFG <- "black" 
.BspawnRetroLTY <- 1 
.BspawnRetroLWD <- 2 
.BspawnRetroPCH <- 21 
.BspawnRetroCEX <- 1.4 
.BspawnRetroLAB  <- "Estimated spawning biomass" 
.BspawnRetroUNIT <- "" 
#
# Settings for .RtRetro 
#
.RtRetroBG <- "white" 
.RtRetroCOL <- "orange" 
.RtRetroFG <- "black" 
.RtRetroLTY <- 1 
.RtRetroLWD <- 2 
.RtRetroPCH <- 21 
.RtRetroCEX <- 1.4 
.RtRetroLAB  <- "Estimated recruitment" 
.RtRetroUNIT <- "" 
#
# Settings for .HCRLB 
#
.HCRLBBG <- "red" 
.HCRLBCOL <- "black" 
.HCRLBFG <- "black" 
.HCRLBLTY <- 3 
.HCRLBLWD <- 1 
.HCRLBPCH <- 24 
.HCRLBCEX <- 1.4 
.HCRLBLAB  <- "Lower Bound" 
.HCRLBUNIT <- "metric tons" 
#
# Settings for .HCRUB 
#
.HCRUBBG <- "yellow" 
.HCRUBCOL <- "black" 
.HCRUBFG <- "black" 
.HCRUBLTY <- 3 
.HCRUBLWD <- 1 
.HCRUBPCH <- 24 
.HCRUBCEX <- 1.4 
.HCRUBLAB  <- "Upper Bound" 
.HCRUBUNIT <- "metric tons" 
#
# Settings for .Bref 
#
.BrefBG <- "green" 
.BrefCOL <- "green" 
.BrefFG <- "black" 
.BrefLTY <- 1 
.BrefLWD <- 1 
.BrefPCH <- 21 
.BrefCEX <- 1.4 
.BrefLAB  <- "Bref" 
.BrefUNIT <- "metric tons" 
#
# Settings for .Fref 
#
.FrefBG <- "green" 
.FrefCOL <- "green" 
.FrefFG <- "black" 
.FrefLTY <- 1 
.FrefLWD <- 1 
.FrefPCH <- 21 
.FrefCEX <- 1.4 
.FrefLAB  <- "Fref" 
.FrefUNIT <- "" 
#
# Settings for .Fhcr 
#
.FhcrBG <- "green" 
.FhcrCOL <- "green" 
.FhcrFG <- "black" 
.FhcrLTY <- 1 
.FhcrLWD <- 1 
.FhcrPCH <- 21 
.FhcrCEX <- 1.4 
.FhcrLAB  <- "Fhcr" 
.FhcrUNIT <- "" 
#
# Settings for .B0 
#
.B0BG <- "gray51" 
.B0COL <- "gray51" 
.B0FG <- "black" 
.B0LTY <- 4 
.B0LWD <- 1 
.B0PCH <- 21 
.B0CEX <- 1.4 
.B0LAB  <- "B0" 
.B0UNIT <- "metric tons" 
#
# Settings for .Bmsy 
#
.BmsyBG <- "green3" 
.BmsyCOL <- "green3" 
.BmsyFG <- "black" 
.BmsyLTY <- 4 
.BmsyLWD <- 2 
.BmsyPCH <- 21 
.BmsyCEX <- 1.4 
.BmsyLAB  <- "Bmsy" 
.BmsyUNIT <- "metric tons" 
#
# Settings for .F0 
#
.F0BG <- "purple" 
.F0COL <- "purple" 
.F0FG <- "black" 
.F0LTY <- 1 
.F0LWD <- 2 
.F0PCH <- 21 
.F0CEX <- 1.4 
.F0LAB  <- "F0" 
.F0UNIT <- "" 
#
# Settings for .F01 
#
.F01BG <- "aquamarine" 
.F01COL <- "aquamarine" 
.F01FG <- "black" 
.F01LTY <- 1 
.F01LWD <- 2 
.F01PCH <- 21 
.F01CEX <- 1.4 
.F01LAB  <- "F0.1" 
.F01UNIT <- "" 
#
# Settings for .Fcra 
#
.FcraBG <- "black" 
.FcraCOL <- "black" 
.FcraFG <- "black" 
.FcraLTY <- 1 
.FcraLWD <- 2 
.FcraPCH <- 21 
.FcraCEX <- 1.4 
.FcraLAB  <- "Fcra" 
.FcraUNIT <- "" 
#
# Settings for .Fmax 
#
.FmaxBG <- "red" 
.FmaxCOL <- "red" 
.FmaxFG <- "black" 
.FmaxLTY <- 1 
.FmaxLWD <- 2 
.FmaxPCH <- 21 
.FmaxCEX <- 1.4 
.FmaxLAB  <- "Fmax" 
.FmaxUNIT <- "" 
#
# Settings for .Fspr 
#
.FsprBG <- "steelblue1" 
.FsprCOL <- "steelblue1" 
.FsprFG <- "black" 
.FsprLTY <- 1 
.FsprLWD <- 2 
.FsprPCH <- 21 
.FsprCEX <- 1.4 
.FsprLAB  <- "FsprX" 
.FsprUNIT <- "" 
#
# Settings for .Fmsy 
#
.FmsyBG <- "green3" 
.FmsyCOL <- "green3" 
.FmsyFG <- "black" 
.FmsyLTY <- 4 
.FmsyLWD <- 2 
.FmsyPCH <- 21 
.FmsyCEX <- 1.4 
.FmsyLAB  <- "Fmsy" 
.FmsyUNIT <- "" 
#
# Settings for .MSY 
#
.MSYBG <- "green3" 
.MSYCOL <- "green3" 
.MSYFG <- "black" 
.MSYLTY <- 4 
.MSYLWD <- 2 
.MSYPCH <- 21 
.MSYCEX <- 1.4 
.MSYLAB  <- "MSY" 
.MSYUNIT <- "metric tons" 
#
# Settings for .Critical 
#
.CriticalBG <- "pink" 
.CriticalCOL <- "pink" 
.CriticalFG <- "black" 
.CriticalLTY <- 1 
.CriticalLWD <- 2 
.CriticalPCH <- 21 
.CriticalCEX <- 1.4 
.CriticalLAB  <- "Critical Zone" 
.CriticalUNIT <- "metric tons" 
#
# Settings for .Cautious 
#
.CautiousBG <- "yellow" 
.CautiousCOL <- "yellow" 
.CautiousFG <- "black" 
.CautiousLTY <- 1 
.CautiousLWD <- 2 
.CautiousPCH <- 21 
.CautiousCEX <- 1.4 
.CautiousLAB  <- "Cautious Zone" 
.CautiousUNIT <- "metric tons" 
#
# Settings for .Healthy 
#
.HealthyBG <- "green" 
.HealthyCOL <- "green" 
.HealthyFG <- "black" 
.HealthyLTY <- 1 
.HealthyLWD <- 2 
.HealthyPCH <- 21 
.HealthyCEX <- 1.4 
.HealthyLAB  <- "Healthy Zone" 
.HealthyUNIT <- "metric tons" 
#
# Settings for .Lrp 
#
.LrpBG <- "pink" 
.LrpCOL <- "black" 
.LrpFG <- "black" 
.LrpLTY <- 3 
.LrpLWD <- 2 
.LrpPCH <- 21 
.LrpCEX <- 1.4 
.LrpLAB  <- "LRP" 
.LrpUNIT <- "metric tons" 
#
# Settings for .Usr 
#
.UsrBG <- "yellow" 
.UsrCOL <- "black" 
.UsrFG <- "black" 
.UsrLTY <- 3 
.UsrLWD <- 2 
.UsrPCH <- 21 
.UsrCEX <- 1.4 
.UsrLAB  <- "USR" 
.UsrUNIT <- "metric tons" 
#
# Settings for .Trp 
#
.TrpBG <- "green" 
.TrpCOL <- "black" 
.TrpFG <- "black" 
.TrpLTY <- 3 
.TrpLWD <- 2 
.TrpPCH <- 21 
.TrpCEX <- 1.4 
.TrpLAB  <- "TRP" 
.TrpUNIT <- "metric tons" 
#
# Settings for .Mp 
#
.MpBG <- "white" 
.MpCOL <- "black" 
.MpFG <- "black" 
.MpLTY <- 2 
.MpLWD <- 1 
.MpPCH <- 21 
.MpCEX <- 1.4 
.MpLAB  <- "Management procedure" 
.MpUNIT <- "" 
#
# Settings for .Om 
#
.OmBG <- "red" 
.OmCOL <- "red" 
.OmFG <- "black" 
.OmLTY <- 2 
.OmLWD <- 1 
.OmPCH <- 21 
.OmCEX <- 1.4 
.OmLAB  <- "Operating model" 
.OmUNIT <- "" 
#
# Settings for .Obj1Dep 
#
.Obj1DepBG <- "red" 
.Obj1DepCOL <- "red" 
.Obj1DepFG <- "black" 
.Obj1DepLTY <- 2 
.Obj1DepLWD <- 2 
.Obj1DepPCH <- 21 
.Obj1DepCEX <- 1.6 
.Obj1DepLAB  <- "Objective 1 Depletion" 
.Obj1DepUNIT <- "" 
#
# Settings for .Obj1Prob 
#
.Obj1ProbBG <- "magenta" 
.Obj1ProbCOL <- "magenta" 
.Obj1ProbFG <- "black" 
.Obj1ProbLTY <- 2 
.Obj1ProbLWD <- 2 
.Obj1ProbPCH <- 21 
.Obj1ProbCEX <- 1.6 
.Obj1ProbLAB  <- "Objective 1 Probability" 
.Obj1ProbUNIT <- "" 
#
# Settings for .Obj1Yr 
#
.Obj1YrBG <- "deepskyblue" 
.Obj1YrCOL <- "deepskyblue" 
.Obj1YrFG <- "black" 
.Obj1YrLTY <- 2 
.Obj1YrLWD <- 2 
.Obj1YrPCH <- 21 
.Obj1YrCEX <- 1.6 
.Obj1YrLAB  <- "Objective 1 Year" 
.Obj1YrUNIT <- "" 
#
# Settings for .Obj2Dep 
#
.Obj2DepBG <- "red" 
.Obj2DepCOL <- "red" 
.Obj2DepFG <- "black" 
.Obj2DepLTY <- 2 
.Obj2DepLWD <- 2 
.Obj2DepPCH <- 21 
.Obj2DepCEX <- 1.6 
.Obj2DepLAB  <- "Objective 2 Depletion" 
.Obj2DepUNIT <- "" 
#
# Settings for .Obj2Prob 
#
.Obj2ProbBG <- "magenta" 
.Obj2ProbCOL <- "magenta" 
.Obj2ProbFG <- "black" 
.Obj2ProbLTY <- 2 
.Obj2ProbLWD <- 2 
.Obj2ProbPCH <- 21 
.Obj2ProbCEX <- 1.6 
.Obj2ProbLAB  <- "Objective 2 Probability" 
.Obj2ProbUNIT <- "" 
#
# Settings for .Obj2Yr 
#
.Obj2YrBG <- "deepskyblue" 
.Obj2YrCOL <- "deepskyblue" 
.Obj2YrFG <- "black" 
.Obj2YrLTY <- 2 
.Obj2YrLWD <- 2 
.Obj2YrPCH <- 21 
.Obj2YrCEX <- 1.6 
.Obj2YrLAB  <- "Objective 2 Year" 
.Obj2YrUNIT <- "" 
#
# Settings for .Obj3Dep 
#
.Obj3DepBG <- "red" 
.Obj3DepCOL <- "red" 
.Obj3DepFG <- "black" 
.Obj3DepLTY <- 2 
.Obj3DepLWD <- 2 
.Obj3DepPCH <- 21 
.Obj3DepCEX <- 1.6 
.Obj3DepLAB  <- "Objective 3 Depletion" 
.Obj3DepUNIT <- "" 
#
# Settings for .Obj3Prob 
#
.Obj3ProbBG <- "magenta" 
.Obj3ProbCOL <- "magenta" 
.Obj3ProbFG <- "black" 
.Obj3ProbLTY <- 2 
.Obj3ProbLWD <- 2 
.Obj3ProbPCH <- 21 
.Obj3ProbCEX <- 1.6 
.Obj3ProbLAB  <- "Objective 3 Probability" 
.Obj3ProbUNIT <- "" 
#
# Settings for .Obj3Yr 
#
.Obj3YrBG <- "deepskyblue" 
.Obj3YrCOL <- "deepskyblue" 
.Obj3YrFG <- "black" 
.Obj3YrLTY <- 2 
.Obj3YrLWD <- 2 
.Obj3YrPCH <- 21 
.Obj3YrCEX <- 1.6 
.Obj3YrLAB  <- "Objective 3 Year" 
.Obj3YrUNIT <- "" 
#
# Settings for .Hcr 
#
.HcrBG <- "white" 
.HcrCOL <- "black" 
.HcrFG <- "black" 
.HcrLTY <- 1 
.HcrLWD <- 2 
.HcrPCH <- 21 
.HcrCEX <- 1 
.HcrLAB  <- "Harvest control rule" 
.HcrUNIT <- "" 
#
# Settings for .Lh 
#
.LhBG <- "white" 
.LhCOL <- "black" 
.LhFG <- "black" 
.LhLTY <- 1 
.LhLWD <- 2 
.LhPCH <- 21 
.LhCEX <- 1.4 
.LhLAB  <- "Life history" 
.LhUNIT <- "" 
#
# Settings for .Mat 
#
.MatBG <- "white" 
.MatCOL <- "black" 
.MatFG <- "black" 
.MatLTY <- 1 
.MatLWD <- 2 
.MatPCH <- 21 
.MatCEX <- 1.4 
.MatLAB  <- "Maturity at age" 
.MatUNIT <- "" 
#
# Settings for .Age 
#
.AgeBG <- "white" 
.AgeCOL <- "black" 
.AgeFG <- "black" 
.AgeLTY <- 1 
.AgeLWD <- 2 
.AgePCH <- 21 
.AgeCEX <- 1.4 
.AgeLAB  <- "Age" 
.AgeUNIT <- "years" 
#
# Settings for .Len 
#
.LenBG <- "white" 
.LenCOL <- "black" 
.LenFG <- "black" 
.LenLTY <- 1 
.LenLWD <- 2 
.LenPCH <- 21 
.LenCEX <- 1.4 
.LenLAB  <- "Length" 
.LenUNIT <- "cm" 
#
# Settings for .Wgt 
#
.WgtBG <- "white" 
.WgtCOL <- "black" 
.WgtFG <- "black" 
.WgtLTY <- 1 
.WgtLWD <- 2 
.WgtPCH <- 21 
.WgtCEX <- 1.4 
.WgtLAB  <- "Weight" 
.WgtUNIT <- "kg" 
#
# Settings for .Per1 
#
.Per1BG <- "white" 
.Per1COL <- "black" 
.Per1FG <- "black" 
.Per1LTY <- 1 
.Per1LWD <- 2 
.Per1PCH <- 21 
.Per1CEX <- 1.4 
.Per1LAB  <- "Period 1" 
.Per1UNIT <- "" 
#
# Settings for .Per2 
#
.Per2BG <- "gray" 
.Per2COL <- "gray" 
.Per2FG <- "black" 
.Per2LTY <- 1 
.Per2LWD <- 2 
.Per2PCH <- 21 
.Per2CEX <- 1.4 
.Per2LAB  <- "Period 2" 
.Per2UNIT <- "" 
