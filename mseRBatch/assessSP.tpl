//*******************************************************************/
// Module      : assessSP.tpl
// Authors     : S.P. Cox, A.R. Kronlund, K.R. Holt
// Procedure   : Fitting surplus production model to multiple indices
//               of abundance. Use indexType=1 for Relative indices
//               and indexType=0 for Absolute biomass estimates.
//               Set these in indexType vector in data file.
//
// References:
//
// Punt, A.E. 2003. Extending production models to include process
//   error in the population dynamics. CJFAS 60: 1217-1228.
// Cox, S.P., Kronlund, A.R., and Lacko, L. Management procedures for
//   ...CSAS Res. Doc. YYYY
// Cox, S.P., Kronlund, A.R., and Benson, A.J. 2013. Distinguishing biological
//   and operational reference points...Environmental Cons (in review)
//
// Date Revised:
//   SPC: 19-Dec-2012 Major revision for new mseR-FinFish
//   Notes 21-Dec 2012 
//        1. sometimes adding new data can increase
//        CV of projected biomass (based on 99 yrs) - probably
//        a property of sample, not of expected CV.
//        2. Although MLE parameters provide reasonable final biomass
//           values, MCMC is not assured of same. I think it is caused
//           by somewhat crazy catch history from mseR initialization. 
//           Output filter on pos_pen==0 avoids writing MCMC sample rows where
//           posfun penalty is used (about 15% of samples)
//*******************************************************************/

DATA_SECTION
  // Total length of catch series.
  init_int nyrs;

  // Number of indices for fitting.
  init_int nIndices;

  init_int nProjYrs;

  init_int trendYrs;

  init_int rSeed;
  !! rSeed = nyrs; // this will force unique seq each yr

  init_int ruleType;

  init_int nQlevels 
  
  init_vector Qlevel(1,nQlevels);

  // Vector of catch biomass (tonnes).
  init_vector initKatch(1,nyrs);

  vector katch(1,nyrs);
  !! katch=initKatch(1,nyrs);

  // Input vector of index types.
  init_vector indexType(1,nIndices);

  // Vector indicating first year for each index.
  init_ivector fYear(1,nIndices);

  // Vector indicating last year for each index.
  init_ivector lYear(1,nIndices);

  // Create vector of index series lengths.
  ivector nobs(1,nIndices);

  !!nobs=lYear-fYear+1;

  // Create vector to hold number of valid observations
  // for use when index values are non-continuous
  ivector validObs(1,nIndices);
  // Input index data
  init_matrix Indices(1,nIndices,1,nyrs);
  
//*******************************************************************/

PARAMETER_SECTION
  objective_function_value f;
  // Parameters to be estimated.
  init_number lnMsy;
  init_number lnFmsy;
  init_number pmMsy(-1); // tuning parameters
  init_number psdMsy(-1);
  init_number pmFmsy(-1);
  init_number psdFmsy(-1);
  init_number rho(-1);
  // Estimated production deviations
  init_bounded_dev_vector lnOmega(2,nyrs-1,-15.,15.,2);

  // Back-transformed parameters.
  number MSY;
  
  // ARK (22-Dec-12) Need std errs for Fmsy.
  //number Fmsy;
  sdreport_number Fmsy;
  sdreport_number Bmsy;
  sdreport_number projExpBio;
  number initDepEst;

  // Derived quantities.
  vector biomass(1,nyrs);
  number depletion;
  number avgF;
  matrix scaledIndices(1,nIndices,1,nyrs);
  !! scaledIndices=-1.;

  // Likelihood quantities.
  vector lnq(1,nIndices);
  vector ss(1,nIndices);
  number SSg;
  number SSw;
  number kappaSq;
  matrix z(1,nIndices,1,nyrs);
  number prior;
  number zSum;
  number like;
  number bpen;
  number fpen;
  number pos_pen;
  number dep_pen;
  number neval;
  //number projExpBio;
//*******************************************************************/

PROCEDURE_SECTION
  pMod();
  calcLikelihood();
  calcPrior();
  if( mceval_phase() ) // when using Bayes-based rules
  {
    switch( ruleType )
    {
      case 1:
        projPMod12();// F-based rule requires B(T+1)
        break;
      case 2:
        projPMod12();// F-based rule requires B(T+1)
        break;
      case 3: 
        projPMod3(); // decline risk rule over quota levels
    }
  }
    // IG(alpha,beta) variance prior
  dvariable alpha = 1.;
  dvariable beta  = 1.;
  dvariable l_kap  = alpha*log(beta)-(alpha+1)*log(kappaSq)-gammln(alpha)-beta/kappaSq;

  f  = like + prior + (-1.)*l_kap;;
  f += fpen;
  f += bpen;
//-------------------------------------------------------------------/

FUNCTION pMod

  // Initialize posfun penalty...constrains biomass to be positive.
  pos_pen = 0.; f = 0.; dep_pen = 0.; fpen = 0.;

  // Back-transform parameters.
  MSY  = mfexp( lnMsy );
  Fmsy = mfexp( lnFmsy );
  Bmsy = MSY/Fmsy;

  // Dynamic model: scaled so that B1=B0=2*Bmsy.
  // Assume initially at B0
  biomass(1) = 2.*Bmsy;
  // Loop over years
  for( int i=1; i<=(nyrs-1); i++ )
  {
    biomass(i+1) = biomass(i) + (2.*Fmsy)*biomass(i)*(1.- biomass(i)/(2.*Bmsy)) - katch(i);
    if( i > 1 )
    {
      biomass(i+1) = biomass(i+1)*mfexp( lnOmega(i) );
    }
    biomass(i+1) = posfun(biomass(i+1),1.,pos_pen);
    fpen += 1000.*pos_pen;
  }
  // Projected biomass for upcoming year (this is used to set next quota)
  projExpBio = biomass(nyrs) + (2.*Fmsy)*biomass(nyrs)*(1.- biomass(nyrs)/(2.*Bmsy)) - katch(nyrs);
  projExpBio = posfun(projExpBio, 1., pos_pen);
  
  bpen += 1000.*pos_pen;
  
  // Average exploitation rate used in objectiveFunction to constrain average
  // F during early phases
  avgF = mean( elem_div( katch,biomass ) );

  // SSB depletion.
  depletion = projExpBio/( 2.*Bmsy );

//-------------------------------------------------------------------/

FUNCTION projPMod12
  // ruleType 1 or 2: run projection only one year ahead to get
  // projected exploitable biomass...used to get
  // next year's quota
  rSeed = rSeed + 1;
  random_number_generator rng(rSeed);
  double omegaProj = randn(rng);
  double totalVar = value(kappaSq);            // est total variance
  double procSD   = sqrt( (1.-value(rho))*totalVar ); // process error std dev
  double FMSY     = value(Fmsy);
  double BMSY     = value(Bmsy);
  double BT       = value(biomass(nyrs));
  
  // Projected biomass for upcoming year (this is used to set next quota)
  double Bproj   = BT + (2.*FMSY)*BT*(1.- BT/(2.*BMSY)) - katch(nyrs);
  Bproj          = Bproj*mfexp( procSD*omegaProj );
  Bproj          = posfun(Bproj, 1., 1);
  double depB0   = Bproj/( 2.*BMSY );
  double depBmsy = Bproj/BMSY;

  if( mcHeader== 0 )
  {
    // Output the parameters needed for reference point calcs.
    mcout << "B0 MSY Fmsy Bmsy BT projSpawnBio projExpBio depB0 depBmsy" << endl;
  
    // Output the MLE values. projExpBio used here to avoid adding random process error on MLE
    mcout << 2.*BMSY <<" "<< MSY <<" "<< FMSY <<" "<< BMSY <<" "<< BT <<" ";
    mcout << projExpBio << " " << projExpBio <<" "<< depB0 <<" "<< depBmsy << endl;
    mcHeader = 1;
  }
  else
  {
    // Note Bproj used here because it involves random process error.
    if( pos_pen==0 )
    {
      mcout << 2.*BMSY <<" "<< MSY <<" "<< FMSY <<" "<< BMSY <<" "<< BT <<" ";
      mcout << projExpBio << " " << projExpBio <<" "<< depB0 <<" "<< depBmsy << endl;
    }
  }    

FUNCTION projPMod3
  // Output projections needed to implement the decline-risk rule
    rSeed = rSeed + 1;
    dvector omegaProj(1,nProjYrs);
    random_number_generator rng(rSeed);
    omegaProj.fill_randn(rng);
    double totalVar = value(kappaSq);            // est total variance
    double procSD   = sqrt( (1.-value(rho))*totalVar ); // process error std dev
    double FMSY     = value(Fmsy);
    double BMSY     = value(Bmsy);
    double BT       = value(biomass(nyrs));

    // Projected biomass quantities
    double depB0   = BT/( 2.*BMSY );
    double depBmsy = BT/BMSY;
    
    double oldB;
    double newB;
    
    // For each quota option, project model forward nProjYrs and determine
    // whether biomass declines relative to current biomass.
    for( int j=1; j<=nQlevels; j++ )
    {
      int Declined = 0;
      oldB = BT; // initialize biomass at current
      for ( int t=1; t<nProjYrs; t++ )
      {
        newB  = oldB + (2.*FMSY)*oldB*(1.-oldB/(2.*BMSY)) - Qlevel(j);
        newB  = newB*mfexp( procSD*omegaProj(t) );
        oldB = posfun(newB, 1., 1);
      }

      if( oldB < BT )
        Declined = 1;
      
      if( mcHeader == 0 )
      {
        // Output Header.
        // This will create reduandant information for each quota level, but will
        // be filtered in R.
        mcout << "B0 MSY Fmsy Bmsy BT projSpawnBio projExpBio depB0 depBmsy" <<" ";

        // Output header for 9-zone feed forward harvest control rule.
        mcout << "Q Declined trendBio" << endl;

        // Output the MLE values. projExpBio used here to avoid adding random process error on MLE
        mcout << 2.*BMSY <<" "<< MSY <<" "<< FMSY <<" "<< BMSY <<" "<< BT <<" ";
        mcout << newB << " " << newB <<" "<< depB0 <<" "<< depBmsy<<" ";
        mcout << Qlevel(j) <<" "<< Declined <<" "<< log(BT/biomass(nyrs-trendYrs+1))/(trendYrs-1) << endl; 

        mcHeader = 1;
      }
      else
      {
        // Output the values. 
        mcout << 2.*BMSY <<" "<< MSY <<" "<< FMSY <<" "<< BMSY << " " << BT << " ";
        mcout << newB << " " << newB << " " << depB0 << " " << depBmsy << " ";
        mcout << Qlevel(j) <<" "<< Declined <<" "<< log(BT/biomass(nyrs-trendYrs+1))/(trendYrs-1) << endl; 
      }
    } // end Qlevels loop    


FUNCTION calcLikelihood
  // Likelihood is EIV and concentrated assuming unknown variances.
  // Catchability MLEs determined for each series except Absolute,
  // which equal 1.0.

  // Initialize likelihood function terms.
  ss.initialize();
  fpen=0.0; bpen=0.;
  //f=0.0;
  z.initialize();
  int i;
  int j;
  validObs.initialize(); lnq.initialize();
  int Ng;

  // Calculations for MLE q assuming multiple indices. It is ok to have only 1.
  for ( i=1;i<=nIndices;i++ )
  {
    zSum=0.0;
    // Loop over years for which data exist
    for ( j=fYear(i);j<=lYear(i);j++ )
    {
      // Compute residual for non-missing values
      if ( Indices(i,j) > 0. )
      {
        z(i,j)       = log(Indices(i,j)/biomass(j)); // residual
        zSum        += z(i,j);                       // for mean calc
        validObs(i) += 1;                            // non-missing obs
      }
    }

    // If indexType =1 (relative), get MLE lnq: 
    if ( indexType(i) > 0. & zSum != 0. )
    {
      lnq(i) = zSum/validObs(i); // mean residual
    }
    // Otherwise, lnq==0 (absolute):
    else
      lnq(i) = 0.0; // bc exp(0)=1

    // Residual SSQ function over valid observations.
    for ( j=fYear(i);j<=lYear(i);j++ )
    {
      if ( Indices(i,j) > 0.0 )
      {
        ss(i)             += pow(z(i,j)-lnq(i),2.0);
        scaledIndices(i,j) = Indices(i,j)/exp(lnq(i)); // index to biomass scale
      }
      else
        scaledIndices(i,j) = -1.;
    }
  }

  // Penalty on average F to stabilize early phases
  // Reduce avgF penalty in last phase.
  if ( last_phase()) // counter for number of function calls made in optimization
    neval+=1;	
  else
  {
    // High avgF penalty in early phase.
    fpen  = 1000.*square(log(avgF/pmFmsy));
    neval = -2.;  // start neval counter at -2 at start of last phase so it equals admb screen output
  }

  // Final obj function value...negative log-posterior plus penalty
  // for non-zero biomass constraint.
  // EIV likelihood.
  Ng  = sum(validObs); // total number of obs over all index series
  SSg = sum(ss);       // total SSQ...
  SSw = norm2(lnOmega);// process error SSQ
  // Estimated total variance
  kappaSq = 1./(Ng+nyrs-2.)*(SSg/rho+SSw/(1.-rho));
  
  // Concentrated likelihood
  like = (Ng+nyrs-2.)/2.*log(kappaSq);

FUNCTION calcPrior
  // Normal priors on MSY and Fmsy
  prior = 0.;
  prior = pow( MSY - pmMsy, 2. )/( 2.*psdMsy*psdMsy );
  prior += pow( Fmsy - pmFmsy, 2. )/( 2.*psdFmsy*psdFmsy );
  

//*******************************************************************/

REPORT_SECTION
  //Outputs as required for management procedure simulations.
  report << "## PMOD: production model results" << endl;
  report << "# nT" << endl;
  report << nyrs << endl;
  report << endl;
  
  report << "## Parameter estimates " << endl;
  report << "# Msy" << endl;
  report << MSY << endl;
  report << "# Fmsy" << endl;
  report << Fmsy << endl;
  report << "# rho" << endl;
  report << rho << endl;
  report << "# kappa" << endl;
  report << sqrt(kappaSq) << endl;
  report << "# tau " << endl;
  report << sqrt( rho*kappaSq ) << endl;
  report << "# sigma " << endl;
  report << sqrt( (1.-rho)*kappaSq ) << endl;
  report <<"# q" << endl;
  report << mfexp(lnq) <<endl;
  report << "# lnOmega" << endl;
  report << lnOmega << endl;
  report << endl;
  
  report << "## Derived variables" << endl;
  report << "# Bmsy" << endl;
  report << Bmsy << endl;
  report << "# D" << endl;
  report << depletion << endl;
  report << "# projSpawnBio" << endl;
  report << projExpBio << endl;
  report << "# projExpBio" << endl;
  report << projExpBio << endl;
  report << "# lastDt" << endl;
  report << katch(nyrs) << endl;
  report << "# Bt" << endl;
  report << biomass << endl;
  report << "# Ct" << endl;
  report << katch << endl;
  report << "# Ft" << endl;
  report << elem_div(katch,biomass) << endl;
  report << "# ItScaled" << endl;
  for ( int i=1;i<=nIndices;i++ )
    report << row(scaledIndices,i) << endl;
  report << endl;
  
  report << "## Priors" << endl;
  report << "# pmMsy" << endl;  
  report << pmMsy << endl;
  report << "# psdMsy" << endl;  
  report << psdMsy << endl;
  report << "# pmFmsy" << endl;  
  report << pmFmsy << endl;
  report << "# psdFmsy" << endl;  
  report << psdFmsy << endl;
  report << endl;
  
  report << "## Minimization properties" << endl;
  report << "# total_likelihood" << endl;
  report << like << endl;
  report << "# total_priors" << endl;
  report << prior << endl;
  report << "# objFun" << endl;
  report << *objective_function_value::pobjfun << endl;
  report << "# maxGrad" << endl;
  report << objective_function_value::gmax << endl;
  report << "# bpen" << endl;
  report << bpen << endl;
  report << "# fpen" << endl;
  report << fpen << endl;
  report << "# iExit" << endl;
  report << iexit << endl;
  report << "# nEval" << endl;
  report << neval << endl;

TOP_OF_MAIN_SECTION
	arrmblsize = 50000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	//gradient_structure::set_MAX_NVAR_OFFSET(5000);
	//gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);

// ARK (22-Dec-12) Compilers vary in the following use of iostream.h.  The
//     Visual C++ 64-bit compiler does not like #include <iostream.h> as it is
//     somehow already included.  So commnet it out for VC++ 64-bit compiles.

GLOBALS_SECTION
	#include <admodel.h>

  // (ARK (22-Dec-12) Comment this out for VC++ 64-bit compiler.
	//#include <iostream.h>
	
	int mcHeader=0;
	int mcTrials=0;
	ofstream mcout("mcout.dat");
