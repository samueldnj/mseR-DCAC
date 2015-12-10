//*******************************************************************/
// Module      : spMulti.tpl
// Authors     : S.P. Cox and A.R. Kronlund
// Procedure   : Fitting surplus production model to multiple indices
//               of abundance. Use indexType=1 for Relative indices
//               and indexType=0 for Absolute biomass estimates.
//               Set these in indexType vector in data file.
//
// References:
//
// Punt, A.E. 2003. Extending production models to include process
//   error in the population dynamics. CJFAS 60: 1217-1228.
//
// Date Revised:
//    25-Apr-06  Initial implementation by Cox.
//    12-May-06  Modified for non-continuous data by Cox.
//    24-Oct-06  Modification by Cox to mixed errors.
//    24-Oct-06  Modifications by Kronlund to integrate into mseSable.
//    02-Nov-06  Re-parameterize to change from "r" to "total
//               production" anomaly.
//    25-Feb-07  Modified to send MCMC output to mceval.out. Note that
//               you MUST do the 2 includes prior to the ofstream call
//               in the GLOBALS section...  don't know why.
//    26-Apr-07  Modified SPC version of spMulti to include MCMC and
//               re-establish matched code between SPC and ARK.
//    04-Aug-09  KRH modified for compadibility mseR when only one survey index
//               is used.   
//    23-Sep-09  KRH removed penalty on F to increase convergence success in mseR.
//    05-Jan-10  KRH reparameterized so that estimated parameters are Bmsy and Fmsy   
//
// Notes:
//
// (1) To generate posterior samples in, e.g., "junk.dat", e.g.
//       spmulti -mcmc 20000 -mcsave 100
//       spmulti -mceval > junk.dat
//*******************************************************************/

DATA_SECTION
  // Total length of catch series.
  init_int nyrs;

  // Number of indices for fitting.
  init_int nIndices;

  // Retrospective analysis year.
  //init_int retroYear;

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
  init_number lnMSY;
  init_number lnFmsy;
  init_number rho(-1); // a minus 1 indicates no estimation 
  init_number likeWgt(-1);
  init_number muPriorMSY(-1);
  init_number sdPriorMSY(-1);
  init_number muPriorFmsy(-1);
  init_number sdPriorFmsy(-1);
  init_bounded_dev_vector lnOmega(2,nyrs-1,-15.,15.,2);

  // Back-transformed parameters.
  number MSY;
  number Fmsy;
  sdreport_number Bmsy;
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
  number fpen;
  number pos_pen;
  number dep_pen;
  number neval;

  number projBiomass;
//*******************************************************************/

PROCEDURE_SECTION
  pMod();
  likelihood();

//-------------------------------------------------------------------/

FUNCTION pMod

  // Initialize posfun penalty...constrains Pt to be positive.
  pos_pen = 0.; f = 0.; dep_pen = 0.; fpen = 0.;

  // Back-transform parameters.
  MSY  = mfexp( lnMSY );
  Fmsy = mfexp( lnFmsy );
  Bmsy = MSY/Fmsy;

  // Dynamic model: scaled so that B1=B0, where B0 = 2*Bmsy.
  biomass(1) = 2.*Bmsy;
  for( int i=1; i<=(nyrs-1); i++ )
  {
    biomass(i+1)=biomass(i) + (2.*Fmsy)*biomass(i)*(1.- biomass(i)/(2.*Bmsy)) - katch(i);
    if( i != 1 )
    {
      biomass(i+1)=biomass(i+1)*mfexp( lnOmega(i) );
    }
    biomass(i+1)=posfun(biomass(i+1),1.,pos_pen);
    fpen += 1000.*pos_pen;
  }

  projBiomass = biomass(nyrs) + (2*Fmsy)*biomass(nyrs)*(1.- biomass(nyrs)/(2*Bmsy)) - katch(nyrs);
  projBiomass = posfun(projBiomass, 1., pos_pen);
  
  fpen += 1000.*pos_pen;
  
  // Average exploitation rate used in phase one to constrain average
  // F to about 0.10.  (Commented out by KRH; 23-Sep-09)
  avgF = mean( elem_div( katch,biomass ) );

  // Calculate depletion.
  depletion = projBiomass/( 2.*Bmsy );

//-------------------------------------------------------------------/

FUNCTION likelihood
  // Likelihood is concentrated assuming unknown variances.
  // Catchability MLEs determined for each series except Absolute,
  // which equal 1.0.

  // Initialize likelihood function terms.
  ss.initialize();
  fpen=0.0;
  //f=0.0;
  z.initialize();
  int i;
  int j;
  validObs.initialize();
  int Ng;

  // Calculations for MLE q.
  for ( i=1;i<=nIndices;i++ )
  {
    //for retrospective, choose lYear as lower of actual lYear(i)
    //or nyrs
    if( lYear(i) > nyrs )
      lYear(i) = nyrs;

    zSum=0.0;
    for ( j=fYear(i);j<=lYear(i);j++ )
    {
      if ( Indices(i,j) > 0. )
      {
        z(i,j) = log(Indices(i,j)/biomass(j));
        zSum+=z(i,j);
        validObs(i)+=1;
      }
    }

    // MLE lnq if relative.
    if ( indexType(i) > 0. & zSum != 0. )
    {
      lnq(i)=zSum/validObs(i);
    }
    // lnq==0 if absolute.
    else
    {
      lnq(i)=0.0;
    }

    // Residual function over valid observations.
    for ( j=fYear(i);j<=lYear(i);j++ )
    {
      if ( Indices(i,j) > 0.0 )
      {
        ss(i)+=pow(z(i,j)-lnq(i),2.0);
        scaledIndices(i,j)=Indices(i,j)/exp(lnq(i));
      }
      else
      {
        scaledIndices(i,j)=-1.;
      }
    }
  }

  // Reduce avgF penalty in last phase. (fpen set to 0 by KRH; 23-Sep-09)
  if ( last_phase())  //avgF prior.
  {
    //fpen=square(log(avgF/avgF));
    //Counter for number of function calls made in optimization; Added by K.Holt 28-Aug-09
    neval+=1;	
  }
  else
  {
    // High avgF penalty in early phase.
    //fpen=10000.*square(log(avgF/Fmsy));
    neval=-2.;  // start neval counter at -2 at start of last phase so it equals admb screen output
  }

  // Final obj function value...negative log-posterior plus penalty
  // for non-zero biomass constraint.

  //Total error likelihood.
  Ng=sum(validObs);
  SSg=sum(ss);
  SSw=norm2(lnOmega);
  kappaSq = 1./(Ng+nyrs-2.)*(SSg/rho+SSw/(1.-rho));

  like = (Ng+nyrs-2.)/2.*log(SSg/rho+SSw/(1.-rho));

  //Prior on Fmsy
  prior = pow( MSY - muPriorMSY, 2. )/( 2.*sdPriorMSY*sdPriorMSY );
  prior += pow( Fmsy - muPriorFmsy, 2. )/( 2.*sdPriorFmsy*sdPriorFmsy );
  
  f=(likeWgt*like)+prior;
  f=f+fpen;

//*******************************************************************/

REPORT_SECTION
  //Outputs as required for management procedure simulations.
  report << "## PMOD: production model results" << endl;
  report << "# nT" << endl;
  report << nyrs << endl;
  report << "# MSY" << endl;
  report << MSY << endl;
  report << "# Bmsy" << endl;
  report << Bmsy << endl;
  report << "# Fmsy" << endl;
  report << Fmsy << endl;
  report << "# rho" << endl;
  report << rho << endl;
  report << "# D" << endl;
  report << depletion << endl;
  report << "# lastBt" << endl;
  report << projBiomass << endl;
  report << "# lastDt" << endl;
  report << katch(nyrs) << endl;
  report << "# Bt" << endl;
  report << biomass << endl;
  report << "# lnOmega" << endl;
  report << lnOmega << endl;
  report << "# Ct" << endl;
  report << katch << endl;
  report << "# Ft" << endl;
  report << elem_div(katch,biomass) << endl;
  report << "# objFun" << endl;
  report << *objective_function_value::pobjfun << endl;
  report << "# maxGrad" << endl;
  report << objective_function_value::gmax << endl;
  report << "# iExit" << endl;
  report << iexit << endl;
  report << "# nEval" << endl;
  report << neval << endl;
  report <<"# q" << endl;
  report << mfexp(lnq) <<endl;
  report << "# ItScaled" << endl;
  for ( int i=1;i<=nIndices;i++ )
    report << row(scaledIndices,i) << endl;

TOP_OF_MAIN_SECTION
	arrmblsize = 50000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	gradient_structure::set_MAX_NVAR_OFFSET(5000);
	gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);

GLOBALS_SECTION
	#include <admodel.h>
	#include <iostream.h>
	  //ofstream mcEval("mceval.out");


