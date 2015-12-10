//********************************************************************
// Module: assessCA.tpl
// Authors : S.P. Cox, A.R. Kronlund, K.R. Holt
// Procedure: Statistical catch-age model
//
// References:
//
// Date Revised:
// 26-Apr-10  Initial implementation by SPC.
// 24-Nov-11  Revised for Risk-Based MP paper
//            Implemented projections
//*******************************************************************/
DATA_SECTION
  // Total length of catch data.
  init_int nT;

  // Number of age-classes.
  init_int nAges;
    
  // Number of projection years...if proj==1
  init_int nProjYrs;
  
  // Number of years for recent trend est
  init_int trendYrs;

  // Random number seed
  init_int rSeed;
  
  // Type of rule information required
  init_int ruleType;
    
  // Fixed parameters and schedules.
  init_number aMat50;
  init_number aMat95;
  init_number Linf;
  init_number L1;
  init_number vonK;
  init_number c1;
  init_number c2;
  init_number sigmaL;
  init_number tau;
  
  // Prior distribution parameters
  init_number pmSteepness;
  init_number psdSteepness;

  // Switch for using age data
  init_number useAges;
  int phase_aSelStep;
  !! if(useAges==0) phase_aSelStep = -1;
  !! if(useAges==1) phase_aSelStep = 3;

  // Vector of catch biomass (tonnes) values.
  init_vector katch(1,nT+nProjYrs);

  // Input abundance index type.
  init_int indexType;

  // Input index data
  init_vector Index(1,nT);

  // Observed proportions-at-age
  init_matrix obsPropAge(1,nT,1,nAges);
  vector obsPa(1,nAges);

  // Quota options for forward projections
  init_int nQlevels;
  init_vector Qlevel(1,nQlevels);
  

//*******************************************************************/
PARAMETER_SECTION
  objective_function_value f;
  init_bounded_number logit_ySteepness(0.05,5.);
  init_number ln_B0;
  init_number ln_M(-1);
  init_bounded_number ln_aSel50(1.5,2.99,2);
  init_bounded_number ln_aSelStep(0.1,3.,2);
  init_number ln_sigmaR(-1);
  init_number rho(-1);
  init_bounded_vector omega(2,nT-aMat50,-5.,5.,3);
  

  // Derived variables.
  number B0;
  number M;
  number ySteepness;
  number rSteepness;
  number aSel50;
  number aSel95;
  number rec_a;
  number rec_b;
  number sigmaR;
  number phi;
  vector tmpPhi;
  number R0;

  // Life schedules
  vector age(1,nAges);
  !! age.fill_seqadd(1,1);
  vector sel(1,nAges);
  vector wt(1,nAges);
  vector wt2(1,nAges);
  vector len(1,nAges);
  vector mat(1,nAges);
  vector surv(1,nAges);
  number g;

  // Derived state variables.
  matrix numbers(1,nT,1,nAges);
  matrix biomass(1,nT,1,nAges);
  vector exploitBiomass(1,nT);
  vector spawnBiomass(1,nT+nProjYrs);
  matrix predPropAge(1,nT,1,nAges);
  vector Ft(1,nT);
  vector uAge(1,nAges);
  vector catAge(1,nAges);

  sdreport_number termDep;
  number projBiomass;

  // Likelihood quantities for index.
  number index_nll;
  number lnq;
  number ss;
  vector z(1,nT);
  number zSum;
  // Counts of actual non-missing data values
  number validObs;
  number pos_pen;

  // Likelihood quantities for age proportion data.
  number kappa_sq;
  vector predPa(1,nAges);
  number age_nll;
  number ss1;
  number ss2;

  // Steepness prior distribution quantities (Mangel and ...2010)
  number muB;
  number tauB;
  number aB;
  number bB;
  number steepness_nlp;
  
  // Counter for number of function evalulations.
  number neval;  
  
GLOBALS_SECTION
  // int mcHeader=0;
  // int mcTrials=0;
  // #include <fstream.h>
  // ofstream mcout("mcout.dat");

 // ARK (24-Nov-11) Required for VC 64-bit.
  #include <admodel.h>
  int mcHeader=0;
  int mcTrials=0;
  //#include <fstream.h>
  ofstream mcout("mcout.dat");

TOP_OF_MAIN_SECTION
  arrmblsize = 20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(25000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
 
//*******************************************************************/
PROCEDURE_SECTION
  f=0.;
  calcLenWeightAge();
  calcMaturity();
  calcSelectivity();
  calcPhi();
  calcRecPars();
  popInit();
  popDynamics();
  if( mc_phase() )
  {
    switch( ruleType )
    {
      case 1:
        projDynamics12();
        break;
        
      case 2:
        projDynamics12();
        break;
      
      case 3:
        projDynamics3();    
        break;      
    }
  }
  calcIndexLikelihood();
  calcAgeLikelihood();
  calcRecPrior();
  f = index_nll + useAges*age_nll + ss2/(2.*pow(sigmaR,2.));
  f += steepness_nlp;
  
  if ( last_phase())
  {
    // Counter for number of function calls made in optimization.
    neval+=1;	
  }
  else
  {
    // Set neval counter at -2 at start of last phase so it equals admb screen output. 
    neval=-2.;                                                                         
  }  
  
//*******************************************************************/
//            Likelihood Function Components                         /
//*******************************************************************/
FUNCTION calcIndexLikelihood
    // Likelihood assumes known variance (tau^2).
    // Catchability MLE determined for each series unless indexType=0
    // for absolute in which case force q=1.

    // Initialize likelihood function terms.
    int i,j; z.initialize();

    // Calculations for MLE q.
    zSum=0.; validObs=0.; 
    for ( j=1;j<=nT;j++ )
    {
        if ( Index(j) > 0.0 )
        {
          z(j) = log( Index(j) ) - log( exploitBiomass(j) );
          zSum += z(j);
          validObs += 1.;
        }
    }
    // MLEe lnq if relative index, lnq=0 if absolute index.
    if ( indexType > 0.0 )
    {
      lnq = zSum/validObs;
    }
    else
    {
      lnq = 0.0;
    }
    // Residual function over valid observations.
    ss1=0.;
    for ( j=1;j<=nT;j++ )
    {
        if ( Index(j) > 0.0 )
        {
            ss1 += pow( (z(j) - lnq), 2. );
        }
    }
    // Negative log-likelihood function value for index series.
    index_nll = ss1/(2.*tau*tau);  

FUNCTION calcAgeLikelihood
    int t; age_nll.initialize();
    // Calculate predicted age-proportions row-by-row.
    for ( t=1;t<=nT;t++ )
    {
        predPa = elem_prod( sel, row( biomass, t) );
        
        predPa /= sum( predPa );
        predPropAge(t)(1,nAges) = predPa;
        // check that age-comp exists for this year
        obsPa = row( obsPropAge, t );  
        if ( obsPa[t,1] > 0. )
        {
  	      // log-likelihood for this age-comp (from MULTIFAN-CL).
  	      for( int a=1; a<=nAges;a++ )
  	        age_nll += log( exp( -pow( obsPa(a)-predPa(a), 2. )/(2.*(predPa(a)*(1.-predPa(a))) + 0.01 ) ) );	     
        }
    }
    // MLE of variance in age-proportions.
    age_nll *= (-1.);  

FUNCTION calcRecPrior
    // Recruitment std error
    sigmaR = exp( ln_sigmaR );
    // Normal prior on recruitment devs
    ss2 = norm2( omega );

    // Beta prior on steepness
    muB = 1.25*pmSteepness-0.25;
    tauB = muB*(1.-muB)/( 1.5625*pow(psdSteepness,2) )-1.; 
    aB = tauB*muB; 
    bB = tauB*(1.-muB); 
    steepness_nlp = (-1.)*( (aB-1.)*log(ySteepness) + (bB-1.)*log(1.-ySteepness) );    

//*******************************************************************/
//					Population dynamics functions                            / 
//*******************************************************************/
FUNCTION popDynamics
    int t;

    for ( t=2;t<=nT;t++ ) 
    {

      // Calc proportion of each age in catch.
      uAge.initialize();
      uAge = elem_prod( sel, row(biomass,t-1) )/exploitBiomass(t-1);

      // Apportion total catch biomass among ages and convert to numbers
      catAge = elem_div(uAge*katch(t-1),wt);
      
      // Update age-1 recruits.
      numbers(t,1) = rec_a*spawnBiomass(t-1)/( 1. + rec_b*spawnBiomass(t-1) );
      if( t <= (nT-aMat50) ){
    	  if( active(omega) ){
    	    numbers(t,1) = numbers(t,1)*mfexp( omega(t) );		
    	  }
      }
      biomass(t,1) = numbers(t,1)*wt(1);     

      // Update numbers-at-age by removing catch numbers and M as discrete events.
      for ( int a=2;a<nAges;a++ )
      {
        numbers(t,a) = numbers(t-1,a-1) - catAge(a-1);
        
        numbers(t,a) = exp(-M)*posfun( numbers(t,a), 0.1, pos_pen );
        
        f+=1000.*pos_pen;
        biomass(t,a) = numbers(t,a)*wt(a);
      }
      // Plus group numbers.
      numbers(t,nAges) = numbers(t-1,nAges-1) + numbers(t-1,nAges) -  catAge(nAges-1) - catAge(nAges);
      numbers(t,nAges) = exp(-M) * posfun( numbers(t,nAges), 2., pos_pen );
      f+=1000.*pos_pen;

      // Plus group biomass.
      biomass(t,nAges) = numbers(t,nAges)*wt(nAges);

      // Exploitable biomass 
      exploitBiomass(t) = sum( elem_prod(sel,biomass(t)) );
      
      // Fishing mortality
      Ft(t) = katch(t)/exploitBiomass(t);
      
      // Spawning biomass.
      spawnBiomass(t) = sum( elem_prod(mat,biomass(t)));
    }
    termDep = spawnBiomass(nT)/B0;
    
FUNCTION projDynamics3
    // run projection during last_phase()
    dvector omegaProj(1,nProjYrs);
    random_number_generator rng(rSeed);
    omegaProj.fill_randn(rng);
    
    // transfer all estimated parameters to dvariables, dvectors, etc.
    // e.g., projVar = value(estVar)
    dvector newN(1,nAges); dvector newB(1,nAges);
    dvector oldN(1,nAges); dvector oldB(1,nAges);
    oldN = value( row(numbers,nT) );
    oldB = value( row(biomass,nT) );
    
    double eBio = value( exploitBiomass(nT) );
    double expBio = 0;
    double sBio = value( spawnBiomass(nT) );
    
    dvector prj_sel = value(sel);
    dvector prj_mat = value(mat);
    dvector prj_wt = value(wt);
    
    // re-derive recruitment model parameters
    double prj_M = value(M);
  	double prj_B0 = value( exp(ln_B0) );
  	double prj_ySteepness = value( exp(logit_ySteepness)/(1.+exp(logit_ySteepness)) );
  	double prj_rSteepness = (prj_ySteepness+0.30)/1.30;
  	double prj_R0 = prj_B0/value(phi);
  	
  	// Beverton-Holt a parameter.
  	double prj_a = 4.*prj_rSteepness*prj_R0/(prj_B0*(1.-prj_rSteepness));
  	// Beverton-Holt b parameter.
  	double prj_b = (5.*prj_rSteepness-1.)/(prj_B0*(1.-prj_rSteepness));    
    double prj_sigmaR = value( sigmaR );
    
    for( int j=1; j<=nQlevels; j++ )
    {
      int Declined = 0;
      for ( int t=1; t<=nProjYrs; t++ )
      {
        // Calc proportion of each age in last year's catch.
        dvector prj_uAge = elem_prod( prj_sel, oldB )/eBio;

        // Apportion last year's total catch biomass among ages and convert to numbers
        dvector prj_catAge = elem_div( prj_uAge*Qlevel(j),prj_wt );

        // Update age-1 recruits.
        newN(1) = prj_a*sBio/( 1. + prj_b*sBio );
    	  newN(1) = newN(1)*mfexp( prj_sigmaR*omegaProj(t) );
        newB(1) = newN(1)*prj_wt(1);     

        // Update numbers-at-age by removing catch and M.
        for ( int a=2; a<nAges; a++ )
        {
          newN(a) = exp(-prj_M)*(oldN(a-1) - prj_catAge(a-1));
          newB(a) = newN(a)*prj_wt(a);
        }
        // Plus group numbers.
        newN(nAges) = exp(-prj_M) * (oldN(nAges-1) + oldN(nAges) -  prj_catAge(nAges-1) - prj_catAge(nAges));

        // Plus group biomass.
        newB(nAges) = newN(nAges)*prj_wt(nAges);

        // Exploitable biomass 
        eBio = sum( elem_prod(prj_sel,newB) );
        if( t=(nT+1) )
          expBio = eBio;

        // Spawning biomass.
        sBio = sum( elem_prod(prj_mat,newB) );
      } // projYrs loop

      // Did the stock decline under this Q?
      if( sBio < value(spawnBiomass(nT)) )
        Declined = 1;
      // Write Q and Declined to file
      
      // Write to mcout.dat file
      // ARK (24-Nov-10) Required for 64-bit VC compiler.
      //ofstream mcout("mcout.dat", ios::app );
          
      if( mcHeader== 0 )
      {
        // Output the leading model parameters needed for reference point calcs.
        // This will create reduandant information for each quota level, but will
        // be filtered in R.

        mcout << "B0 R0 rSteepness rec.a rec.b M aSel50 aSel95 lnsigmaR rho aMat50 aMat95 ";
        mcout << "Linf L1 vonK c1 c2 sigmaL tau ";
        
        // Output needed for 9-zone feed forward harvest control rule.
        mcout << "Q Declined trendBio spawnBiomass eBio termDep" << endl;

        // Output the values.
        mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
        mcout << " " << M << " " << aSel50 << " " << aSel95 << " " << ln_sigmaR << " " << rho << " ";
        mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
        mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
                
        mcout << Qlevel(j) << " " << Declined << " "
              << log(spawnBiomass(nT-trendYrs+1)/spawnBiomass(nT))/trendYrs << " "
              << spawnBiomass(nT) << " " << expBio << " " << termDep << endl; 

        mcHeader = 1;
      }
      else
      {
        // Output the values.
        mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
        mcout << " " << M << " " << aSel50 << " " << aSel95 << " " << ln_sigmaR << " " << rho << " ";
        mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
        mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
                
        mcout << Qlevel(j) << " " << Declined << " "
              << log(spawnBiomass(nT-trendYrs+1)/spawnBiomass(nT))/trendYrs << " "
              << spawnBiomass(nT) << " " << expBio << " " << termDep << endl; 
      }    
    } // Qlevels loop    

FUNCTION projDynamics12
    // ruleType 1 or 2: run projection only one year ahead to get
    // projected exploitable biomass...used to get
    // next year's quota
    random_number_generator rng(rSeed);
    double omegaProj = randn(rng);

    // transfer all estimated parameters to dvariables, dvectors, etc.
    // e.g., projVar = value(estVar)
    dvector newN(1,nAges); dvector newB(1,nAges);
    dvector oldN(1,nAges); dvector oldB(1,nAges);
    oldN = value( row(numbers,nT) );
    oldB = value( row(biomass,nT) );

    double eBio = value( exploitBiomass(nT) );
    double sBio = value( spawnBiomass(nT) );

    dvector prj_sel = value(sel);
    dvector prj_mat = value(mat);
    dvector prj_wt = value(wt);

    // re-derive recruitment model parameters
    double prj_M = value(M);
  	double prj_B0 = value( exp(ln_B0) );
  	double prj_ySteepness = value( exp(logit_ySteepness)/(1.+exp(logit_ySteepness)) );
  	double prj_rSteepness = (prj_ySteepness+0.30)/1.30;
  	double prj_R0 = prj_B0/value(phi);

  	// Beverton-Holt a parameter.
  	double prj_a = 4.*prj_rSteepness*prj_R0/(prj_B0*(1.-prj_rSteepness));
  	// Beverton-Holt b parameter.
  	double prj_b = (5.*prj_rSteepness-1.)/(prj_B0*(1.-prj_rSteepness));    
    double prj_sigmaR = value( sigmaR );
    
    // Calc proportion of each age in last year's catch.
    dvector prj_uAge = elem_prod( prj_sel, oldB )/eBio;

    // Apportion last year's total catch biomass among ages and convert to numbers
    dvector prj_catAge = elem_div( prj_uAge*katch(nT),prj_wt );

    // Update age-1 recruits.
    newN(1) = prj_a*sBio/( 1. + prj_b*sBio );
	  newN(1) = newN(1)*mfexp( prj_sigmaR*omegaProj );
    newB(1) = newN(1)*prj_wt(1);     

    // Update numbers-at-age by removing catch and M.
    for ( int a=2; a<nAges; a++ )
    {
      newN(a) = exp(-prj_M)*(oldN(a-1) - prj_catAge(a-1));
      newB(a) = newN(a)*prj_wt(a);
    }
    // Plus group numbers.
    newN(nAges) = exp(-prj_M) * (oldN(nAges-1) + oldN(nAges) -  prj_catAge(nAges-1) - prj_catAge(nAges));

    // Plus group biomass.
    newB(nAges) = newN(nAges)*prj_wt(nAges);

    // Exploitable biomass 
    eBio = sum( elem_prod(prj_sel,newB) );

    // Spawning biomass.
    sBio = sum( elem_prod(prj_mat,newB) );

    // Write to mcout.dat file
    // ARK (24-Nov-10) Required for 64-bit VC compiler.
    // ofstream mcout("mcout.dat", ios::app );    

    if( mcHeader== 0 )
    {
      // Output the leading model parameters needed for reference point calcs.
      mcout << "B0 R0 rSteepness rec.a rec.b M aSel50 aSel95 lnsigmaR rho aMat50 aMat95 ";
      mcout << "Linf L1 vonK c1 c2 sigmaL tau ";
        
      // Output needed for constant F or 3-zone feedback harvest control rule.
      mcout << "spawnBiomass eBio termDep" << endl;
      
      // Output the values.
      mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
      mcout << " " << M << " " << aSel50 << " " << aSel95 << " " << ln_sigmaR << " " << rho << " ";
      mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
      mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
      
      mcout << spawnBiomass(nT) << " " << eBio << " " << termDep << endl; 
      mcHeader = 1;
    }
    else
    {
      // Output the values.
      mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
      mcout << " " << M << " " << aSel50 << " " << aSel95 << " " << ln_sigmaR << " " << rho << " ";
      mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
      mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
        
      mcout << spawnBiomass(nT) << " " << eBio << " " << termDep << endl; 
    }    

//*******************************************************************/
//                    Initialization functions                       /
//*******************************************************************/

FUNCTION popInit
    // Initialize state variables.
    numbers.initialize();      biomass.initialize();
    spawnBiomass.initialize(); exploitBiomass.initialize();
    // Equilibrium unfished states
    numbers(1)(1,nAges-1) = R0*surv(1,nAges-1);
    numbers(1)(nAges)     = R0*surv(nAges)/(1.-exp(-M));
    biomass(1)(1,nAges)   = elem_prod( row(numbers,1), wt );

    exploitBiomass(1) = sum( elem_prod( sel, row(biomass,1) ) );
    spawnBiomass(1)   = sum( elem_prod( mat, row(biomass,1) ) );    


FUNCTION calcRecPars
  	// Estimated parameters - unfished biomass and steepness.
  	B0 = exp(ln_B0);
  	// This transformation bounds steepness to 0.23-0.97
  	ySteepness = exp(logit_ySteepness)/(1.+exp(logit_ySteepness));
  	rSteepness = (ySteepness+0.30)/1.30;

  	// Unfished recruitment: phi=SSBPR as computed below
  	R0 = B0/phi;
  	
  	// Beverton-Holt a parameter.
  	rec_a = 4.*rSteepness*R0/(B0*(1.-rSteepness));
  	// Beverton-Holt b parameter.
  	rec_b = (5.*rSteepness-1.)/(B0*(1.-rSteepness));
    
FUNCTION calcPhi
  	// Calculate equilibrium unfished survivorship.
  	M = mfexp(ln_M);
  	surv(1) = 1.0;
  	for ( int a=2; a<=nAges; a++ )
  	  surv(a) = surv(a-1) * exp(-M);

  	// phi=SSBPR function for 2 < ages < A.
  	tmpPhi = elem_prod( surv(1,(nAges-1)), mat(1,(nAges-1)) );
  	tmpPhi = elem_prod( tmpPhi, wt(1,(nAges-1)) );

  	//phi, including plus group.
  	phi = sum(tmpPhi) + surv(nAges)*mat(nAges)*wt(nAges)/( 1.0-exp(-M) );
	  
FUNCTION calcLenWeightAge
    // Length
    len = Linf + (L1-Linf)*exp(-vonK*(age-1.));
    
    // Weight
    // ARK (15-Dec-11) Note that OM has bias correction, should it be here too?
    // The bias correction appears to be about 1.23.  Is this right!!!????
    // wtAge <- c1*lenAge^c2*(1.+0.5*c2*(c2-1.)*sigmaL*sigmaL)
    //wt = c1*pow(len,c2)*(1.+0.5*c2*(c2-1.)*sigmaL*sigmaL);
    
    wt = c1*pow(len,c2);
    
    // ARK (15-Dec-11) This creates difference in units with OM, but convergence
    // will fail without it during feedback simulations. Numerical scaling issue?
    // wt /= 1.e6;

FUNCTION calcSelectivity
    aSel50 = exp( ln_aSel50 );
    aSel95 = aSel50 + exp( ln_aSelStep );
    g = log(19.)/( aSel95 - aSel50 );
  	sel = 1./( 1. + exp(-g*( age - aSel50 ) ) );
    
FUNCTION calcMaturity
    g = log(19.)/( aMat95 - aMat50 );
  	mat = 1./( 1. + exp(-g*( age - aMat50 ) ) );    

//*******************************************************************/
REPORT_SECTION
  //Outputs required for management procedure simulations
  report << "## assessCA model parameter and state variable estimates" << endl;
  report << "## Model dimensions" << endl;
  report << "# nT" << endl;
  report << nT << endl;
  report << "# nAges" << endl;
  report << nAges << endl;
  report << endl;

  report << "## Parameter estimates" << endl;
  report << "# logit.ySteepness" << endl;
  report << logit_ySteepness << endl;
  report << "# rSteepness" << endl;
  report << rSteepness << endl;
  report << "# rec.a" << endl;
  report << rec_a << endl;
  report << "# rec.b" << endl;
  report << rec_b << endl;
  report << "# R0" << endl;
  report << R0 << endl;
  report << "# B0" << endl;
  report << B0 << endl;
  report << "# M " << endl;
  report << M << endl;
  report << "# aSel50" << endl;
  report << aSel50 << endl;
  report << "# aSel95" << endl;
  report << aSel95 << endl;
  report << "# omega" << endl;
  report << omega << endl;
  report << "# Rt" << endl;
  report << column( numbers,1 ) << endl;
  report << "# tau" << endl;
  report << tau << endl;
  report << "# q" << endl;
  report << exp( lnq ) << endl;
  report << endl;

  report << "## Derived variables" << endl;
  report << "# spawnBt" << endl;
  report << spawnBiomass(1,nT) << endl;
  report << "# exploitBt" << endl;
  report << exploitBiomass(1,nT) << endl;
  report << "# Ft" << endl;
  report << Ft << endl;
  report << "# D" << endl;
  report << spawnBiomass[nT]/spawnBiomass[1] << endl;
  report << "# projBiomass" << endl;
  report << spawnBiomass[nT+nProjYrs] << endl;
  report << "# trendBio" << endl;
  report << log(spawnBiomass[nT-trendYrs+1]/spawnBiomass[nT]) << endl;
  report << endl;

  report << "## Priors" << endl;
  report << "# pmSteepness" << endl;
  report << pmSteepness << endl;
  report << "# psdSteepness" << endl;
  report << psdSteepness << endl;
  report << endl;

  report << "## Minimization properties" << endl;
  report << "# index_nll" << endl;
  report << index_nll << endl;
  report << "# age_nll" << endl;
  report << age_nll << endl;
  report << "# steepness_nlp" << endl;
  report << steepness_nlp << endl;
  report << "# objFun" << endl;
  report << *objective_function_value::pobjfun << endl;
  report << "# maxGrad" << endl;
  report << objective_function_value::gmax << endl;
  report << "# iExit" << endl;
  report << iexit << endl;
  report << "# nEval" << endl;
  report << neval << endl;  
  report << endl;

  report << "## Life history schedules" << endl;
  report << "# len" << endl;
  report << len << endl;
  report << "# wt" << endl;
  report << wt << endl;
  report << "# mat" << endl;
  report << mat << endl;
  report << "# sel" << endl;
  report << sel << endl;
  report << endl;

  report << "## Input data" << endl;
  report << "# katch" << endl;
  report << katch << endl;
  report << "# lastDt" << endl;
  report << katch(nT) << endl;
  report << "# ItScaled" << endl;
  report << Index/exp( lnq ) << endl;
  report << "# obsPropAge" << endl;
  report << obsPropAge << endl;
  report << "# predPropAge" << endl;
  report << predPropAge << endl;

  report << "# biomass" << endl;
  report << biomass << endl;

  report << "# numbers" << endl;
  report << numbers << endl;
