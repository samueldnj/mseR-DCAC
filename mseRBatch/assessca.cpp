  #include <admodel.h>
  int mcHeader=0;
  int mcTrials=0;
  int mcCounter=0;
  //#include <fstream.h>
  ofstream mcout("mcout.dat");
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <assessca.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  nT.allocate("nT");
  nAges.allocate("nAges");
  nProjYrs.allocate("nProjYrs");
  trendYrs.allocate("trendYrs");
  rSeed.allocate("rSeed");
  ruleType.allocate("ruleType");
  aMat50.allocate("aMat50");
  aMat95.allocate("aMat95");
  Linf.allocate("Linf");
  L1.allocate("L1");
  vonK.allocate("vonK");
  c1.allocate("c1");
  c2.allocate("c2");
  sigmaL.allocate("sigmaL");
  pmSteepness.allocate("pmSteepness");
  psdSteepness.allocate("psdSteepness");
  pmM.allocate("pmM");
  psdM.allocate("psdM");
  qChangeTime.allocate("qChangeTime");
  pmQ.allocate(1,2,"pmQ");
  psdQ.allocate(1,2,"psdQ");
  sigmaM.allocate("sigmaM");
  lowBndLnB0.allocate("lowBndLnB0");
  upperBndLnB0.allocate("upperBndLnB0");
  fixFSel.allocate("fixFSel");
  fixSSel.allocate("fixSSel");
  nSamples.allocate("nSamples");
  nSamplesS.allocate("nSamplesS");
 if(fixFSel==0) phase_aSelStep = 2;
 if(fixFSel==1) phase_aSelStep = -1;
 if(fixSSel==0) phase_aSelStepS = 2;
 if(fixSSel==1) phase_aSelStepS = -1;
  katch.allocate(1,nT,"katch");
  indexType.allocate(1,2,"indexType");
  Index.allocate(1,nT,"Index");
  obsPropAge.allocate(1,nT,1,nAges,"obsPropAge");
  obsPa.allocate(1,nAges);
  obsPropAgeS.allocate(1,nT,1,nAges,"obsPropAgeS");
  obsPaS.allocate(1,nAges);
  nQlevels.allocate("nQlevels");
  Qlevel.allocate(1,nQlevels,"Qlevel");
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  f.allocate("f");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  log_initN_mult.allocate(1,nAges,-4,4,1,"log_initN_mult");
  logit_ySteepness.allocate(0.05,5.,"logit_ySteepness");
  ln_B0.allocate(lowBndLnB0,upperBndLnB0,"ln_B0");
  ln_M.allocate(2,"ln_M");
  ln_aSel50.allocate(-1.,4.5,phase_aSelStep,"ln_aSel50");
  ln_aSelStep.allocate(-1.,4.5,phase_aSelStep,"ln_aSelStep");
  ln_aSelS50.allocate(-1.,4.5,phase_aSelStepS,"ln_aSelS50");
  ln_aSelSStep.allocate(-1.,4.5,phase_aSelStepS,"ln_aSelSStep");
  rho.allocate(-1,"rho");
  omega.allocate(2,nT-2,-5.,5.,3,"omega");
  mDevs.allocate(2,nT,-2.,2.,4,"mDevs");
  B0.allocate("B0");
  #ifndef NO_AD_INITIALIZE
  B0.initialize();
  #endif
  M.allocate("M");
  #ifndef NO_AD_INITIALIZE
  M.initialize();
  #endif
  ySteepness.allocate("ySteepness");
  #ifndef NO_AD_INITIALIZE
  ySteepness.initialize();
  #endif
  rSteepness.allocate("rSteepness");
  #ifndef NO_AD_INITIALIZE
  rSteepness.initialize();
  #endif
  aSel50.allocate("aSel50");
  #ifndef NO_AD_INITIALIZE
  aSel50.initialize();
  #endif
  aSel95.allocate("aSel95");
  #ifndef NO_AD_INITIALIZE
  aSel95.initialize();
  #endif
  aSelS50.allocate("aSelS50");
  #ifndef NO_AD_INITIALIZE
  aSelS50.initialize();
  #endif
  aSelS95.allocate("aSelS95");
  #ifndef NO_AD_INITIALIZE
  aSelS95.initialize();
  #endif
  rec_a.allocate("rec_a");
  #ifndef NO_AD_INITIALIZE
  rec_a.initialize();
  #endif
  rec_b.allocate("rec_b");
  #ifndef NO_AD_INITIALIZE
  rec_b.initialize();
  #endif
  phi.allocate("phi");
  #ifndef NO_AD_INITIALIZE
  phi.initialize();
  #endif
  tmpPhi.allocate("tmpPhi");
  R0.allocate("R0");
  #ifndef NO_AD_INITIALIZE
  R0.initialize();
  #endif
  tau.allocate("tau");
  #ifndef NO_AD_INITIALIZE
  tau.initialize();
  #endif
  sigmaR.allocate("sigmaR");
  #ifndef NO_AD_INITIALIZE
  sigmaR.initialize();
  #endif
  Mt.allocate(1,nT,"Mt");
  #ifndef NO_AD_INITIALIZE
    Mt.initialize();
  #endif
  age.allocate(1,nAges,"age");
  #ifndef NO_AD_INITIALIZE
    age.initialize();
  #endif
 age.fill_seqadd(1,1);
  sel.allocate(1,nAges,"sel");
  #ifndef NO_AD_INITIALIZE
    sel.initialize();
  #endif
  selS.allocate(1,nAges,"selS");
  #ifndef NO_AD_INITIALIZE
    selS.initialize();
  #endif
  wt.allocate(1,nAges,"wt");
  #ifndef NO_AD_INITIALIZE
    wt.initialize();
  #endif
  wt2.allocate(1,nAges,"wt2");
  #ifndef NO_AD_INITIALIZE
    wt2.initialize();
  #endif
  len.allocate(1,nAges,"len");
  #ifndef NO_AD_INITIALIZE
    len.initialize();
  #endif
  mat.allocate(1,nAges,"mat");
  #ifndef NO_AD_INITIALIZE
    mat.initialize();
  #endif
  surv.allocate(1,nAges,"surv");
  #ifndef NO_AD_INITIALIZE
    surv.initialize();
  #endif
  g.allocate("g");
  #ifndef NO_AD_INITIALIZE
  g.initialize();
  #endif
  gS.allocate("gS");
  #ifndef NO_AD_INITIALIZE
  gS.initialize();
  #endif
  numbers.allocate(1,nT,1,nAges,"numbers");
  #ifndef NO_AD_INITIALIZE
    numbers.initialize();
  #endif
  biomass.allocate(1,nT,1,nAges,"biomass");
  #ifndef NO_AD_INITIALIZE
    biomass.initialize();
  #endif
  exploitBiomass.allocate(1,nT,"exploitBiomass");
  #ifndef NO_AD_INITIALIZE
    exploitBiomass.initialize();
  #endif
  exploitBiomassS.allocate(1,nT,"exploitBiomassS");
  #ifndef NO_AD_INITIALIZE
    exploitBiomassS.initialize();
  #endif
  spawnBiomass.allocate(1,nT+nProjYrs,"spawnBiomass");
  #ifndef NO_AD_INITIALIZE
    spawnBiomass.initialize();
  #endif
  predPropAge.allocate(1,nT,1,nAges,"predPropAge");
  #ifndef NO_AD_INITIALIZE
    predPropAge.initialize();
  #endif
  predPropAgeS.allocate(1,nT,1,nAges,"predPropAgeS");
  #ifndef NO_AD_INITIALIZE
    predPropAgeS.initialize();
  #endif
  Ft.allocate(1,nT,"Ft");
  #ifndef NO_AD_INITIALIZE
    Ft.initialize();
  #endif
  uAge.allocate(1,nAges,"uAge");
  #ifndef NO_AD_INITIALIZE
    uAge.initialize();
  #endif
  catAge.allocate(1,nAges,"catAge");
  #ifndef NO_AD_INITIALIZE
    catAge.initialize();
  #endif
  termDep.allocate("termDep");
  projSpawnBio.allocate("projSpawnBio");
  #ifndef NO_AD_INITIALIZE
  projSpawnBio.initialize();
  #endif
  projExpBio.allocate("projExpBio");
  #ifndef NO_AD_INITIALIZE
  projExpBio.initialize();
  #endif
  projExpBioS.allocate("projExpBioS");
  #ifndef NO_AD_INITIALIZE
  projExpBioS.initialize();
  #endif
  kappaSq.allocate("kappaSq");
  #ifndef NO_AD_INITIALIZE
  kappaSq.initialize();
  #endif
  lnq.allocate(1,2,"lnq");
  #ifndef NO_AD_INITIALIZE
    lnq.initialize();
  #endif
  ss.allocate("ss");
  #ifndef NO_AD_INITIALIZE
  ss.initialize();
  #endif
  z.allocate(1,nT,"z");
  #ifndef NO_AD_INITIALIZE
    z.initialize();
  #endif
  zSum.allocate("zSum");
  #ifndef NO_AD_INITIALIZE
  zSum.initialize();
  #endif
  validObs.allocate("validObs");
  #ifndef NO_AD_INITIALIZE
  validObs.initialize();
  #endif
  pos_pen.allocate("pos_pen");
  #ifndef NO_AD_INITIALIZE
  pos_pen.initialize();
  #endif
  kappa_sq.allocate("kappa_sq");
  #ifndef NO_AD_INITIALIZE
  kappa_sq.initialize();
  #endif
  predPa.allocate(1,nAges,"predPa");
  #ifndef NO_AD_INITIALIZE
    predPa.initialize();
  #endif
  predPaS.allocate(1,nAges,"predPaS");
  #ifndef NO_AD_INITIALIZE
    predPaS.initialize();
  #endif
  age_nll.allocate("age_nll");
  #ifndef NO_AD_INITIALIZE
  age_nll.initialize();
  #endif
  age_nllS.allocate("age_nllS");
  #ifndef NO_AD_INITIALIZE
  age_nllS.initialize();
  #endif
  Z1.allocate("Z1");
  #ifndef NO_AD_INITIALIZE
  Z1.initialize();
  #endif
  Z2.allocate("Z2");
  #ifndef NO_AD_INITIALIZE
  Z2.initialize();
  #endif
  Z3.allocate("Z3");
  #ifndef NO_AD_INITIALIZE
  Z3.initialize();
  #endif
  muB.allocate("muB");
  #ifndef NO_AD_INITIALIZE
  muB.initialize();
  #endif
  tauB.allocate("tauB");
  #ifndef NO_AD_INITIALIZE
  tauB.initialize();
  #endif
  aB.allocate("aB");
  #ifndef NO_AD_INITIALIZE
  aB.initialize();
  #endif
  bB.allocate("bB");
  #ifndef NO_AD_INITIALIZE
  bB.initialize();
  #endif
  steepness_nlp.allocate("steepness_nlp");
  #ifndef NO_AD_INITIALIZE
  steepness_nlp.initialize();
  #endif
  M_nlp.allocate("M_nlp");
  #ifndef NO_AD_INITIALIZE
  M_nlp.initialize();
  #endif
  neval.allocate("neval");
  #ifndef NO_AD_INITIALIZE
  neval.initialize();
  #endif
}

void model_parameters::userfunction(void)
{
  f =0.0;
  f=0.;
  calcLenWeightAge();
  calc_Mt();
  calcMaturity();
  calcSelectivity();
  calcPhi();
  calcRecPars();
  popInit();
  popDynamics();
  calcIndexLikelihood_split();
  calcAgeLikelihood();
  calcRecPrior();
  calcMPrior();
  // length of recruitment series (process error)
  int nRecs = omega.indexmax() - omega.indexmin() + 1;
  // estimate of total variance index (rho) + recs (1-rho)
  kappaSq = (Z1/rho + Z2/(1.-rho))/( validObs + nRecs -1.);
  // estimated log-rec devs standard error
  sigmaR  = sqrt( (1.-rho)*kappaSq );
  // estimated obs error standard error
  tau     = sqrt( rho*kappaSq );
  // negative-log-posterior components------------------------------------------------
  // EIV likelihood
  dvariable l_IR = 0.5*(validObs + nRecs -1.)*log( kappaSq ); // EIV likelihood
  // IG(alpha,beta) variance prior
  dvariable alpha = 1.;
  dvariable beta  = 1.;
  dvariable l_kap  = alpha*log(beta)-(alpha+1)*log(kappaSq)-gammln(alpha)-beta/kappaSq;
  // ages likelihood
  dvariable l_P  = age_nll;  //fishery
  dvariable l_PS  = age_nllS;	  //survey
  // steepness prior
  dvariable l_h  = steepness_nlp;                             
  // M prior
  dvariable l_M  = M_nlp + Z3; 
  f = l_IR + l_P + l_PS + l_h + l_M + (-1.)*l_kap;
  // Compute normal prior pdf on catchability coeffs
  dvariable l_q;
  if( indexType(1)==1 )
  {
    // first q
    l_q  = 0.5*pow(lnq(1) - log(pmQ(1)),2)/pow(psdQ(1),2);                                    
    f += l_q;
  }
  if( indexType(2)==1)
  {
    // second q
    l_q  = 0.5*pow(lnq(2) - log(pmQ(2)),2)/pow(psdQ(2),2);                                    
    f += l_q;
  }
  if ( last_phase() )
  {
    // Counter for number of function calls made in optimization.
    neval+=1;
  }
  else
  {
    // Set neval counter at -2 at start of last phase so it equals admb screen output. 
    neval=-2.;                                                                         
  }  
  // run projections during mcmc phase
  if( mceval_phase() )
  {
    mcCounter += 1;
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
}

void model_parameters::calcIndexLikelihood_split(void)
{
    // Index Likelihood for case of split index series. Estimating
    // q1 for 1,...qChangeTime-1 and q2 for qChangeTime,...nT. Each
    // q has its own prior mean and sd.
    // Initialize likelihood function terms.
    int i,j; z.initialize();
    // Calculations for MLE q.
    zSum=0.; validObs=0.; 
    for ( j=1;j<=(qChangeTime-1);j++ )
    {
        if ( Index(j) > 0.0 )
        {
          z(j) = log( Index(j) ) - log( exploitBiomassS(j) );
          zSum += z(j);
          validObs += 1.;
        }
    }
    lnq(1) = zSum/validObs;
    if( indexType(1)==0 )
      lnq(1) = 0.;
    zSum=0.; validObs=0.; 
    for ( j=qChangeTime;j<=nT;j++ )
    {
        if ( Index(j) > 0.0 )
        {
          z(j) = log( Index(j) ) - log( exploitBiomassS(j) );
          zSum += z(j);
          validObs += 1.;
        }
    }
    lnq(2) = zSum/validObs;
    if( indexType(2)==0 )
      lnq(2) = 0.;
    // Residual function over valid observations.
    Z1=0.;
    for ( j=1;j<=nT;j++ )
    {
        if ( Index(j) > 0.0 )
        {
          if( j<qChangeTime )
            Z1 += pow( (z(j) - lnq(1)), 2. );
          else
            Z1 += pow( (z(j) - lnq(2)), 2. );
        }
    }
    // Z1 is ready for total likelihood
}

void model_parameters::calcIndexLikelihood(void)
{
    // Likelihood assumes known variance (tau^2).
    // Catchability MLE determined for each series unless indexType=0
    // for absolute in which case force q=1.
    // Initialize likelihood function terms.
    /*int i,j; z.initialize();
    // Calculations for MLE q.
    zSum=0.; validObs=0.; 
    for ( j=1;j<=nT;j++ )
    {
        if ( Index(j) > 0.0 )
        {
          z(j) = log( Index(j) ) - log( exploitBiomassS(j) );
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
    Z1=0.;
    for ( j=1;j<=nT;j++ )
    {
        if ( Index(j) > 0.0 )
        {
            Z1 += pow( (z(j) - lnq), 2. );
        }
    }
    // Z1 is ready for total likelihood
    */
}

void model_parameters::calcAgeLikelihood(void)
{
    int t; age_nll.initialize();   age_nllS.initialize();
    // Calculate predicted age-proportions row-by-row.
   //FISHERY
    for ( t=1;t<=nT;t++ )
    {
        predPa = elem_prod( sel, row( biomass, t) );	   
        predPa /= sum( predPa );
        predPropAge(t)(1,nAges) = predPa;
        // check that age-comp exists for this year
        obsPa = row( obsPropAge, t );  
        if ( obsPa(nAges) > 0. )
        {
  	      // log-likelihood for this age-comp (from MULTIFAN-CL).
  	      for( int a=1; a<=nAges;a++ )
  	      {
  	        age_nll += log(2.*3.14*(predPa(a)*(1.-predPa(a))+0.1/nAges));
  	        age_nll += log( exp( -nSamples*pow( obsPa(a)-predPa(a), 2. )/(2.*(1.-predPa(a))*predPa(a) +0.1/nAges) )  +.01 );
  	      }
        }
    }
    // MLE of variance in age-proportions.
    age_nll *= (-0.5); 
    //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    //SURVEY
    for ( t=1;t<=nT;t++ )
    {
        predPaS = elem_prod( selS, row( biomass, t) );	   //SURVEY
        predPaS /= sum( predPaS );
        predPropAgeS(t)(1,nAges) = predPaS;
        // check that age-comp exists for this year
        obsPaS = row( obsPropAgeS, t );  
        if ( obsPaS(nAges) > 0. )
        {
  	      // log-likelihood for this age-comp (from MULTIFAN-CL).
  	      for( int a=1; a<=nAges;a++ )
  	      {
  	        age_nllS += log(2.*3.14*(predPaS(a)*(1.-predPaS(a))+0.1/nAges));
  	        age_nllS += log( exp( -nSamplesS*pow( obsPaS(a)-predPaS(a), 2. )/(2.*(1.-predPaS(a))*predPaS(a) +0.1/nAges) )  +.01 );
  	      }
        }
    }
        // MLE of variance in age-proportions.
    age_nllS *= (-0.5); 
     //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
}

void model_parameters::calcRecPrior(void)
{
    // log-recruitment devs sum-of-squares
    Z2 = norm2( omega );
    // Z2 is now ready for inclusion in total likelihood
    // Beta prior on steepness
    muB = 1.25*pmSteepness-0.25;
    tauB = muB*(1.-muB)/( 1.5625*pow(psdSteepness,2) )-1.; 
    aB = tauB*muB; 
    bB = tauB*(1.-muB); 
    steepness_nlp = (-1.)*( (aB-1.)*log(ySteepness) + (bB-1.)*log(1.-ySteepness) );    
}

void model_parameters::calcMPrior(void)
{
    M_nlp = 0.5*pow(M-pmM,2)/pow(psdM,2);
}

void model_parameters::calc_Mt(void)
{
    // Fill random walk.
    Mt(1) = mfexp(ln_M);
    for( int t=2; t<=nT; t++ )
    {
        // Random walk in log_Mj
        Mt(t) = mfexp( log( Mt(t-1) ) + mDevs(t) );
    }
    Z3 = 0.;
    if( active(mDevs) )
      Z3 = 0.5*norm2( mDevs )/pow(sigmaM,2);
}

void model_parameters::popDynamics(void)
{
    int t;
    for ( t=2;t<=nT;t++ ) 
    {
      // Calc proportion of each age in FISHERY
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
        // ARK (25-Oct-13) Consider lowering posfun number to 0.000001.
        numbers(t,a) = exp(-Mt(t))*posfun( numbers(t,a), 0.001, pos_pen );
        f+=1000.*pos_pen;
        biomass(t,a) = numbers(t,a)*wt(a);
      }
      // Plus group numbers.  ARK (25-Oct-13) Consider lowering posfun number to 0.000001.
      numbers(t,nAges) = numbers(t-1,nAges-1) + numbers(t-1,nAges) -  catAge(nAges-1) - catAge(nAges);
      numbers(t,nAges) = exp(-Mt(t)) * posfun( numbers(t,nAges), 0.001, pos_pen );
      f+=1000.*pos_pen;
      // Plus group biomass.
      biomass(t,nAges) = numbers(t,nAges)*wt(nAges);
      // Exploitable biomass 
      exploitBiomass(t) = sum( elem_prod(sel,biomass(t)) );	  //Fishery exploitable biomass
      exploitBiomassS(t) = sum( elem_prod(selS,biomass(t)) );	//RF Survey exploitable biomass
      // Fishing mortality
      // Sean's fix. ARK (25-Oct-13)
      // F(t) = -log( 1.0 - katch(t)/exploitBiomass(t) );
      Ft(t) = katch(t)/exploitBiomass(t);
      // Spawning biomass.
      spawnBiomass(t) = sum( elem_prod(mat,biomass(t)));
    }
    termDep = spawnBiomass(nT)/B0;
}

void model_parameters::projDynamics3(void)
{
    // run projection during mceval_phase() for rule_type=3
    random_number_generator rng(rSeed+mcCounter);
    dvector omegaProj(1,nProjYrs);
    omegaProj.fill_randn(rng);
    int pt;
    // transfer all differential variables to constants
    dvector newN(1,nAges); dvector newB(1,nAges);
    dvector oldN(1,nAges); dvector oldB(1,nAges);
    dvector projYrsSSB(1,nProjYrs);
    double eBio = value( exploitBiomass(nT) );
    double eBioS = value( exploitBiomassS(nT) );
    double expBio = 0;
    double expBioS = 0;
    double sBio = value( spawnBiomass(nT) );
    dvector prj_sel = value(sel);
    dvector prj_selS = value(selS);
    dvector prj_mat = value(mat);
    dvector prj_wt = value(wt);
    // re-derive recruitment model parameters
    double prj_M = value( Mt(nT) );
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
      // Initialize numbers-at-age and biomass-at-age
      oldN = value( row(numbers,nT) );
      oldB = value( row(biomass,nT) );
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
        // Note use of if-then statements here to keep biomass => 0. Not
        // a problem bc these are not being differentiated. Only used in
        // mceval_phase
        for ( int a=2; a<nAges; a++ )
        {
          newN(a) = exp(-prj_M)*(oldN(a-1) - prj_catAge(a-1));
          if( newN(a) < 0. ) newN(a) = 0.;
          newB(a) = newN(a)*prj_wt(a);
        }
        // Plus group numbers.
        newN(nAges) = exp(-prj_M) * (oldN(nAges-1) + oldN(nAges) -  prj_catAge(nAges-1) - prj_catAge(nAges));
        if( newN(nAges) < 0. ) newN(nAges) = 0.;
        // Plus group biomass.
        newB(nAges) = newN(nAges)*prj_wt(nAges);
        // Exploitable biomass 	fishery
        eBio = sum( elem_prod(prj_sel,newB) );
       // Exploitable biomass survey
        eBioS = sum( elem_prod(prj_selS,newB) );
       // Spawning biomass.
        sBio = sum( elem_prod(prj_mat,newB) );
        if( t == 1 )
        {
           projExpBio   = eBio;
	   projExpBioS   = eBioS;
           projSpawnBio = sBio;
        } 
        // Update old numbers and biomass for next year.
        oldN = newN;
        oldB = newB;
        projYrsSSB(t) = sBio;
      } // projYrs loop
      // Did the stock decline under this quota?
      if( sBio < value(spawnBiomass(nT)) )
        Declined = 1;
      // Output current parameters and stats to mcout.dat file
      // ARK (24-Nov-10) Required for 64-bit VC compiler.
      //ofstream mcout("mcout.dat", ios::app );
      if( mcHeader == 0 )
      {
        // Output the leading model parameters needed for reference point calcs.
        // This will create reduandant information for each quota level, but will
        // be filtered in R.
        mcout << "B0 R0 rSteepness rec.a rec.b M aSel50 aSel95 aSelS50 aSelS95 sigmaR rho aMat50 aMat95 ";
        mcout << "Linf L1 vonK c1 c2 sigmaL tau ";
        // Output needed for 9-zone feed forward harvest control rule.
        mcout << "Q Declined trendBio spawnBio projSpawnBio projExpBio projExpBioS termDep" << endl;
        //*for( pt=1; pt<=nProjYrs;pt++ ) 
        //{
        //  mcout << "projYrsSSB" << pt <<" ";  
        //}*/
        //<< endl;
        // Output the values.
        mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
        mcout << " " << M << " " << aSel50 << " " << aSel95 <<" " << aSelS50 << " " << aSelS95 << " " << sigmaR << " " << rho << " ";
        mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
        mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
        mcout << Qlevel(j) << " " << Declined << " "
              << log(spawnBiomass(nT)/spawnBiomass(nT-trendYrs+1))/(trendYrs-1) << " "
              << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio <<" " << projExpBioS<< " " << termDep << endl; 
              //<< spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " " << termDep << " ";
              //for( pt=1; pt<=nProjYrs;pt++ ) 
              //  mcout <<projYrsSSB(pt)<<" ";
        //mcout <<" "<< endl;
        mcHeader = 1;
      }
      else
      {
        // Output the values.
        mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
         mcout << " " << M << " " << aSel50 << " " << aSel95 <<" " << aSelS50 << " " << aSelS95 << " " << sigmaR << " " << rho << " ";
        mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
        mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
        mcout << Qlevel(j) << " " << Declined << " "
              << log(spawnBiomass(nT)/spawnBiomass(nT-trendYrs+1))/(trendYrs-1) << " "
              << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio <<" " << projExpBioS<< " " << termDep << endl;  
              //<< spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " " << termDep << " ";
              //for( pt=1; pt<=nProjYrs;pt++ ) 
              //  mcout <<projYrsSSB(pt)<<" ";
        //mcout <<" "<< endl;
        // Note: uncomment the above for loops etc to output projected biomasses
      } 
    } // Qlevels loop    
}

void model_parameters::projDynamics12(void)
{
    // ruleType 1 or 2: run projection only one year ahead to get
    // projected exploitable biomass...used to get next year's quota
    random_number_generator rng(rSeed+mcCounter);
    double omegaProj;
    omegaProj= randn(rng);
    // transfer all differential variables to constants.
    dvector newN(1,nAges); dvector newB(1,nAges);
    dvector oldN(1,nAges); dvector oldB(1,nAges);
    oldN = value( row(numbers,nT) );
    oldB = value( row(biomass,nT) );
    double eBio = value( exploitBiomass(nT) );
    double eBioS = value( exploitBiomassS(nT) );    //survey
    double sBio = value( spawnBiomass(nT) );
    dvector prj_sel = value(sel);
    dvector prj_selS = value(selS);		//survey
    dvector prj_mat = value(mat);
    dvector prj_wt = value(wt);
    // re-derive recruitment model parameters
    double prj_M = value( Mt(nT) );
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
    projExpBio = eBio;
    projExpBioS = eBioS; //survey
    // Spawning biomass.
    sBio = sum( elem_prod(prj_mat,newB) );
    projSpawnBio = sBio;
    // Write to mcout.dat file
    // ARK (24-Nov-10) Required for 64-bit VC compiler.
    // ofstream mcout("mcout.dat", ios::app );    
    //if ( mc_phase() )
    //{
      if( mcHeader== 0 )
      {
        // Output the leading model parameters needed for reference point calcs.
        mcout << "B0 R0 rSteepness rec.a rec.b M aSel50 aSel95 aSelS50 aSelS95 sigmaR rho aMat50 aMat95 ";
        mcout << "Linf L1 vonK c1 c2 sigmaL tau ";
        // re-calc sigmaR
        // Output needed for constant F or 3-zone feedback harvest control rule.
        mcout << "spawnBio projSpawnBio projExpBio projExpBioS termDep" << endl;
        // Output the values.
        mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
        mcout << " " << prj_M << " " << aSel50 << " " << aSel95 << " " << aSelS50 << " " << aSelS95 << " " << sigmaR << " " << rho << " ";
        mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
        mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";      
        mcout << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " "<< projExpBioS << " ";
        mcout << termDep << endl; 
        mcHeader = 1;
      }
      else
      {
        // Output the values.
        mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
        mcout << " " << prj_M << " " << aSel50 << " " << aSel95 << " " << aSelS50 << " " << aSelS95 << " " << sigmaR << " " << rho << " ";
        mcout << aMat50 << " " << aMat95 << " " << Linf << " " << L1  << " " << vonK << " ";
        mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
        mcout << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " "<< projExpBioS << " ";
        mcout << termDep << endl; 
      }
    //}    
}

void model_parameters::popInit(void)
{
    // Initialize state variables.
    numbers.initialize();      biomass.initialize();
    spawnBiomass.initialize(); exploitBiomass.initialize();
    // Equilibrium unfished states
    numbers(1)(1,nAges-1) = R0*surv(1,nAges-1);
    numbers(1)(nAges)     = R0*surv(nAges)/(1.-exp(-Mt(1)));
    numbers(1)            = elem_prod(numbers(1), exp(log_initN_mult) );
    biomass(1)(1,nAges)   = elem_prod( row(numbers,1), wt );
    exploitBiomass(1) = sum( elem_prod( sel, row(biomass,1) ) );      //fishery
    exploitBiomassS(1) = sum( elem_prod( selS, row(biomass,1) ) );  //survey
    spawnBiomass(1)   = sum( elem_prod( mat, row(biomass,1) ) );    
}

void model_parameters::calcRecPars(void)
{
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
}

void model_parameters::calcPhi(void)
{
  	// Calculate equilibrium unfished survivorship.
    Mt(1) = mfexp(ln_M);
    surv(1) = 1.0;
    for ( int a=2; a<=nAges; a++ )
      surv(a) = surv(a-1) * exp(-Mt(1));
  	// phi=SSBPR function for 2 < ages < A.
  	tmpPhi = elem_prod( surv(1,(nAges-1)), mat(1,(nAges-1)) );
  	tmpPhi = elem_prod( tmpPhi, wt(1,(nAges-1)) );
  	//phi, including plus group.
    phi = sum(tmpPhi) + surv(nAges)*mat(nAges)*wt(nAges)/( 1.0-exp(-Mt(1)) );
}

void model_parameters::calcLenWeightAge(void)
{
    // Length
    len = Linf + (L1-Linf)*exp(-vonK*(age-1.));
    // Weight
    wt = c1*pow(len,c2);    
}

void model_parameters::calcSelectivity(void)
{
    // derived selectivity parameters: the additive
    // exponential form ensures that aSel95 > aSel50
    // for asymptotic selectivity function
    //FISHERY
    aSel50 = exp( ln_aSel50 );
    aSel95 = aSel50 + exp( ln_aSelStep );
    g = log(19.)/( aSel95 - aSel50 );
  	sel = 1./( 1. + exp(-g*( age - aSel50 ) ) );
     //SURVEY
     aSelS50 = exp( ln_aSelS50 );
     aSelS95 = aSelS50 + exp( ln_aSelSStep );
     gS = log(19.)/( aSelS95 - aSelS50 );
     selS = 1./( 1. + exp(-gS*( age - aSelS50 ) ) );
}

void model_parameters::calcMaturity(void)
{
    g = log(19.)/( aMat95 - aMat50 );
  	mat = 1./( 1. + exp(-g*( age - aMat50 ) ) );    
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  //Outputs required for management procedure simulations
  // Get projected spawning and exploitable biomass for rep file output.
  projDynamics12();
  report << "## assessCA model parameter and state variable estimates" << endl;
  report << "## Model dimensions" << endl;
  report << "# nT" << endl;
  report << nT << endl;
  report << "# nAges" << endl;
  report << nAges << endl;
  report << endl;
  report << "## Parameter estimates" << endl;
  report << "# initN_mult" << endl;
  report << exp(log_initN_mult) << endl;
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
  report << "# M" << endl;
  report << Mt(nT) << endl;
  report << "# aSel50" << endl;
  report << aSel50 << endl;
  report << "# aSel95" << endl;
  report << aSel95 << endl;
  report << "# aSelS50" << endl;
  report << aSelS50 << endl;
  report << "# aSelS95" << endl;
  report << aSelS95 << endl;
  report << "# omega" << endl;
  report << omega << endl;
  report << "# Rt" << endl;
  report << column( numbers,1 ) << endl;
  report << "# Mt" << endl;
  report << Mt << endl;
  report << "# kappa" << endl;
  report << sqrt(kappaSq) << endl;  
  report << "# tau" << endl;
  report << tau << endl;
  report << "# sigmaR" << endl;
  report << sigmaR << endl;
  report << "# q" << endl;
  report << exp( lnq(2) ) << endl;
  report << "# q1" << endl;
  report << exp( lnq(1) ) << endl;
  report << "# q2" << endl;
  report << exp( lnq(2) ) << endl;
  report << endl;
  report << "## Derived variables" << endl;
  report << "# spawnBt" << endl;
  report << spawnBiomass(1,nT) << endl;
  report << "# exploitBt" << endl;
  report << exploitBiomass(1,nT) << endl;
   report << "# exploitBtS" << endl;
  report << exploitBiomassS(1,nT) << endl;
  report << "# Ft" << endl;
  report << Ft << endl;
  report << "# D" << endl;
  report << spawnBiomass[nT]/spawnBiomass[1] << endl;
  report << "# projSpawnBio" << endl;
  report << projSpawnBio << endl;
  report << "# projExpBio" << endl;
  report << projExpBio << endl;
   report << "# projExpBioS" << endl;
  report << projExpBioS << endl;
  report << "# trendBio" << endl;
  report << log(spawnBiomass[nT]/spawnBiomass[nT-trendYrs+1])/(nT-trendYrs+1) << endl;
  report << endl;
  report << "## Priors" << endl;
  report << "# pmSteepness" << endl;
  report << pmSteepness << endl;
  report << "# psdSteepness" << endl;
  report << psdSteepness << endl;
  report << "# pmM" << endl;
  report << pmM << endl;
  report << "# psdM" << endl;
  report << psdM << endl;
  report << endl;
  report << "## Minimization properties" << endl;
  report << "# eiv_nll" << endl;
  int nRecs = omega.indexmax() - omega.indexmin() + 1;
  report << (validObs + nRecs -1.)*log( kappaSq ) << endl;
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
  report << Index(1,qChangeTime-1)/exp( lnq(1) ) << 
            Index(qChangeTime,nT)/exp( lnq(2) ) << endl;
  report << "# obsPropAge" << endl;
  report << obsPropAge << endl;
  report << "# predPropAge" << endl;
  report << predPropAge << endl;
  report << "# obsPropAgeS" << endl;
  report << obsPropAgeS << endl;
  report << "# predPropAgeS" << endl;
  report << predPropAgeS << endl;
  report << "# biomass" << endl;
  report << biomass << endl;
  report << "# numbers" << endl;
  report << numbers << endl;
}

void model_parameters::preliminary_calculations(void){
#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
  arrmblsize = 20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(25000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(10000000);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
