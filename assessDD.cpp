  // int mcHeader=0;
  // int mcTrials=0;
  // #include <fstream.h>
  // ofstream mcout("mcout.dat");
  // ARK (24-Nov-11) Required for VC 64-bit.
  #include <admodel.h>
  int mcHeader=0;
  int mcTrials=0;
  int mcCounter=0;
  //#include <fstream.h>
  ofstream mcout("mcout.dat");
#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <assessDD.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  nT.allocate("nT");
  nProjYrs.allocate("nProjYrs");
  trendYrs.allocate("trendYrs");
  rSeed.allocate("rSeed");
  ruleType.allocate("ruleType");
  Linf.allocate("Linf");
  L1.allocate("L1");
  vonK.allocate("vonK");
  c1.allocate("c1");
  c2.allocate("c2");
  sigmaL.allocate("sigmaL");
  kage.allocate("kage");
  alpha_g.allocate("alpha_g");
  rho_g.allocate("rho_g");
  wk.allocate("wk");
  pmSteepness.allocate("pmSteepness");
  psdSteepness.allocate("psdSteepness");
  pmM.allocate("pmM");
  psdM.allocate("psdM");
  katch.allocate(1,nT,"katch");
  indexType.allocate("indexType");
  Index.allocate(1,nT,"Index");
  sigC.allocate("sigC");
  unfished.allocate("unfished");
  nQlevels.allocate("nQlevels");
  Qlevel.allocate(1,nQlevels,"Qlevel");
    ft_count=0;
     for(i=1;i<=nT;i++){
          if(katch(i)>0 ) ft_count++;
      }
    cout<<"ft_count\n"<<ft_count<<endl;
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  f.allocate("f");
  logit_ySteepness.allocate(1,"logit_ySteepness");
  ln_B0.allocate(1.,6.,"ln_B0");
  ln_M.allocate(2,"ln_M");
  rho.allocate(-1,"rho");
  omega.allocate(2,nT,-15.,15.,3,"omega");
  ln_Ft.allocate(1,ft_count,-30.,3.0,1,"ln_Ft");
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
  rec_a.allocate("rec_a");
  #ifndef NO_AD_INITIALIZE
  rec_a.initialize();
  #endif
  rec_b.allocate("rec_b");
  #ifndef NO_AD_INITIALIZE
  rec_b.initialize();
  #endif
  R0.allocate("R0");
  #ifndef NO_AD_INITIALIZE
  R0.initialize();
  #endif
  N0.allocate("N0");
  #ifndef NO_AD_INITIALIZE
  N0.initialize();
  #endif
  tau.allocate("tau");
  #ifndef NO_AD_INITIALIZE
  tau.initialize();
  #endif
  sigmaR.allocate("sigmaR");
  #ifndef NO_AD_INITIALIZE
  sigmaR.initialize();
  #endif
  snat.allocate("snat");
  #ifndef NO_AD_INITIALIZE
  snat.initialize();
  #endif
  wbar.allocate("wbar");
  #ifndef NO_AD_INITIALIZE
  wbar.initialize();
  #endif
  surv.allocate(1,nT,"surv");
  #ifndef NO_AD_INITIALIZE
    surv.initialize();
  #endif
  rec.allocate(1,nT,"rec");
  #ifndef NO_AD_INITIALIZE
    rec.initialize();
  #endif
  meanwt.allocate(1,nT,"meanwt");
  #ifndef NO_AD_INITIALIZE
    meanwt.initialize();
  #endif
  g.allocate("g");
  #ifndef NO_AD_INITIALIZE
  g.initialize();
  #endif
  kk.allocate("kk");
  #ifndef NO_AD_INITIALIZE
  kk.initialize();
  #endif
  numbers.allocate(1,nT,"numbers");
  #ifndef NO_AD_INITIALIZE
    numbers.initialize();
  #endif
  biomass.allocate(1,nT,"biomass");
  #ifndef NO_AD_INITIALIZE
    biomass.initialize();
  #endif
  exploitBiomass.allocate(1,nT,"exploitBiomass");
  #ifndef NO_AD_INITIALIZE
    exploitBiomass.initialize();
  #endif
  spawnBiomass.allocate(1,nT+nProjYrs,"spawnBiomass");
  #ifndef NO_AD_INITIALIZE
    spawnBiomass.initialize();
  #endif
  Ft.allocate(1,nT,"Ft");
  #ifndef NO_AD_INITIALIZE
    Ft.initialize();
  #endif
  Ct.allocate(1,nT,"Ct");
  #ifndef NO_AD_INITIALIZE
    Ct.initialize();
  #endif
  Ct_resid.allocate(1,nT,"Ct_resid");
  #ifndef NO_AD_INITIALIZE
    Ct_resid.initialize();
  #endif
  Ft_resid.allocate("Ft_resid");
  #ifndef NO_AD_INITIALIZE
  Ft_resid.initialize();
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
  kappaSq.allocate("kappaSq");
  #ifndef NO_AD_INITIALIZE
  kappaSq.initialize();
  #endif
  lnq.allocate("lnq");
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
  Z1.allocate("Z1");
  #ifndef NO_AD_INITIALIZE
  Z1.initialize();
  #endif
  Z2.allocate("Z2");
  #ifndef NO_AD_INITIALIZE
  Z2.initialize();
  #endif
  Ct_like.allocate("Ct_like");
  #ifndef NO_AD_INITIALIZE
  Ct_like.initialize();
  #endif
  pF.allocate("pF");
  #ifndef NO_AD_INITIALIZE
  pF.initialize();
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
  f=0.;
  calcRecPars();
  getF();
  popInit();
  popDynamics();
    calcIndexLikelihood();
  calcRecPrior();
  calcMPrior();
  // Length of recruitment series (process error)
  int nRecs = omega.indexmax() - omega.indexmin() + 1;
  // Estimate of total variance index (rho) + recs (1-rho)
  kappaSq = (Z1/rho + Z2/(1.-rho))/( validObs + nRecs -1.);
  // Estimated log-rec devs standard error
  sigmaR = sqrt( (1.-rho)*kappaSq );
  // Estimated obs error standard error
  tau = sqrt( rho*kappaSq );
  // Negative-log-posterior components ---------------------------------------//
  // EIV likelihood
  dvariable l_IR = 0.5*(validObs + nRecs -1.)*log( kappaSq ); // EIV likelihood
  // IG(alpha,beta) variance prior
  dvariable alpha = 1.;
  dvariable beta  = 1.;
  dvariable l_kap = alpha*log(beta)-(alpha+1)*log(kappaSq)-gammln(alpha)-beta/kappaSq;
  // steepness prior
  dvariable l_h  = steepness_nlp;                             
  // M prior
  dvariable l_M  = M_nlp;                                     
  f = l_IR + l_h + l_M + (-1.)*l_kap+ Ct_like;// ;// ;+ pF
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
  if(mceval_phase() )
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

void model_parameters::calcIndexLikelihood(void)
{
	// Likelihood assumes known variance (tau^2).
	// Catchability MLE determined for each series unless indexType=0 for 
	// absolute in which case force q=1.
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
	Z1=0.;
	for ( j=1;j<=nT;j++ )
	{
	  if ( Index(j) > 0.0 )
	  {
	  	Z1 += pow( (z(j) - lnq), 2. );
	  }
	}
	// Z1 is ready for inclusion in total likelihood
	double pi=3.141593;
	//Calc catch likelihood ONLY DO THIS IF DRIVING WITH ESTIMATED F
	    	// sigC=0.1; //ARK DELETE THIS WHEN SIGC ON GUI!!!
	    	dvariable like;
	  	Ct_like=0.;
	  	for(i=1;i<=nT;i++)
		{
			if(katch(i)>0.){	
				Ct_resid(i) = log(katch(i))-log(Ct(i)+1.e-20);
				like=square(Ct_resid(i))/(2.*square(sigC)) + log(sigC) + (0.5*log(2.*pi));
				Ct_like+=like;}
		}
 	//penalty on mean F
 	/*
 	double meanF=0.1; //move these pars to data file
 	double sigF2=5.;
 	double sigF1=1.;
 	//dvar_vector log_ft(1,nT); log_ft.initialize();
 	//for(i=1;i<=nT;i++){
 	//	if(Ft(i)>0) log_ft(i)=log(Ft(i));
 	//}
 	dvariable log_fbar = mean(ln_Ft);
 	if(last_phase())
 	{	
 		Ft_resid=log_fbar-log(meanF);
 		pF = square(Ft_resid)/(2.*square(sigF2)) + log(sigF2) + (0.5*log(2.*pi));//dnorm(log_fbar,log(meanF),sigF2);
 	}
 	else
 	{
 		Ft_resid=log_fbar-log(meanF);
 		pF = square(Ft_resid)/(2.*square(sigF1)) + log(sigF1) + (0.5*log(2.*pi));//dnorm(log_fbar,log(meanF),sigF1);
 	}
 	*/
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

void model_parameters::getF(void)
{
	Ct.initialize();
	Ft.initialize();
	int i=1;
	int t;
	for(t=1;t<=nT;t++)
	{	
		if(katch(t)>0) {
			Ft(t) = mfexp(ln_Ft(i));
			i++;
		}	
	}   
}

void model_parameters::popDynamics(void)
{
  int t;
  // Could add 1 year to this loop (and 1 year to each vector)to get 1-year
  // ahead projection and std error. Right now, only get std error via MCMC.
  for ( t=2;t<=nT;t++ ) 
  {
    // Update age-1 recruits
	  if (t <= kage) rec(t) = rec(1);
	  else rec(t) = rec_a*spawnBiomass(t-kage)/( 1. + rec_b*spawnBiomass(t-kage) );
    if(active(omega)) rec(t) = rec(t)*mfexp( omega(t));  
    // Update biomass and numbers	
	biomass(t) = surv(t-1)*(rho_g*biomass(t-1)+alpha_g*numbers(t-1))+wk*rec(t);	  
	 numbers(t)=surv(t-1)*numbers(t-1)+rec(t);
    // Calculate predicted weight in dynamics, possible option to fit to it.
	  meanwt(t)=biomass(t)/numbers(t);
    // Exploitable and Spawning biomass - same as biomass - preserve these objects
    exploitBiomass(t) = biomass(t);
    spawnBiomass(t) = biomass(t);
     Ct(t)=biomass(t)*(1-mfexp(-M-Ft(t)))*(Ft(t)/(Ft(t)+M));
     surv(t) =mfexp(-M-Ft(t));
   /*
   	cout << "t Catch, Biomass" << endl;
   	cout << t <<endl << katch(t) <<endl << biomass(t) << endl; 
   	cout << "Ft, M, surv, rec, biomass, numbers" << endl;
		cout << Ft(t) << endl;
		cout << M << endl;
		cout << surv(t) << endl;
		cout << rec(t) << endl;
		cout << biomass(t) << endl;
		cout << numbers(t) << endl;
    exit(1);
		*/
  }
  termDep = spawnBiomass(nT)/B0;
}

void model_parameters::projDynamics3(void)
{
  // Project population forward nProjYrs for range of
  // quota options Qlevels (1...nQlevels)
  // run projection during last_phase()
  dvector omegaProj(1,nProjYrs);
  random_number_generator rng(rSeed);
  omegaProj.fill_randn(rng);
  int pt;
  // transfer all estimated parameters to dvariables, dvectors, etc.
  // e.g., projVar = value(estVar)
  //double newN; double newB; double newR; double newW; double newS;
  double oldN; double oldB; double oldR; double oldW; double oldS; 
  oldN = value(numbers(nT));
  oldB = value(biomass(nT));
  oldW = value(meanwt(nT));
  oldS = value(surv(nT));
  double eBio = value( exploitBiomass(nT));
  double sBio = value( spawnBiomass(nT)); //this spawning biomass is for tracking stock status
  // re-derive recruitment model parameters
  double prj_B0  = value(exp(ln_B0));
  double prj_ySteepness = value(exp(logit_ySteepness)/(1.+ exp(logit_ySteepness)));
  double prj_rSteepness = (prj_ySteepness+0.30)/1.30;
  //DD pars - may or may not update the first three in projections
  double prj_alpha_g = alpha_g;
  double prj_rho_g = rho_g;
  double prj_wk = wk;
  double prj_M  = value(M);
  double prj_snat      = mfexp(-prj_M);
  //FOR NOW ASSUME THAT ALPHA_G RHO_G AND WK ARE FIXED
  double prj_wbar=(prj_snat*prj_alpha_g+prj_wk*(1-prj_snat))/(1-prj_rho_g*prj_snat);
  double prj_N0 = prj_B0/prj_wbar;
  double prj_R0 =  (1-prj_snat)*prj_N0; //H&W 1992 p339
  // Beverton-Holt a/b parameters.
  double prj_a      = 4.*prj_rSteepness*prj_R0/(prj_B0*(1.-prj_rSteepness));
  double prj_b      = (5.*prj_rSteepness-1.)/(prj_B0*(1.-prj_rSteepness));    
  double prj_sigmaR = value( sigmaR);
  //vectors for dynamics
  dvector prj_biomass(1,nProjYrs);
  dvector prj_numbers(1,nProjYrs);
  dvector prj_rec(1,nProjYrs);
  dvector prj_surv(1,nProjYrs);
  dvector prj_Ft(1,nProjYrs);
  // Fill another spawning biomass vector lagB with kage-1 values before nT for
  // the recruitment function so that recruitment lag is accounted for, would
  // not normally need to do this, except that we are not appending to the old
  // spawning biomass in projections so biomasses before nT are lost.
  // Will not actually use the last kage values but saves using an if statement
  // when filling the rest of lagB in loop below.
	dvector lagB(1,(nProjYrs+kage));  
	lagB.initialize();
	int kcount=1;
	for(int ii=1; ii<=kage; ii++)
  {
	  //cout<<kage<<endl<<kcount<<endl;
		lagB(ii) = value(spawnBiomass(nT-kage+kcount));
		kcount++; 
	}
	//cout<<"Pre-loop lagB"<<endl;
	//cout<<lagB<<endl;
  //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@//
  for( int j=1; j<=nQlevels; j++ )
  {
    int Declined = 0;
    for ( int t=1; t<=nProjYrs; t++ )
    {
			//cout<<"proj year = "<<t<<endl;
			// Update age-1 recruits, note that lagB(1) is actually biomass(nT-kage+1),
      // i.e., the lag is already built into this vector.
      // e.g., if kage=2, this year's recruitment depends on the year before nT).
			prj_rec(t) = prj_a*lagB(t)/( 1. + prj_b*lagB(t)); //do not change the index in lagB
			prj_rec(t) = prj_rec(t)*mfexp(prj_sigmaR*omegaProj(t));
			// Update biomass and numbers
		  if(t==1)
      {
     		prj_biomass(t)=oldS*(prj_rho_g*oldB+prj_alpha_g*oldN)+prj_wk*prj_rec(t); 
     		prj_numbers(t)=oldS*oldN+prj_rec(t);
     		prj_Ft(t) = -log(1.- Qlevel(j)/eBio); // Fishing mortality: approximation - use in surv(t)
				//prj_surv(t) = exp(-prj_M - prj_Ft(t)); // Update for next proj year
 				prj_surv(t) = exp(-prj_M)*(1.- Qlevel(j)/prj_biomass(t));
     	}
      else
      {     
     		prj_biomass(t)=prj_surv(t-1)*(rho_g*prj_biomass(t-1)+alpha_g*prj_numbers(t-1))+prj_wk*prj_rec(t); 
     		prj_numbers(t)=prj_surv(t-1)*prj_numbers(t-1)+prj_rec(t);
    		//prj_Ft(t) = -log(1.- Qlevel(j)/prj_biomass(t)); // Fishing mortality: approximation - use in surv(t)
 				prj_surv(t) = exp(-prj_M)*(1.- Qlevel(j)/prj_biomass(t)); // Update for next proj year
      } //end ifelse
      meanwt(t)=prj_biomass(t)/prj_numbers(t);
    	//for testing against spreadsheet DelayDifferenceTest.xls - checks out for 10 years with fixed pars and tiny rec devs (RF April 23 2013)
	   	/*
	   	cout<<prj_rec(t)<<endl;
	   	cout<<prj_surv(t)<<endl;
 			cout<<prj_numbers(t)<<endl;
 			cout<<prj_biomass(t)<<endl<<endl;
 			*/
      // Update the lagB vector kage years into the future for use in SR function
      // in kage years.  This results in a spawning biomass vector that has the
      // recruitment lag built into it.
      lagB(t+kage) = prj_biomass(t); 
      //cout<<"In-loop lagB"<<endl;
      //cout<<lagB<<endl;
			// Exploitable biomass 
			eBio =  prj_biomass(t);
			if( t==1 ) projExpBio = eBio;
			// Spawning biomass for stock status
			sBio = prj_biomass(t);
			if ( t==1)  projSpawnBio = sBio;
	  } // projYrs loop
	  /*
	  cout<<endl<<"nprojyrs, projyr, catch,  lagged biomass and projected biomass"<<endl;
	  cout<<nProjYrs<<endl;
	  cout<<Qlevel<<endl;
    cout<<lagB<<endl;
	  cout<<prj_surv<<endl;
    cout<<prj_biomass<<endl<<endl;
    */
		// Did the stock decline under this Q?
		if( sBio < value(spawnBiomass(nT)) ) Declined = 1;
		// Write Q and Declined to file
		// Write to mcout.dat file
		// ARK (24-Nov-10) Required for 64-bit VC compiler.
		//ofstream mcout("mcout.dat", ios::app );
 		if( mcHeader== 0 )
		{
			// Output Header for the leading model parameters needed for reference point
      // calcs. This will create reduandant information for each quota level,
      // but will be filtered in R.
			mcout << "B0 R0 rSteepness rec.a rec.b M sigmaR rho ";
			mcout << "Linf L1 vonK c1 c2 sigmaL tau ";
			// Output needed for 9-zone feed forward harvest control rule.
			mcout << "Q Declined trendBio spawnBio projSpawnBio projExpBio termDep" << endl;
			// Output the values.
			mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
			mcout << " " << M << " " << sigmaR << " " << rho << " ";
			mcout << Linf << " " << L1  << " " << vonK << " ";
			mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
			mcout << Qlevel(j) << " " << Declined << " "
			      << log(spawnBiomass(nT)/spawnBiomass(nT-trendYrs+1))/(trendYrs-1) << " "
			      << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " " << termDep << endl; 
  		      //<< spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " " << termDep << " ";
 	          //for( pt=1; pt<=nProjYrs;pt++ ) 
		        //  mcout <<projYrsSSB(pt)<<" ";
       			//mcout <<" "<< endl;
			mcHeader = 1;
		}
		else
		{
			// Output the values...
			mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
			mcout << " " << M << " " << sigmaR << " " << rho << " ";
			mcout << Linf << " " << L1  << " " << vonK << " ";
			mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
			mcout << Qlevel(j) << " " << Declined << " "
			      << log(spawnBiomass(nT)/spawnBiomass(nT-trendYrs+1))/(trendYrs-1) << " "
			      << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " "
            << termDep << endl;
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
  // ruleType 1 or 2: run projection only 1 year ahead to get
  // projected exploitable biomass...used to get next year's quota.
  random_number_generator rng(rSeed);
  double omegaProj = randn(rng);
  // Transfer all estimated parameters to dvariables, dvectors, etc.
  // e.g., projVar = value(estVar)
  dvariable newN; dvariable newB; dvariable newR; dvariable newW; 
  dvariable oldN; dvariable oldB; dvariable oldR; dvariable oldW; 
  oldN = value(numbers(nT));
  oldB = value(biomass(nT));
  oldW = value(meanwt(nT));
  double sBio = value( spawnBiomass(nT)); //for stock status
  double lagBio = value( spawnBiomass(nT-kage+1)); //for calculating recruits
  double eBio = value( exploitBiomass(nT));
  // Re-derive recruitment model parameters
  double prj_B0         = value( exp(ln_B0));
  double prj_ySteepness = value( exp(logit_ySteepness)/(1.+exp(logit_ySteepness)) );
  double prj_rSteepness = (prj_ySteepness+0.30)/1.30;
  // May or may not update these in projections
  double prj_alpha_g = alpha_g;
  double prj_rho_g = rho_g;
  double prj_wk = wk;
  double prj_M    = value(M);
  double prj_snat = mfexp(-prj_M);
  //FOR NOW ASSUME THAT ALPHA_G RHO_G AND WK ARE FIXED
  // ARK: try prj_wbar = wbar
  double prj_wbar = (prj_snat*prj_alpha_g+prj_wk*(1.-prj_snat))/(1.-prj_rho_g*prj_snat);
  double prj_N0   = prj_B0/prj_wbar;
  double prj_R0   =  (1.-prj_snat)*prj_N0; //H&W 1992 p339
  // Beverton-Holt a/b parameters.
  // ARK: Check this matches SPC consistently.
  double prj_a      = 4.*prj_rSteepness*prj_R0/(prj_B0*(1.-prj_rSteepness));
  double prj_b      = (5.*prj_rSteepness-1.)/(prj_B0*(1.-prj_rSteepness));    
  double prj_sigmaR = value( sigmaR);
  // Update age-k recruits.
  //sBio is set above to be spawnBiomass(nT-kage) - check what this hcr is doing
  newR = prj_a*lagBio/( 1. + prj_b*lagBio );
  newR = newR*mfexp( prj_sigmaR*omegaProj );
  // Fishing mortality: approximation
  // double prj_Ft = -log(1.- (katch(nT)/eBio));
  double prj_surv = exp(-prj_M)*(1.- (katch(nT)/eBio));
  // Update biomass and numbers	
  newB = prj_surv*(prj_rho_g*oldB + prj_alpha_g*oldN) + prj_wk*newR;
  newN = prj_surv*oldN+newR;
  newW = newB/newN;
  // Exploitable biomass 
  eBio = value(newB);
  projExpBio = eBio;
  // Spawning biomass.
  sBio = value(newB);  //don't need to account for lag in recruitment here because this value doesn't get used for any more recruitment calculations
  projSpawnBio = sBio;
  cout << endl << "Final year catch and biomass and projected biomass" << endl;
  cout << katch(nT) << endl;
  cout << exploitBiomass(nT) << endl;
  cout << projSpawnBio << endl << endl;
  // Write to mcout.dat file
  // ARK (24-Nov-10) Required for 64-bit VC compiler.
  // ofstream mcout("mcout.dat", ios::app );    
  if( mcHeader== 0 )
  {
    // Output the leading model parameters needed for reference point calcs.
    mcout << "B0 R0 rSteepness rec.a rec.b M sigmaR rho ";
    mcout << "Linf L1 vonK c1 c2 sigmaL tau ";
    mcout << "rho_g alpha_g wk ";
    // Output needed for constant F or 3-zone feedback harvest control rule.
    mcout << "spawnBio projSpawnBio projExpBio termDep" << endl;
    // Output the values.
    mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
    mcout << " " << M << " " << sigmaR << " " << rho << " ";
    mcout << Linf << " " << L1  << " " << vonK << " ";
    mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
    mcout << rho_g << " " << alpha_g << " " << wk << " ";      
    mcout << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " " << termDep << endl; 
    mcHeader = 1;
  }
  else
  {
    // Output the values.
    mcout << B0 << " " << R0 << " " << rSteepness << " " << rec_a << " " << rec_b;
    mcout << " " << M << " " << sigmaR << " " << rho << " ";
    mcout << Linf << " " << L1  << " " << vonK << " ";
    mcout << c1 << " " << c2 << " " << sigmaL << " " << tau << " ";
    mcout << rho_g << " " << alpha_g << " " << wk << " ";      
    mcout << spawnBiomass(nT) << " " << projSpawnBio << " " << projExpBio << " " << termDep << endl; 
  }          
}

void model_parameters::popInit(void)
{
  // unfished=1; //ARK REMOVE THIS WHEN UNFISHED ON GUI  
  // Initialize state variables.
  numbers.initialize();
  biomass.initialize();
  spawnBiomass.initialize();
  exploitBiomass.initialize();
  surv(1) =mfexp(-M-Ft(1));
  if(unfished==1)
  {
	  numbers(1) = N0;
	  biomass(1) = B0;
	  meanwt(1)  = wbar;
	  // Retain these objects, spawning biomass usually assumed to be
	  // total biomass age >=k (H&W 1992, p332).
	  spawnBiomass(1)  = B0;
	  exploitBiomass(1)= B0;
  }
  else
  {
  	dvariable sfished = surv(1); //equilibrium survivorship at initial fishing mortality (gear 1 commercial fishery)
	  meanwt(1) = (sfished*alpha_g + wk*(1-sfished))/(1-rho_g*sfished);
	  biomass(1) = -(meanwt(1)*(wk*rec_a-1)+sfished*(alpha_g+rho_g* meanwt(1)))/(rec_b*(sfished*alpha_g+sfished*rho_g* meanwt(1)- meanwt(1)));
	  numbers(1) = biomass(1)/meanwt(1);
   	spawnBiomass(1) = biomass(1);
   	exploitBiomass(1) = biomass(1);
  }
  //recruitment and catch
  rec(1)= rec_a*spawnBiomass(1)/( 1. + rec_b*spawnBiomass(1));
  Ct(1)=biomass(1)*(1-mfexp(-M-Ft(1)))*(Ft(1)/(Ft(1)+M));
 //cout<<Ft(1)<<endl<<"  "<<exploitBiomass(1)<<endl<<"  "<<surv(1)<<endl<<"  "<<rec(1)<<endl; 
}

void model_parameters::calcRecPars(void)
{
  // Estimated parameters - unfished biomass and steepness.
  B0 = mfexp(ln_B0);
  M = mfexp(ln_M);
  // This transformation bounds steepness to 0.23-0.97
  ySteepness = exp(logit_ySteepness)/(1.+exp(logit_ySteepness));
  rSteepness = (ySteepness+0.30)/1.30;
  // DD initialisation
  // Equlibrium mean weight - obtained from weq = Beq/Neq and solving for weq
  // i.e., weq = [surv(alpha.Neq + rho.Beq + wk.R] / [surv.Neq + R]
  // with substitutions Neq = Beq/weq and R = Neq(1 - surv)
  // From SJDM, also used by Sinclair in 2005 p cod assessment.
  snat = mfexp(-M); //unfished survival rate
  wbar = (snat*alpha_g+wk*(1-snat))/(1-rho_g*snat);
  N0   = B0/wbar;
  //Unfished recruitment
  R0 = (1-snat)*N0; //H&W 1992 p339
  //Beverton-Holt a parameters
  rec_a = 4.*rSteepness*R0/(B0*(1.-rSteepness));
  rec_b = (5.*rSteepness-1.)/(B0*(1.-rSteepness));
}

void model_parameters::report()
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
  //projDynamics3(); 
  report << "## assessDD model parameter and state variable estimates" << endl;
  report << "## Model dimensions" << endl;
  report << "# nT" << endl;
  report << nT << endl;
  report << endl;
  report << "## Parameter estimates" << endl;
  report << "# logit.ySteepness" << endl;
  report << logit_ySteepness << endl;
  report << "# rSteepness" << endl;
  report << rSteepness << endl;
  report << "# ln_B0" << endl;
  report << ln_B0 << endl;
  report << "# M" << endl;
  report << M << endl;
  report << "# rho" << endl;
  report << rho << endl;
  report << "# omega" << endl;
  report << omega << endl;
  report << "# ln_Ft" << endl;
  report << ln_Ft << endl;
  report << "# rec.a" << endl;
  report << rec_a << endl;
  report << "# rec.b" << endl;
  report << rec_b << endl;
  report << "# B0" << endl;
  report << B0 << endl;
  report << "# R0" << endl;
  report << R0 << endl;
  report << "# N0" << endl;
  report << N0 << endl;
  report << "# Rt" << endl;
  report << rec << endl;
  report << "# SSQ_I_Z1" << endl;
  report << Z1 << endl;
  report << "# SSQ_R_Z2" << endl;
  report << Z2 << endl;
  report << "# kappa" << endl;
  report << sqrt(kappaSq) << endl;  
  report << "# tau" << endl;
  report << tau << endl;
  report << "# sigmaR" << endl;
  report << sigmaR << endl;
  report << "# q" << endl;
  report << exp( lnq ) << endl;
  report << endl;
  report << "## Derived variables" << endl;
  report << "# snat" << endl;
  report << snat << endl;
  report << "# wbar0" << endl;
  report << wbar << endl;
  report << "# surv" << endl;
  report << surv(1,nT) << endl;
  report << "# spawnBt" << endl;
  report << spawnBiomass(1,nT) << endl;
  report << "# exploitBt" << endl;
  report << exploitBiomass(1,nT) << endl;
  report << "# exploitBtS" << endl;		       //same as expBio but needed for plotting - match CAA
  report << exploitBiomass(1,nT) << endl;
  report << "# Ft" << endl;
  report << Ft << endl;
  report << "# D" << endl;
  report << spawnBiomass[nT]/spawnBiomass[1] << endl;
  report << "# projSpawnBio" << endl;
  report << projSpawnBio << endl;
  report << "# projExpBio" << endl;
  report << projExpBio << endl;
  report << "# projExpBioS" << endl; //same as expBio but needed for plotting - match CAA
  report << projExpBio << endl;
  report << "# trendBio" << endl;
  report << log(spawnBiomass[nT-trendYrs+1]/spawnBiomass[nT]) << endl;
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
  report << "# steepness_nlp" << endl;
  report << steepness_nlp << endl;
  report << "# M_nlp" << endl;
  report << M_nlp << endl;
   report << "# Ct_like" << endl;
  report << Ct_like << endl;
   report << "# pF" << endl;
  report << pF << endl;
  report << "# objFun" << endl;
  report << *objective_function_value::pobjfun << endl;
  report << "# maxGrad" << endl;
  report << objective_function_value::gmax << endl;
  report << "# iExit" << endl;
  report << iexit << endl;
  report << "# nEval" << endl;
  report << neval << endl;  
  report << endl;
  report << "## Input data" << endl;
  report << "# sigC" << endl;
    report << sigC << endl;
  report << "# katch" << endl;
  report << katch << endl;
  report << "# Ct" << endl;
    report << Ct << endl;
     report << "# Ct_resid" << endl;
  report << Ct_resid << endl;
  report << "# lastDt" << endl;
  report << katch(nT) << endl;
  report << "# ItScaled" << endl;
  report << Index/exp( lnq ) << endl;
  report << "# kage" << endl;
  report << kage << endl;
  report << "# rho_g" << endl;
  report << rho_g << endl;
  report << "# alpha_g" << endl;
  report << alpha_g << endl;
  report << "# wk" << endl;
  report << wk << endl;
  report << "# biomass" << endl;
  report << biomass << endl;
  report << "# numbers" << endl;
  report << numbers << endl;
}

void model_parameters::preliminary_calculations(void){
  admaster_slave_variable_interface(*this);
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
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
 
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
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
