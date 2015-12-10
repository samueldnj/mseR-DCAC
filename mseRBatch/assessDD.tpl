//****************************************************************************//
// Module: assessDD.tpl
// Authors : R.E. Forrest, S.P. Cox, A.R. Kronlund, K.R. Holt
// Procedure: Delay difference model
//
// References: Hilborn and Walters 1992
//
// Date Revised:
// 26-Apr-10 Initial implementation by SPC.
// 24-Nov-11 Revised for Risk-Based MP paper
//           Implemented projections
// 05-Apr-13 RF modified assessCA.tpl for delay difference model
// 02-Jun-13 ARK modified MCMC out for symmmetry with PMOD and CAAMOD.

// TO DO: FIX projDynamics3
// UPDATE TREATMENT OF SIGMAR AND RHO TO MORE RECENT APPROACH IN ASSESSCA
//****************************************************************************//

DATA_SECTION
  // Total length of catch data.
  init_int nT;

 // Number of projection years...if proj==1
  init_int nProjYrs;
  
  // Number of years for recent trend est
  init_int trendYrs;

  // Random number seed
  init_int rSeed;
  
  // Type of rule information required
  init_int ruleType;
    
  // Fixed parameters and schedules.
  init_number Linf;
  init_number L1;
  init_number vonK;
  init_number c1;
  init_number c2;
  init_number sigmaL;
    
  //RF: NEED TO ADD THESE OBJECTS TO DATA FILE
  init_int kage; //age at knife-edge recruitment
  //init_vector wbar_obs(1,nT); //observed mean weight - fitting option
  init_number alpha_g;  //growth alpha (intercept of Ford-Walford plot; derived from wk and wk-1, H&W 1992, p 334)
  init_number rho_g;  //growth rho (slope of Ford-Walford plot; H&W 1992, p 332)
  init_number wk;
    
  // Prior distribution parameters
  init_number pmSteepness;
  init_number psdSteepness;
  init_number pmM;
  init_number psdM;

  // Vector of catch biomass values.
  init_vector katch(1,nT);

  // Input abundance index type.
  init_int indexType;
 
  // Input index data
  init_vector Index(1,nT);
  
  // Sigma C for catch fitting
  init_number sigC;
  
  // Initialize at unfished (1) or fished equilibrium (0)
  init_int unfished;

 // Quota options for forward projections
  init_int nQlevels;
  init_vector Qlevel(1,nQlevels);
  //!!cout<<Qlevel<<endl; 
  //!!exit(1);
  
  // number sigC; //ARK REMOVE THIS LINE WHEN SIGC IN GUI - REMOVE VALUE FROM INDEX LIKELIHOOD FUNCTION AS WELL
  // int unfished; //ARK REMOVE THIS LINE WHEN UNFISHED IN GUI - REMOVE VALUE FROM INIT FUNCTION AS WELL
  
	int ft_count;
	int i;
 LOC_CALCS
    ft_count=0;
     for(i=1;i<=nT;i++){
          if(katch(i)>0 ) ft_count++;
      }
    cout<<"ft_count\n"<<ft_count<<endl;
 END_CALCS

//*******************************************************************/
PARAMETER_SECTION
  objective_function_value f;
  // ARK: This puts a lower bound on steepness at about 0.5, upper bound 0.99.
  init_number logit_ySteepness(1);
  //init_bounded_number logit_ySteepness(0.05,5.,1);
  //init_bounded_number logit_ySteepness(-1.,5.,1);  
  // ARK: This was bounded by 5., 15. much too high...
  init_bounded_number ln_B0(1.,6.); //need to make these boundaries dynamic
  init_number ln_M(2); //2
  init_number rho(-1);
  init_bounded_vector omega(2,nT,-15.,15.,3);//3
  init_bounded_vector ln_Ft(1,ft_count,-30.,3.0,1);

  // Derived variables.
  number B0;
  number M;
  number ySteepness;
  number rSteepness;
  number rec_a;
  number rec_b;
  number R0;
  number N0;
  number tau;
  number sigmaR;
  
  //DD pars :: RF (04-Apr-2013)
  number snat; //unfished survival
  number wbar; //equil mean weight
  vector surv(1,nT); //survival
  vector rec(1,nT); //recruits
  vector meanwt(1,nT); //mean weight
    
  number g;
  number kk;
 
  // Derived state variables.
  vector numbers(1,nT);
  vector biomass(1,nT);
  vector exploitBiomass(1,nT);
  vector spawnBiomass(1,nT+nProjYrs);
  vector Ft(1,nT);
  vector Ct(1,nT); //predicted catch
   vector Ct_resid(1,nT);
  number Ft_resid;
  
  sdreport_number termDep;
  number projSpawnBio;
  number projExpBio;

  // Likelihood quantities for index.
  number kappaSq;
  number lnq;
  number ss;
  vector z(1,nT);
  number zSum;
  // Counts of actual non-missing data values
  number validObs;
  number pos_pen;

  number Z1;      // index sum-of-sqs
  number Z2;      // rec-dev sum-of-sqs
  number Ct_like; // objective function component for predicted catch
  number pF;      // penalty for mean F too high

  // Prior distribution quantities
  number muB;
  number tauB;
  number aB;
  number bB;
  number steepness_nlp;
  number M_nlp;
  
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
  int mcCounter=0;
  //#include <fstream.h>
  ofstream mcout("mcout.dat");

TOP_OF_MAIN_SECTION
  arrmblsize = 20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(25000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
 
//****************************************************************************//

PROCEDURE_SECTION
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
  
//****************************************************************************//
// Likelihood Function Components                                             //
//****************************************************************************//

FUNCTION calcIndexLikelihood
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

FUNCTION calcRecPrior
  // log-recruitment devs sum-of-squares
	Z2 = norm2( omega );
	// Z2 is now ready for inclusion in total likelihood

	// Beta prior on steepness
	muB = 1.25*pmSteepness-0.25;
	tauB = muB*(1.-muB)/( 1.5625*pow(psdSteepness,2) )-1.; 
	aB = tauB*muB; 
	bB = tauB*(1.-muB); 
  steepness_nlp = (-1.)*( (aB-1.)*log(ySteepness) + (bB-1.)*log(1.-ySteepness) );
    
FUNCTION calcMPrior
    M_nlp = 0.5*pow(M-pmM,2)/pow(psdM,2);
    
//****************************************************************************//
// Population dynamics functions                                              // 
//****************************************************************************//
FUNCTION getF
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


FUNCTION popDynamics
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
   
    		
FUNCTION projDynamics3
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

FUNCTION projDynamics12
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

//****************************************************************************//
// Initialization functions                                                   //
//****************************************************************************//

FUNCTION popInit
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
     

FUNCTION calcRecPars
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

//****************************************************************************//
REPORT_SECTION
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
