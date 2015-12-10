DATA_SECTION
  // read one line of mcmc output from assessCA
  init_adstring dataFileName; 
  init_int nAges;
  init_number B0;
  init_number R0;
  init_number rSteepness;
  init_number rec_a;
  init_number rec_b;
  init_number M;
  init_number aSel50;
  init_number aSel95;
  init_number lnsigmaR;
  init_number rho;
  init_number aMat50;
  init_number aMat95;
  init_number Linf;
  init_number L1;
  init_number vonK;
  init_number c1;
  init_number c2;
  init_number sigmaL;
  init_number tau;
  init_number spawnBiomass;
  init_number eBio;
  init_number termDep;

PARAMETER_SECTION
  // FMSY
  init_bounded_number FMSY(0.001,3.);
  
  objective_function_value logY;
  
  // derived quantities
  //number U;
  number Rstar;  // equil recruitment
  number phiY;   // equil yield-per-recruit
  number phiS;   // equil spawnBio-per-recruit
  
  number BMSY;
  number MSY;
  
  // life history schedules 
  vector age(1,nAges); // age vector
  number S;            // annual survival rate
  vector la(1,nAges);  // survivorship-to-age
  vector ma(1,nAges);  // maturity-at-age
  vector sa(1,nAges);  // selectivity-at-age
  vector wa(1,nAges);  // weight-at-age
  vector len(1,nAges); // length-at-age

GLOBALS_SECTION
  #include <admodel.h>

PROCEDURE_SECTION
  calcYield();

RUNTIME_SECTION
  convergence_criteria 0.001
  
FUNCTION calcYield

  age.fill_seqadd(1,1);
  len = Linf + (L1-Linf)*exp(-vonK*(age-1.));
  wa = c1*pow( len, c2 );
  sa = 1./( 1. + exp(-(log(19.)/( aSel95 - aSel50 ))*( age - aSel50 ) ) );
  ma = 1./( 1. + exp(-(log(19.)/( aMat95 - aMat50 ))*( age - aMat50 ) ) );    
  
  // Calculate equilibrium survivorship as function of FMSY
  la(1) = 1.0;
  for ( int a=2; a<nAges; a++ )
    la(a) = la(a-1) * exp(-M - sa(a-1)*FMSY);
  la(nAges) = la(nAges-1)*exp(-M-sa(nAges-1)*FMSY)/(1.-exp(-M-sa(nAges)*FMSY));

  // phiS=SSBPR
  dvar_vector tmpPhi(1,nAges);
  tmpPhi = elem_prod( la, ma );
  tmpPhi = elem_prod( tmpPhi, wa );
  phiS   = sum(tmpPhi);

  // phiY = YPR
  tmpPhi = elem_prod( la, wa );
  tmpPhi = elem_prod( tmpPhi, sa );
  phiY   = sum(tmpPhi)*FMSY*(1.-exp(-M-FMSY))/(M+FMSY);

  // Calculate equilibrium recruitment
  Rstar = ( rec_a*phiS - 1. )/( rec_b*phiS );
  BMSY  = Rstar*phiS;
  MSY   = Rstar*phiY;
  logY  = (-1.)*log(MSY);
      
REPORT_SECTION
  report << BMSY <<" " << MSY << " " << FMSY << endl;
  
  // This sends output to the data file used as input, to preserve a unique name.
  ofstream repFile(dataFileName);
  repFile << BMSY << " " << MSY << " " << FMSY << endl;
  //adstring copyRep2Ran = "cp refptsca.rep " + dataFileName;
  //system(copyRep2Ran);
















