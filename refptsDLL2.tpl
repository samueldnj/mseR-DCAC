DATA_SECTION
  // read one line of mcmc output from assessCA
  //init_adstring dataFileName; 
  dll_init_int nAges;
  dll_init_vector pars(1,23);

PARAMETER_SECTION
  dll_init_bounded_number FMSY(0.001,3.);
  dll_number BMSY;
  dll_number MSY;  
  objective_function_value logY;

GLOBALS_SECTION
  #include <admodel.h>

PROCEDURE_SECTION
  dvariable rec_a = pars(4);
  dvariable rec_b = pars(5);
  dvariable M = pars(6);
  dvariable aSel50 = pars(7);
  dvariable aSel95 = pars(8);
  dvariable lnsigmaR = pars(9);
  dvariable rho = pars(10);
  dvariable aMat50 = pars(11);
  dvariable aMat95 = pars(12);
  dvariable Linf = pars(13);
  dvariable L1 = pars(14);
  dvariable vonK = pars(15);
  dvariable c1 = pars(16);
  dvariable c2 = pars(17);
  dvariable sigmaL = pars(18);
  dvariable tau = pars(19);
  dvariable spawnBiomass = pars(20);
  dvariable projSpawnBio = pars(21);
  dvariable projExpBio = pars(22);
  dvariable termDep = pars(23);
  
  // derived quantities - not returned
  dvariable Rstar;  // equil recruitment
  dvariable phiY;   // equil yield-per-recruit
  dvariable phiS;   // equil spawnBio-per-recruit
  dvariable S;      // annual survival rate
  dvar_vector age(1,nAges); // age vector
  dvar_vector la(1,nAges);  // survivorship-to-age
  dvar_vector ma(1,nAges);  // maturity-at-age
  dvar_vector sa(1,nAges);  // selectivity-at-age
  dvar_vector wa(1,nAges);  // weight-at-age
  dvar_vector len(1,nAges); // length-at-age

  age.fill_seqadd(1,1);
  len = Linf + (L1-Linf)*exp(-vonK*(age-1.));
  wa = c1*pow( len, c2 );
  sa = 1./( 1. + exp(-(log(19.)/( aSel95 - aSel50 ))*( age - aSel50 ) ) );
  ma = 1./( 1. + exp(-(log(19.)/( aMat95 - aMat50 ))*( age - aMat50 ) ) );    

  // Calculate equilibrium survivorship as function of FMSY
  la(1) = 1.0;
  for ( int a=2; a<nAges; a++ )
    la(a) = la(a-1) * exp(-M - sa(a-1)*FMSY);
  la(nAges)   = la(nAges-1)*exp(-M-sa(nAges-1)*FMSY)/(1.-exp(-M-sa(nAges)*FMSY));

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

RUNTIME_SECTION
  convergence_criteria 0.001
TOP_OF_MAIN_SECTION
  arrmblsize = 20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(25000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
















