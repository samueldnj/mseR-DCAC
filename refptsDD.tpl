DATA_SECTION
  // read one line of mcmc output from assessDD
  init_adstring dataFileName; 
  init_int nAges;
  init_number B0;
  init_number R0;
  init_number rSteepness;
  init_number rec_a;
  init_number rec_b;
  init_number M;
  init_number alpha_g;
  init_number rho_g;
  init_number wk;

PARAMETER_SECTION
  // FMSY
  init_bounded_number FMSY(0.001,1);
  
  objective_function_value logY;
  
  // derived quantities
  //number U;
  number Rstar;  // equil recruitment
  number phiY;   // equil yield-per-recruit
  number phiS;   // equil spawnBio-per-recruit
  
  number BMSY;
  number MSY;
  
  // life history schedules 
  number se;            // eqm survival rate
  number be;           // eqm biomass
  number we;          // eqm weight
  
GLOBALS_SECTION
  #include <admodel.h>

PROCEDURE_SECTION
  calcYield();

RUNTIME_SECTION
  convergence_criteria 0.001
  
FUNCTION calcYield

  // Calculate equilibrium survivorship as function of FMSY
 se = mfexp(-M -FMSY);
 we = (alpha_g*se+wk *(1-se))/(1-rho_g*se);
 be = -1*((-we + se*alpha_g + se*rho_g*we + wk*rec_a*we)/(rec_b*(-we + se*alpha_g + se*rho_g*we))); //Martell
  
  // Calculate equilibrium yield
  MSY   = (FMSY/(M+FMSY))*(1-mfexp(-M -FMSY))*be;
  logY  = (-1.)*log(MSY);
  BMSY = be;
 
REPORT_SECTION
  report << BMSY <<" "<< MSY <<" "<< FMSY <<endl;
  ofstream repFile(dataFileName);
    repFile << BMSY <<" "<< MSY <<" "<< FMSY <<endl;
  //adstring copyRep2Ran = "cp refpts.rep " + dataFileName;
  //system(copyRep2Ran);
















