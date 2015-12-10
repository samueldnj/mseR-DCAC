  #include <admodel.h>
#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <refptsDD.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  dataFileName.allocate("dataFileName");
  nAges.allocate("nAges");
  B0.allocate("B0");
  R0.allocate("R0");
  rSteepness.allocate("rSteepness");
  rec_a.allocate("rec_a");
  rec_b.allocate("rec_b");
  M.allocate("M");
  alpha_g.allocate("alpha_g");
  rho_g.allocate("rho_g");
  wk.allocate("wk");
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  FMSY.allocate(0.001,1,"FMSY");
  logY.allocate("logY");
  Rstar.allocate("Rstar");
  #ifndef NO_AD_INITIALIZE
  Rstar.initialize();
  #endif
  phiY.allocate("phiY");
  #ifndef NO_AD_INITIALIZE
  phiY.initialize();
  #endif
  phiS.allocate("phiS");
  #ifndef NO_AD_INITIALIZE
  phiS.initialize();
  #endif
  BMSY.allocate("BMSY");
  #ifndef NO_AD_INITIALIZE
  BMSY.initialize();
  #endif
  MSY.allocate("MSY");
  #ifndef NO_AD_INITIALIZE
  MSY.initialize();
  #endif
  se.allocate("se");
  #ifndef NO_AD_INITIALIZE
  se.initialize();
  #endif
  be.allocate("be");
  #ifndef NO_AD_INITIALIZE
  be.initialize();
  #endif
  we.allocate("we");
  #ifndef NO_AD_INITIALIZE
  we.initialize();
  #endif
}

void model_parameters::userfunction(void)
{
  calcYield();
}

void model_parameters::set_runtime(void)
{
  dvector temp("{0.001}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
}

void model_parameters::calcYield(void)
{
  // Calculate equilibrium survivorship as function of FMSY
 se = mfexp(-M -FMSY);
 we = (alpha_g*se+wk *(1-se))/(1-rho_g*se);
 be = -1*((-we + se*alpha_g + se*rho_g*we + wk*rec_a*we)/(rec_b*(-we + se*alpha_g + se*rho_g*we))); //Martell
  // Calculate equilibrium yield
  MSY   = (FMSY/(M+FMSY))*(1-mfexp(-M -FMSY))*be;
  logY  = (-1.)*log(MSY);
  BMSY = be;
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
  report << BMSY <<" "<< MSY <<" "<< FMSY <<endl;
  ofstream repFile(dataFileName);
    repFile << BMSY <<" "<< MSY <<" "<< FMSY <<endl;
  //adstring copyRep2Ran = "cp refpts.rep " + dataFileName;
  //system(copyRep2Ran);
}

void model_parameters::preliminary_calculations(void){
  admaster_slave_variable_interface(*this);
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

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
