  #include <admodel.h>
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <refptsca.htp>

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
  aSel50.allocate("aSel50");
  aSel95.allocate("aSel95");
  lnsigmaR.allocate("lnsigmaR");
  rho.allocate("rho");
  aMat50.allocate("aMat50");
  aMat95.allocate("aMat95");
  Linf.allocate("Linf");
  L1.allocate("L1");
  vonK.allocate("vonK");
  c1.allocate("c1");
  c2.allocate("c2");
  sigmaL.allocate("sigmaL");
  tau.allocate("tau");
  spawnBiomass.allocate("spawnBiomass");
  eBio.allocate("eBio");
  termDep.allocate("termDep");
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  FMSY.allocate(0.001,3.,"FMSY");
  logY.allocate("logY");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
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
  age.allocate(1,nAges,"age");
  #ifndef NO_AD_INITIALIZE
    age.initialize();
  #endif
  S.allocate("S");
  #ifndef NO_AD_INITIALIZE
  S.initialize();
  #endif
  la.allocate(1,nAges,"la");
  #ifndef NO_AD_INITIALIZE
    la.initialize();
  #endif
  ma.allocate(1,nAges,"ma");
  #ifndef NO_AD_INITIALIZE
    ma.initialize();
  #endif
  sa.allocate(1,nAges,"sa");
  #ifndef NO_AD_INITIALIZE
    sa.initialize();
  #endif
  wa.allocate(1,nAges,"wa");
  #ifndef NO_AD_INITIALIZE
    wa.initialize();
  #endif
  len.allocate(1,nAges,"len");
  #ifndef NO_AD_INITIALIZE
    len.initialize();
  #endif
}

void model_parameters::userfunction(void)
{
  logY =0.0;
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
  report << BMSY <<" " << MSY << " " << FMSY << endl;
  // This sends output to the data file used as input, to preserve a unique name.
  ofstream repFile(dataFileName);
  repFile << BMSY << " " << MSY << " " << FMSY << endl;
  //adstring copyRep2Ran = "cp refptsca.rep " + dataFileName;
  //system(copyRep2Ran);
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
