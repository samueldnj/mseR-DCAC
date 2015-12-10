  #include <admodel.h>
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <refptsDLL2.htp>

model_data::model_data(int argc,char * argv[],dll_args& ad_dll) : ad_comm(argc,argv)
{
  nAges.allocate(ad_dll.nAges,"nAges");
  pars.allocate(ad_dll.pars,1,23,"pars");
}

model_parameters::model_parameters(int sz,int argc,char * argv[], dll_args& ad_dll) : 
 model_data(argc,argv,ad_dll) , function_minimizer(sz)
{
  initializationfunction();
  FMSY.allocate(ad_dll.FMSY,0.001,3.,"FMSY");
  BMSY.allocate(ad_dll.BMSY,"BMSY");
  MSY.allocate(ad_dll.MSY,"MSY");
  logY.allocate("logY");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
}

void model_parameters::userfunction(void)
{
  logY =0.0;
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
}

void model_parameters::set_runtime(void)
{
  dvector temp("{0.001}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
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

void model_parameters::report(void){}

void model_parameters::final_calcs(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;
extern "C" {

#if !defined(__MSVC32__)
#  define __declspec(x)
#endif

#if !defined(__BORLANDC__)
#  define _export
#else
#  define _export __stdcall
#endif

__declspec(dllexport) void _export refptsDLL2(int *_nAges,double *_pars,double *_FMSY,double *_BMSY,double *_MSY,char ** dll_options)
{
  arrmblsize = 20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(25000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
  int argc=1;
  try {
    char **argv=parse_dll_options("refptsDLL2",argc,*dll_options);
    do_dll_housekeeping(argc,argv);
    dll_args ad_dll(_nAges,_pars,_FMSY,_BMSY,_MSY);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv,ad_dll);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    ad_make_code_reentrant();
  }
  catch (spdll_exception spe){ 
    if (ad_printf && spe.e) (*ad_printf) ("abnormal exit from newtest\n");
  }
}
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
