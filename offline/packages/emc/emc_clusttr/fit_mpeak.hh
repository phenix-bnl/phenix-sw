//==================================================================
#ifndef _fit_mpeak_hh_
#define _fit_mpeak_hh_
//==================================================================

#include <TH1.h>
#include <TF1.h>

TF1* fit_mpeak(TH1* hist = 0,float gmin=0.22,float gmax=0.35,float emin=0.1,float emax=0.6,char* addname="_mip",int opt = 0,char* fitopt="");
#endif
//==================================================================

