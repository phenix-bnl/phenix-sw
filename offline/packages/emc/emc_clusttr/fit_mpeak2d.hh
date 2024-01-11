//==================================================================
#ifndef _fit_mpeak2d_hh_
#define _fit_mpeak2d_hh_
//==================================================================

#include <TH1.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TH2.h>

TGraphErrors* fit_mpeak2d(TH2F* h2,TH2F* h2s,int bin,int* bincut,char* addname="_gra",char* fitopt="");
			  
//==================================================================
#endif
//==================================================================

