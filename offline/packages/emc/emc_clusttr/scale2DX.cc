#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TSystem.h>
#include <TROOT.h>
#include <TFile.h>
#include <TDirectory.h>
#include <TKey.h>
#include <TTree.h>
#include <TBranch.h>
#include <TNtuple.h>
#include <TCanvas.h>
#include <TPad.h>
#include <TStyle.h>
#include <TPostScript.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#endif

#include "scale2DX.hh"


TH2F* scale2DX(TH2F* h2,char* opt,int minnumcut,int minscale){
  int bin;
  float cont;
  float scale;
  int xbin,xbin1,xbin2;
  int ybin,ybin1,ybin2;
  xbin1 = h2->GetXaxis()->GetFirst();
  xbin2 = h2->GetXaxis()->GetLast();
  char* opt_cut;
  opt_cut = strstr(opt,"cut");
  if( opt_cut )
    cout<<" scale2DX:: Option cut "<<endl;

  for( xbin = xbin1; xbin <= xbin2 ; xbin++ ){
    ybin1 = h2->GetYaxis()->GetFirst();
    ybin2 = h2->GetYaxis()->GetLast();
    scale = 0;
    if( opt_cut )
      for( ybin = ybin1; ybin <= ybin2 ; ybin++ ){
	bin = h2->GetBin(xbin,ybin);
	cont = h2->GetBinContent(bin);
	if( cont > minnumcut )
	  scale += cont;
      }
    else
      scale = h2->Integral(xbin,xbin,ybin1,ybin2);
    if( scale != 0 && scale > minscale )
      scale = 1./fabs(scale);
    else
      scale = 0.;
    for( ybin = ybin1; ybin <= ybin2 ; ybin++ ){
      bin = h2->GetBin(xbin,ybin);
      cont = h2->GetBinContent(bin);
      h2->SetBinContent(bin, cont * scale );
    }
  }
  return h2;
};
//=========================================================================

