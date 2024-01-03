//-----------------------------------------------------------------------------
//
// Generate white transverse-momentum distribution
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <cmath>
#include "InitializeExpPt.h"

TH1* InitializeExpPt(const double PtMin, const double PtMax,
    const double Mass, const double InvSlope)
{
  const int nbins = 1000;
  const double binwidth = (PtMax-PtMin)/nbins;
  TH1F * pthistogram = new TH1F("pt","pt",nbins,PtMin,PtMax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    double Pt = PtMin + (ibin-1)*binwidth + binwidth/2.0;
    double Mt = std::sqrt(Mass*Mass+Pt*Pt);
    double weight = Pt*std::exp(-1.0*Mt/InvSlope);
    pthistogram->AddBinContent(ibin,weight);
  }

  return pthistogram;
}
