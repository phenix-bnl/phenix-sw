//-----------------------------------------------------------------------------
//
// Generate white transverse-momentum distribution
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include "InitializeWhitePt.h"

TH1F * InitializeWhitePt(const double PtMin, const double PtMax)
{
  const int nbins = 1000;
  TH1F * pthistogram = new TH1F("pt","pt",nbins,PtMin,PtMax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    float weight = 1.0;
    pthistogram->AddBinContent(ibin,weight);
  }

  return pthistogram;
}
