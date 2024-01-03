//-----------------------------------------------------------------------------
//
// Generate white rapidity distributions
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include "InitializeWhiteY.h"

TH1* InitializeWhiteY(const double YMin, const double YMax)
{
  const int nbins = 1000;
  TH1F * yhistogram = new TH1F("y","y",nbins,YMin,YMax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    float weight = 1.0;
    yhistogram->AddBinContent(ibin,weight);
  }

  return yhistogram;
}
