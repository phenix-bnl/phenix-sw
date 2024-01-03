//-----------------------------------------------------------------------------
//
// Generate rapidity distributions for the PHENIX generator
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <string>
#include "InitializeYPHENIX.h"
#include "Tools.h"

TH1* InitializeYPHENIX(const int ParticleID)
{
  const int nbins = 1000;
  const float ymin  = -1.0;
  const float ymax  = 1.0;
  TH1F * yhistogram = new TH1F(createString("y",ParticleID).c_str(),
      "y",nbins,ymin,ymax);

  for ( int ibin=1; ibin<=nbins; ibin++ )
  {
    float weight = 1.0;
    yhistogram->AddBinContent(ibin,weight);
  }

  return yhistogram;
}
