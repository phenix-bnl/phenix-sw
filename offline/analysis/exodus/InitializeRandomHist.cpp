#include <TH1.h>
#include <TRandom.h>
#include "InitializeRandomHist.h"

TH1F * InitializeRandomHist(TH1F *histogram)
{
  for ( int i=1; i<=100000*gRandom->Rndm()+1; i++ )
  {
    histogram->GetRandom();
  }

  return histogram;
}
