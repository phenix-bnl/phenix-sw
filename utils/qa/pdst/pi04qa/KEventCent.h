#ifndef __KEventCent__
#define __KEventCent__

#include <stdio.h>
//#include "TObject.h"

#define MAX_NCLUSTER 1024

class KEventCent {
 public:

  KEventCent();
  virtual ~KEventCent();

  void setMultBins( int i, int lower, int upper );
  void setCentBins( int i, float lower, float upper );

  int getCentBin( float icent , int nbins);
  int getMultBin( int nemc, int nbins );

  Int_t        mult_lower[20];    // 20 is the maximum number of mult
  Int_t        mult_upper[20];    // or cent bins supported
  float        cent_lower[20];
  float        cent_upper[20];

 private:

};

#endif


