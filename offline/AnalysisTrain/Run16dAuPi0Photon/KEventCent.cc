#include <stdio.h>
#include <iostream>
#include "TObject.h"
#include "KEventCent.h"

// Implementations of KEventCent

KEventCent::KEventCent() {
   for(int i=0; i<20; i++){
      mult_lower[i] = 0;
      mult_upper[i] = 0;
      cent_lower[i] = 0;
      cent_upper[i] = 0;
   }
}

KEventCent::~KEventCent() {}

void KEventCent::setMultBins( unsigned int i, int lower, int upper ) {
  if ( i >= maxbins ) throw OutOfRange();
  mult_lower[i] = lower;
  mult_upper[i] = upper;
}

void KEventCent::setCentBins( unsigned int i, float lower, float upper ) {
  if ( i >= maxbins ) throw OutOfRange();
  cent_lower[i] = lower;
  cent_upper[i] = upper;
}

int KEventCent::getMultBin( int nemc, int nbins ) {
  for (int i=0; i<nbins; i++) {
    if ( nemc > mult_lower[i] && nemc <= mult_upper[i] ) {
      return i;
    }
  }
  return -1;
}

int KEventCent::getCentBin( float percent, int nbins ) {
  for (int i=0; i<nbins; i++) {
    if ( percent > cent_lower[i] && percent <= cent_upper[i] ) {
      return i;
    }
  }
  return -1;
}



