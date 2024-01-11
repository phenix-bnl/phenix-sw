#ifndef TCRKDCFITTER_INCLUDED
#define TCRKDCFITTER_INCLUDED

// This class perform a fit to DC hit patters. The fitted curve is used
//  to associate RICH and DC tracks
//
// Note that this is a class that "wraps" a function, and as such it is not
// a complete object. As such it lacks "minimum" set of operators (default
// ctor, copy ctor etc. Therefore, you should be careful to use this class

#include "dDchTracks.h"
#include "dDchHit.h"

class TCrkDcFitter {
public:
  TCrkDcFitter(DDCHHIT_ST *hit);
  int fit(DDCHTRACKS_ST *trk, float *x, float *u);
  ~TCrkDcFitter();
  // more member functions later...

private:
  DDCHHIT_ST   *fHit;
  void CopyHits(DDCHTRACKS_ST*,int,int,int*,float*,float*,float*,float*);
  void CopyUV(DDCHTRACKS_ST*,int,int,float,float,int*,float*,float*,float*);
};

void print_DcHit(DDCHHIT_ST *h, float z0);

#endif
