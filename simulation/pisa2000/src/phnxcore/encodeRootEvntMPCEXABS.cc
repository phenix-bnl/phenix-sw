#include <iostream>
#include "PISAEvent.h"
#include "MPCEXABSPISAHit.h"
#include <cstdlib>

void encodeRootEvntMPCEXABS(int ich, int kentries, int iData[], float fData[], 
		       int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );
  
  Int_t nfile = -1;    // Will change in Off-Line

  Int_t p = 0;

  for (int k=0; k<kentries; k++)
  {
    Int_t track    = iData[p++];     // track in subevent
    Int_t evnt     = static_cast<Int_t>(fData[p++]);
    Int_t incc     = static_cast<Int_t>(fData[p++]);
    Float_t dedx   = fData[p++];
 
    // Original subevent output, kept for future use
    pisaevent->AddMPCEXABSHit(
      evnt, incc, dedx, isubevent, track, nfile );

  }

  return;
  
}
