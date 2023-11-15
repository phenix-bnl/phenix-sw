// T.K. Ghosh,   Vanderbilt, 08. 06.99
/////////////////////////////////////////////a

#include <iostream>
#include <cstdlib>

#include "PISAEvent.h"
//
// Based on decodeEvntCtraCrk.cc in STAF
// Change index pointer by 1 unit up for iData
//
void encodeRootEvntCtr(int *iqf, int kentries , int iData [], 
                       int isubevent, PISAEvent *pisaevent)
{

  //
  // CTR encoding is NOT yet debugged !!
  //
  Int_t *iBank = 0;
  Int_t nRows = 0;
  Int_t p = 0;

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  
  Int_t *lqf = &iqf[kentries];  // HAVE TO FIX THIS!!

// Each table corresponds to one plane of a fictitious tracker

  for (int i = 0; i < 3; i++) {
    switch (i) {
      case 0:
        iBank = iData;
        p = 0;
        nRows = 4;  // this will be changed to Kentries later
        break;
      case 1:
        iBank = &(iqf[iData[-9-1]]);
        p = 1;
        nRows = 5;  // this will be changed to Kentries later
        break;
      case 2:
        iBank = &(iqf[lqf[iData[-9-1]-1]]);
        nRows = 4;  // this will be changed to Kentries later
        p = 1;
        break;
    }

    Float_t *fBank = (Float_t *)iBank;
    for (int j = 0; j < nRows; j++) {
      Int_t detector = i;
      Float_t x        = fBank[p++];
      Float_t y        = fBank[p++];
      Float_t z        = fBank[p++];
      Short_t pid      = iBank[p++];
//      itra     = trtrno(iBank[p++],iSubEvent);
      Int_t itra     = iBank[p++];
      Float_t pvx      = fBank[p++];
      Float_t pvy      = fBank[p++];
      Float_t pvz      = fBank[p++];
      Float_t vx       = fBank[p++];
      Float_t vy       = fBank[p++];
      Float_t vz       = fBank[p++];

      pisaevent->AddCtrHit(detector, x, y, z, pid, itra, pvx, pvy, pvz, vx, vy, vz,
			   mctrack, nfile, isubevent);
    }
  }

  return;
}


