#include <iostream>
#include "PISAEvent.h"

void encodeRootEvntDch(int arm, int kentries, int iData[], float fData[], 
		       int isubevent, PISAEvent *pisaevent)
{
  //
  // Based on decodeRootEvntDC.cc from STAF, written by Dave Morrison
  // 
  // This routine will be called twice in each subevent
  // First time for West Arm, second time for East Arm
  //
  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id    = -1;    // Will change in Off-Line

  Int_t p = 0;
  Int_t iArm = arm;              // Count from 0 as of Feb. 15, 1998

  Float_t xyzinloc[3];
  Float_t xyzoutloc[3];
  Float_t xyzinglo[3];
  for (int k=0; k<kentries; k++){
    xyzinloc[0]        = fData[p++];
    xyzinloc[1]        = fData[p++];
    xyzinloc[2]        = fData[p++];
    Float_t tof        = fData[p++];
    xyzoutloc[0]       = fData[p++];
    xyzoutloc[1]       = fData[p++];
    xyzoutloc[2]       = fData[p++];
    Int_t plane        = iData[p++] - 1;    // Count from 0 as of Feb. 15, 1998
    Int_t track        = iData[p++];        // track in subevent number
    Int_t cell         = iData[p++] - 1;    // Count from 0 as of Feb. 15, 1998
    p++;   // Skip the particle ID
    xyzinglo[0]        = fData[p++];
    xyzinglo[1]        = fData[p++];
    xyzinglo[2]        = fData[p++];
    Float_t pathLength = fData[p++];

    pisaevent->AddDchHit(xyzinloc,    tof,   xyzoutloc,
			 plane,       cell,  xyzinglo,
			 pathLength,  track, isubevent,
			 iArm, id, mctrack, nfile);
    
  }  //loop over all track entries

  return;
}
