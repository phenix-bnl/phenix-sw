#include <iostream>
#include "PISAEvent.h"

void encodeRootEvntPri(int kentries, int idata[], float fdata[], 
		       int isubevent, PISAEvent *pisaevent)
{
  Int_t argtrue_track = -1;  // Will change in Off-Line
  Int_t argnfile = -1;       // Will change in Off-line
  Int_t argisubevent = isubevent;

  Int_t p = 0;
  for (int k=0; k<kentries; k++){
    Int_t argevtrack = idata[p++]; 
    Int_t argidpart  = idata[p++];
    Float_t argpx    = fdata[p++];
    Float_t argpy    = fdata[p++];
    Float_t argpz    = fdata[p++];
    Int_t argntrack  = idata[p++];

    pisaevent->AddPriHit(argtrue_track, argisubevent, argntrack, argidpart,
			 argpx, argpy, argpz, argevtrack, argnfile);

  }  //loop over all track entries

  return;
}



