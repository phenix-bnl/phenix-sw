#include "PISAEvent.h"
#include "EmcPISAHit.h"
#include <iostream>
#include <cstdlib>

void encodeRootEvntEmc(int kentries, int i[], float f[], 
                       int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t argmctrack = -1;  // Will change in Off-Line
  Int_t argnfile = -1;    // Will change in Off-Line
  Int_t argisubevent = isubevent;

  Int_t p = 0;
  for (int k=0; k<kentries; k++)
  {
    Int_t argi1     = i[p++];

    Int_t argadd1   = i[p++];  // use iwall
    Int_t argadd2   = i[p++];  // use itype

    Float_t argdele = f[p++];
    Float_t argposx = f[p++];
    Float_t argposy = f[p++];
    Float_t argposz = f[p++];
    Float_t argtof  = f[p++];
    Int_t argindex1 = i[p++];
    Int_t argindex2 = i[p++];
    Int_t argnumed  = i[p++];

    p++;  // skip spart
    p++;  // skip ncycle

    Int_t argtrack  = i[p++];
 
    // Original subevent output, kept for future use
    pisaevent->AddEmcHit(argi1, argdele, argposx, argposy,
			   argposz, argtof, argindex1, argindex2, 
			   argnumed, argadd1, argadd2, argtrack, 
			   argmctrack, argisubevent, argnfile);

  }  // loop over all track entries

  return;
}



