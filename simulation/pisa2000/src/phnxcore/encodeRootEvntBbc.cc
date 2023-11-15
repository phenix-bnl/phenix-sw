#include <iostream>
#include "PISAEvent.h"
#include "BbcPISAHit.h"
#include <cstdlib>

void encodeRootEvntBbc(int kentries, int i[], float f[], 
		       int isubevent, PISAEvent *pisaevent)
{
  
  assert( pisaevent->GetStartFlag() != 2 );

  Int_t argmctrack = -1;     // Will change in Off-Line
  Int_t argnfile = -1;       // Will change in Off-line
  Int_t argisubevent = isubevent;

  Int_t p = 0;
  for (int k=0; k<kentries; k++){
    Short_t argpmt = i[p++];
    Float_t argx =   f[p++];
    Float_t argy =   f[p++];
    Float_t argz =   f[p++];
    Float_t argdel = f[p++];
    Float_t argtof = f[p++];
    Short_t argpid = i[p++];
    Float_t argpx =  f[p++];
    Float_t argpy =  f[p++];
    Float_t argpz =  f[p++];
    Float_t arglen = f[p++];
    Int_t argtrack = i[p++];

    pisaevent->AddBbcHit(argx,  argy,   argz,   argpx,  argpy,    argpz, argdel,
      argtof,arglen, argpmt, argpid, argtrack, argisubevent,
      argmctrack, argnfile);
    
  }
  
  return;
}
