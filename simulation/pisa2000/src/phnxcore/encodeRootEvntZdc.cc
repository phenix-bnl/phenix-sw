#include <iostream>
#include "PISAEvent.h"
#include "ZdcPISAHit.h"
#include <cstdlib>

void encodeRootEvntZdc(int kentries, int i[], float f[], int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t argmctrack = -1;     // Will change in Off-Line
  Int_t argnfile = -1;       // Will change in Off-line
  Int_t argisubevent = isubevent;

  Int_t p = 0;
  Int_t ip = 0;
  
  for (int k=0; k<kentries; k++)
  {
    Float_t argxm =   f[p++];
    Float_t argym =   f[p++];
    Float_t argzm =   f[p++];
    Float_t argdele = f[p++];
    Float_t argpxm =  f[p++];
    Float_t argpym =  f[p++];
    Float_t argpzm =  f[p++];
    Float_t argtof =  f[p++];
    Int_t argpid =    static_cast<int>(f[p++]);
    Int_t argdirect = static_cast<int>(f[p++]);
    Int_t argmodule = static_cast<int>(f[p++]);

    Int_t argtrack =  i[ip++];  

    pisaevent->AddZdcHit(argxm, argym, argzm, argpxm, argpym, argpzm, argdele,
      argtof,argpid, argdirect, argmodule,
      argtrack, argisubevent,argmctrack, argnfile);
    
  }
  
  return;
}
