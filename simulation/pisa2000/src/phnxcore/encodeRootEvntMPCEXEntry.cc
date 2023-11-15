#include <iostream>
#include "PISAEvent.h"
#include "MPCEXEntryPISAHit.h"
#include <cstdlib>

void encodeRootEvntMPCEXEntry(int ich, int kentries, int iData[], float fData[], 
			      int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );
  
  Int_t nfile = -1;    // Will change in Off-Line

  Int_t p = 0;

  for (int k=0; k<kentries; k++)
  {
    
    Int_t track = iData[p++];     // track in subevent
    Float_t vx  = fData[p++]; 
    Float_t vy  = fData[p++]; 
    Float_t vz  = fData[p++]; 
    Float_t px  = fData[p++]; 
    Float_t py  = fData[p++]; 
    Float_t pz  = fData[p++]; 
  
    // Original subevent output, kept for future use
    pisaevent->AddMPCEXEntryHit( vx, vy, vz, 
				 px, py, pz,
				 isubevent, track, nfile );

  }

  return;
  
}
