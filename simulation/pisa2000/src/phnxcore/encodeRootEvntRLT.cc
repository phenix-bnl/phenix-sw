#include <iostream>
#include "PISAEvent.h"
#include "RLTPISAHit.h"
#include <cstdlib>

using namespace std;

//__________________________________________________________________
// Authors: M. C. McCain and L. A. Linden Levy
void encodeRootEvntRLT(
  int irpc, int kentries, int iData[], float fData[], 
  int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id    = -1;    // Will change in Off-Line
  
  Int_t p = 0;
  Float_t xyzinloc[3];
  Float_t xyzoutloc[3];
  Float_t xyzinglo[3];

  for (int k=0; k<kentries; k++)
  {
    Int_t track        = iData[p++];     // track in subevent
    xyzinloc[0]        = fData[p++];
    xyzinloc[1]        = fData[p++];
    xyzinloc[2]        = fData[p++];
    xyzoutloc[0]       = fData[p++];
    xyzoutloc[1]       = fData[p++];
    xyzoutloc[2]       = fData[p++];
    Float_t tof        = fData[p++];

    p++;  // Skip particle ID output (could do trtrno redundancy check)
    Float_t dedx       = fData[p++];

    xyzinglo[0]        = fData[p++];
    xyzinglo[1]        = fData[p++];
    xyzinglo[2]        = fData[p++];

    Float_t pathLength = fData[p++];
    
 
    pisaevent->AddrltHit(xyzinloc, xyzoutloc, xyzinglo,
      tof, dedx, pathLength, track, 
      id, irpc, isubevent,  mctrack, nfile );
 
  } 
  
  return;
}
