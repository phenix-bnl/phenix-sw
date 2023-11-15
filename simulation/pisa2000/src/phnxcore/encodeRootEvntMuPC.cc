#include <iostream>
#include "PISAEvent.h"
#include "MuPCPISAHit.h"
#include <cstdlib>

using namespace std;

//_________________________________________________________________________________
void encodeRootEvntMuPC(
  int ich, int kentries, int iData[], float fData[], 
  int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id    = -1;    // Will change in Off-Line

  Int_t ipc = 0;
  if(ich <= 2) ipc = 1;
  if(ich>2 && ich<=4) ipc = 2;
  if(ich>4 && ich<=6) ipc = 3;

  Int_t p = 0;
  Int_t arm;
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

    if (xyzinglo[2] >= 0.0) arm = 0;   // Count from 0 as of Feb. 15, 1998                                                            
    else arm = 1;   // Count from 0 as of Feb. 15, 1998                   

    Float_t pathLength = fData[p++];
 
    // Original subevent output, kept for future use
    pisaevent->AddMuPCHit(xyzinloc, xyzoutloc, xyzinglo,
      tof, dedx, pathLength, track, arm, 
      id, ipc, isubevent,  mctrack, nfile );
    
  }
  
  return;
}
