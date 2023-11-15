#include <iostream>
#include "PISAEvent.h"
#include "PadPISAHit.h"
#include <cstdlib>

void encodeRootEvntPad(
  int isector, int kentries, int iData[], float fData[], 
  int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id    = -1;    // Will change in Off-Line

  Int_t sector = -1;
  Int_t ipc = 0;

  if(isector>0 && isector < 17) {
    sector = isector - 1;  // PC1 Count from 0, as of Feb. 15, 1998
    ipc = 1;
  }
  if(isector>100 && isector < 109) {
    sector = isector - 101;  // PC2 Count from 0, as of Feb. 15, 1998
    ipc = 2;
  }

  if(isector>200 && isector < 209) {
    sector = isector - 201;  // PC3 Count from 0, as of Feb. 15, 1998
    ipc = 3;
  }

  if(ipc == 0){
    std::cerr << "\n  encodeRootEvnt Pad <E>: bad sector value";
    std::cerr << " Sector = " << isector << std::endl;
    std::exit(1);
  }

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

    if (xyzinglo[0] >= 0.0) arm = 0;
    else arm = 1;

    xyzinglo[1]        = fData[p++];
    xyzinglo[2]        = fData[p++];
    Float_t pathLength = fData[p++];
    
    // Original subevent output, kept for future use
    pisaevent->AddPadHit(xyzinloc, xyzoutloc, xyzinglo,
      tof, dedx, pathLength, track, arm, sector,
      id, ipc, isubevent,  mctrack, nfile );
    
  }
 
  return;
}
