#include <iostream>
// T. K. Ghosh  07.27 99 , Vanderbilt

#include "PISAEvent.h"
#include "TecPISAHit.h"
#include <cstdlib>


void encodeRootEvntTec(int argplane, int kentries, int i[], float f[], 
                       int isubevent, PISAEvent *pisaevent)
{
  
  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;    // Will change in Off-Line
  Int_t id = -1;       // Will change in Off-Line

  Int_t plane = argplane;

  Int_t iArm = -1;     // Will change below

  Float_t xyzinloc[3];
  Float_t xyzoutloc[3];
  Float_t xyzinglo[3];


  Int_t p = 0;
  for (int k=0; k<kentries; k++)
  {
    Int_t track = i[p++];  // track in subevent
    xyzinloc[0] = f[p++];
    xyzinloc[1] = f[p++];
    xyzinloc[2] = f[p++];
    xyzoutloc[0] = f[p++];
    xyzoutloc[1] = f[p++];
    xyzoutloc[2] = f[p++];
    Float_t tof = f[p++];

    p++;        // skip location for the particle ID output
    p++;        // skip location for the std::exit TOF

    Int_t sector = i[p++]-1;  // Number from 0 as of Feb. 15, 1998
    Float_t dedx = f[p++];
    xyzinglo[0] = f[p++];
    xyzinglo[1] = f[p++];
    xyzinglo[2] = f[p++];
    if(xyzinglo[0] > 0)
      iArm = 0;   // Number from 0 as of Feb. 15, 1998
    else
      iArm = 1;   // Number from 0 as of Feb. 15, 1998 

   // Original subevent output, kept for future use
   pisaevent->AddTecHit(
    mctrack, xyzinloc, xyzoutloc, tof, sector, dedx, xyzinglo, 
    iArm, nfile, id, plane, isubevent, track);
  } 

  return;
  
}




















































