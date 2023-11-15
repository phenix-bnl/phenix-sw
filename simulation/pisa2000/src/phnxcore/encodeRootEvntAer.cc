#include <iostream>
#include "PISAEvent.h"
#include "AerPISAHit.h"
#include <cstdlib>

void encodeRootEvntAer(int kentries, int iData[], float fData[], 
		       int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;

  Int_t p = 0;
  for (int k=0; k<kentries; k++)
  {

    Float_t xyzglobal[3];
    Float_t pmomxyz[3];
    Float_t vertxyz[3];
 
    xyzglobal[0] = fData[p++];  // x position
    xyzglobal[1] = fData[p++];  // y position
    xyzglobal[2] = fData[p++];  // z position
    Float_t dele = fData[p++];  // energy loss
    pmomxyz[0] = fData[p++];    // px momentum component
    Int_t aerID = iData[p++];  // particle ID
    Int_t track = iData[p++];   // track in subevent number
    Int_t quadrant = iData[p++];   // AER layer
    pmomxyz[1] = fData[p++];    // py momentum component
    pmomxyz[2] = fData[p++];    // pz momentum component  
    Float_t pathLength = fData[p++];  // path Length in cm
    Float_t tof = fData[p++];   // TOF in ns
    Float_t stepLength = fData[p++];   // step length in cm
    Float_t etot = fData[p++];   // total energy of particle
    Float_t charge = fData[p++]; // charge of particle
    Float_t momentum = fData[p++]; // momentum of particle
    vertxyz[0] = fData[p++];
    vertxyz[1] = fData[p++];
    vertxyz[2] = fData[p++];

    // Original subevent output, kept for future use
    pisaevent->AddAerHit( 
      xyzglobal,  pmomxyz,
      dele,  track,  quadrant,  aerID,
      pathLength, tof, stepLength, etot,
      charge, momentum, vertxyz, isubevent, mctrack,
      nfile);

  }

  return;
}
