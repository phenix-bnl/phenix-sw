#include <iostream>
#include "PISAEvent.h"
#include "HbdPISAHit.h"
#include <cstdlib>

void encodeRootEvntHbd(int kentries, int iData[], float fData[], 
		       int isubevent, PISAEvent *pisaevent)
{
  
  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;

  Int_t p = 0;
  for (int k=0; k<kentries; k++)
  {

    Float_t xyzin[3];
    Float_t xyzout[3];
    Float_t pxyz[3];
 
    xyzin[0] = fData[p++];            // x entry position
    xyzin[1] = fData[p++];            // y entry position
    xyzin[2] = fData[p++];            // z entry position
    pxyz[0] = fData[p++];             // x momentum
    pxyz[1] = fData[p++];             // y momentum
    pxyz[2] = fData[p++];             // z momentum
    Float_t tof = fData[p++];         // TOF in ns
    Int_t hbdID = iData[p++];         // particle ID
    Int_t track = iData[p++];         // track in subevent number
    xyzout[0] = fData[p++];           // x std::exit position
    xyzout[1] = fData[p++];           // y std::exit position
    xyzout[2] = fData[p++];           // z std::exit position
    Float_t dele = fData[p++];        // energy loss
    Float_t pathLength = fData[p++];  // path Length in cm
    Int_t detector = iData[p++];
    Int_t sector = iData[p++];
    Int_t padrow = iData[p++];
    /*CAA detflag = 1 for hit in a TPC padrow, 2 for hit in HBD radiator gas, 3 for HBD CsI. */
    Int_t detflag = iData[p++];

    //
    // Original subevent output, kept for future use
    //
    pisaevent->AddHbdHit( xyzin,  pxyz,
      tof,  hbdID, track, xyzout,
      dele, pathLength, 
      detector, sector, padrow, detflag,
      isubevent, mctrack,  nfile);

   }

  return;
}
