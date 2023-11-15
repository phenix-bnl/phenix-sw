#include <iostream>
#include "PISAEvent.h"
#include "SvxPISAHit.h"
#include <cstdlib>

void encodeRootEvntSvx(
  int kentries, int iData[], float fData[], 
  int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;

  Int_t p = 0;
  for (int k=0; k<kentries; k++){

    Float_t xyzglobal[3];
    Float_t pmomxyz[3];
// Corrected by V. L. Rykov 09/03/2003
    Float_t xyzlocalIn[3];
    Float_t xyzlocalOut[3];
// Added global in/out Jul 2006 Hubert van Hecke:
    Float_t xyzglobalIn[3];
    Float_t xyzglobalOut[3];
    Int_t   hitVolume[9];
 
    xyzglobal[0]         = fData[p++];  // x position
    xyzglobal[1]         = fData[p++];  // y position
    xyzglobal[2]         = fData[p++];  // z position
    Float_t dele         = fData[p++];  // energy loss
    pmomxyz[0]           = fData[p++];  // px momentum component
    Int_t siliID         = iData[p++];  // particle ID
    Int_t track          = iData[p++];  // track in subevent number
    Int_t layer          = iData[p++];  // Silicon layer
    pmomxyz[1]           = fData[p++];  // py momentum component
    pmomxyz[2]           = fData[p++];  // pz momentum component
// Corrected by V. L. Rykov 09/03/2003
    Float_t timeOfFlight = fData[p++];  // Time-Of-Flight

    xyzlocalIn[0]        = fData[p++];  // x local entry position
    xyzlocalIn[1]        = fData[p++];  // y local entry position
    xyzlocalIn[2]        = fData[p++];  // z local entry position
    xyzlocalOut[0]       = fData[p++];  // x local exit position
    xyzlocalOut[1]       = fData[p++];  // y local exit position
    xyzlocalOut[2]       = fData[p++];  // z local exit position

// Added global in/out Jul 2006 Hubert van Hecke
    xyzglobalIn[0]        = fData[p++];  // x global entry position
    xyzglobalIn[1]        = fData[p++];  // y global entry position
    xyzglobalIn[2]        = fData[p++];  // z global entry position
    xyzglobalOut[0]       = fData[p++];  // x global exit position
    xyzglobalOut[1]       = fData[p++];  // y global exit position
    xyzglobalOut[2]       = fData[p++];  // z global exit position

    hitVolume[0]         = iData[p++];  // hit volume tree
    hitVolume[1]         = iData[p++];  // hit volume tree
    hitVolume[2]         = iData[p++];  // hit volume tree
    hitVolume[3]         = iData[p++];  // hit volume tree
    hitVolume[4]         = iData[p++];  // hit volume tree
    hitVolume[5]         = iData[p++];  // hit volume tree
    hitVolume[6]         = iData[p++];  // hit volume tree
    hitVolume[7]         = iData[p++];  // hit volume tree
    hitVolume[8]         = iData[p++];  // hit volume tree
    //
    // Original subevent output, kept for future use
    //
    // Corrected by V. L. Rykov 09/03/2003
    // Added global in/out Jul 2006 Hubert van Hecke
    pisaevent->AddSvxHit( xyzglobal,  pmomxyz, dele, timeOfFlight,
			    xyzlocalIn, xyzlocalOut,
			    xyzglobalIn, xyzglobalOut,
			    hitVolume,
			    track,  layer,  siliID,
			    isubevent,   mctrack,  nfile);

  }
  
  return;
}
