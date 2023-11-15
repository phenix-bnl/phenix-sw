#include <iostream>
#include "PISAEvent.h"
#include "FclPISAHit.h"
#include <cstdlib>

void encodeRootEvntFcl(int kentries, int iData[], float fData[], 
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
 
    xyzglobal[0] = fData[p++];  // x position
    xyzglobal[1] = fData[p++];  // y position
    xyzglobal[2] = fData[p++];  // z position
    Float_t dele = fData[p++];  // energy loss

    // std::cout << "\n encodeRootEvntFcl: kentries " << kentries;
    // std::cout << ",  energy loss " << dele;

    pmomxyz[0] = fData[p++];    // px momentum component
    Int_t siliID = iData[p++];  // particle ID
    Int_t track = iData[p++];   // track in subevent number
    Int_t layer = iData[p++];   // Silicon layer

    // std::cout << ",  FCL layer " << layer << std::endl;
    
    pmomxyz[1] = fData[p++];    // py momentum component
    pmomxyz[2] = fData[p++];    // pz momentum component  

    // Original subevent output, kept for future use
    pisaevent->AddFclHit( xyzglobal,  pmomxyz,
      dele,  track,  layer,  siliID,
      isubevent,   mctrack,  nfile);

  }
  
  return;
}
