#include "PISAEvent.h"
#include "CrkPISAHit.h"
#include <iostream>
#include <cstdlib>



void encodeRootEvntCrk(int *iqf, int kentries, int iData[], 
		       int isubevent, PISAEvent *pisaevent)
{
  
  assert( pisaevent->GetStartFlag() != 2 );

  Int_t mctrack = -1;     // Will change in Off-Line
  Int_t nfile = -1;       // Will change in Off-line  
  Int_t nCells = kentries;  
  for (Int_t i=0; i<nCells; i++) 
  {
    Int_t *iCellData = &(iqf[iData[-i-11]]); // we start from one location up
    Int_t nRows = iCellData[0];
    Float_t *fCellData = (float *)iCellData;
    Int_t p = 1; 
    // we use p++ instead of ++p (to be like rest of PISA encoders)
    for (Int_t j=0; j<nRows; j++) 
    {
      Short_t pmt    = iData[i*3];
      Float_t x      = fCellData[p++];
      Float_t y      = fCellData[p++];
      Float_t z      = fCellData[p++];
      Float_t tof    = fCellData[p++];
      Float_t px     = fCellData[p++];
      Float_t py     = fCellData[p++];
      Float_t pz     = fCellData[p++];
      Short_t pid    = iCellData[p++];
      Int_t tra      = iCellData[p++];  // track in subevent
      Int_t parent   = iCellData[p++];  // parent track of track in subevent
      Int_t nbf      = iCellData[p++];
      Int_t bi1      = iCellData[p++];
      Int_t bi2      = iCellData[p++];
      Float_t bp1    = fCellData[p++];
      Float_t bp2    = fCellData[p++];

      pisaevent->AddCrkHit(pmt, x, y, z, tof, px, py, pz, pid, tra, parent, nbf,
        bi1, bi2, bp1, bp2, mctrack, nfile, isubevent);

    }
 
  }
  
  return;
}



















