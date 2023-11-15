#include "DchPISAPara.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// DchPISAPara.cc                                                       //
//                                                                      //
// Implementation of DCH Parameters in PISA                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(DchPISAPara)

DchPISAPara::DchPISAPara(const Int_t iData [], const Float_t fData [])
{
   //
   // Constructor taken from decodeParaDC.cc in STAF written by Dave Morrison
   // The dchgeo[0] Table elements are replace by the private data members of this class
   //


  ncells        = iData[1];   // Should check that parameters start at iData[1] position
  ngusset       = iData[2];
  ti_switch     = iData[3];

  const Int_t F_OFFSET = 3;
  suppzlength = fData[1 + F_OFFSET];  // MUST have offset location for fData here
  inradius    = fData[2 + F_OFFSET];
  outradius   = fData[3 + F_OFFSET];
  phibotw     = fData[4 + F_OFFSET];
  phitopw     = fData[5 + F_OFFSET];
  phibote     = fData[6 + F_OFFSET];
  phitope     = fData[7 + F_OFFSET];

  for (Int_t i = 0; i < 40; i++) {
    rplane[i] = fData[8 + i + F_OFFSET];
  }

  rplane[40]  = 247.4;
  planethick  = fData[48 + F_OFFSET];
  uvangle     = fData[49 + F_OFFSET];
  winthickin  = fData[50 + F_OFFSET];
  winthickout = fData[51 + F_OFFSET];
  supptiside  = fData[52 + F_OFFSET];
  suppalside  = fData[53 + F_OFFSET];
  suppzthick  = fData[54 + F_OFFSET];
  supptibase  = fData[55 + F_OFFSET];
  suppalbase  = fData[56 + F_OFFSET];
  x1baserad   = fData[57 + F_OFFSET];
  x2baserad   = fData[58 + F_OFFSET];
  x1basez     = fData[59 + F_OFFSET];
  x2basez     = fData[60 + F_OFFSET];
  x1slotthick = fData[61 + F_OFFSET];
  x2slotthick = fData[62 + F_OFFSET];
  x1slotz     = fData[63 + F_OFFSET];
  x2slotz     = fData[64 + F_OFFSET];
  x1suppthick = fData[65 + F_OFFSET];
  x2suppthick = fData[66 + F_OFFSET];
  x1suppz     = fData[67 + F_OFFSET];
  x2suppz     = fData[68 + F_OFFSET];
  x1rextent   = fData[69 + F_OFFSET];
  x2rextent   = fData[70 + F_OFFSET];
  u1rextent   = fData[71 + F_OFFSET];
  v1rextent   = fData[72 + F_OFFSET];
  u2rextent   = fData[73 + F_OFFSET];
  v2rextent   = fData[74 + F_OFFSET];
  u1basez     = fData[75 + F_OFFSET];
  v1basez     = fData[76 + F_OFFSET];
  u2basez     = fData[77 + F_OFFSET];
  v2basez     = fData[78 + F_OFFSET];
  u1slotz     = fData[79 + F_OFFSET];
  v1slotz     = fData[80 + F_OFFSET];
  u2slotz     = fData[81 + F_OFFSET];
  v2slotz     = fData[82 + F_OFFSET];
  u1suppz     = fData[83 + F_OFFSET];
  v1suppz     = fData[84 + F_OFFSET];
  u2suppz     = fData[85 + F_OFFSET];
  v2suppz     = fData[86 + F_OFFSET];
  cfibinrad   = fData[87 + F_OFFSET];
  cfiboutrad  = fData[88 + F_OFFSET];

}

