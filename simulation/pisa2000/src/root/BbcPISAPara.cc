#include "BbcPISAPara.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// BbcPISAPara.cc                                                       //
//                                                                      //
// Implementation of BBC Parameters in PISA                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(BbcPISAPara)

BbcPISAPara::BbcPISAPara(const Int_t iData [], const Float_t fData [])
{
   //
   // Constructor taken from decodeParaBbc.cc in STAF written by Dave Morrison
   // The bbcgeo[0] Table elements are replace by the private data members of this class
   //
   // In STAF DIO Dave Morrison has started the filling at the backbd[1] position
   // The backbd[0] is not defined
   // This can work if the BBC code in STAF does not use backbd[0]
   // Same is true for some of the other parameter arrays here
   //

   color     = iData[0];
   seen      = iData[1];
   medabs    = iData[2];
   medatt    = iData[3];
   medbac    = iData[4];
   medcov    = iData[5];
   medfro    = iData[6];
   medmot    = iData[7];
   medpmt    = iData[8];
   medqua    = iData[9];
   medstr    = iData[10];
   absorb[0] = fData[11];
   absorb[1] = fData[12];
   absorb[2] = fData[13];
   backbd[1] = fData[24];
   backbd[2] = fData[25];
   backbd[3] = fData[26];

   covert    = fData[27];
   frontb[1] = fData[28];
   frontb[2] = fData[29];
   frontb[3] = fData[30];
   pmtsiz[1] = fData[31];
   pmtsiz[2] = fData[32];  
   pmtsiz[3] = fData[33];

   for (Int_t i = 0; i < 10; i++) {
     quartz[i] = fData[14+i];
   }

   spacin    = fData[44];
   struc[1]  = fData[45];
   struc[2]  = fData[46];
   struc[3]  = fData[47];
   zposit[1] = fData[48];
   zposit[2] = fData[49];

}

