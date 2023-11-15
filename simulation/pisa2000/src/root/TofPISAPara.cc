#include "TofPISAPara.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TofPISAPara.cc                                                       //
//                                                                      //
// Implementation of Tof Parameters in PISA                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(TofPISAPara)

TofPISAPara::TofPISAPara(const Int_t iData [], const Float_t fData [])
{
   //
   //

  tofl_rpos     = fData[1];
  tfsp_phi_1    = fData[2];
  tfsp_phi_2    = fData[3];
  tfsp_dimen[0] = fData[4];
  tfsp_dimen[1] = fData[5];
  tfsp_dimen[2] = fData[6];
  tfsp_nslat    = iData[7];
  tfsp_isegm    = iData[8];
  tflp_phi_1    = fData[9];
  tflp_phi_2    = fData[10];
  tflp_phi_3    = fData[11];
  tflp_phi_4    = fData[12];
  tflp_dimen[0] = fData[13];
  tflp_dimen[1] = fData[14];
  tflp_dimen[2] = fData[15];
  tflp_nslat    = iData[16];
  tflp_isegm    = iData[17];
  color_tof     = iData[18];
  med_tof       = iData[19];

}

