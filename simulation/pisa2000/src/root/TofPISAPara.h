#ifndef _TofPISAPARA_
#define _TofPISAPARA_

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
// Description of the TOF parameters (event independent)                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"


class TofPISAPara : public TObject {

private:
  Float_t tofl_rpos   ;
  Float_t tfsp_phi_1  ;
  Float_t tfsp_phi_2  ;
  Float_t tfsp_dimen[3];
  Int_t tfsp_nslat    ;
  Int_t tfsp_isegm    ;
  Float_t tflp_phi_1  ;
  Float_t tflp_phi_2  ;
  Float_t tflp_phi_3  ;
  Float_t tflp_phi_4  ;
  Float_t tflp_dimen[3];
  Int_t tflp_nslat    ;
  Int_t tflp_isegm    ;
  Int_t color_tof     ;
  Int_t med_tof       ;



public:
   TofPISAPara() { }  // Default constructor (needed for arrays of objects)
   TofPISAPara(const Int_t i[], const Float_t f[]);  // Main constructor

   virtual ~TofPISAPara() { }

   ClassDef(TofPISAPara,1)  // A TOF  parameter instance
};

#endif

