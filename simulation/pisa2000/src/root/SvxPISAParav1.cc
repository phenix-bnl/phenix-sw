
#include <iostream>
#include "SvxPISAParav1.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Implementation of SVX Parameters in PISA, version 1                  //
// Sasha Lebedev (lebedev@iastate.edu)                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(SvxPISAParav1)

SvxPISAParav1::SvxPISAParav1(const Int_t iData [], const Float_t fData [])
{
  sili_br_nlayers = iData[0];
  sili_sideLayers = iData[1];
  nhh 		  = iData[2];           
  nbrv 		  = iData[3];         
  necv 		  = iData[4];        
  
  sili_cg_rmn     = fData[0];
  sili_cg_thck    = fData[1];
  sili_cg_inthck  = fData[2];
  sili_cg_tempc   = fData[3];
  sili_cg_npcon   = (int)fData[4];
  sili_cg_z[0]    = fData[5];       
  sili_cg_rmx[0]  = fData[6];    
  sili_cg_z[1]    = fData[7];       
  sili_cg_rmx[1]  = fData[8];    
  sili_cg_xdisp   = fData[9];
  sili_cg_ydisp   = fData[10];
  sili_cg_zdisp   = fData[11];

  for(int i=0; i<sili_br_nlayers; i++) {
    sili_br_snhalfx[i] = fData[12+i*15];  
    sili_br_snhalfy[i] = fData[13+i*15];
    sili_br_snhalfz[i] = fData[14+i*15];
    sili_br_x0add  [i] = fData[15+i*15];
    sili_br_snzgap [i] = fData[16+i*15];
    sili_br_tilt   [i] = fData[17+i*15];
    sili_br_nsn    [i] = (int)fData[18+i*15]; 
    sili_br_r      [i] = fData[19+i*15];
    sili_br_z      [i] = fData[20+i*15];
    sili_br_dphi   [i] = fData[21+i*15];
    sili_br_nsec   [i] = (int)fData[22+i*15];
    for(int j=0; j<sili_br_nsec[i]; j++) {
      sili_br_phic[j][i] = fData[23+i*15+j*2];
      sili_br_nlad[j][i] = (int)fData[24+i*15+j*2];
    }
  }

  for(int i=0; i<sili_sideLayers; i++) {
    sili_phi1_side   [i] = fData[72+i*11]; 
    sili_dph_side    [i] = fData[73+i*11];
    sili_z1_side     [i] = fData[74+i*11];
    sili_rmin1_side  [i] = fData[75+i*11];
    sili_rmax1_side  [i] = fData[76+i*11];
    sili_z2_side     [i] = fData[77+i*11];
    sili_rmin2_side  [i] = fData[78+i*11];
    sili_rmax2_side  [i] = fData[79+i*11];
    sili_npdv_side   [i] = fData[80+i*11];
    sili_nz_side     [i] = fData[81+i*11];
    sili_zCenter_side[i] = fData[82+i*11];
  }

  initialized=1;

}

void SvxPISAParav1::identify(std::ostream& os) const {os << "SvxPISAParav1 object"; return;}

void SvxPISAParav1::Reset() {return;}

int SvxPISAParav1::isValid() const {if(initialized) {return 1;} else {return 0;}}

