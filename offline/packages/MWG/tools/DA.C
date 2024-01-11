/*!
	\file DA.C
	\brief calculates deflection angle between the ZVTX-STI vector and Pxyz_STI
        /06/07/2007 MXL
*/

#include "Tools.h"
#include <PHMuoTracksOut.h>
#include <cmath>

//___________________________________________________
Float_t Tools::DA( PHMuoTracksOut* &muo, Int_t ipart, Float_t BbcZVertex) 
{
  //track position at station 1	
  float xSTI  = muo->get_xpos(1,ipart);
  float ySTI  = muo->get_ypos(1,ipart);
  float zSTI  = muo->get_zpos(1,ipart);
  float pxSTI = muo->get_px(1,ipart);
  float pySTI = muo->get_py(1,ipart);
  float pzSTI = muo->get_pz(1,ipart);

  float pSTI = std::sqrt(pxSTI*pxSTI + pySTI*pySTI + pzSTI*pzSTI);

  float  costheta_xyz, xyz_St1;

  float dANGLE_xyz=-999;

  xyz_St1 = std::sqrt(xSTI*xSTI+ySTI*ySTI+(zSTI-BbcZVertex)*(zSTI-BbcZVertex) );
  
  costheta_xyz = (pxSTI*xSTI + pySTI* ySTI + pzSTI*(zSTI-BbcZVertex) ) / (pSTI*xyz_St1) ;
	
  if ( std::fabs(costheta_xyz) < 1.0 ) {
    dANGLE_xyz = std::acos(costheta_xyz);
  }
  else{
    dANGLE_xyz = 0.0;
  }
  dANGLE_xyz = dANGLE_xyz*0.5*(1.25 +2*pSTI);

  return dANGLE_xyz;

}
