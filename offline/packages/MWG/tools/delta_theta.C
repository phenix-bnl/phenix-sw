#include "Tools.h"
#include <PHMuoTracksOut.h>
#include <cmath>

//=============returns angular difference between tracks position 
//             vector and momentum vector at MuTr station 1
Float_t Tools::delta_theta(PHMuoTracksOut* &muo, Int_t itrk)
{
  //position vectors, fyi: track z-vertex equals BBC z-vertex
  Float_t x1 = muo->get_xpos(1,itrk);
  Float_t y1 = muo->get_ypos(1,itrk);
  Float_t z1 = muo->get_zpos(1,itrk)-muo->get_zpos(0,itrk);  
  Float_t Pos1 = sqrt(x1*x1 + y1*y1 + z1*z1);
  
  //momentum vectors
  Float_t px1 = muo->get_px(1,itrk);
  Float_t py1 = muo->get_py(1,itrk);
  Float_t pz1 = muo->get_pz(1,itrk);
  Float_t P1 = sqrt(px1*px1 + py1*py1 + pz1*pz1);

  Float_t scalar = (x1*px1 + y1*py1 + z1*pz1)/(Pos1*P1);
  if(scalar>=1) scalar = 1;

  return acos(scalar);  //radians
}

//=============same as above but multiplied by momentum
Float_t Tools::p_delta_theta(PHMuoTracksOut* &muo, Int_t itrk)
{
  Float_t angle_diff = Tools::delta_theta(muo, itrk);  //radians
  
  //vertex momentum
  Float_t px0 = muo->get_px(0,itrk);
  Float_t py0 = muo->get_py(0,itrk);
  Float_t pz0 = muo->get_pz(0,itrk);
  Float_t P0 = sqrt(px0*px0 + py0*py0 + pz0*pz0);
  
  //station 1 momentum
  Float_t px1 = muo->get_px(1,itrk);
  Float_t py1 = muo->get_py(1,itrk);
  Float_t pz1 = muo->get_pz(1,itrk);
  Float_t P1 = sqrt(px1*px1 + py1*py1 + pz1*pz1);
  
  return (P0+P1)/2*angle_diff;
}
