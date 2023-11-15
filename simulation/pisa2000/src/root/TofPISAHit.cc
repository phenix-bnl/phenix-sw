
// $Id: TofPISAHit.cc,v 1.2 2007/11/13 22:27:49 hpereira Exp $

/*!
\file  TofPISAHit.cc
\brief container for time of flight pisa hits
\author  T. K. Ghosh
\version $Revision: 1.2 $
\date    $Date: 2007/11/13 22:27:49 $
*/

#include "TofPISAHit.h"

ClassImp(TofPISAHit)


using namespace std;

//____________________________________________________________________________
vector<TofPISAHit> TofPISAHit::_hits;

//____________________________________________________________________________
TofPISAHit::TofPISAHit(
  Int_t argsubvol, Int_t argpanel, Int_t argcolumn, 
  Int_t argpslat, Int_t  argslat_seq, Int_t argpartl, Float_t argxm, Float_t argym, 
  Float_t argzm, Float_t argpos_hit_slat, Float_t argpxm, Float_t argpym, Float_t argpzm, 
  Float_t argtof, Float_t argdele, Int_t argtrack, Int_t argmctrack,
  Int_t argisubevent, Int_t argnfile)
{
  subvol = argsubvol;
  panel = argpanel;
  column = argcolumn;
  pslat = argpslat;
  slat_seq = argslat_seq;
  partl = argpartl;
  pos_m[0] = argxm;
  pos_m[1] = argym;
  pos_m[2] = argzm;
  pos_hit_slat = argpos_hit_slat;
  p_m[0] = argpxm;
  p_m[1] = argpym;
  p_m[2] = argpzm;
  dele = argdele;
  tof = argtof;
  track = argtrack;
  mctrack = argmctrack;
  isubevent = argisubevent;
  nfile = argnfile;
  
}
