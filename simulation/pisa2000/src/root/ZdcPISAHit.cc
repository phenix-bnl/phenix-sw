// $Id: ZdcPISAHit.cc,v 1.2 2007/11/13 22:27:49 hpereira Exp $

/*!
\file  ZdcPISAHit.h
\brief container for zero degree calorimeter pisa hits
\author  T. K. Ghosh
\version $Revision: 1.2 $
\date    $Date: 2007/11/13 22:27:49 $
*/

#include "ZdcPISAHit.h"

ClassImp(ZdcPISAHit)

using namespace std;

//____________________________________________________________________________
vector<ZdcPISAHit> ZdcPISAHit::_hits;
  
//____________________________________________________________________________
ZdcPISAHit::ZdcPISAHit(
  Float_t argxm,
  Float_t argym,
  Float_t argzm,
  Float_t argpxm,
  Float_t argpym,
  Float_t argpzm,
  Float_t argdele,
  Float_t argtof,
  Int_t   argpid,
  Int_t   argdir,
  Int_t   argmod,
  Int_t   argtrack,
  Int_t   argisubevent,
  Int_t   argmctrack,
  Int_t   argnfile)
{
  pos_m[0] = argxm;
  pos_m[1] = argym;
  pos_m[2] = argzm;
  dele     = argdele;
  p_m[0]   = argpxm;
  p_m[1]   = argpym;
  p_m[2]   = argpzm;
  tof      = argtof;
  partl       = argpid;
  north_south = argdir;
  module      = argmod;
  track       = argtrack;
  isubevent   = argisubevent;
  nfile       = argnfile;
  
}
