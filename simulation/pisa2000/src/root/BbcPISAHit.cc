// $Id: BbcPISAHit.cc,v 1.3 2007/11/13 22:27:43 hpereira Exp $

/*!
  \file  BbcPISAHit.cc
  \brief container for BBC pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:43 $
*/

#include "BbcPISAHit.h"

using namespace std;

ClassImp(BbcPISAHit)

//____________________________________________________________________________
vector<BbcPISAHit> BbcPISAHit::_hits;
  
//____________________________________________________________________________
BbcPISAHit::BbcPISAHit(
  Float_t argx,   Float_t argy,   Float_t argz,
  Float_t argpx,  Float_t argpy,  Float_t argpz,
  Float_t argdel, Float_t argtof, Float_t arglen,
  Short_t argpmt, Short_t argpid, Int_t argtrack, Int_t argisubevent,
  Int_t argmctrack, Int_t argnfile )
{
  posx = pos[0] = argx;
  posy = pos[1] = argy;
  posz = pos[2] = argz;

  momx = mom[0] = argpx;
  momy = mom[1] = argpy;
  momz = mom[2] = argpz;

  del = argdel;
  tof = argtof;
  len = arglen;

  pmt = argpmt;
  pid = argpid;
  track = argtrack;
  isubevent = argisubevent;
  mctrack = argmctrack;
  nfile = argnfile;
  
  return;
}
