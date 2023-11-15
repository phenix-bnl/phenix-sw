// $Id: RxnPISAHit.cc,v 1.4 2007/11/13 22:27:48 hpereira Exp $

/*!
\file  RxnPISAHit.cc
\brief container for reaction plane detector pisa hits
\author  C. F. Maguire
\version $Revision: 1.4 $
\date    $Date: 2007/11/13 22:27:48 $
*/

#include "RxnPISAHit.h"

ClassImp(RxnPISAHit)

using namespace std;

//____________________________________________________________________________
vector<RxnPISAHit> RxnPISAHit::_hits;

//____________________________________________________________________________
RxnPISAHit::RxnPISAHit( void )
{
  
  for (int i = 0; i < 3;i++)
  {
    xyzinloc[i] = 0;
    xyzoutloc[i] = 0;
    xyzinglo[i] = 0;
  }
  
  pmomxyz[0] = 0;
  pmomxyz[1] = 0; 
  pmomxyz[2] = 0;
  
  mctrack = 0;
  id = 0;
  tof = 0;
  dedx = 0;
  pathLength = 0;
  track = 0;
  arm = 0;
  isubevent = 0;
  nfile = 0;
  return;
}
  
//____________________________________________________________________________
RxnPISAHit::RxnPISAHit(
  Float_t argxyzinloc[], Float_t argxyzoutloc[], Float_t argxyzinglo[], Float_t argpmomxyz[],
  Float_t argtof, Float_t argdedx, Float_t argpathLength, Int_t argtrack,
  Int_t argarm, Int_t argid,
  Int_t argisubevent,  Int_t argmctrack, Int_t argnfile)
{
  
  for (int i = 0; i < 3;i++)
  {
    xyzinloc[i] = argxyzinloc[i];
    xyzoutloc[i] = argxyzoutloc[i];
    xyzinglo[i] = argxyzinglo[i];
  }
  
  pmomxyz[0]     = argpmomxyz[0];
  pmomxyz[1]     = argpmomxyz[1];
  pmomxyz[2]     = argpmomxyz[2];
  
  mctrack = argmctrack;
  id = argid;
  tof = argtof;
  dedx = argdedx;
  pathLength = argpathLength;
  track = argtrack;
  arm = argarm;
  isubevent = argisubevent;
  nfile = argnfile;
  return;
}
