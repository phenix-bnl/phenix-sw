
// $Id: TfwPISAHit.cc,v 1.3 2007/11/13 22:27:49 hpereira Exp $

/*!
\file  TfwPISAHit.cc
\brief container for time of flight west pisa hits
\author  C. F. Maguire
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:49 $
*/

#include "TfwPISAHit.h"

ClassImp(TfwPISAHit)

using namespace std;


//____________________________________________________________________________
vector<TfwPISAHit> TfwPISAHit::_hits;

//____________________________________________________________________________
TfwPISAHit::TfwPISAHit( void )
{
  
  for (int i = 0; i < 3;i++)
  {
    xyzinloc[i] = 0;
    xyzoutloc[i] = 0;
    xyzinglo[i] = 0;
  }
  mctrack = 0;
  id = 0;
  tof = 0;
  dedx = 0;
  pathLength = 0;
  track = 0;
  panel = 0;
  isubevent = 0;
  nfile = 0;
  return;
}

//____________________________________________________________________________
TfwPISAHit::TfwPISAHit(
  Float_t argxyzinloc[], Float_t argxyzoutloc[], Float_t argxyzinglo[],
  Float_t argtof, Float_t argdedx, Float_t argpathLength, Int_t argtrack,
  Int_t argpanel, Int_t argid,
  Int_t argisubevent,  Int_t argmctrack, Int_t argnfile)
{
  
  for (int i = 0; i < 3;i++)
  {
    xyzinloc[i] = argxyzinloc[i];
    xyzoutloc[i] = argxyzoutloc[i];
    xyzinglo[i] = argxyzinglo[i];
  }
  mctrack = argmctrack;
  id = argid;
  tof = argtof;
  dedx = argdedx;
  pathLength = argpathLength;
  track = argtrack;
  panel = argpanel;
  isubevent = argisubevent;
  nfile = argnfile;
  return;
}









