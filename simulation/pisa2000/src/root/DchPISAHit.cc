// $Id: DchPISAHit.cc,v 1.5 2007/11/13 22:27:44 hpereira Exp $

/*!
\file  DchPISAHit.cc
\brief container for drift chambers pisa hits
\author  T. K. Ghosh
\version $Revision: 1.5 $
\date    $Date: 2007/11/13 22:27:44 $
*/

#include "DchPISAHit.h"

using namespace std;

ClassImp(DchPISAHit)


//____________________________________________________________________________
vector<DchPISAHit> DchPISAHit::_hits;
  
//____________________________________________________________________________
DchPISAHit::DchPISAHit( void )
{
  
  for (int i = 0; i < 3; i++)
  {
    xyzinloc[i] = 0;
    xyzoutloc[i] = 0;
    xyzinglo[i] = 0;
  }
  tof = 0;
  plane = 0;
  cell = 0;
  pathLength = 0;
  track = 0; 
  isubevent = 0;
  iArm = 0;
  id = 0;
  mctrack = 0;
  nfile = 0;
  return;
}

//____________________________________________________________________________
DchPISAHit::DchPISAHit(
  Float_t argxyzinloc[], Float_t argtof, Float_t argxyzoutloc[],
  Int_t argplane, Int_t argcell,  Float_t argxyzinglo[],
  Float_t argpathLength, Int_t argtrack, Int_t argisubevent,
  Int_t argiArm, Int_t argid, Int_t argmctrack, Int_t argnfile)
{
  for (int i = 0; i < 3; i++)
  {
    xyzinloc[i] = argxyzinloc[i];
    xyzoutloc[i] = argxyzoutloc[i];
    xyzinglo[i] = argxyzinglo[i];
  }
  tof= argtof;
  plane = argplane;
  cell = argcell;
  pathLength = argpathLength;
  track = argtrack;  // track in subevent
  isubevent = argisubevent;
  iArm = argiArm;
  id = argid;
  mctrack = argmctrack;
  nfile = argnfile;
  return;
}

