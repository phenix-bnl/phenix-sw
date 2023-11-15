// $Id: PadPISAHit.cc,v 1.3 2007/11/13 22:27:47 hpereira Exp $

/*!
\file  PadPISAHit.h
\brief container for pad chambers pisa hits
\author  T. K. Ghosh
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:47 $
*/

#include "PadPISAHit.h"

ClassImp(PadPISAHit)

using namespace std;

//______________________________________________________________________
vector<PadPISAHit> PadPISAHit::_hits;
int PadPISAHit::_pc1_count = 0;
int PadPISAHit::_pc2_count = 0;
int PadPISAHit::_pc3_count = 0;

//______________________________________________________________________
PadPISAHit::PadPISAHit( void )

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
  arm = 0;
  sector = 0;
  ipc = 0;
  isubevent = 0;
  nfile = 0;
  
  return;
}

//______________________________________________________________________
PadPISAHit::PadPISAHit(
  Float_t argxyzinloc[], Float_t argxyzoutloc[], Float_t argxyzinglo[],
  Float_t argtof, Float_t argdedx, Float_t argpathLength, Int_t argtrack,
  Int_t argarm, Int_t argsector, Int_t argid, Int_t argipc,
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
  arm = argarm;
  sector = argsector;
  ipc = argipc;
  isubevent = argisubevent;
  nfile = argnfile;
  return;
}
