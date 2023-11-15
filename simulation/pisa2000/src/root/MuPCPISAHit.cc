
// $Id: MuPCPISAHit.cc,v 1.3 2007/11/13 22:27:45 hpereira Exp $

/*!
\file  MuPCPISAHit.h
\brief container for muon pad chambers pisa hits
\author  W. Xie
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:45 $
*/

#include "MuPCPISAHit.h"

ClassImp(MuPCPISAHit)

using namespace std;

//______________________________________________________________________
vector<MuPCPISAHit> MuPCPISAHit::_hits;
int MuPCPISAHit::_mupc1_count = 0;
int MuPCPISAHit::_mupc2_count = 0;
int MuPCPISAHit::_mupc3_count = 0;

//______________________________________________________________________
MuPCPISAHit::MuPCPISAHit( void )
{
  mctrack = 0;
  xyzinloc[0]  = 0;
  xyzinloc[1]  = 0;
  xyzinloc[2]  = 0;
  xyzoutloc[0] = 0;
  xyzoutloc[1] = 0;
  xyzoutloc[2] = 0;
  id = 0;
  tof = 0;
  dedx = 0;
  xyzinglo[0]= 0;
  xyzinglo[1] = 0;
  xyzinglo[2]= 0;
  pathLength= 0;
  track = 0;
  arm = 0;
  ipc = 0;
  isubevent = 0;
  nfile = 0;
}

//______________________________________________________________________
MuPCPISAHit::MuPCPISAHit(
  Float_t argxyzinloc[], 
  Float_t argxyzoutloc[],
  Float_t argxyzinglo[], 
  Float_t argtof, 
  Float_t argdedx, 
  Float_t argpathLength, 
  Int_t argtrack, 
  Int_t argarm, 
  Int_t argid, 
  Int_t argipc, 
  Int_t argisubevent,  
  Int_t argmctrack, 
  Int_t argnfile)
{
  mctrack=argmctrack;
  xyzinloc[0]  = argxyzinloc[0];
  xyzinloc[1]  = argxyzinloc[1];
  xyzinloc[2]  = argxyzinloc[2];
  xyzoutloc[0] = argxyzoutloc[0];
  xyzoutloc[1] = argxyzoutloc[1];
  xyzoutloc[2] = argxyzoutloc[2];
  id = argid;
  tof = argtof;
  dedx = argdedx;
  xyzinglo[0]=argxyzinglo[0];
  xyzinglo[1]=argxyzinglo[1];
  xyzinglo[2]=argxyzinglo[2];
  pathLength=argpathLength;
  track = argtrack;
  arm = argarm;
  ipc = argipc;
  isubevent = argisubevent;
  nfile = argnfile;
}
