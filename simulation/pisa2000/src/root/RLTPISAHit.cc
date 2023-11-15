// $Id: RLTPISAHit.cc,v 1.2 2007/11/13 22:27:47 hpereira Exp $

/*!
\file  RLTPISAHit.h
\brief container for relative luminosity telescope pisa hits
\author  T. K. Ghosh
\version $Revision: 1.2 $
\date    $Date: 2007/11/13 22:27:47 $
*/

#include "RLTPISAHit.h"

ClassImp(RLTPISAHit)

using namespace std;

//______________________________________________________________________
vector<RLTPISAHit> RLTPISAHit::_hits;
int RLTPISAHit::_rpc1_count = 0;
int RLTPISAHit::_rpc2_count = 0;
int RLTPISAHit::_rpc3_count = 0;

//______________________________________________________________________
RLTPISAHit::RLTPISAHit( void )
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
  xyzinglo[0] = 0;
  xyzinglo[1] = 0;
  xyzinglo[2] = 0;
  pathLength = 0;
  track = 0;
  irpc = 0;
  isubevent = 0;
  nfile = 0;
}

//______________________________________________________________________
RLTPISAHit::RLTPISAHit(Float_t argxyzinloc[], Float_t argxyzoutloc[], 
			 Float_t argxyzinglo[], Float_t argtof, Float_t argdedx,
			 Float_t argpathLength, Int_t argtrack, Int_t argid, Int_t argirpc,
			 Int_t argisubevent, Int_t argmctrack, Int_t argnfile)
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
  //  arm = argarm;
  irpc = argirpc;
  isubevent = argisubevent;
  nfile = argnfile;
}
