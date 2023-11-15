
// $Id: TecPISAHit.cc,v 1.2 2007/11/13 22:27:48 hpereira Exp $

/*!
\file  TecPISAHit.cc
\brief container for time expansion chamber pisa hits
\author  T. K. Ghosh
\version $Revision: 1.2 $
\date    $Date: 2007/11/13 22:27:48 $
*/

#include "TecPISAHit.h"

ClassImp(TecPISAHit)

using namespace std;


//____________________________________________________________________________
vector<TecPISAHit> TecPISAHit::_hits;
 
//____________________________________________________________________________
TecPISAHit::TecPISAHit( void )
{
  id = 0;
  iArm = 0;
  xyzinloc[0] = 0;
  xyzinloc[1] = 0;
  xyzinloc[2] = 0;
  tof = 0;
  xyzoutloc[0] = 0;
  xyzoutloc[1] = 0;
  xyzoutloc[2] = 0;
  plane = 0;
  mctrack = 0;
  sector = 0;
  dedx = 0;
  xyzinglo[0] = 0;
  xyzinglo[1] = 0;
  xyzinglo[2] = 0;

  track = 0;  // track in subevent
  isubevent = 0;
  nfile = 0;

} 
//____________________________________________________________________________
TecPISAHit::TecPISAHit(
  Int_t argmctrack, Float_t argxyzinloc[], 
  Float_t argxyzoutloc[], Float_t argtof, Int_t argsector, Float_t argdedx,
  Float_t argxyzinglo[], Int_t argiArm, Int_t argnfile, Int_t argid, 
  Int_t argplane,Int_t argisubevent, Int_t argtrack )
{
  id = argid;
  iArm = argiArm;
  xyzinloc[0] = argxyzinloc[0];
  xyzinloc[1] = argxyzinloc[1];
  xyzinloc[2] = argxyzinloc[2];
  tof = argtof;
  xyzoutloc[0] = argxyzoutloc[0];
  xyzoutloc[1] = argxyzoutloc[1];
  xyzoutloc[2] = argxyzoutloc[2];
  plane = argplane;
  mctrack = argmctrack;
  sector = argsector;
  dedx = argdedx;
  xyzinglo[0] = argxyzinglo[0];
  xyzinglo[1] = argxyzinglo[1];
  xyzinglo[2] = argxyzinglo[2];

  track = argtrack;  // track in subevent
  isubevent = argisubevent;
  nfile = argnfile;

}






