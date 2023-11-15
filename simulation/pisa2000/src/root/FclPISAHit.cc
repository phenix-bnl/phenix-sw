
// $Id: FclPISAHit.cc,v 1.2 2007/11/13 22:27:44 hpereira Exp $

/*!
\file  FclPISAHit.cc
\brief container for Forward calorimeter pisa hits
\author  T. K. Ghosh
\version $Revision: 1.2 $
\date    $Date: 2007/11/13 22:27:44 $
*/

#include "FclPISAHit.h"

ClassImp(FclPISAHit)

using namespace std;

//____________________________________________________________________________
vector<FclPISAHit> FclPISAHit::_hits;
  
//____________________________________________________________________________
FclPISAHit::FclPISAHit( void )
{
  mctrack = 0;
  xyzglobal[0]  = 0;
  xyzglobal[1]  = 0;
  xyzglobal[2]  = 0;
  pmomxyz[0] = 0;
  pmomxyz[1] = 0;
  pmomxyz[2] = 0;
  idPart = 0;
  dele = 0;
  track = 0;
  layer = 0;
  isubevent = 0;
  nfile = 0;

}

//____________________________________________________________________________
FclPISAHit::FclPISAHit(
  Float_t argxyzglobal[], Float_t argpmomxyz[],
  Float_t argdele, Int_t argtrack, Int_t arglayer, Int_t argfclID,
  Int_t argisubevent,  Int_t argmctrack, Int_t argnfile)
{
  mctrack=argmctrack;
  xyzglobal[0]  = argxyzglobal[0];
  xyzglobal[1]  = argxyzglobal[1];
  xyzglobal[2]  = argxyzglobal[2];
  pmomxyz[0] = argpmomxyz[0];
  pmomxyz[1] = argpmomxyz[1];
  pmomxyz[2] = argpmomxyz[2];
  idPart = argfclID;
  dele = argdele;
  track = argtrack;
  layer = arglayer;
  isubevent = argisubevent;
  nfile = argnfile;

}
