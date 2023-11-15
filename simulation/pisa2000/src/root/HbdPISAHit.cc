// $Id: HbdPISAHit.cc,v 1.3 2007/11/13 22:27:44 hpereira Exp $

/*!
\file  HbdPISAHit.h
\brief container for hadron blind detector pisa hits
\author  H. Pereira
\version $Revision: 1.3 $
\date    $Date: 2007/11/13 22:27:44 $
*/

#include "HbdPISAHit.h"

ClassImp(HbdPISAHit)

using namespace std;

//____________________________________________________________________________
vector<HbdPISAHit> HbdPISAHit::_hits;

//____________________________________________________________________________
HbdPISAHit::HbdPISAHit( void )
{
  
  mctrack = 0;
  xyzin[0]  = 0;
  xyzin[1]  = 0;
  xyzin[2]  = 0;
  xyzout[0]  = 0;
  xyzout[1]  = 0;
  xyzout[2]  = 0;
  pxyz[0] = 0;
  pxyz[1] = 0;
  pxyz[2] = 0;
  idPart = 0;
  dele = 0;
  track = 0;
  pathLength = 0;
  tof = 0;
  detector = 0;
  sector = 0;
  padrow = 0;
  detflag = 0;
  isubevent = 0;
  nfile = 0;

}

//____________________________________________________________________________
HbdPISAHit::HbdPISAHit(
  Float_t argxyzin[], Float_t argpxyz[],
  Float_t argtof, Int_t arghbdID, Int_t argtrack, 
  Float_t argxyzout[], Float_t argdele, 
  Float_t argpathLength, 
  Int_t argdetector, Int_t argsector, Int_t argpadrow,
  Int_t argdetflag,
		       Int_t argisubevent,  Int_t argmctrack, Int_t argnfile)
{
  mctrack = argmctrack;
  xyzin[0]  = argxyzin[0];
  xyzin[1]  = argxyzin[1];
  xyzin[2]  = argxyzin[2];
  xyzout[0]  = argxyzout[0];
  xyzout[1]  = argxyzout[1];
  xyzout[2]  = argxyzout[2];
  pxyz[0] = argpxyz[0];
  pxyz[1] = argpxyz[1];
  pxyz[2] = argpxyz[2];
  idPart = arghbdID;
  dele = argdele;
  track = argtrack;
  pathLength = argpathLength;
  tof = argtof;
  detector = argdetector;
  sector = argsector;
  padrow = argpadrow;
  detflag = argdetflag;
  isubevent = argisubevent;
  nfile = argnfile;

}
