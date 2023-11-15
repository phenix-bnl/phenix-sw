// $Id: AerPISAHit.cc,v 1.3 2007/11/13 22:27:42 hpereira Exp $

/*!
  \file  AerPISAHit.cc
  \brief container for pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:42 $
*/

#include "AerPISAHit.h"

ClassImp(AerPISAHit)

using namespace std;

//____________________________________________________________________________
vector<AerPISAHit> AerPISAHit::_hits;
  
//____________________________________________________________________________
AerPISAHit::AerPISAHit()
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
  pathLength = 0;
  tof = 0;
  charge = 0;
  stepLength = 0;
  etot = 0;
  momentum = 0;
  vertxyz[0] = 0;
  vertxyz[1] = 0;
  vertxyz[2] = 0;
  isubevent = 0;
  nfile = 0;
  
}
  
//____________________________________________________________________________
AerPISAHit::AerPISAHit(
  Float_t argxyzglobal[], Float_t argpmomxyz[],
  Float_t argdele, Int_t argtrack, Int_t arglayer, Int_t argaerID,
  Float_t argpathLength, Float_t argtof, Float_t argstepLength,
  Float_t argetot, Float_t argcharge, Float_t argmomentum,
  Float_t argvertxyz[], Int_t argisubevent, Int_t argmctrack,
  Int_t argnfile)
{
  mctrack=argmctrack;
  xyzglobal[0]  = argxyzglobal[0];
  xyzglobal[1]  = argxyzglobal[1];
  xyzglobal[2]  = argxyzglobal[2];
  pmomxyz[0] = argpmomxyz[0];
  pmomxyz[1] = argpmomxyz[1];
  pmomxyz[2] = argpmomxyz[2];
  idPart = argaerID;
  dele = argdele;
  track = argtrack;
  layer = arglayer;
  pathLength = argpathLength;
  tof = argtof;
  charge = argcharge;
  stepLength = argstepLength;
  etot = argetot;
  momentum = argmomentum;
  vertxyz[0] = argvertxyz[0];
  vertxyz[1] = argvertxyz[1];
  vertxyz[2] = argvertxyz[2];
  isubevent = argisubevent;
  nfile = argnfile;
}
