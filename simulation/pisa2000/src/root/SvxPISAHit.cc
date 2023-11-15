// $Id: SvxPISAHit.cc,v 1.4 2011/02/08 22:44:04 hubert Exp $

/*!
  \file  SvxPISAHit.cc
  \brief container for pisa hits
  \author  V. L. Rykov, H. Van Hecke
  \version $Revision: 1.4 $
  \date    $Date: 2011/02/08 22:44:04 $
*/

#include "SvxPISAHit.h"

using namespace std;

ClassImp(SvxPISAHit)
  
//____________________________________________________________________________
vector<SvxPISAHit> SvxPISAHit::_hits;
  
//____________________________________________________________________________
SvxPISAHit::SvxPISAHit( void )
{
  mctrack = 0;
  xyzglobal[0] = 0;
  xyzglobal[1] = 0;
  xyzglobal[2] = 0;
  pmomxyz[0] = 0;
  pmomxyz[1] = 0;
  pmomxyz[2] = 0;
  idPart = 0;
  dele = 0;
  timeOfFlight = 0;

  xyzlocalIn[0] = 0;
  xyzlocalIn[1] = 0;
  xyzlocalIn[2] = 0;
  xyzlocalOut[0] = 0;
  xyzlocalOut[1] = 0;
  xyzlocalOut[2] = 0;

  xyzglobalIn[0] = 0;
  xyzglobalIn[1] = 0;
  xyzglobalIn[2] = 0;
  xyzglobalOut[0] = 0;
  xyzglobalOut[1] = 0;
  xyzglobalOut[2] = 0;

  hitVolume[0] = 0;
  hitVolume[1] = 0;
  hitVolume[2] = 0;
  hitVolume[3] = 0;
  hitVolume[4] = 0;
  hitVolume[5] = 0;
  hitVolume[6] = 0;
  hitVolume[7] = 0;
  hitVolume[8] = 0;
  track = 0;
  layer = 0;
  isubevent = 0;
  nfile = 0;
}

//____________________________________________________________________________
SvxPISAHit::SvxPISAHit(
  Float_t argxyzglobal[], Float_t argpmomxyz[],
  Float_t argdele, Float_t argtimeOfFlight,
  Float_t argxyzlocalIn[],  Float_t argxyzlocalOut[],
  Float_t argxyzglobalIn[], Float_t argxyzglobalOut[],
  Int_t arghitVolume[],
  Int_t argtrack, Int_t arglayer, Int_t argsiliID,
  Int_t argisubevent,  Int_t argmctrack, Int_t argnfile)
{
  mctrack = argmctrack;
  xyzglobal[0] = argxyzglobal[0];
  xyzglobal[1] = argxyzglobal[1];
  xyzglobal[2] = argxyzglobal[2];
  pmomxyz[0] = argpmomxyz[0];
  pmomxyz[1] = argpmomxyz[1];
  pmomxyz[2] = argpmomxyz[2];
  idPart = argsiliID;
  dele = argdele;
  timeOfFlight = argtimeOfFlight;

  xyzlocalIn[0] = argxyzlocalIn[0];
  xyzlocalIn[1] = argxyzlocalIn[1];
  xyzlocalIn[2] = argxyzlocalIn[2];
  xyzlocalOut[0] = argxyzlocalOut[0];
  xyzlocalOut[1] = argxyzlocalOut[1];
  xyzlocalOut[2] = argxyzlocalOut[2];

  xyzglobalIn[0] = argxyzglobalIn[0];
  xyzglobalIn[1] = argxyzglobalIn[1];
  xyzglobalIn[2] = argxyzglobalIn[2];
  xyzglobalOut[0] = argxyzglobalOut[0];
  xyzglobalOut[1] = argxyzglobalOut[1];
  xyzglobalOut[2] = argxyzglobalOut[2];

  hitVolume[0] = arghitVolume[0];
  hitVolume[1] = arghitVolume[1];
  hitVolume[2] = arghitVolume[2];
  hitVolume[3] = arghitVolume[3];
  hitVolume[4] = arghitVolume[4];
  hitVolume[5] = arghitVolume[5];
  hitVolume[6] = arghitVolume[6];
  hitVolume[7] = arghitVolume[7];
  hitVolume[8] = arghitVolume[8];
  track = argtrack;
  layer = arglayer;
  isubevent = argisubevent;
  nfile = argnfile;
}
