// $Id: EmcPISAHit.cc,v 1.2 2007/11/13 22:27:44 hpereira Exp $

/*!
  \file  EmcPISAHit.h
  \brief container for EMCal pisa hits
  \author  T. K. Ghosh
  \version $Revision: 1.2 $
  \date    $Date: 2007/11/13 22:27:44 $
*/
#include "EmcPISAHit.h"

using namespace std;


ClassImp(EmcPISAHit)

//____________________________________________________________________________
vector<EmcPISAHit> EmcPISAHit::_hits;
  
//____________________________________________________________________________ 
EmcPISAHit::EmcPISAHit(
  Int_t argi1, Float_t argdele, Float_t argposx, Float_t argposy,
  Float_t argposz, Float_t argtof, Int_t argindex1, Int_t argindex2, 
  Int_t argnumed, Int_t argadd1, Int_t argadd2, Int_t argtrack, 
  Int_t argmctrack, Int_t argisubevent, Int_t argnfile)
{

  i1 = argi1;
  dele = argdele;
  posx = argposx;
  posy = argposy;
  posz = argposz;
  tof = argtof;
  index1 = argindex1;
  index2 = argindex2;
  numed = argnumed;
  add1 = argadd1; 
  add2 = argadd2; 
  track = argtrack;
  isubevent = argisubevent;
  mctrack = argmctrack;
  nfile = argnfile;

}



