// $Id: PriPISAHit.cc,v 1.2 2007/11/13 22:27:47 hpereira Exp $

/*!
  \file  PriPISAHit.cc
  \brief container for primary particles
  \author  T. K. Ghosh
  \version $Revision: 1.2 $
  \date    $Date: 2007/11/13 22:27:47 $
*/

#include "PriPISAHit.h"

ClassImp(PriPISAHit)

using namespace std;

//____________________________________________________________________________
vector<PriPISAHit> PriPISAHit::_hits;
  
//____________________________________________________________________________
PriPISAHit::PriPISAHit(
  Int_t argtrue_track, Int_t argisubevent,  Int_t argntrack, Int_t argidpart,
  Float_t argpx, Float_t argpy, Float_t argpz, Int_t argevttrack, 
  Int_t argnfile)
{
  evttrack = argevttrack;
  idpart = argidpart;
  px = argpx;
  py = argpy;
  pz = argpz;
  ntrack = argntrack;
  isubevent = argisubevent;
  true_track = argtrue_track;
  nfile = argnfile;

}

		 



