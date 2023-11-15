// $Id: NCCPISAHit.cc,v 1.10 2007/11/13 22:27:46 hpereira Exp $

#include "NCCPISAHit.h"

ClassImp(NCCPISAHit)

using namespace std;

//______________________________________________________________________
vector<NCCPISAHit> NCCPISAHit::_hits;
int NCCPISAHit::_ncc1_count = 0;
int NCCPISAHit::_ncc2_count = 0;

//______________________________________________________________________
NCCPISAHit::NCCPISAHit(
  Int_t argevnt, Int_t argincc,  
  Int_t argtwrid, Int_t argsenid, 
  Float_t argtof, Float_t argdedx, 
  Int_t argisubevent, Int_t argtrack, Int_t argnfile):
  
  evnt( argevnt ),
  id(0),
  arm( argincc == 1 ? 1:0 ),
  mctrack(0),
  dedx( argdedx ),
  track( argtrack ),
  incc( argincc ),
  twrid( argtwrid ),
  senid( argsenid ),
  tof( argtof ),
  isubevent( argisubevent ),
  nfile( argnfile )
{}
