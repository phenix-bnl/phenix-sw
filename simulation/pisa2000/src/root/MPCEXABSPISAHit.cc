
#include "MPCEXABSPISAHit.h"

ClassImp(MPCEXABSPISAHit)

using namespace std;

//______________________________________________________________________
vector<MPCEXABSPISAHit> MPCEXABSPISAHit::_hits;
int MPCEXABSPISAHit::_ncc1_count = 0;
int MPCEXABSPISAHit::_ncc2_count = 0;

//______________________________________________________________________
MPCEXABSPISAHit::MPCEXABSPISAHit(
  Int_t argevnt, Int_t argincc,  
  Float_t argdedx, 
  Int_t argisubevent, Int_t argtrack, Int_t argnfile):
  
  evnt( argevnt ),
  id(0),
  arm( argincc == 1 ? 1:0 ),
  mctrack(0),
  dedx( argdedx ),
  track( argtrack ),
  incc( argincc ),
  isubevent( argisubevent ),
  nfile( argnfile )
{
}
