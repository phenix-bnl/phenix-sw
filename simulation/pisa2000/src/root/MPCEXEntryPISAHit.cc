
#include "MPCEXEntryPISAHit.h"

ClassImp(MPCEXEntryPISAHit)

using namespace std;

//______________________________________________________________________
vector<MPCEXEntryPISAHit> MPCEXEntryPISAHit::_hits;
int MPCEXEntryPISAHit::_count = 0;

//______________________________________________________________________
MPCEXEntryPISAHit::MPCEXEntryPISAHit(
  Float_t argvx, Float_t argvy, Float_t argvz, 
  Float_t argpx, Float_t argpy, Float_t argpz,
  Int_t argisubevent, Int_t argtrack, Int_t argnfile):
  
  vx( argvx ),
  vy( argvy ),
  vz( argvz ),
  px( argpx ),
  py( argpy ),
  pz( argpz ),
  track( argtrack ),
  isubevent( argisubevent ),
  nfile( argnfile ),
  mctrack( -1 )
{
}
