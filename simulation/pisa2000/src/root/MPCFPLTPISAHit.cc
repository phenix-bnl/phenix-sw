
#include "MPCFPLTPISAHit.h"
#include <iostream>

ClassImp(MPCFPLTPISAHit)

using namespace std;

//______________________________________________________________________
vector<MPCFPLTPISAHit> MPCFPLTPISAHit::_hits;
int MPCFPLTPISAHit::_ncc1_count = 0;
int MPCFPLTPISAHit::_ncc2_count = 0;

//______________________________________________________________________
MPCFPLTPISAHit::MPCFPLTPISAHit(
  Int_t argevnt, Int_t argincc,  
  Float_t argdedx, 
  Int_t argisubevent, Int_t argtrack, Int_t argnfile):
  
  evnt( argevnt ),
  id(0),
  arm( argincc ),
  mctrack(0),
  dedx( argdedx ),
  track( argtrack ),
  incc( argincc ),
  isubevent( argisubevent ),
  nfile( argnfile )
{
  if(argincc<10) 
    arm = (argincc==1) ? 1:0; 
  else if(argincc<20)
    arm = ((argincc%10)==1) ? 1:0; 
  else if(argincc<30)
    arm = ((argincc%20)==1) ? 1:0; 
  else if(argincc<40)
    arm = ((argincc%30)==1) ? 1:0; 
}
