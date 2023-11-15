// $Id: MutPISAHit.cc,v 1.3 2007/11/13 22:27:46 hpereira Exp $

/*!
  \file  MutPISAHit.cc
  \brief container for pisa hits
  \author  T. K. Ghosh, H. Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/11/13 22:27:46 $
*/

#include "TDirectory.h"
#include "MutPISAHit.h"

using namespace std;

ClassImp(MutPISAHit)

//_____________________________________________________
vector< MutPISAHit> MutPISAHit::_hits;

//_____________________________________________________________________________
MutPISAHit::MutPISAHit(Int_t argtrack, Int_t argplane, Short_t argpid, Float_t argt, 
  Float_t arge, Float_t argx, Float_t argy, Float_t argz, Float_t argpx, 
  Float_t argpy, Float_t argpz, Int_t argmctrack, Int_t argnfile, 
  Int_t argisubevent ): 
  track( argtrack ),
  plane( argplane ),
  pid( argpid ),
  t( argt ),
  e( arge ),
  mctrack( argmctrack ),
  nfile( argnfile ),
  isubevent( argisubevent )
{
  pos[0] = argx;
  pos[1] = argy;
  pos[2] = argz;
  mom[0] = argpx;
  mom[1] = argpy;
  mom[2] = argpz;  
}
