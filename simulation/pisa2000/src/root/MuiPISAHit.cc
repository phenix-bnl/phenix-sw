// $Id: MuiPISAHit.cc,v 1.2 2007/11/13 22:27:45 hpereira Exp $

/*!
  \file  MuiPISAHit.cc
  \brief container for pisa hits
  \author  T. K. Ghosh, H. Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2007/11/13 22:27:45 $
*/

#include "MuiPISAHit.h"
using namespace std;

ClassImp(MuiPISAHit)

//_____________________________________________________
vector< MuiPISAHit> MuiPISAHit::_hits;

//________________________________________________________________
MuiPISAHit::MuiPISAHit( void ):
  itrksub( 0 ),
  plane_num( 0 ),
  trk_id( 0 ),
  tof( 0 ),
  de( 0 ),
  mctrack( 0 ),
  nfile( 0 ),
  isubevent( 0 )
{
  rhit[0] = 0;
  rhit[1] = 0;
  rhit[2] = 0;
  phit[0] = 0;
  phit[1] = 0;
  phit[2] = 0;
}

//________________________________________________________________
MuiPISAHit::MuiPISAHit(Int_t argitrksub, Int_t argplane_num, 
  Int_t argtrk_id, Float_t argtof, Float_t argde, Float_t argrhit[],
  Float_t argphit[], Int_t argmctrack, Int_t argnfile,
  Int_t argisubevent ):
  itrksub( argitrksub ),
  plane_num( argplane_num ),
  trk_id( argtrk_id ),
  tof( argtof ),
  de( argde ),
  mctrack( argmctrack ),
  nfile( argnfile ),
  isubevent( argisubevent )
{
  rhit[0] = argrhit[0];
  rhit[1] = argrhit[1];
  rhit[2] = argrhit[2];
  phit[0] = argphit[0];
  phit[1] = argphit[1];
  phit[2] = argphit[2];
}






















