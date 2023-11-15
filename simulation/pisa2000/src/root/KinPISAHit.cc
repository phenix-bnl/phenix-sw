
// $Id: KinPISAHit.cc,v 1.8 2013/03/05 13:25:56 bbannier Exp $

/*!
\file  KinPISAHit.cc
\brief container for pisa hits
\author  T. K. Ghosh, H. Pereira
\version $Revision: 1.8 $
\date    $Date: 2013/03/05 13:25:56 $
*/

#include <algorithm>
#include <cmath>
#include <iostream>
#include "KinPISAHit.h"

using namespace std;

ClassImp(KinPISAHit)

//_____________________________________________________________________________
vector<KinPISAHit> KinPISAHit::_KinHitEvt;
vector<Int_t> KinPISAHit::_KinTrkIndex; 
vector<Int_t> KinPISAHit::_KinTrkOrigin; 

double DEG_TO_RAD = M_PI/180.0;

//_____________________________________________________________________________
KinPISAHit::KinPISAHit(
  Int_t argtrue_track, Int_t argisubevent,  Int_t argntrack,
  Int_t argidpart, Float_t argptot, Float_t argpthet,
  Float_t argpphi, Float_t argr_vertex, Float_t argz_vertex,
  Float_t argth_vertx, Float_t argph_vertx, Int_t argitparent,
  Int_t argidparent, Int_t argnfile):
  
  true_track( argtrue_track ),
  isubevent( argisubevent ),
  ntrack( argntrack ),
  evttrack( -1 ),
  idpart( argidpart ),
  ptot( argptot ),
  pthet( argpthet ),
  pphi( argpphi ),
  r_vertex( argr_vertex ),
  z_vertex( argz_vertex ),
  th_vertx( argth_vertx ),
  ph_vertx( argph_vertx ),
  itparent( argitparent ),
  idparent( argidparent ),
  nfile( argnfile )
  
{}

//_____________________________________________________________________________
Float_t KinPISAHit::GetPx() const 
{return GetPtot()*std::cos( DEG_TO_RAD*GetPhi() )*std::sin( DEG_TO_RAD*GetPthet() ); }

//_____________________________________________________________________________
Float_t KinPISAHit::GetPy() const 
{return GetPtot()*std::sin( DEG_TO_RAD*GetPhi() )*std::sin( DEG_TO_RAD*GetPthet() ); }

//_____________________________________________________________________________
Float_t KinPISAHit::GetPz() const 
{return GetPtot()*std::cos( DEG_TO_RAD*GetPthet() ); }

//_____________________________________________________________________________
Float_t KinPISAHit::GetXvertex() const 
{return GetRvertex()*std::cos( DEG_TO_RAD*GetPhvertx() ); }

//_____________________________________________________________________________
Float_t KinPISAHit::GetYvertex() const 
{return GetRvertex()*std::sin( DEG_TO_RAD*GetPhvertx() ); }

//_____________________________________________________________________________
void KinPISAHit::PrintAssociations( void )
{
  
  cout << "KinPISAHit::PrintAssociations" << endl;
  
  // get hits from array
  KinPISAHit *KinHitPtr = KinPISAHit::GetKinHitEvt();  
  for( int kin = 0; kin < KinPISAHit::GetKinCount(); kin++)
  { cout << " index: " << kin << " " << KinHitPtr[kin] << endl; }
  
  if( false )
  {
    for( unsigned int i=0; i < _KinTrkIndex.size(); i++ )
    { cout << "  _KinTrkIndex[" << i << "] = " << _KinTrkIndex[i] << endl; }
    cout << endl;
    
    for( unsigned int i=0; i < _KinTrkOrigin.size(); i++ )
    { cout << "  _KinTrkOrigin[" << i << "] = " << _KinTrkOrigin[i] << endl; }
    cout << endl;
  }
  
  return;
  
}

//_____________________________________________________________________________
std::ostream& operator << ( std::ostream& out, const KinPISAHit& hit )
{
  
  out 
    << "file: " << hit.GetNfile() 
    << " subevent: " << hit.GetIsubevent() 
    << " geant track: " << hit.GetNtrack() 
    << " true track: " << hit.GetTrue_track() 
    << " parent: " << hit.GetItparent();

  return out;
}

//_____________________________________________________________________________
KinPISAHit* KinPISAHit::Find( int true_track )
{
  assert( true_track-1 >= 0 );
  assert( true_track-1 < int(_KinTrkIndex.size()) );
  int index( _KinTrkIndex[true_track-1] );

  assert( index < int(_KinHitEvt.size()) );
  return &_KinHitEvt[index];
}

//_____________________________________________________________________________
KinPISAHit* KinPISAHit::FindOrigin( int true_track )
{
  assert( true_track-1 >= 0 );
  assert( true_track-1 < int(_KinTrkOrigin.size()) );
  int index( _KinTrkOrigin[true_track-1] );

  assert( index < int(_KinHitEvt.size()) );
  return &_KinHitEvt[index];
}
