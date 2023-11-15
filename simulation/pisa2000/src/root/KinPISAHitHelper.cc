
// $Id: KinPISAHitHelper.cc,v 1.4 2014/12/02 23:39:21 snowball Exp $

/*!
\file  KinPISAHitHelper.cc
\brief Helper class used to associate unique track id for full pisa event, and keep track of ancestry information
\author  H. Pereira
\version $Revision: 1.4 $
\date    $Date: 2014/12/02 23:39:21 $
*/

#include <algorithm>
#include <cassert>
#include <cmath>
#include <map>
#include <iostream>

#include "KinPISAHitHelper.h"
#include "KinPISAHit.h"

using namespace std;


//______________________________________________________________________________
bool KinPISAHitHelper::SameContentsFTor::operator() (KinPISAHit* first, KinPISAHit* second ) const
{
  
  assert( first && second );
  
  return
    first->GetIsubevent() == second->GetIsubevent()&&
    first->GetNtrack() == second->GetNtrack() && 
    first->GetIdpart() == second->GetIdpart() && 
    first->GetPtot() == second->GetPtot() && 
    first->GetPtot() == second->GetPtot() && 
    first->GetPhi() == second->GetPhi() && 
    first->GetRvertex() == second->GetRvertex() && 
    first->GetZvertex() == second->GetZvertex() && 
    first->GetThvertx() == second->GetThvertx() && 
    first->GetPhvertx() == second->GetPhvertx();

  
}

//______________________________________________________________________________
KinPISAHitHelper::KinPISAHitHelper( void ):
  _true_track_id( 0 )
  {}
  
//______________________________________________________________________________
void KinPISAHitHelper::reset( void )
{ 
  _true_track_id = 0; 
  _true_track_map.clear();
}

//______________________________________________________________________________
void KinPISAHitHelper::add( TClonesArray& array, int n_hits, int file_id )
{
    
  KinPISAHitSet local_hits;
  
  // loop over all hits in array; store in set
  for( int i=0; i<n_hits; i++)
  {
    
    KinPISAHit *local_hit_ptr = static_cast<KinPISAHit*>(array.UncheckedAt(i));
    
    // assign file id
    local_hit_ptr->SetNfile( file_id );    
      
    // insert current hit in set, check the return value
    // this is a sanity check to make sure that no hit is lost
    local_hits.insert( local_hit_ptr );
    
  }
  
  // at this point all tracks have been added in set, and sorted.
  // one can assign true track id
  for( KinPISAHitSet::iterator iter = local_hits.begin(); iter != local_hits.end(); iter++ )
  {
    
    // assign true track to hit
    (*iter)->SetTrue_track( _true_track_id+1 );
    
    // also insert in "local" map, to speed up future look-up of true track from track 
    // geant unique id
    _true_track_map.insert( std::make_pair( 
      GeantId( (*iter)->GetNfile(), (*iter)->GetIsubevent(), (*iter)->GetNtrack() ), 
      (*iter)->GetTrue_track() ) );
    
    _true_track_id++;
  }
  
  // store number of tracks for this pisaEvent
  KinPISAHit::SetKinMaxTrack( _true_track_id );

  // loop over hits again and assign true track for duplicated hits in the array
  for( int i=0; i<n_hits; i++)
  {
    
    KinPISAHit *local_hit_ptr = (KinPISAHit*)array.UncheckedAt(i);
    
    // find matching hit in local set
    KinPISAHitSet::const_iterator iter( local_hits.find( local_hit_ptr ) );
    assert( iter != local_hits.end() );
    
    // copy true track id
    local_hit_ptr->SetTrue_track( (*iter)->GetTrue_track() );
    
    // find hit matching parent ID, if not zero (which means the particle is a primary
    if( local_hit_ptr->GetItparent() != 0 )
    {
      
      KinPISAHitSet::const_iterator iter( find_if( local_hits.begin(), local_hits.end(), IsParentFTor( local_hit_ptr ) ) );
      //assert( iter != local_hits.end() );
      
      int sign = ( local_hit_ptr->GetItparent() > 0 ) ? 1:-1;
      if(iter != local_hits.end()) local_hit_ptr->SetItparent( (*iter)->GetTrue_track()*sign );
      else local_hit_ptr->SetItparent(0);
      
    }
    
  }

  // fill KinPISAHit array
  for( int i=0; i<n_hits; i++)
  { KinPISAHit::AddKinHit( *(KinPISAHit*)array.UncheckedAt(i) ); }

}

//______________________________________________________________________________
void KinPISAHitHelper::associate( void ) const
{

  // get all hits from KinPISAHit array
  KinPISAHit *KinHitPtr = KinPISAHit::GetKinHitEvt();  

  // store first KinHit matching a given "true track" in an array, indexed by true track
  std::vector<int> true_tracks( KinPISAHit::GetKinMaxTrack(), -1 );
  for( int kin = 0; kin < KinPISAHit::GetKinCount(); kin++)
  { 
    if( true_tracks[KinHitPtr[kin].GetTrue_track()-1] < 0 )
    { true_tracks[KinHitPtr[kin].GetTrue_track()-1] = kin; }
  }

  /* 
  fill 
  1/ kinTrkIndex, the index of the first KinHit index corresponding to a given track, using the true_tracks
  2/ KinTrkOrig, the index of the first KinHit index corresponding to the primary track of a given track
  */
  for( int trk=0; trk<KinPISAHit::GetKinMaxTrack(); trk++) 
  {
    
    //
    int index = true_tracks[trk];
    assert( index >= 0 );
    KinPISAHit::SetKinTrkIndex( trk, index );
    
    // get corresponding pisa hit
    KinPISAHit *local( &KinHitPtr[index] );
    
    // navigate pisa hits to get the primary particle corresponding to this track
    while( local->GetItparent() > 0 )
    {
      
      // check whether current ancestor track is not already associated with primary
      // if this is case the same primary can be used for this track
      if( KinPISAHit::GetKinTrkOrigin()[local->GetTrue_track()-1] != 0 ) 
      {
        KinPISAHit::SetKinTrkOrigin( trk, KinPISAHit::GetKinTrkOrigin()[local->GetTrue_track()-1] );
        break;
      }
      
      index = true_tracks[ local->GetItparent() - 1 ];
      assert( index >= 0 );
      local = &KinHitPtr[index];
    }
    
    // set origin
    if( local->GetItparent() <= 0 ) KinPISAHit::SetKinTrkOrigin( trk, index );

  }
  
}

//______________________________________________________________________________
int KinPISAHitHelper::findTrueTrack( int file, int event, int track ) const
{
  TrueTrackMap::const_iterator iter( _true_track_map.find( GeantId( file, event, track ) ) );
  return (iter == _true_track_map.end() ) ? 0: iter->second;
}
