//////////////////////////////////////////////////////////////////
//
// Utility class: mFvtxRejectTrack
// Author: S.Kelly 
// Date: 5/09/03
// Description: 
//              
//////////////////////////////////////////////////////////////////

// FVTXOO headers
#include <mFvtxRejectTrack.h>
#include <mFvtxRejectTrackPar.h>
#include <TMutNode.h>
#include <PHException.h>
#include <FVTXOO.h>
#include <PHTimer.h>
#include <TFvtxCoordMap.h>
#include <TMutTrackUtil.h>

// PHENIX headers
#include <PHGeometry.h>
#include <PHVector.h>

/*! \ingroup modules */

// STL/BOOST
#include <iostream>
#include <string>
#include <list>
#include <algorithm>

using namespace std;

//_____________________________________________________________
mFvtxRejectTrack::mFvtxRejectTrack() : 
  _mod_par(0),
  _trk_map(0),
  _coord_map(0),
  _total_tracks(0),
  _accepted_tracks(0),
  _timer(PHTimeServer::get()->insert_new("mFvtxRejectTrack"))
{
  FVTXOO::TRACE("initializing module mFvtxRejectTrack");
}

//_____________________________________________________________
// Event method.
PHBoolean mFvtxRejectTrack::event(PHCompositeNode* top_node, unsigned short octant){
  bool out( event(top_node) );
  return out;
}

//_____________________________________________________________
PHBoolean mFvtxRejectTrack::event(PHCompositeNode* top_node)
{
  
  _timer.get()->restart(); 
  
  try { 
    
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // update counter on total number of tracks
    _total_tracks += _trk_map->size();

    // Tag ghost 
    tag_ghosts();    

    // update counter on number of accepted tracks
    TFvtxTrkMap::iterator iter( _trk_map->range() );
    while( TFvtxTrkMap::pointer ptr = iter.next() )
    if( !ptr->get()->get_ghost() )
    _accepted_tracks ++;

    // Remove outliers from remaining tracks if enabled
    if(_mod_par->get_reject_outliers()) reject_outliers();

  } catch(exception& e) {    
    FVTXOO::TRACE(e.what());
    return False;    
  }  
  
  // Timer
  _timer.get()->stop();
  if( _mod_par->get_verbosity() >= FVTXOO::SOME ) _timer.get()->print(); 
  if( _mod_par->get_verbosity() >= FVTXOO::ALOT ) _trk_map->print(); 
  return True;
}

//_____________________________________________________________
void mFvtxRejectTrack::print_summary( ostream& out )
{

  FVTXOO::PRINT( out, "mFvtxRejectTrack::print_summary" );
  cout << "_total_tracks=" << _total_tracks << endl;
  cout << "_accepted_tracks=" << _accepted_tracks << endl;
  FVTXOO::PRINT( out, "**" );

}

//_____________________________________________________________
void mFvtxRejectTrack::set_interface_ptrs(PHCompositeNode* top_node)
{  
  
  // module runtime parameters
  //
  _mod_par = TMutNode<mFvtxRejectTrackPar>::find_node(top_node,"mFvtxRejectTrackPar");
  
  // SET IOC POINTERS 
  //
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
} 

//____________________________________________________________________
void mFvtxRejectTrack::tag_ghosts()
{  
  int nhits_i, nhits_j;
  float chi_i, chi_j, rqual;

  // Loop over tracks [
  //  Second loop over other track [
  //    Count number of hits that are common to each track at each station
  //      and put into local list if the number in common pass your particular threshold
  //    Sort in order of increasing chi_square
  //    Keep best track according to chi-square and nhits comparisons and tag others as ghosts
  //  ]
  // ]
  typedef list<TFvtxTrkMap::pointer> track_list;

  TFvtxTrkMap::iterator trk_iter = _trk_map->range();

  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next()) 
  {

    // check if first track is worth testing

    // if track is already a ghost continue
    if (trk_ptr->get()->get_ghost()) continue;

    // if track could not be fitted with kalman filter or with the bend plane fit stations 1/2/3) tag as ghost and continue
    if( !(trk_ptr->get()->get_reco_success() ))
      {
	trk_ptr->get()->set_ghost();
	continue;
      }

    // check there are tracks to check after current
    if( !trk_iter.current()) continue;

/*    double phimin1 = trk_ptr->get()->get_phi_min(FVTXOO::Station4, 0);
    double phimax1 = trk_ptr->get()->get_phi_max(FVTXOO::Station4, 0);
    double thetamin1 = trk_ptr->get()->get_theta_min(FVTXOO::Station4, 0);
    double thetamax1 = trk_ptr->get()->get_theta_max(FVTXOO::Station4, 0); */

    // get the number of coords / station for this trk
    TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
    
    // Loop over other tracks in track list and count number of hits in common
    // If track has already been flagged as a ghost or has not been fully
    // reconstructed, then skip it.    
    track_list local_list;
    local_list.push_back(trk_ptr);
    TFvtxTrkMap::iterator trk_iter_2 = trk_iter;
    while(TFvtxTrkMap::pointer trk_ptr_2 = trk_iter_2.next()) 
    {
      
      // if track is already a ghost continue
      if (trk_ptr_2->get()->get_ghost()) continue;

      // if track could not be fitted with kalman filter or with the bend plane fit stations 1/2/3) tag as ghost and continue
      if(!(trk_ptr_2->get()->get_reco_success() )  )
	{
	  trk_ptr_2->get()->set_ghost();
	  continue;
	}
 
      // ****CHANGE THIS TO CHECK IF TRACKS IN CLOSE PROXIMITY OF EACH OTHER****
      // May need to fix phi comparisons for wrap-around:
/*      double phimin2 = trk_ptr_2->get()->get_phi_min(FVTXOO::Station4, 0);
      double phimax2 = trk_ptr_2->get()->get_phi_max(FVTXOO::Station4, 0);
      double thetamin2 = trk_ptr_2->get()->get_theta_min(FVTXOO::Station4, 0);
      double thetamax2 = trk_ptr_2->get()->get_theta_max(FVTXOO::Station4, 0);
      if(phimax2<phimin1) continue;
      if(phimin2>phimax1) continue;
      if(thetamax2<thetamin1) continue;
      if(thetamin2>thetamax1) continue; */

      // retrieve associated coordinates
      TFvtxCoordMap::key_iterator coord_iter_2 = trk_ptr_2->get()->get_associated<TFvtxCoord>();

      // number of shared coordinates
      unsigned short n_shared_coords = 0;

      // restart loop over first tracks coordinates
      coord_iter.reset();
      while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()) {
        
	// loop over second track coordinates
        coord_iter_2.reset();
        while(TFvtxCoordMap::pointer coord_ptr_2 = coord_iter_2.next()) 
	  {
	    if (coord_ptr->get()->get_key()==coord_ptr_2->get()->get_key())
	      {
		// increment number of shared coordinates
		n_shared_coords++;

		// found coordinate matching the first track current coordinate, break loop
		break;
	      }
	  }
      
	// test if number of shared coordinates passed maximum. If yes break loop
	if( n_shared_coords > _mod_par->get_max_shared_coords() ) break;

      }

      // get number of shared coordinates
      if( n_shared_coords > _mod_par->get_max_shared_coords() ) 
      {
        
	// if no check on momentum match is to be performed, push track to local list
        if( _mod_par->get_dp_cut() < 0 ) local_list.push_back(trk_ptr_2);
        
        else {
        
          // otherwise loop over all tracks in the list and see if momentum match for one of them
          for( track_list::iterator iter = local_list.begin(); iter != local_list.end(); iter++ )
          if( check_momentum( trk_ptr_2, *iter ) ) {    
            local_list.push_back(trk_ptr_2);
            break;
          }
        
        }
      
      }
            
    }  
    
    // inner loop over tracks
    // Sort local list in order of increasing chi square
    local_list.sort(less_chisq_ftor());      
    
    // Compare chi-square and nhits of all tracks and try to select
    // best single track out of list.  Tag all others as ghosts.
    if(local_list.size() <= 1) continue;

    // first iterator in group
    track_list::iterator trk_list_iter_i = local_list.begin();
    nhits_i = (*trk_list_iter_i)->get()->get_n_coord();
    chi_i = (*trk_list_iter_i)->get()->get_w_chi_square_pdf();
    
    // second iterator in group (start from the second track)
    track_list::iterator trk_list_iter = local_list.begin();
    ++trk_list_iter;

    for(;trk_list_iter!=local_list.end(); ++trk_list_iter) {
  	  
      nhits_j = (*trk_list_iter)->get()->get_n_coord();
      chi_j = (*trk_list_iter)->get()->get_w_chi_square_pdf();
      rqual = (nhits_i/chi_i)/(nhits_j/chi_j);

      // If better chi-square AND nhitsi >= nhitsj, then keep track_i
      // else If fewer hits but chi-square better by factor of 5 or more,
      //    keep track with fewer hits
      // else keep track with larger number of hits

      if(
        nhits_i>nhits_j && 
        (rqual > _mod_par->get_ghost_qual_subset_fac() ||
        chi_i < _mod_par->get_ghost_chi_min()) )
      {
        (*trk_list_iter)->get()->set_ghost();
      }  else if (nhits_i == nhits_j && chi_i < chi_j)
      {
        (*trk_list_iter)->get()->set_ghost();
      } else if (rqual < _mod_par->get_ghost_quality_cut() ) {
        (*trk_list_iter_i)->get()->set_ghost();
        trk_list_iter_i = trk_list_iter;
        nhits_i = nhits_j;
        chi_i = chi_j;
      } else {
        (*trk_list_iter)->get()->set_ghost();
      }
    }  // Loop over potential ghost tracks
    
  }  // Loop over all tracks

}

//_____________________________________________________________
void mFvtxRejectTrack::reject_outliers()
{  
  // Loop over tracks and punt if not successfully reconstructed or ghost
  //
  TFvtxTrkMap::iterator trk_iter = _trk_map->range();
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next()){
    // Chi square increment that given coordinate contributes to track
    //
    unsigned long trk_key = trk_ptr->get()->get_key().get_obj_key();
    TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
    if(_mod_par->get_verbosity() == FVTXOO::ALOT) {
      cout << "track " << trk_key << "  increment = ";
    }
    while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){
      if(_mod_par->get_verbosity() == FVTXOO::ALOT)
	    cout << coord_ptr->get()->get_chi_sqr_inc(trk_key) << " ";

      // If chi square increment is above threshold then disassociate TFvtxCoord with
      // current track
      if(coord_ptr->get()->get_chi_sqr_inc(trk_key) > _mod_par->get_max_chi_inc())
      PHKey::disassociate(coord_ptr, trk_ptr);

    }
    
    if(_mod_par->get_verbosity() == FVTXOO::ALOT) 
    cout << endl;

  }  
}

//___________________________________________________________
bool mFvtxRejectTrack::check_momentum( TFvtxTrkMap::const_pointer trk_ptr_0, TFvtxTrkMap::const_pointer trk_ptr_1 ) const
{

  // get track parameters
  const TMutTrkPar& trk_par_0( *trk_ptr_0->get()->get_trk_par() );
  const TMutTrkPar& trk_par_1( *trk_ptr_1->get()->get_trk_par() );
  
  // make relative momentum difference
  double dp_rel( sqrt( 
    MUTOO::SQUARE( trk_par_1.get_px() - trk_par_0.get_px() )+
    MUTOO::SQUARE( trk_par_1.get_py() - trk_par_0.get_py() )+
    MUTOO::SQUARE( trk_par_1.get_pz() - trk_par_0.get_pz() ) )/trk_par_0.get_ptot() );

  // returns true if small enough
  return (dp_rel < _mod_par->get_dp_cut() );

}
