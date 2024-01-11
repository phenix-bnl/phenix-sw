// $Id: mMutMuiRoad.cxx,v 1.32 2011/07/14 04:26:09 richi Exp $

//////////////////////////////////////////////////////////////////
/*! 
  \file mMutMuiRoad.cxx
  \brief road to track association based on proximity at first gap
  \author	Chun Zhang
  \version $Revision: 1.32 $
  \date		$Date: 2011/07/14 04:26:09 $
*/
//////////////////////////////////////////////////////////////////

// MUTOO headers
#include "mMutMuiRoad.h"
#include "mMutMuiRoadPar.h"
#include <TMutNode.h>
#include <TMutGeo.h>
#include <PHException.h>
#include <TMutTrackUtil.h>
#include <MUTOO.h>

// PHENIX headers
#include <PHGeometry.h>

// STL/BOOST
#include<iostream>
#include<string>

using namespace std;

//____________________________________________________
mMutMuiRoad::mMutMuiRoad():
  _timer(PHTimeServer::get()->insert_new("mMutMuiRoad") )
{
  MUTOO::TRACE("initializing module mMutMuiRoad",MUTOO::ALOT);
}

//____________________________________________________
PHBoolean mMutMuiRoad::event(PHCompositeNode* top_node)
{
    
  // start timer
  _timer.get()->restart();
    
  try {
  
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // Associate MUIOO roads with TMutTrk
    associate_muioo();
       
    // dump associations
    if( _mod_par->get_verbosity() >= MUTOO::SOME ) dump_associations();
		
  } catch(std::exception& e) {		
    MUTOO::TRACE(e.what());
    _timer.get()->stop();
    return False;
  }		
  
  _timer.get()->stop();
  return True;
  
}

//____________________________________________________
void mMutMuiRoad::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameter
  _mod_par = TMutNode<mMutMuiRoadPar>::find_node(top_node, "mMutMuiRoadPar");
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  _road_mapO = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
}

//____________________________________________________
void mMutMuiRoad::associate_muioo()
{

  // Loop over mutr tracks [
  //	 Make a map of <distance, road> pairs to store muid road objects from smallest to largest proximity at gap0
  //	 Extrapolate linearly the track to muid gap0 via TMutTrkPar.
  //	 Loop over muid roads [
  //		 If the distance between the track extrapolation point and gap0 point is less than searching window [
  //			 Push the pointer of muid road object into road_list.
  //		 ]
  //	 ]
  //	 Select best matching road from the list.
  //	 Associated the best matching road with track.
  // ]
  
  for(int iarm= 0; iarm < MUTOO::NumberOfArms; iarm++) 
  {
    
    TMutTrkMap::iterator trk_iter = _trk_map->get(iarm);
    TMuiRoadMapO::iterator road_iter = _road_mapO->get(iarm);
    
    while(TMutTrkMap::pointer trk_ptr = trk_iter.next()) 
    {
      
      // check if track is accepted
      if( !accept_track( trk_ptr ) ) continue;
      
      // Nagle - first we need to disassociate all previous muid roads from mutr tracks...
      TMuiRoadMapO::key_iterator road_iter2 = trk_ptr->get()->get_associated<TMuiRoadO>();
      while(TMuiRoadMapO::pointer road_ptr2 = road_iter2.next()) PHKey::disassociate(road_ptr2,trk_ptr);
      
      // allocate road to track associations
      association_map associations;
      
      // road to track associations
      road_iter.reset();
      while(TMuiRoadMapO::pointer road_ptr=road_iter.next()) 
      {
        
        // retrieve muid first point
        PHPoint mui_point = road_ptr->get()->get_gap0_point();
        
        // get track point from last track parameters extrapolated to the muid z	
        PHPoint trk_point( TMutTrackUtil::linear_track_model( 
          &trk_ptr->get()->get_trk_par_list()->back(), 
          mui_point.getZ() ) );
        
        double distance = PHGeometry::distancePointToPoint(mui_point,trk_point);
        
        if( distance < _mod_par->get_muid_road_proximity_cut() && !(road_ptr->get()->get_ghost_flag())) 
        {
          double proximity( get_proximity( trk_ptr, road_ptr ) );
          associations.insert( association_pair( proximity, road_ptr ) );
          
          if( _mod_par->get_verbosity() >= MUTOO::SOME ) 
          { cout << "mMutMuiRoad::associate_muioo - found road with proximity " << proximity << endl; }
        } else if( _mod_par->get_verbosity() >= MUTOO::SOME ) { 
          cout 
            << "mMutMuiRoad::associate_muioo - distance cut failed ("
            << distance << "). skipped " << endl;
        }
          
      }
      
      // check some associations have been found
      if( associations.empty() ) continue;
      
      // If road list is not empty associate best road of each depth
      for(int depth = 2; depth < 5; depth++) 
      { 
        
        TMuiRoadMapO::pointer bestroad = select_roadO(trk_ptr, associations, depth);
        
        if(!bestroad) continue;
        PHKey::associate(trk_ptr,bestroad);
        
      }
      
    }	
  }
}

//____________________________________________________
TMuiRoadMapO::pointer mMutMuiRoad::select_roadO(
    TMutTrkMap::pointer trk_ptr, 
    const mMutMuiRoad::association_map& associations, 
    int depth)
{
      
	// Loop over associations. 
	// since they are sorted by increasing matching, stop as soon as one associated road depth match argument
	// and return the corresponding road.
	// return 0 if no match found.
	
	for( association_map::const_iterator iter = associations.begin(); iter != associations.end(); ++iter)
  { if( iter->second->get()->get_depth() == depth ) return iter->second; }

	return 0;
}

//____________________________________________________
double mMutMuiRoad::get_proximity( TMutTrkMap::pointer trk_ptr, TMuiRoadMapO::pointer road_ptr ) const
{
		
  // retrieve muid point
  PHPoint mui_point( road_ptr->get()->get_gap0_point() );
  
  // retrieve mutr point
  const TMutTrkPar& trk_par( trk_ptr->get()->get_trk_par_list()->back() );
  PHPoint trk_point( TMutTrackUtil::linear_track_model( &trk_par, mui_point.getZ() ) );					
  
  // get distance between the two points
  float distance = PHGeometry::distancePointToPoint(mui_point,trk_point);
  
  // normalized momentum vector from track		
  PHVector trk_vect( trk_par.get_tangent( trk_ptr->get()->get_arm() ) );
  
  // normalized tangent from road
  const TMutFitPar& road_fit_par( *road_ptr->get()->get_const_fitpar() );
  PHVector muid_vect( road_fit_par.get_tangent( road_ptr->get()->get_arm() ) );
  
  // delta theta between road and track directions	
  float delta_theta = acos(trk_vect.dot(muid_vect));
  
  double proximity = sqrt(
    MUTOO::SQUARE( distance/_mod_par->get_sigma_match_dist() )+
    MUTOO::SQUARE( delta_theta/_mod_par->get_sigma_match_ang()) );
  
  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  { cout << "mMutMuiRoad::get_proximity - delta_theta=" << delta_theta << " distance=" << distance << " proximity=" << proximity << endl; }

  return proximity;

}

//____________________________________________________
void mMutMuiRoad::dump_associations( void ) const
{

  MUTOO::PRINT( cout, "mMutMuiRoad::dump_associations" );
  
  TMutTrkMap::iterator iter = _trk_map->range();
  while( TMutTrkMap::pointer trk_ptr = iter.next() ) {
    TMuiRoadMapO::key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
    while( TMuiRoadMapO::pointer road_ptr = road_iter.next() )
      cout 
	<< "association -"
	<< " track=" << trk_ptr->get()->get_key().get_obj_key()
	<< " road=" << road_ptr->get()->get_key().get_obj_key()
	<< " depth=" << road_ptr->get()->get_depth()
	<< endl;
  }
  
  MUTOO::PRINT( cout, "**" );
  
}

//____________________________________________________
bool mMutMuiRoad::accept_track( TMutTrkMap::const_pointer trk_ptr ) const
{

  // check if track is a ghost
  if( trk_ptr->get()->get_ghost() ) return false;

  // check that track is properly reconstructed
  if( !(trk_ptr->get()->get_reco_success() || trk_ptr->get()->get_bp_fit3() ) ) return false;
      
  // check that track has track parameters (should not happen in case of reco_success)
  if( !trk_ptr->get()->get_trk_par_list()->size() ) return false;      

  // all check passed
  return true;

}
