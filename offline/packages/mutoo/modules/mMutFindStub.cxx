// $Id: mMutFindStub.cxx,v 1.19 2017/07/11 16:13:13 phnxbld Exp $

/*!
	\file mMutFindStub.cxx
	\brief find stubs in all stations
	\author S.Kelly, H.Pereira
	\version $Revision: 1.19 $
	\date $Date: 2017/07/11 16:13:13 $
*/

// MUTOO headers
#include <mMutFindStub.h>
#include <mMutFindStubPar.h>
#include <TMutNode.h>
#include <TMutHitMap.h>
#include <TMutStubMap.h>
#include <TMutGeo.h>
#include <PHException.h>
#include <MUTOO.h>
#include <PHTimer.h>
#include <TMutGapCoordMap.h>
#include <TMutCoordMap.h>
#include <TMutTrkMap.h>
#include <TMutTrackUtil.h>
#include <PHGeometry.h>

// STL/BOOST/GSL
#include <gsl/gsl_fit.h>
#include <cmath>
#include <iostream>
#include <string>
#include <list>
#include <boost/array.hpp>

using namespace std;

//_________________________________________________________
mMutFindStub::mMutFindStub() : 
  _timer( PHTimeServer::get()->insert_new("mMutFindStub") )
{
  MUTOO::TRACE("initializing module mMutFindStub",MUTOO::ALOT);
}

//_________________________________________________________
PHBoolean mMutFindStub::event(PHCompositeNode* top_node)
{

  _timer.get()->restart(); 
  
  try { 
    
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    // Find Stubs using chosen algorithm
    find_stubs();
    
    // Do the associations
    associate_coords();
    associate_gap_coords();
    
    // Apply stub cuts
    //apply_stub_cuts();
    
    // Eliminate duplicate stubs
    //eliminate_duplicates();
    
  } catch(exception& e) {
    
    MUTOO::TRACE(e.what());
    return False;
    
  }	
  
  // If verbose dump the contents of the cluster map
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _stub_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();		 
  
  return True;
}

//_________________________________________________________
void mMutFindStub::set_interface_ptrs(PHCompositeNode* top_node)
{	

  // module runtime parameters
  _mod_par = TMutNode<mMutFindStubPar>::find_node(top_node,"mMutFindStubPar");
  _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
  _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
  _stub_map->clear();
    
} 

//_________________________________________________________
void mMutFindStub::find_stubs()
{
  // The stub finding algorithm is encapsulated in TMutStubFinder
  TMutStubFinder stub_finder;
  
  // Clear local list
  _stubs.clear();
  
  // Loop over all half octants and envoke the stub finder if given
  // half octant has hits.
  for(int arm=0; arm<MUTOO::NumberOfArms;++arm) 
    for(int station=0; station<MUTOO::NumberOfStations ;++station)
      for(int octant=0; octant<MUTOO::NumberOfOctants ;++octant)
	for(int half_octant=0; half_octant<MUTOO::NumberOfHalfOctants ;++half_octant)
	  if(_coord_map->get(arm,station,octant,half_octant).count()){
	    
	    stub_finder.find(_coord_map, arm,station,octant,half_octant);
	    
	    // Splice all stubs found in this section into local list
	    _stubs.splice(_stubs.end(), stub_finder.get_stub_list());
	    
	  }
  
  // Get the list of stubs found by the algorithm and instantiate an IOC
  // for those with more than 3 associated coordinates.
  TMutStubFinder::stub_list::const_iterator stub_list_iter = _stubs.begin();
  for(;stub_list_iter!=_stubs.end();++stub_list_iter)
  {
    
    if(_mod_par->get_verbosity() == MUTOO::ALOT) 
    { cout << *stub_list_iter; }
    
    TMutStubMap::iterator stub_iter = _stub_map->insert_new(stub_list_iter->get_arm(), 
							    stub_list_iter->get_station(),
							    stub_list_iter->get_octant(),
							    stub_list_iter->get_half_octant());
    
    // Seed the full fit with stub parameters
    stub_iter->get()->set_fit_par(&stub_list_iter->get_fit_par());
  }
}

//_________________________________________________________
void mMutFindStub::associate_coords()
{
  // Loop over TMutStub [
  //	Loop over TMutCoord in same half octant
  //		It coord is close enough to extrapolated
  //		stub, then call associate_coord(..)
  //	]
  // ]
  
  // Get an iterator to all stubs
  TMutStubMap::iterator stub_iter = _stub_map->range();
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){					
    
    unsigned short arm = stub_ptr->get()->get_arm();
    unsigned short station = stub_ptr->get()->get_station();
    unsigned short octant = stub_ptr->get()->get_octant();
    unsigned short half_octant = stub_ptr->get()->get_half_octant();
    
    // Get an iterator to all TMutCoord in same half octant
    TMutCoordMap::iterator coord_iter = _coord_map->get(arm,station,octant,half_octant);
    while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){					
      
      // Extract the z, PHLine and stub parameters
      PHLine coord = coord_ptr->get()->get_coord();			
      double z_coord = coord.getBasepoint().getZ();
      const TMutFitPar* fit_par = stub_ptr->get()->get_fit_par();
      
      // Calculate distance form TMutCoord to extrapolated stub
      PHPoint trk_point = TMutTrackUtil::linear_track_model(fit_par,z_coord);
      double distance = PHGeometry::distanceLinePoint(coord,trk_point);
      
      // If the coordinate is close enough to the extrapolated Stub then associate
      if(distance < _mod_par->get_coord_proximity_cut())
	associate_coord(stub_ptr,coord_ptr,trk_point,distance);
    }
  }
}

//_________________________________________________________
void mMutFindStub::associate_coord(TMutStubMap::pointer stub_ptr,
				   TMutCoordMap::pointer in_coord_ptr,
				   const PHPoint& trk_point,
				   double in_distance)
{
  // Loop over TMutCoord associated with this stub
  //	 if we find a coord at same locations as input [
  //		if new coord is closer [
  //			remove old association
  //			associate new coord
  //			return
  //		] else [
  //			do nothing
  //			return
  //	 ]
  // ]
  //
  // (if we get here no previous association in this location exists)
  // Associate stub and coord
  TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
    
    // Check for existing TMutCoord at this location and keep the closest one.
    if(coord_ptr->get()->get_gap() ==	in_coord_ptr->get()->get_gap() &&
       coord_ptr->get()->get_cathode() == in_coord_ptr->get()->get_cathode()) {
      
      double distance = PHGeometry::distanceLinePoint(coord_ptr->get()->get_coord(),trk_point);
      
      // Keep the old one.
      if(distance < in_distance) return;
      else {
	
	// Keep the new one, remove the old one.
	PHKey::disassociate(stub_ptr,coord_ptr);
	PHKey::associate(stub_ptr, in_coord_ptr);
	return;
			}
    } 
  }
  
  // No existing coordinate at this location
  PHKey::associate(stub_ptr, in_coord_ptr);
}

//_________________________________________________________
void mMutFindStub::associate_gap_coords()
{
  
  // Name for local (sortable) list of TMutGapCoordMap::pointers
  
  TMutStubMap::iterator stub_iter = _stub_map->range();
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    
    // Protect against doing the associations twice
    if(stub_ptr->get()->get_associated<TMutGapCoord>().count()) continue;
    
    /* 
       new gap coord to stub association, 
       based on stub to coord+coord to gap coord association
    */
    
    // loop over associated coordinates
    TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
    while( TMutCoordMap::pointer coord_ptr = coord_iter.next() ) {
      
      // loop over associated gap coordinates
      TMutGapCoordMap::key_iterator gap_coord_iter = coord_ptr->get()->get_associated<TMutGapCoord>();
      while( TMutGapCoordMap::pointer gap_coord_ptr = gap_coord_iter.next() ) 
	if( gap_coord_ptr->get()->is_associated<TMutStub>( stub_ptr ) ) continue;
	else {
	  
	  
	  // retrieve coords associated to gap coordinate
	  TMutCoordMap::key_iterator coord_iter_2 = gap_coord_ptr->get()->get_associated<TMutCoord>();
	  bool accepted( true );
	  while( TMutCoordMap::pointer coord_ptr_2 = coord_iter_2.next() ) 
	    if( !coord_ptr_2->get()->is_associated<TMutStub>( stub_ptr ) ) {
	      accepted = false;
	      break;
	    }
	  
	  if( accepted ) PHKey::associate( gap_coord_ptr, stub_ptr );
	  
	} // loop over gap coordinates
    }	 // loop over coordinates
    
    if(_mod_par->get_verbosity() == MUTOO::ALOT){
      TMutGapCoordMap::key_iterator gap_iter = stub_ptr->get()->get_associated<TMutGapCoord>();
      cout 
	<< "mMutFindTrack::associate_stub_gap_coords - stub " 
	<< stub_ptr->get()->get_key().get_obj_key() << " coords=" 
	<< stub_ptr->get()->get_associated<TMutCoord>().count() << " gap_coords="
	<< stub_ptr->get()->get_associated<TMutGapCoord>().count() << endl;
    }
    
    if(_mod_par->get_verbosity() == MUTOO::MAX){
      TMutGapCoordMap::key_iterator gap_iter = stub_ptr->get()->get_associated<TMutGapCoord>();
      cout << "assoc. gap coord: ";
      while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next())
	cout << gap_ptr->get()->get_key().get_obj_key() << " ";
      cout << endl;
    }
  }

}

//_________________________________________________________
void mMutFindStub::apply_stub_cuts()
{	
  // Here we punt on stubs that don't have a minimum number of associated
  // TMutCoord, TMutGapCoord.
  TMutStubMap::iterator stub_iter = _stub_map->range();
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == MUTOO::Station3){
      
      // Minimum hits coord, gap coord cut
      if(stub_ptr->get()->get_n_coord() < _mod_par->get_min_coord_st3() ||
	 stub_ptr->get()->get_n_gap_coord() < _mod_par->get_min_gap_coord_st3() ) {
	_stub_map->erase(stub_ptr->get()->get_key());
      } 
    } else {
      
      // Minimum hits coord, gap coord cut
      if(stub_ptr->get()->get_n_coord() < _mod_par->get_min_coord_st12() ||
	 stub_ptr->get()->get_n_gap_coord() < _mod_par->get_min_gap_coord_st12() ) {
	_stub_map->erase(stub_ptr->get()->get_key());
      } 
    }
  }
}

//_________________________________________________________
void mMutFindStub::eliminate_duplicates()
{
  typedef vector<TMutStubMap::value_type> coord_list;
  coord_list remove_list;
  TMutStubMap::iterator stub_iter = _stub_map->range();
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    TMutStubMap::iterator stub_iter2 = stub_iter;
    ++stub_iter2;
    while(TMutStubMap::pointer stub_ptr2 = stub_iter2.next())
      if(coord_list_equal(stub_ptr, stub_ptr2)) remove_list.push_back(*stub_ptr2);
  }
  
  coord_list::iterator remove_iter = remove_list.begin();
  for(;remove_iter != remove_list.end(); ++remove_iter)
    _stub_map->erase(remove_iter->get()->get_key());
  
}

//_________________________________________________________
bool mMutFindStub::coord_list_equal(
				    const TMutStubMap::pointer stub1,
				    const TMutStubMap::pointer stub2 ) 
{
  TMutCoordMap::key_iterator stub1_iter = stub1->get()->get_associated<TMutCoord>();	
  TMutCoordMap::key_iterator stub2_iter = stub1->get()->get_associated<TMutCoord>();	
  if(stub1_iter.count() != stub2_iter.count()) return false;
  while(TMutCoordMap::pointer stub1_ptr = stub1_iter.next()){
    bool match = false;
    stub2_iter = stub2->get()->get_associated<TMutCoord>();
    while(TMutCoordMap::pointer stub2_ptr = stub2_iter.next())
      if(stub1_ptr->get()->get_key() == stub2_ptr->get()->get_key()) match = true;
    if(!match) return false;
  }
  return true;
}
