// $Id: mMutFindTrack.cxx,v 1.86 2017/07/11 16:13:14 phnxbld Exp $

/*!
  \file mMutFindTrack.cxx
  \brief Associate TMutCoord with TMutTrk
  \author S.Kelly
  \version $Revision: 1.86 $
  \date $Date: 2017/07/11 16:13:14 $
*/

// MUTOO headers
#include <MUTOO.h>
#include <MutStrip.h>
#include <PHException.h>
#include <PHGeometry.h>
#include <PHTFileServer.h>
#include <PHVector.h>
#include <TMutHitMap.h>
#include <TMutBPUtil.h>
#include <TMutCoordMap.h>
#include <TMutErrorStats.h>
#include <TMutGapCoordMap.h>
#include <TMutGeo.h>
#include <TMutNode.h>
#include <TMutStubMap.h>
#include <TMutTrackUtil.h>
#include <TMutTrkMap.h>

// ROOT
#include<TNtuple.h>
#include<TTree.h>

// STL/BOOST/GSL
#include <gsl/gsl_fit.h>
#include <cmath>
#include <iostream>
#include <string>
#include <boost/array.hpp>

#include "mMutFindClus.h"
#include "mMutFitClus.h"
#include "mMutMatchCoord.h"
#include "mMutBPFit.h"
#include "mMutStubFit.h"
#include "mMutFindTrack.h"
#include "mMutFindTrackPar.h"

using namespace std;

/*! \ingroup modules */

//_______________________________________________________________________
mMutFindTrack::mMutFindTrack() : 
  _timer(PHTimeServer::get()->insert_new("mMutFindTrack")),
  _filename( "mMutFindTrack.root" ),
  _created_tracks(0)
{
  MUTOO::TRACE("initializing module mMutFindTrack",MUTOO::ALOT);

  // initialize local reconstruction modules
  _mMutFindClus_mod = 0;
  _mMutFitClus_mod = 0;
  _mMutMatchCoord_mod = 0;
  _mMutStubFit_mod = new mMutStubFit();
  _mMutBPFit_mod = new mMutBPFit();
  
  _n_tracks.assign(0);
  _octant_mask.assign(0);

}

//_______________________________________________________________________
mMutFindTrack::~mMutFindTrack() 
{ 
  finish_evaluation(); 

  if( _mMutFindClus_mod ) delete _mMutFindClus_mod;
  if( _mMutFitClus_mod ) delete _mMutFitClus_mod;
  if( _mMutMatchCoord_mod ) delete _mMutMatchCoord_mod;

  delete _mMutStubFit_mod;
  delete _mMutBPFit_mod;
}

//_______________________________________________________________________
//! Event method
PHBoolean mMutFindTrack::event(PHCompositeNode* top_node)
{

  _timer.get()->restart(); 
  try { 
    
    // Reset IOC pointers
    set_interface_ptrs(top_node);
        
    // set local reconstruction modules  interface pointers
    if( _mod_par->get_use_local_clusters() )
    {
      //cout << "mMutFindTrack::event - using local clusters" << endl;
      _mMutFindClus_mod->set_interface_ptrs( top_node );
      _mMutFitClus_mod->set_interface_ptrs( top_node );
      _mMutMatchCoord_mod->set_interface_ptrs( top_node );
    }

    _mMutStubFit_mod->set_interface_ptrs(top_node);
        
    // Reset exception array
    _exception_pattern.assign( false );
    
    // initiallize ntuples (done only once if flag is true)
    if( _mod_par->get_do_evaluation() ) initialize_track_ntuples();
    
    // initiallize stub ntuples (done only once if flag is true)
    if( TMutStubFinder::get_do_evaluation() ) {
      TMutStubFinder::initialize_evaluation();
      TMutStubFinder::Stub::initialize_evaluation();
    }
    
    // reset number of tracks
    _n_tracks.assign( 0 );
        
    // Initialize tracks with or without the MUID
    if(_mod_par->get_mode() == mMutFindTrackPar::USE_MUID) start_tracks_muid();
    else start_tracks_no_muid();
    
    //	Do full stub fit on found stubs (needed to use error propagation
    //	in windowing and to have best fit results for BPFit):
    if (_mod_par->get_window_mode() == mMutFindTrackPar::BPFIT)
    {
    
      _mMutStubFit_mod->event(top_node, MUTOO::South, MUTOO::Station3);
      _mMutStubFit_mod->event(top_node, MUTOO::North, MUTOO::Station3);
    
    }
    
    if( _mod_par->get_verbosity() >= MUTOO::SOME ) 
    { 
      cout << "mMutFindTrack::event -" 
        << " station3 tracks: " << _trk_map->size() 
        << " stubs: " << _stub_map->size()
        << endl; 
    }

    find_in_station(MUTOO::Station2);		
    associate_trk(MUTOO::Station3);
    associate_trk(MUTOO::Station2);

    //	Do full stub fit on found stubs:
    if (_mod_par->get_window_mode() == mMutFindTrackPar::BPFIT)
    {
    
      _mMutStubFit_mod->event(top_node, MUTOO::South, MUTOO::Station2);
      _mMutStubFit_mod->event(top_node, MUTOO::North, MUTOO::Station2);
      _mMutBPFit_mod->event(top_node);
    
    }

    if( _mod_par->get_verbosity() >= MUTOO::SOME ) 
    { 
      cout << "mMutFindTrack::event -" 
        << " station2 tracks: " << _trk_map->size() 
        << " stubs: " << _stub_map->size()
        << endl; 
    }

    find_in_station(MUTOO::Station1);

    // Do the TMutTrk -- TMutCoord & TMutGapCoord associations
    associate_trk(MUTOO::Station1);
    
    if( _mod_par->get_verbosity() >= MUTOO::SOME ) 
    { 
      cout << "mMutFindTrack::event -" 
        << " station1 tracks: " << _trk_map->size() 
        << " stubs: " << _stub_map->size()
        << endl; 
    }

    // set track hit pattern
    TMutTrkMap::iterator trk_iter( _trk_map->range() );
    while( TMutTrkMap::pointer trk_ptr = trk_iter.next() )
    { trk_ptr->get()->set_hit_pattern( trk_ptr->get()->TMutTrk::get_hit_pattern() ); }
    
    // If verbose dump the contents of the track map
    if(_mod_par->get_verbosity() >= MUTOO::ALOT) 
    {
      _trk_map->print();
      TMutTrkMap::iterator trk_iter( _trk_map->range() );
      while( TMutTrkMap::pointer trk_ptr = trk_iter.next() )
      print_associations( trk_ptr );
    }
    
    if(_mod_par->get_verbosity() >= MUTOO::SOME) {
      _timer.get()->print();	
      print_summary();
    }

  } catch(exception& e) {		
    
    MUTOO::TRACE(e.what());
    _timer.get()->stop();
    return False;		
  
  }	
  
  _timer.get()->stop();
  
  return True;
}

//_______________________________________________________________________
//! Reset IOC and external interface pointers 
void mMutFindTrack::set_interface_ptrs(PHCompositeNode* top_node)
{		

  _top_node = static_cast<PHCompositeNode*>(top_node);

  // module runtime parameters
  _mod_par = TMutNode<mMutFindTrackPar>::find_node(top_node,"mMutFindTrackPar");

  // initialize local reconstruction modules
  if( _mod_par->get_use_local_clusters() )
  {
    if( !_mMutFindClus_mod ) _mMutFindClus_mod = new mMutFindClus();
    if( !_mMutFitClus_mod ) _mMutFitClus_mod = new mMutFitClus();
    if( !_mMutMatchCoord_mod ) _mMutMatchCoord_mod = new mMutMatchCoord();
  }

  // TMutCoord IOC
  _gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap");	

  // TMutCoord IOC
  _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");	
  
  // TMutStub IOC
  _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");	
  _stub_map->clear();
  
  // TMutTrack IOC
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  _trk_map->clear();

  // TMuMuiRoadO IOC
  _road_mapO = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");

} 

//_______________________________________________________________________
void mMutFindTrack::start_tracks_no_muid()
{	

  // Loop over all arms octants
  for(int arm=0; arm<MUTOO::NumberOfArms;++arm)
  for(int octant=0; octant<MUTOO::NumberOfOctants;++octant)
  {

    /*
      if using local clusters, run cluster finder, cluster fit and find gap coordinates
      for this octant, in station 3
    */
    if( _mod_par->get_use_local_clusters() )
    {
      _mMutFindClus_mod->event( _top_node, arm, MUTOO::Station3, octant );
      _mMutFitClus_mod->event( _top_node, arm, MUTOO::Station3, octant );
      _mMutMatchCoord_mod->event( _top_node, arm, MUTOO::Station3, octant );
    }
    
    // loop over all half octants and find stubs
    for(int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants; ++half_octant ) 
    {


      //	Find all stubs in given octant, half octant
      TMutStubFinder stub_finder;			
      try {
	stub_finder.find(_coord_map, arm, MUTOO::Station3, octant, half_octant); 
      } catch (exception& e) {
      
	MUTOO::TRACE(e.what());
	
	ostringstream what; 
	what 
	  << "station " << MUTOO::Station3 
	  << ", octant " << octant 
	  << ", half " << half_octant 
	  << " skipped.\n";
        
	MUTOO::TRACE(what.str());
	continue;				
      
      }
  
      stub_finder.evaluate(_top_node);
  
      // For each station 3 stub -- create a TMutTrk and TMutStub IO and associate them 
      // Stubs are associated with tracks if the pass the minimum coordinate cut defined
      // in the module parameter table.
      TMutStubFinder::stub_list& stub_list = stub_finder.get_stub_list();
      TMutStubFinder::stub_list::iterator stub_list_iter = stub_list.begin();
      for(;stub_list_iter!=stub_list.end();++stub_list_iter) 
      {
  
	// Check that the current stub passes stub cuts (false -> no windows)
	if(!stub_cuts(stub_list_iter)) continue;
	
	// First create the stub IO
	TMutStubMap::iterator stub_iter = _stub_map->insert_new(stub_list_iter->get_arm(), 
								stub_list_iter->get_station(),
								stub_list_iter->get_octant(),
								stub_list_iter->get_half_octant());
        
	// Seed the full fit with stub parameters
	stub_iter->get()->set_fit_par(&stub_list_iter->get_fit_par());			
        
	// Stub coords into the TMutStub IO
	typedef TMutStubFinder::Stub::coord_list stub_coord_list;
	stub_coord_list& coord_list = stub_list_iter->get_coord_list();
	stub_coord_list::iterator coord_iter = coord_list.begin();		
	double min_z = DBL_MAX;
	double max_z = -DBL_MAX;
	for(;coord_iter!=coord_list.end();++coord_iter){
	  // Associate coord and stub
	  TMutCoordMap::value_type coord = *coord_iter;
	  PHKey::associate(*stub_iter, coord);
          
	  // Keep track of max and min z values
	  min_z = min(coord.get()->get_mean_z(), min_z);
	  max_z = max(coord.get()->get_mean_z(), max_z);
	}
        
	// create the track IO
	TMutTrkMap::iterator trk_iter = _trk_map->insert_new(stub_list_iter->get_arm(),
							     stub_list_iter->get_octant());
        
	trk_iter->get()->set_fit_par(&stub_list_iter->get_fit_par());
        
	// Do the association
	PHKey::associate(stub_iter.current(), trk_iter.current());
      }
    }
  }

  // Do the TMutStub -- TMutGapCoord associations
  associate_stub_gap_coords();

}

//____________________________________________________________________
void mMutFindTrack::start_tracks_muid() 
{
  
  if(!_road_mapO) return;
  TMutStubFinder stub_finder;	

  // Find all station 3 stubs (with no windows)
  for(int arm=0; arm<MUTOO::NumberOfArms;++arm)
  for(int octant=0; octant<MUTOO::NumberOfOctants;++octant)
  {

    /*
      if using local clusters, run cluster finder, cluster fit and find gap coordinates
      for this octant, in station 3
    */
    if( _mod_par->get_use_local_clusters() )
    {
      _mMutFindClus_mod->event( _top_node, arm, MUTOO::Station3, octant );
      _mMutFitClus_mod->event( _top_node, arm, MUTOO::Station3, octant );
      _mMutMatchCoord_mod->event( _top_node, arm, MUTOO::Station3, octant );
    }

    // loop over half octants and build stubs
    for(int half_octant=0; half_octant<MUTOO::NumberOfHalfOctants;++half_octant) 
    {
  
      try {
        stub_finder.find(_coord_map, arm, MUTOO::Station3, octant, half_octant);
      } catch (exception& e) {
        cout << e.what() << endl;
        continue;
      }
  
    }
  }

  // For each station 3 stub, if the stub can be associated
  // with a muid road, we create a stub io and a new track
  // associate them together, and associate the new track 
  // with muid road.
  TMutStubFinder::stub_list& stub_list = stub_finder.get_stub_list();
  TMutStubFinder::stub_list::iterator stub_list_iter = stub_list.begin();
  for(;stub_list_iter!=stub_list.end();++stub_list_iter)
  {

    unsigned short arm = stub_list_iter->get_arm();

    // Loop over road map.
    TMuiRoadMapO::iterator road_iter = _road_mapO->get(arm);
    
    // Z position of last plane in station 3
    //
    double sta3_position = TMutGeo::get_strip_geom(arm,2,0,0,1,1,0)->getGlobalPositionBegin().getZ();
    
    // Initialize local trk_iter stub_iter so we only add one TMutStub/TMutTrk interface object in below loop
    //
    TMutStubMap::iterator stub_iter;
    TMutTrkMap::iterator trk_iter;
    
    while(TMuiRoadMapO::pointer road_ptr=road_iter.next()) 
    {
      
      // MUID road quality cuts
      if (!passed_muid_cuts(road_ptr)) continue;
      
      PHPoint mui_point = TMutTrackUtil::linear_track_model(road_ptr->get()->get_const_fitpar(), sta3_position);

      double phi = atan2(mui_point.getY(),mui_point.getX());
      double rho = sqrt(mui_point.getY()*mui_point.getY() + mui_point.getX()*mui_point.getX());
      double phiwin = atan(_mod_par->get_muid_road_proximity_cut(road_ptr->get()->get_arm())/rho);
      double phimin = phi - phiwin;
      double phimax = phi + phiwin;

      double the = atan2( rho, mui_point.getZ());
      double thewin = abs(atan(_mod_par->get_muid_road_proximity_cut(road_ptr->get()->get_arm())/mui_point.getZ()));
      double themin = the - thewin;
      double themax = the + thewin; 
      
      // check arm is correct, stub is large enough
      if(!(stub_list_iter->get_n_coord() >= _mod_par->get_min_stub_coord() && stub_list_iter->get_arm()==arm ) ) continue;
      
      PHPoint stub_point= TMutTrackUtil::linear_track_model(&stub_list_iter->get_fit_par(), sta3_position);
      
      double distance = PHGeometry::distancePointToPoint(mui_point,stub_point);
      
      if( distance >= _mod_par->get_muid_road_proximity_cut(road_ptr->get()->get_arm()) ) continue;
      
      // First create the stub IO
      if(stub_iter.at_end()) 
      {
        stub_iter = _stub_map->insert_new(arm, 
          stub_list_iter->get_station(),
          stub_list_iter->get_octant(),
          stub_list_iter->get_half_octant());
  
        // Seed the full fit with stub parameters
        stub_iter->get()->set_fit_par(&stub_list_iter->get_fit_par());			
        
        // Store equivalent window in stub, to be used in evaluation:
        stub_list_iter->set_phi_min(phimin);
        stub_list_iter->set_phi_max(phimax);
        stub_list_iter->set_theta_min(themin);
        stub_list_iter->set_theta_max(themax);
        
        // Stub coords into the TMutStub IO		
        typedef TMutStubFinder::Stub::coord_list stub_coord_list;
        stub_coord_list& coord_list = stub_list_iter->get_coord_list();
        stub_coord_list::iterator coord_iter = coord_list.begin();
        double min_z = DBL_MAX;
        double max_z = -DBL_MAX;
        for(;coord_iter!=coord_list.end();++coord_iter) {
          
          // Associate coord and stub
          TMutCoordMap::value_type coord = *coord_iter;
          PHKey::associate(*stub_iter, coord);
          
          // Keep track of max and min z values
          min_z = min(coord.get()->get_mean_z(), min_z);
          max_z = max(coord.get()->get_mean_z(), max_z);		
        }
        
        // Now create the track IO
        trk_iter = _trk_map->insert_new(arm,stub_list_iter->get_octant());
        trk_iter->get()->set_fit_par(&stub_list_iter->get_fit_par());
        
        // Do the association track/stub association
        PHKey::associate(stub_iter.current(), trk_iter.current());
        
      }
      
      // associate road and current track			
      PHKey::associate(road_ptr, trk_iter.current());				

    }
  } 
  
  // Do the TMutStub -- TMutGapCoord associations
  associate_stub_gap_coords();	
  
  // Evaluation
  stub_finder.evaluate(_top_node);
  
  // Fill created_tracks ntuple:
  if( _mod_par->get_do_evaluation() ) write_track_ntuple(2);

}


//_______________________________________________________________________
bool mMutFindTrack::passed_muid_cuts(TMuiRoadMapO::const_pointer road_ptr) const
{

  // If flag is set, only look at "golden" roads:
  if (_mod_par->get_muid_use_golden() && !road_ptr->get()->get_golden()) return false;
  
  if(road_ptr->get()->get_depth() < _mod_par->get_muid_road_depth() ||
     road_ptr->get()->get_fit_par().get_chi_square() > _mod_par->get_muid_chipdf() ||
     road_ptr->get()->get_nhit() < _mod_par->get_muid_nhit() || 
     road_ptr->get()->get_max_hit_plane() > _mod_par->get_muid_max_hit_plane() ) 
    {		
      return false;
    }

  return true;

}


//_______________________________________________________________________
void mMutFindTrack::write_track_ntuple(unsigned short station)
{

  float ntvar[4] = {0};
  
  // check _created_tracks
  if( !_created_tracks ) return;
  
  for(int iarm= 0; iarm < 2; iarm++) {
    TMutTrkMap::iterator trk_iter = _trk_map->get(iarm);
    ntvar[0] = trk_iter.count();
    ntvar[1] = (float)iarm;
    ntvar[2] = (float)station;
    if (_road_mapO){
      TMuiRoadMapO::iterator road_iter = _road_mapO->get(iarm);
      ntvar[3] = road_iter.count();
    }

    _created_tracks->Fill(ntvar);

  }

  static ULong_t auto_save=0;
  if(auto_save++%100 == 0) _created_tracks->AutoSave();

}

//_______________________________________________________________________
void mMutFindTrack::find_stubs(TMutTrkMap::const_pointer trk_ptr, const unsigned short& arm, const unsigned short& station, const unsigned short& octant)
{

  /*
    if using local clusters, run cluster finder, cluster fit and find gap coordinates
    for this octant, unless already done
  */
  if( _mod_par->get_use_local_clusters() && !(_octant_mask[arm]&(1<<octant)) )
  {
    _mMutFindClus_mod->event( _top_node, arm, station, octant );
    _mMutFitClus_mod->event( _top_node, arm, station, octant );
    _mMutMatchCoord_mod->event( _top_node, arm, station, octant );
  }


  // If not using stub finder windows then check to see if the stub finder
  // has already been run for this octant and punt if it has.
  if( !_mod_par->get_use_stub_finder_windows() && (_octant_mask[arm]&(1<<octant) ) ) return; 

  // Instantiate stub finder algorithm
  TMutStubFinder stub_finder;
  if(_mod_par->get_use_stub_finder_windows() == true) 
  {

    // Define theta phi window for this track
    define_window_in_station(trk_ptr,station);

    // Run the stub finder (find all stubs in theta phi window)
    //
    stub_finder.find(_coord_map, 
      arm, 
      station, 
      octant, 
      0,
      true,
      _theta_window,
      _phi_window);
    
    stub_finder.find(_coord_map, 
      arm,
      station, 
      octant,
      1,
      true,
      _theta_window,
      _phi_window);
    
  } else {
    
    // Run the stub finder in selected half octants
    stub_finder.find(_coord_map, 
      arm, 
      station, 
      octant, 
      0);
    
    stub_finder.find(_coord_map, 
      arm, 
      station, 
      octant, 
      1);
    
  }
  
  stub_finder.evaluate(_top_node);
  
  TMutStubFinder::stub_list& stub_list = stub_finder.get_stub_list();		
  
  // Loop over found stubs and write a TMutStub IO for each one that
  // passes cuts.
  TMutStubFinder::stub_list::iterator stub_list_iter = stub_list.begin();
  for(;stub_list_iter!=stub_list.end();++stub_list_iter){
    
    if(_mod_par->get_use_stub_finder_windows())
    {
      
      // If we are running in windowed mode we check that this stub wasn't
      // found already (each track runs the stub finder with windows defined
      // by that track)	Lexical compare a la the stub_equal_ftor.
      bool already_found = false;
      list<TMutStubFinder::Stub>::iterator master_iter = _master_list.begin();
      for(;master_iter!=_master_list.end();++master_iter)
      { if(master_iter->get_coord_list() == stub_list_iter->get_coord_list()) already_found = true; }

      if(already_found) continue;
      
      // This is a new stub so we cache it in the master list
      _master_list.push_back(*stub_list_iter);
    }

    // Check that the current stub passes stub cuts
    if(!stub_cuts(stub_list_iter)) continue;
    
    // First create the stub IO
    TMutStubMap::iterator stub_iter = _stub_map->insert_new(stub_list_iter->get_arm(), 
      stub_list_iter->get_station(),
      stub_list_iter->get_octant(),
      stub_list_iter->get_half_octant());
    
    // Stub data into the TMutStub IO
    stub_iter->get()->set_fit_par(&stub_list_iter->get_fit_par());
    stub_iter->get()->set_theta_min(stub_list_iter->get_theta_window().first);
    stub_iter->get()->set_theta_max(stub_list_iter->get_theta_window().second);
    stub_iter->get()->set_phi_min(stub_list_iter->get_phi_window().first);
    stub_iter->get()->set_phi_max(stub_list_iter->get_phi_window().second);
    
    // Stub coords into the TMutStub IO
    typedef TMutStubFinder::Stub::coord_list stub_coord_list;
    stub_coord_list& coord_list = stub_list_iter->get_coord_list();
    stub_coord_list::iterator coord_iter = coord_list.begin();		
    double min_z = DBL_MAX;
    double max_z = -DBL_MAX;
    for(;coord_iter!=coord_list.end();++coord_iter){
      // Associate coord and stub
      // 
      TMutCoordMap::value_type coord = *coord_iter;
      PHKey::associate(*stub_iter, coord);
      
      // Keep track of max and min z values
      min_z = min(coord.get()->get_mean_z(), min_z);
      max_z = max(coord.get()->get_mean_z(), max_z);
    }
    
    // Set US and DS endpoints of stub
    //
    if(stub_iter->get()->get_arm() == MUTOO::South){
      stub_iter->get()->set_z_begin(max_z);
      stub_iter->get()->set_z_end(min_z);
    } else {
      stub_iter->get()->set_z_begin(min_z);
      stub_iter->get()->set_z_end(max_z);
    }		
  }
  
  // set octant as checked
  _octant_mask[arm]|=(1<<octant);
  
}

//____________________________________________________________________
void mMutFindTrack::find_in_station( const unsigned short& station )
{
  
  // Loop over tracks [
  //	 Find stubs in given station and same arm, octant as track
  //	 Add stubs to track (if in given window) 
  // ]
  
  // Push tracks onto local pointer list
  typedef list<TMutTrkMap::value_type> trk_ptr_list;
  trk_ptr_list local_trk_list;

  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
  { local_trk_list.push_back(*trk_ptr); }

  // Clear octant mask
  _octant_mask.assign(0);

  // Clear the master stub list 
  _master_list.clear();
  
  // Note: this loop is over the local list because add_stubs_to_trk
  // can clone a track (ie insert a track into the track map).	Looping
  // over the track map directly leads to a runaway loop.
  for( trk_ptr_list::iterator trk_list_iter = local_trk_list.begin(); trk_list_iter!=local_trk_list.end();++trk_list_iter) 
  {
    
    // Check that track has stub in previous station if not skip it.
    unsigned short previous_station = (station == MUTOO::Station1) ? MUTOO::Station2 : MUTOO::Station3;
    if(!trk_list_iter->get()->has_stub(previous_station)) continue;

    try {

      // Find the stubs
      find_stubs(
        &(*trk_list_iter),
        trk_list_iter->get()->get_arm(),
        station,
        trk_list_iter->get()->get_octant());
      
    } catch(exception& e) {			
      
      // PR failed for this octant but continue to next
      MUTOO::TRACE(e.what());
      
      // one must skip the octant definitely, to avoid multiple exceptions and spare reconstruction time
      // to do so one sets the corresponding mask in _octant_mask to 1.
      _octant_mask[trk_list_iter->get()->get_arm()] |= (1<<trk_list_iter->get()->get_octant());

      continue;			
      
    }
    
    // Do the TMutStub -- TMutGapCoord associations
    associate_stub_gap_coords();
    
    // Update current track with found stub(s). In the case
    // of more that one stub found in window create new track.
    // as per need.	Stubs are added to track only if in defined
    // theta, phi window.
    add_stubs_to_trk( &(*trk_list_iter), station);
    
  }
  
  if( _mod_par->get_do_evaluation() ) write_track_ntuple(station);
  
}

//____________________________________________________________________
void mMutFindTrack::add_stubs_to_trk(TMutTrkMap::pointer trk_ptr, const unsigned short& station)
{

  // Calc theta, phi window for this track. (Results are 
  // stored in module data members _theta_window, _phi_window).	
  define_window_in_station(trk_ptr, station);
    
  // Get an iterator to all stubs in same octant as track
  TMutStubMap::iterator stub_iter = _stub_map->get(trk_ptr->get()->get_arm(),
						   station,
						   trk_ptr->get()->get_octant());
  try 
  {

    while(TMutStubMap::pointer stub_ptr = stub_iter.next()) 
    {
      
      
      // Check theta/phi window
      //
      if(!check_window(stub_ptr)) continue;
            
      // Do the association
      if(!trk_ptr->get()->has_stub(stub_ptr->get()->get_station())) {

        // Track doesn't have a stub in this station
        PHKey::associate(stub_ptr, trk_ptr);

      } else {			

        // Track already has a stub in this station
        clone_trk(trk_ptr,stub_ptr);

      }
    }
    
    if( 
      (!stub_iter.count()) 
      && _mod_par->get_verbosity() >= MUTOO::SOME 
      ) cerr << "mMutFindTrack::add_stubs_to_trk - no stub in station " << station << endl;
    
    
    else if(
      (!trk_ptr->get()->has_stub(station) ) 
      && _mod_par->get_verbosity() >= MUTOO::SOME 
      ) cerr << "mMutFindTrack::add_stubs_to_trk - no matching stub in station " << station << endl;
    
    
  } catch(exception& e) { 
    int index( trk_ptr->get()->get_arm() + MUTOO::NumberOfArms*trk_ptr->get()->get_octant() );
    if( !_exception_pattern[index]	)
    {
      _exception_pattern[index] = true;
      MUTOO::TRACE(e.what()); 
    }
  }
}

//____________________________________________________________________
void 
mMutFindTrack::clone_trk(TMutTrkMap::pointer in_trk_ptr, TMutStubMap::pointer in_stub_ptr)
{
  // check if the max number of cloned tracks for given arm is reached 
  if( _n_tracks[ in_trk_ptr->get()->get_arm() ]	> _mod_par->get_max_n_tracks() ) {
    TMutErrorStats::set_error(TMutErrorStats::CLONE_TRACK,
            in_trk_ptr->get()->get_arm(),
            in_stub_ptr->get()->get_station(),
            in_trk_ptr->get()->get_octant()); 
    
    ostringstream what;
    what << "too many tracks in clone_trk (" 
   << in_trk_ptr->get()->get_arm() << "," 
   << in_trk_ptr->get()->get_octant() << ")";
      
    throw runtime_error(DESCRIPTION( what.str() ));
        
  } else _n_tracks[ in_trk_ptr->get()->get_arm() ]++;

  // New track same octant
  TMutTrkMap::iterator trk_iter = _trk_map->insert_new(in_trk_ptr->get()->get_arm(),
                   in_trk_ptr->get()->get_octant());
  
  // Input track stubs are associated (excluding current station)
  TMutStubMap::key_iterator stub_iter = in_trk_ptr->get()->get_associated<TMutStub>();
  while(TMutStubMap::pointer stub_ptr = stub_iter.next())
    if(stub_ptr->get()->get_station() != in_stub_ptr->get()->get_station())
      PHKey::associate(trk_iter.current(),stub_ptr);
  
  // Input track coords are associated (excluding current station)
  TMutCoordMap::key_iterator coord_iter = in_trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next())
  {
    if(coord_ptr->get()->get_station() != in_stub_ptr->get()->get_station())
    { PHKey::associate(trk_iter.current(),coord_ptr); }
  }
  
  // Associate the input stub iterator
  PHKey::associate(trk_iter.current(),in_stub_ptr);
  
  // Pull the windows from the old track
  trk_iter->get()->set_fit_par(in_trk_ptr->get()->get_fit_par());
  for(int sta=0; sta<MUTOO::NumberOfStations; ++sta){
    trk_iter->get()->set_phi_min(sta,in_trk_ptr->get()->get_phi_min(sta));
    trk_iter->get()->set_phi_max(sta,in_trk_ptr->get()->get_phi_max(sta));
    trk_iter->get()->set_theta_min(sta,in_trk_ptr->get()->get_theta_min(sta));
    trk_iter->get()->set_theta_max(sta,in_trk_ptr->get()->get_theta_max(sta));
  }

  // copy status from old trk
  trk_iter->get()->copy_status( *in_trk_ptr->get() );
  
  // Associate the MUID road
  TMuiRoadMapO::key_iterator road_iter = in_trk_ptr->get()->get_associated<TMuiRoadO>();
  while(TMuiRoadMapO::pointer road_ptr = road_iter.next()){
    PHKey::associate(trk_iter.current(), road_ptr);
  }
  
}

//____________________________________________________________________
bool mMutFindTrack::check_window(TMutStubMap::const_pointer stub_ptr) const
{

  // dump all windows
  if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
    
    cout 
      << " mMutFindTrack::check_window -"
      << " station=" << stub_ptr->get()->get_station()	 
      << " theta=" << stub_ptr->get()->get_theta() 
      << " window = {" << _theta_window.first << "," << _theta_window.second << "}" 
      << endl;

    cout 
      << " mMutFindTrack::check_window -"
      << " station=" << stub_ptr->get()->get_station()	 
      << " phi=" << stub_ptr->get()->get_phi() 
      << " window = {" << _phi_window.first << "," << _phi_window.second << "}" 
      << endl;
      
  }
      
  // Check that the current stub is within window defined by track
  if(stub_ptr->get()->has_point() && !stub_ptr->get()->check_theta_window(_theta_window))
  {
    
    if(_mod_par->get_verbosity() >= MUTOO::ALOT) 
    {
    
      MUTOO::PRINT(cout,"mMutFindTrack::check_window");
      cout 
        << " failed - station = " << stub_ptr->get()->get_station()
        << " theta = " << stub_ptr->get()->get_theta() << "	 window = {" 
        << _theta_window.first << "," << _theta_window.second << "}" 
        << endl;
      stub_ptr->get()->print();
      MUTOO::PRINT(cout,"**");
    
    }
    return false;
  }
  
  if(stub_ptr->get()->has_point() && !stub_ptr->get()->check_phi_window(_phi_window))
  {
    if(_mod_par->get_verbosity() >= MUTOO::ALOT) 
    {
      MUTOO::PRINT(cout,"mMutFindTrack::check_window");
      cout	
        << " failed - station = " << stub_ptr->get()->get_station()
        << " phi = " << stub_ptr->get()->get_phi() 
        << " window = {" << _phi_window.first << "," << _phi_window.second << "}" 
        << endl;
      stub_ptr->get()->print();
      MUTOO::PRINT(cout,"mMutFindTrack::check_window");
    }
    return false;
  }	
  return true;
}

//_______________________________________________________________________
bool mMutFindTrack::stub_cuts(TMutStubFinder::stub_list::const_iterator stub_iter)			 
{		
  
  // cuts on number of coordinates
  if( (stub_iter->get_station() < MUTOO::Station3 && stub_iter->get_n_coord() < _mod_par->get_min_stub_coord()) || 
      (stub_iter->get_station() == MUTOO::Station3 && stub_iter->get_n_coord() < _mod_par->get_min_stub_coord_st3())) 
  {
    
    if(_mod_par->get_verbosity() == MUTOO::MAX) 
    {
      MUTOO::TRACE("mMutFindTrack::stub cuts min coord cut");
      cout << *stub_iter;
    }
    
    return false;
  
  }

  // cuts on number of pointes
  if( (stub_iter->get_station() < MUTOO::Station3 && stub_iter->get_n_point() < _mod_par->get_min_stub_point()) || 
      (stub_iter->get_station() == MUTOO::Station3 && stub_iter->get_n_point() < _mod_par->get_min_stub_point_st3()) ) 
  {
    if(_mod_par->get_verbosity() == MUTOO::MAX) 
    {
      MUTOO::TRACE("mMutFindTrack::stub cuts min point cut");
      cout << *stub_iter;
    }
    return false;	
  }
    
  // Check the the stub angles are not insane
  if(fabs(stub_iter->get_dwdz()) < _mod_par->get_dwdz_min()	||
     fabs(stub_iter->get_dwdz()) > _mod_par->get_dwdz_max()	) 
  {
    
    if(_mod_par->get_verbosity() == MUTOO::MAX) 
    {
      MUTOO::TRACE("mMutFindTrack::stub cuts failed drdz cut");
      cout << *stub_iter;
    }
    return false;
  }	
  return true;
}

//_______________________________________________________________________
void mMutFindTrack::define_window_in_station( TMutTrkMap::const_pointer trk_ptr, const unsigned short& station) 
{

  // Start from stubs in 3 if station==2, in 2 if station==1
  unsigned short target_station = (station == MUTOO::Station2) ? MUTOO::Station3 : MUTOO::Station2;

  if (_mod_par->get_window_mode() == mMutFindTrackPar::BPFIT)
  {
    
    TMutBPUtil::get_bp_window(trk_ptr, station);
    
    _phi_window = make_pair(
          trk_ptr->get()->get_phi_min(station), 
          trk_ptr->get()->get_phi_max(station)
          );
    
    _theta_window = make_pair(
            trk_ptr->get()->get_theta_min(station), 
            trk_ptr->get()->get_theta_max(station)
            );
    
    if( _mod_par->get_verbosity() >= MUTOO::SOME )
      { 
	cout << "mMutFindTrack::define_window_in_station -"
	     << " track: " << trk_ptr->get()->get_key().get_obj_key()
	     <<	" station: " << station 
	     << " phi: (" << _phi_window.first << "," << _phi_window.second << ")"
	     << " theta: (" << _theta_window.first << "," << _theta_window.second << ")"
	     << endl;
      }

  } else {
    
    if (_mod_par->get_window_mode() != mMutFindTrackPar::STUBFIT)
    MUTOO::TRACE("Bad window mode specification in mMutFindTrack, defaulting to STUBFIT");
    
    TMutStubMap::const_pointer use_stub_ptr = 0;
    TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
    while(TMutStubMap::const_pointer stub_ptr = stub_iter.next())
    {
      if(stub_ptr->get()->get_station() == target_station) {
        use_stub_ptr = stub_ptr;
        break;
      }
    }
    
    // No stub in required station -- return false (This will terminate the PR for this track)
    if(use_stub_ptr == 0)
    { throw logic_error(DESCRIPTION("No relevant stub for define_window_in_station")); }
    
    double phi = use_stub_ptr->get()->get_phi();
    
    // Define phi interval around extrapolated point
    _phi_window = make_pair(
      phi - _mod_par->get_dphi(), 
      phi + _mod_par->get_dphi());
    
    // Write the phi window data to the track object
    trk_ptr->get()->set_phi_min(station,_phi_window.first);
    trk_ptr->get()->set_phi_max(station,_phi_window.second);
    
    double theta = use_stub_ptr->get()->get_theta();
    
    // Define theta window 
    _theta_window = make_pair
      (theta - _mod_par->get_dtheta(), 
      theta + _mod_par->get_dtheta());
    
    // Write the theta window data to the track object
    trk_ptr->get()->set_theta_min(station,_theta_window.first);
    trk_ptr->get()->set_theta_max(station,_theta_window.second);	
  }

}

//_______________________________________________________________________
void mMutFindTrack::associate_stub_gap_coords()
{

  TMutStubMap::iterator stub_iter = _stub_map->range();
  while(TMutStubMap::pointer stub_ptr = stub_iter.next())
  {
    
    // Protect against doing the associations twice
    if( !stub_ptr->get()->get_associated<TMutGapCoord>().empty()) continue;
    
    // loop over associated coordinates
    TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
    while( TMutCoordMap::pointer coord_ptr = coord_iter.next() ) 
    {
    
      // skip coordinates that are not in first cathode
      if( coord_ptr->get()->get_cathode() != MUTOO::Cathode_2 ) continue;
      
      // loop over associated gap coordinates
      TMutGapCoordMap::key_iterator gap_coord_iter = coord_ptr->get()->get_associated<TMutGapCoord>();
      while( TMutGapCoordMap::pointer gap_coord_ptr = gap_coord_iter.next() ) 
      {
        
        // useless now that we only look at Cathode_2 coordinates for the first loop
        // // check if association was not already done.
        // if( stub_ptr->get()->is_associated<TMutGapCoord>( gap_coord_ptr ) ) continue;
        
        bool accepted( true );

        // retrieve coords associated to gap coordinate
        TMutCoordMap::key_iterator coord_iter_2 = gap_coord_ptr->get()->get_associated<TMutCoord>();
        while( TMutCoordMap::pointer coord_ptr_2 = coord_iter_2.next() ) 
        {
          
          // check if coord is in first cathode and is associated to stub
          if( coord_ptr_2->get()->get_cathode() == MUTOO::Cathode_1 && !stub_ptr->get()->is_associated<TMutCoord>( coord_ptr_2 ) ) 
          {
            accepted = false;
            break;
          }
          
        }
          
        if( accepted ) PHKey::associate( gap_coord_ptr, stub_ptr );
        
      }
    }
    
    if(_mod_par->get_verbosity() == MUTOO::ALOT)
    {
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

//_______________________________________________________________________
void mMutFindTrack::associate_trk(unsigned short station)
{
  // Loop over tracks
  //		Loop over associated stubs
  //			Loop over associated TMutCoord
  //				Associated TMutTrk and TMutCoord
  //			]
  //			Loop over associated TMutGapCoord
  //				Associate TMutTrk and TMutGapCoord
  //			]
  //		]
  // ]

  //	Add capability to do associations as you go through each station
  //	==> need to only add associations for this station

  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next()){
        
    TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();

    // Punt on track with less than 3 stubs
    //
    if(stub_iter.count() < 3 && station == MUTOO::Station1) {
      trk_ptr->get()->set_no_estimate();
      continue;
    }

    TMutStubMap::pointer us_stub_ptr = 0;
    TMutStubMap::pointer ds_stub_ptr = 0;
    unsigned short min_station = USHRT_MAX;
    unsigned short max_station = 0;

    while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
      
      // Capture upstream stub ptr
      if( stub_ptr->get()->get_station() < min_station ) {
  min_station = stub_ptr->get()->get_station();
  us_stub_ptr = stub_ptr;
      }

      if( stub_ptr->get()->get_station() > max_station ) {
  max_station = stub_ptr->get()->get_station();
  ds_stub_ptr = stub_ptr;
      }
      
      //	Only add associations for this station:
      if (stub_ptr->get()->get_station() == station) {
      
        TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
        while(TMutCoordMap::pointer coord_ptr = coord_iter.next())
    PHKey::associate(coord_ptr, trk_ptr);

        TMutGapCoordMap::key_iterator gap_iter = stub_ptr->get()->get_associated<TMutGapCoord>();
        while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next())
    PHKey::associate(gap_ptr, trk_ptr);

      }
    }
    
    // Estimate the momentum of the track based upons a dphi,theta->momentum lookup
    // if we are at station 1.

    if (station == MUTOO::Station1){
      double momentum=0;
      if(_mod_par->get_init_mode() == mMutFindTrackPar::DPHI) {
        momentum = trk_ptr->get()->calculate_momentum_point_point();
      } else if(_mod_par->get_init_mode() == mMutFindTrackPar::E_DPHI) {
        momentum = trk_ptr->get()->calculate_momentum_point_point2();
      } else {
        momentum = 20.0;
      }

      // Choice of z reference set in runtime parameter table
      unsigned short ref_station = (_mod_par->get_ref_plane() == mMutFindTrackPar::UPSTREAM) ?
        MUTOO::Station1 : MUTOO::Station3;

      // Attempt to estimate tangent from stubs -- first of pair indicates status
      pair<bool,PHVector> tangent_pair = trk_ptr->get()->estimate_tangent(ref_station);

      // Attempt to estimate charge from stubs -- first of pair indicates status
      pair<bool,int> charge_pair = trk_ptr->get()->estimate_charge();

      // If everything is copasetic then initialize the track -- otherwise use a default
      // initialization and set the NO_ESTIMATE bit.
      if(momentum>0 && tangent_pair.first && charge_pair.first) {
     
        // Intersection of track tangent with ref plane
        PHPoint anchor;
        if(_mod_par->get_ref_plane() == mMutFindTrackPar::UPSTREAM) {	
    anchor.setX(us_stub_ptr->get()->get_fit_par()->get_x());
    anchor.setY(us_stub_ptr->get()->get_fit_par()->get_y());
    anchor.setZ(us_stub_ptr->get()->get_fit_par()->get_z());						
        } else {
    anchor =	TMutTrackUtil::linear_track_model(ds_stub_ptr->get()->get_fit_par(),
                  ds_stub_ptr->get()->get_fit_par()->get_z_end());				
        }
  
        TMutTrkPar local_trk(anchor.getX(),
           anchor.getY(),
           anchor.getZ(),
           momentum * tangent_pair.second.getX(),
           momentum * tangent_pair.second.getY(),
           momentum * tangent_pair.second.getZ(),
           charge_pair.second);
  
        // Initial momentum magnitude is estimated using a dphi,theta->momentum lookup and 
        // is tangent to the station one stub.

        trk_ptr->get()->set_trk_par(local_trk);		
  
        // If estimated momentum is below threshold set the low momentum bit
        if(momentum<_mod_par->get_low_momentum_threshold()) trk_ptr->get()->set_low_mom();
      
      } else {
      
        PHVector p_vector(us_stub_ptr->get()->get_fit_par()->get_x(),
        us_stub_ptr->get()->get_fit_par()->get_y(),
        us_stub_ptr->get()->get_fit_par()->get_z());
      
        p_vector.normalize();
        TMutTrkPar local_trk(us_stub_ptr->get()->get_fit_par()->get_x(),
           us_stub_ptr->get()->get_fit_par()->get_y(),
           us_stub_ptr->get()->get_fit_par()->get_z(),
           10*p_vector.getX(),
           10*p_vector.getY(),
           10*p_vector.getZ(),
           1);
  
        // Initial momentum 10 GeV magnitude tangent to the line connecting
        // the station 3 stub with the with the nominal vertex (0,0,0)
        trk_ptr->get()->set_trk_par(local_trk);					
  
        // Set no estimate bit, indicating that either charge||tangent||momentum estimate
        // failed
        //
        // trk_ptr->get()->set_no_estimate();

      }			
    }	// If station 1, do track estimation 
        
  }	// loop over tracks
}	

//_________________________________________________________________________
void mMutFindTrack::print_summary()
{
  MUTOO::PRINT(cout,"mMutFindTrack::print_summary");
  TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){

    cout << "track summary: key: " << trk_ptr->get()->get_key().get_obj_key() <<
      " arm: " << trk_ptr->get()->get_arm() << " oct: " << trk_ptr->get()->get_octant(); 

    cout << " road key/depth: "; 
    TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
    while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next()){
      cout << road_ptr->get()->get_key().get_obj_key() << ":" << road_ptr->get()->get_depth() << " ";			
    }

    cout << " stubs: ";
    TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
    while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
      cout << stub_ptr->get()->get_key().get_obj_key() << " "; 
    }
    cout << endl;
  }
  MUTOO::PRINT(cout,"**");
}

//___________________________________________
bool mMutFindTrack::initialize_track_ntuples( void )
{
  
  if( _created_tracks ) return false;
  
  _filename = _mod_par->get_evaluation_filename();
  PHTFileServer::get().open( _filename );
  _created_tracks = new TNtuple(
    "created_tracks",
    "created_tracks",
    "ntracks:arm:station:nroads"
  );	
  
  return true;

}

//___________________________________________
void mMutFindTrack::finish_evaluation(void) 
{
  MUTOO::TRACE("mMutFindTrack::finish_evaluation");
  if( !_created_tracks ) return;
  
  PHTFileServer::get().write( _filename );
  _created_tracks = 0;

}

//___________________________________________
void mMutFindTrack::print_stub_summary()
{
  MUTOO::PRINT(cout,"mMutFindTrack::print_stub_summary");
  TMutStubMap::const_iterator stub_iter = _stub_map->range();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    cout << "stub summary: key: " << stub_ptr->get()->get_key().get_obj_key() <<
      " arm: " << stub_ptr->get()->get_arm() << " sta: " << stub_ptr->get()->get_station() << " oct: " << stub_ptr->get()->get_octant(); 
    cout << " coords: ";
    TMutCoordMap::const_key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
    while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
      cout << coord_ptr->get()->get_key().get_obj_key() << " "; 
    }
    cout << endl;
  }
  MUTOO::PRINT(cout,"**");
}
    
//_______________________________________________________________________
void mMutFindTrack::print_associations( TMutTrkMap::const_pointer trk_ptr )
{ 
  cout << "mMutFindTrack::print_associations - trk=" << trk_ptr->get()->get_key().get_obj_key();
  
  // dump associated roads
  cout << " roads=";
  TMuiRoadMapO::const_key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );
  while( TMuiRoadMapO::const_pointer road_ptr = road_iter.next() )
  cout << " " << road_ptr->get()->get_key().get_obj_key();
  
  // dump associated coords
  cout << " coords=";
  TMutCoordMap::const_key_iterator coord_iter( trk_ptr->get()->get_associated<TMutCoord>() );
  while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
  cout << " " << coord_ptr->get()->get_key().get_obj_key();
  cout << endl;
}
