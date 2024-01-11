// $Id: mFvtxFastSim.cxx,v 1.11 2015/06/11 20:59:17 snowball Exp $

/*!
	\file mFvtxFastSim.cxx
	\brief fast forward vertex simulation module. Generate TFvtxMCHit from already generated TMutMCTrk
	\author H. Pereira Da Costa
	\version $Revision: 1.11 $
	\date $Date: 2015/06/11 20:59:17 $
*/

#include <FVTXOO.h>
#include <FvtxGeom.h>
#include <TMutTrackUtil.h>
#include <TMutTrkPar.hh>
#include <PHTrackIntegratorKF.h>
#include <iostream>

#include "mFvtxFastSim.h"

using namespace std;

//_____________________________________________
mFvtxFastSim::mFvtxFastSim() : 
	_timer( PHTimeServer::get()->insert_new("mFvtxFastSim") )
{
	_total_mc_hits.assign(0);
	_accepted_mc_hits.assign(0);

	integrator = new PHTrackIntegratorKF();

	MUTOO::TRACE("initializing module mFvtxFastSim");

}

//_____________________________________________
mFvtxFastSim::~mFvtxFastSim() 
{
  if(integrator) delete integrator;
}

//_____________________________________________
void mFvtxFastSim::print_summary( ostream& out ) 
{ 
	FVTXOO::PRINT( out, "mFvtxFastSim::print_summary" );
	for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
	for( int station=0; station < FVTXOO::MAX_STATION; station++ )
	out << "arm=" << arm << " station=" << station 
		<< " total_mc_hits=" << _total_mc_hits[ mFvtxFastSimPar::get_acceptance_index( arm, station ) ]
		<< " accepted_mc_hits=" << _accepted_mc_hits[ mFvtxFastSimPar::get_acceptance_index( arm, station ) ]
		<< endl;
	FVTXOO::PRINT( out, "**" );
}

//_____________________________________________
PHBoolean mFvtxFastSim::event(PHCompositeNode* top_node)
{
	_timer.get()->restart();	 
	try { 

		// Reset IOC pointers
		set_interface_ptrs(top_node);

		// Loop over TMutMCTrk and do the digitization
		simulator_loop();

	} catch(std::exception& e) {
		FVTXOO::TRACE(e.what());
		return False;
	}	

	// If verbose dump the contents of trks and primary
	//
	_timer.get()->stop();
	if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _mc_hit_map->print();
	return True;
}

//_____________________________________________
void mFvtxFastSim::simulator_loop()
{
	// get iterator over all tracks
	TMutMCTrkMap::iterator iter( _mc_trk_map->range() );
	while( TMutMCTrkMap::pointer ptr = iter.next() )
	{
		// process track
		trk_loop( ptr );
	}
}

//_____________________________________________
void mFvtxFastSim::trk_loop( TMutMCTrkMap::pointer trk_ptr )
{

	//! get origin track parametes
	TMutTrkPar trk_par_orig(
			trk_ptr->get()->get_x_orig(),
			trk_ptr->get()->get_y_orig(),
			trk_ptr->get()->get_z_orig(),
			trk_ptr->get()->get_px_orig(),
			trk_ptr->get()->get_py_orig(),
			trk_ptr->get()->get_pz_orig(),
			static_cast<int>(trk_ptr->get()->get_charge())
		);
	
	// loop over Fvtx station
	for( unsigned int station_id = 0; station_id < static_cast<unsigned int>( FVTXOO::MAX_STATION ); station_id++ )
	{
          for( unsigned int cage_id = 0; cage_id < static_cast<unsigned int>( FVTXOO::MAX_CAGE ); cage_id++ )
          {
		const FvtxStation *station = FvtxGeom::get_arm( trk_ptr->get()->get_arm() )->get_cage( cage_id )->get_station( station_id );
		create_mc_hits( station, trk_ptr, trk_par_orig );
		_total_mc_hits[ mFvtxFastSimPar::get_acceptance_index( trk_ptr->get()->get_arm(), station_id ) ]++;
          }
  	}
 	
}

//_____________________________________________
void mFvtxFastSim::create_mc_hits( const FvtxStation* station, TMutMCTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par )
{
  integrator->clear();
  try {
    
    // store arm and station localy
    const unsigned int& arm_id( station->index().arm() );
    const unsigned int& cage_id( station->index().cage() );
    const unsigned int& station_id( station->index().station() ); 
    
    // get fvtx station z
    double z( station->get_z() );
    
    // extrapolate (in magnetic field) to FVTX station
    
    integrator->initialize( trk_par );
    integrator->extrapolate( z );
    
    // check extrapolation status
    if( integrator->get_error() ) {
      if( _mod_par->get_verbosity() >= FVTXOO::ALOT ) {
	cerr 
	  << "mFvtxFastSim::create_mc_hit - extrapolation failed "
	  << " (" << trk_par.get_z() << " -> " << z << ")" << endl;
      }
      return;
    }
    
    TMutTrkPar station_trk_par;
    integrator->finish( station_trk_par );
    const PHPoint& station_point( station_trk_par.get_point() );
    const PHVector& momentum( station_trk_par.get_momentum() );
    
    // retrieve Columns hit by the track parameters
    list<FvtxSector*> sectors( station->find_sectors( station_point ) );
    for( list<FvtxSector*>::const_iterator sector_iter = sectors.begin(); sector_iter != sectors.end(); sector_iter++ )
      for( unsigned int column_id = 0; column_id < FVTXGEOM::NumberOfColumns; column_id++ )
	{
	  // check if trk par hits the column
	  /* 
	     a linear extrapolation is performed towards the column exact z.
	     True Geant extrapolation is done only if the hit is inside the radius
	  */
	  FvtxColumn* column = (*sector_iter)->get_column( column_id );
	  PHPoint column_point( 
			       station_point.getX() + (momentum.getX()/momentum.getZ())*( column->get_z() - station_point.getZ() ), 
			       station_point.getY() + (momentum.getY()/momentum.getZ())*( column->get_z() - station_point.getZ() ),
			       column->get_z() );
	  
	  if( !column->contains( station_trk_par.get_point() ) ) continue;
	  
	  // extrapolate (in magnetic field) to FVTX station
	  //PHTrackIntegratorKF integrator;
	  integrator->clear();
	  integrator->initialize( station_trk_par );
	  integrator->extrapolate( column->get_z() );
	  
	  // check extrapolation status
	  if( integrator->get_error() ) {
	    if( _mod_par->get_verbosity() >= FVTXOO::NONE ) {
	      cerr 
		<< "mFvtxFastSim::create_mc_hit - extrapolation failed "
		<< " (" << trk_par.get_z() << " -> " << z << ")" << endl;
	    }
	    continue;
	  }
	  
	  TMutTrkPar column_trk_par;
	  integrator->finish( column_trk_par );
	  
	  // get strips and relative path length
	  typedef map< unsigned int, double > StripMap;
	  StripMap strip_map;
	  
	  if( _mod_par->get_do_clusters() )
	    {
	      
	      // calculate entrance and exit point
	      PHPoint point_in( TMutTrackUtil::linear_track_model( &column_trk_par, column->get_z()-column->get_delta_z()/2 ) );
	      PHPoint point_out( TMutTrackUtil::linear_track_model( &column_trk_par, column->get_z()+column->get_delta_z()/2 ) );
	      
	      /* 
		 look for strip indices mathing entrance and exit point of the track par	
		 and corresponding path length ratio; add to MC strip
	      */
	      list<FvtxStrip*> strips( column->find_strips( point_in, point_out ) );
	      for( list<FvtxStrip*>::iterator iter = strips.begin(); iter != strips.end(); iter++ )
		{
		  // calculate path length in strip
		  double path_length = (*iter)->get_path_length( point_in, point_out );
		  strip_map.insert( make_pair( (*iter)->get_strip_index(), path_length ) );
		}
	      
	    } else {
	    
	    /* 
	       do not create clusters. Only add the strip corresponding to the 
	       point in the middle of the column
	    */
	    
	    FvtxStrip* strip( column->find_strip( column_trk_par.get_point() ) );
	    if( strip ) strip_map.insert( make_pair( strip->get_strip_index(), 1 ) );
	    
	  }
	  
	  // check that some fired strips have been found
	  if( strip_map.empty() ) continue;
	  
	  // create MC hit
	  const unsigned int& sector_id( (*sector_iter)->index().sector() );
	  TFvtxMCHitMap::iterator mc_hit_iter( _mc_hit_map->insert_new( 
								       arm_id,
								       cage_id,
								       station_id,
								       sector_id,
								       column_id ) );
	  mc_hit_iter->get()->set_x( column_trk_par.get_x() );
	  mc_hit_iter->get()->set_y( column_trk_par.get_y() );
	  mc_hit_iter->get()->set_z( column_trk_par.get_z() );
	  
	  mc_hit_iter->get()->set_px( column_trk_par.get_px() );
	  mc_hit_iter->get()->set_py( column_trk_par.get_py() );
	  mc_hit_iter->get()->set_pz( column_trk_par.get_pz() );
	  
	  mc_hit_iter->get()->set_tof( 0 );
	  mc_hit_iter->get()->set_eloss( 1 );			
	  mc_hit_iter->get()->set_track_id( trk_ptr->get()->get_track_id() );
	  
	  // add strips to the mc_hit
	  for( StripMap::iterator iter = strip_map.begin(); iter != strip_map.end(); iter++ )
	    mc_hit_iter->get()->add_strip( iter->first, iter->second );
	  
	  // associate to mc_track
	  PHKey::associate( trk_ptr, mc_hit_iter.current() );
	  
	  // increment counter
	  _accepted_mc_hits[ mFvtxFastSimPar::get_acceptance_index( arm_id, station_id ) ]++;
	  
	  // dump
	  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
	    {
	      cout << "mFvtxFastSim::create_mc_hit - index: " << column->index() << endl;
	      mc_hit_iter.current()->get()->print();
	    }
	  
	}
    
  } catch( exception& e ) {
    cout << e.what() << endl;
  }
  

  return;
  
}
	
//______________________________________________________________________
/*! Reset IOC and external interface pointers */
void mFvtxFastSim::set_interface_ptrs(PHCompositeNode* top_node)
{	
	// module runtime parameters
	_mod_par = TMutNode<mFvtxFastSimPar>::find_node(top_node,"mFvtxFastSimPar");

	// IOC pointers
	_mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(top_node,"TFvtxMCHitMap");
	_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
} 

