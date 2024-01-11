// $Id: mMuiResponse.cxx,v 1.4 2009/05/12 02:16:28 shoji Exp $

/*!
	 \file mMuiResponse.cxx
	 \brief convert MC hits into hits
	 \author C. Zhang
	 \version $Revision: 1.4 $
	 \date $Date: 2009/05/12 02:16:28 $
*/

// MUTOO/MUIOO headers
//
#include "mMuiResponse.h"
#include "mMuiResponsePar.h"
#include "TMuiMCHitMapO.h"
#include "TMuiHitMapO.h"
#include "TMuiHVMask.h"

//Mut Geometry
#include "TMuiGeometry.hh"

//Mut Calibration

// STL/BOOST/GSL
//
#include <iostream>
#include <string>
#include <gsl/gsl_randist.h>

/*! \ingroup modules */
using namespace std;

//____________________________________
mMuiResponse::mMuiResponse() :	 
	_timer( PHTimeServer::get()->insert_new( "mMuiResponse" ) )
{
	MUIOO::TRACE("initializing module mMuiResponse");	
}

//____________________________________
PHBoolean mMuiResponse::event(PHCompositeNode* top_node)
{
	_timer.get()->restart();	 
	try { 
		// Reset IOC pointers
		set_interface_ptrs(top_node);	
		_hit_map->clear();
					
		// Loop over TMuiMCHits and generate TMuiHits.
		// Add noise and apply masking where appropriate.
		response_loop();

	} catch(std::exception& e) {
		MUIOO::TRACE(e.what());
		return False;
	}		
	// If verbose dump the contents of the hit map
	//
	_timer.get()->stop();
	if(_mod_par->get_verbosity() >= MUIOO::ALOT) _hit_map->print();
	if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();
	return True;
}

//____________________________________
void 
mMuiResponse::set_interface_ptrs(PHCompositeNode* top_node)
{	
	// Module runtime parameters
	_mod_par = TMutNode<mMuiResponsePar>::find_node(top_node,"mMuiResponsePar");

	// IOC 
	_mc_hit_map = TMutNode<TMuiMCHitMapO>::find_node(top_node,"TMuiMCHitMapO");
	_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
	_hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");
} 

//____________________________________
void
mMuiResponse::response_loop()
{
	// Get an iterator to all the TMuiMCHits
	TMuiMCHitMapO::iterator mc_hit_iter = _mc_hit_map->range();

	// Loop over TMuiMCHits
	while(TMuiMCHitMapO::pointer mc_hit_ptr = mc_hit_iter.next())	
	fill_hit(mc_hit_ptr);
}

//____________________________________
bool 
mMuiResponse::is_alive(TMuiMCHitMapO::const_pointer mc_hit_ptr) 
{
	// TBI, some kind of code to check if the hit is in dead
	// area.
	//	.......................
	// return flase

	// Kill the hit if a random number is larger than the eff.
	//
	double x = gsl_ran_flat(_rng.get(),0.0,100.0);
	if (x > _mod_par->get_twopack_eff()) return false;
	if(_mod_par->get_verbosity() >= MUIOO::ALOT) {
		cout <<" ######## check gsl_ran in mMuiRespose ######### " << endl;
		cout <<" # gsl_ran_uniform = " << x << " in mMuiRespose #" << endl;
		cout <<" # twopack efficiency = "<<_mod_par->get_twopack_eff() <<"	#" << endl;
		cout <<" #####################################################" <<endl;
	}
	return true;
}

//____________________________________
void
mMuiResponse::fill_hit(TMuiMCHitMapO::pointer mc_hit_ptr)
{
  UShort_t arm = mc_hit_ptr->get()->get_arm();
  UShort_t plane = mc_hit_ptr->get()->get_plane();
  
  typedef TMuiMCHitO::twopack_list twopack_list;
  typedef TMuiMCHitO::twopack_list::iterator twopack_iterator;
  
  // Get an iterator for list of TMuiMCTwoPack from TMuiMCHit
  //
  twopack_list* twopacks = mc_hit_ptr->get()->get_mutable_twopack_list();
  
  // for checking if the particle passed two tubes or one tube
  PHPoint GlobalX( mc_hit_ptr->get()->get_x(),
		   mc_hit_ptr->get()->get_y(),
		   mc_hit_ptr->get()->get_z() );
  
  PHVector DirV( mc_hit_ptr->get()->get_px(),
		 mc_hit_ptr->get()->get_py(),
		 mc_hit_ptr->get()->get_pz() );
  DirV.normalize();
  
  for(twopack_iterator twopack_iter = twopacks->begin(); twopack_iter!=twopacks->end();++twopack_iter)
    {
      // retrieve panel, orientation and twopack
      UShort_t panel =	twopack_iter->get_panel();
      UShort_t orientation =	twopack_iter->get_orient();
      UShort_t twopack =	twopack_iter->get_twopack_index();
      
      TMuiPanelGeo *p = TMuiGeometry::Geom()->getPanel( arm, plane, panel );
      PHPoint intersection = p->ProjectToPanel (GlobalX, DirV);
      PHPoint local_intersect = p->TransformToPanel (intersection);
      PHVector local_direction = p->RotateToPanel (DirV);
      
      // check z length muon passed through
      float dz = 0;
      float fraction = 0;
      TMuiTwoPackGeo *t = p->TwoPackPointer( (EOrient_t)orientation, twopack );
      t->FrontTube().IsInTube(local_intersect, local_direction, dz);
      fraction += dz;
      t->BackTube().IsInTube(local_intersect, local_direction, dz);
      fraction += dz;
      
      // GetThickness() returns thickness of tubes
      float thick_twopack = 2 * t->GetThickness();
      fraction /= thick_twopack;
      
      // Check the HV and set the two pack status if this hit is masked
      if(_mod_par->get_use_hv_mask() && 
	 !check_efficiency(arm,plane,panel,orientation,twopack,fraction))
	{
	  twopack_iter->set_status(TMuiMCTwoPack::MASKED);
	  continue;
	}
      
      TMuiHitMapO::iterator hit_iter = _hit_map->get( arm,
						      plane,
						      panel,
						      orientation,
						      twopack );
      
      // if no matching hit found, create a new one; insert to map; 
      if( hit_iter.at_end() ) 
	hit_iter = _hit_map->insert_new(arm, plane, panel, orientation, twopack);
      
      // associate to MC hit
      PHKey::associate( mc_hit_ptr, hit_iter.current() ); 
    }
  
}

//____________________________________
bool mMuiResponse::check_efficiency(UShort_t arm, 
				    UShort_t plane, 
				    UShort_t panel, 
				    UShort_t orientation, 
				    UShort_t twopack,
				    float fraction) const
{		
  // Efficiency due to HV Mask
  if(_mod_par->get_use_hv_mask())
    {
      // Check HV state
      double random = gsl_ran_flat(_rng.get(),0.0,1.0);
      double effic_twopack = TMuiHVMask::get_effic_twopack(arm,plane,panel,orientation,twopack);
      
      if( effic_twopack < 1.0 )
	{
	  double effic_dz = 1.0 - exp( log( 1.0 - effic_twopack ) * fraction );
	  if( random > effic_dz ) return false;
	}
    }
  
  // Efficiency due to everything else
  double random = gsl_ran_flat(_rng.get(),0.0,1.0);
  double effic = _mod_par->get_twopack_eff();
  if (random > effic) return false;
  
  return true;
}
