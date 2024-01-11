// $Id: mMutFindTrackMC.cxx,v 1.18 2011/07/14 04:26:09 richi Exp $
/*!
  \file		mMutFindTrackMC.cxx
  \brief	 Associate TMutCoord with TMutTrk using monte-carlo information (perfect pattern recognition)
  \author	S.Kelly
  \version $Revision: 1.18 $
  \date		$Date: 2011/07/14 04:26:09 $
*/

// MUTOO headers
//
#include <mMutFindTrackMC.h>
#include <mMutFindTrackMCPar.h>
#include <TMutNode.h>
#include <TMutHitMap.h>
#include <TMutVtxMap.h>
#include <TMutTrkMap.h>
#include <TMutCoordMap.h>
#include <TMutClusMap.h>
#include <TMutHitMap.h>
#include <TMutMCTrkMap.h>
#include <TMutMCHitMap.h>
#include <PHException.h>
#include <MUTOO.h>
#include <PHTimer.h>
#include <TMutGapCoordMap.h>
#include <TMutCoordMap.h>
#include <TMutTrkMap.h>
#include <TMutStubMap.h>
#include <TMutTrackUtil.h>
#include <PHGeometry.h>
#include <PHTrackIntegratorKF.h>
#include <TMutGeo.h>

// #include <TMutMSCovar.h>

// STL/BOOST/GSL
//
#include <gsl/gsl_fit.h>
#include <cmath>
#include <iostream>
#include <string>
#include <boost/array.hpp>

using namespace std;

/*! \ingroup modules */
//______________________________________________________________
//! Constructor
mMutFindTrackMC::mMutFindTrackMC() : 
  _timer( PHTimeServer::get()->insert_new("mMutFindTrackMC") )
{
  MUTOO::TRACE("initializing module mMutFindTrackMC",MUTOO::ALOT);
}

//______________________________________________________________
//! Event method
PHBoolean mMutFindTrackMC::event(PHCompositeNode* top_node)
{
  
  _timer.get()->restart(); 
  
  try { 
    // Reset IOC pointers
    set_interface_ptrs(top_node);		

    // clear maps
    _stub_map->clear();
    _trk_map->clear();
    _vtx_map->clear();
    
    // Promote TMutMCHit association from TMutHit to TMutCoord
    promote_associations();		

    // find TMutTrk and associate TMutCoord using TMutMCTrk/TMutMCHit
    find_tracks();

    // find TMutStub and associate with TMutTrk using TMutMCTrk/TMutMCHit
    find_stubs();

    // Set the TMutRecoPar at the vertex (for vertex fit)
    set_reco_vtx_pars();

    if(_mod_par->get_vtx_mode() > mMutFindTrackMCPar::VTX_NONE)
    find_vtx();
    
  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }	
  
  // If verbose dump the contents of the cluster map
  
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _trk_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();		 
  
  return True;
}

//______________________________________________________________
//! Reset IOC and external interface pointers 
void mMutFindTrackMC::set_interface_ptrs(PHCompositeNode* top_node)
{	
  
  // module runtime parameters
  //
  _mod_par = TMutNode<mMutFindTrackMCPar>::find_node(top_node,"mMutFindTrackMCPar");
  
  _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  _mc_hit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap");
  _vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
  _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
  _clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
} 

//______________________________________________________________
void mMutFindTrackMC::promote_associations()
{
  // TMutMCHits are associated with TMutHit objects after running the mMutResponse
  // module.	It is convienient here to associate TMutMCHit's with TMutCoord since
  // these are associated with TMutTrk objects and are used in the reconstruction.
  // So here we loop over TMutCoord objects -- trace through their association tree
  // until we get to the underlying TMutMCHit and make the association between 
  // TMutMCHit and TMutCoord explicit.
  TMutCoordMap::iterator coord_iter = _coord_map->range();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
    
    // TMutCoord -> TMutClus
    TMutClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
    while(TMutClusMap::pointer clus_ptr = clus_iter.next()){
      
      // TMutClus -> TMutHit
      TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
      if(!hit_iter.at_end()){
        TMutHitMap::pointer hit_ptr = hit_iter.current();
        
        // TMutHit->TMutMCHit
        TMutMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMutMCHit>();
        while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
        PHKey::associate(mc_hit_ptr,coord_ptr);
        
      }
    }
  }
}

//______________________________________________________________
void mMutFindTrackMC::find_tracks(){
  
  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next())
  {
    
    // If no hits associated with this MC track -- continue
    TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
    if(mc_hit_iter.at_end()) continue;
    
    // Insert a new track into the track map
    TMutTrkMap::iterator trk_iter = _trk_map->insert_new(mc_trk_ptr->get()->get_arm(),
               mc_hit_iter->get()->get_octant());
    
    // Associate the new TMutMCTrk with the TMutTrk (IMPORTANT -- this invalidates the
    // above key_iterator)
    //
    PHKey::associate(trk_iter.current(),mc_trk_ptr);

    // Initialize the TMutTrkPar in the TMutTrk using the downstream parameters from the
    // TMutMCTrk object
    //
    if(_mod_par->get_init_mode() == mMutFindTrackMCPar::PERFECT){
      TMutTrkPar local_trk(mc_trk_ptr->get()->get_x_us_gap(),
         mc_trk_ptr->get()->get_y_us_gap(),
         mc_trk_ptr->get()->get_z_us_gap(),
         mc_trk_ptr->get()->get_px_us_gap(),
         mc_trk_ptr->get()->get_py_us_gap(),
         mc_trk_ptr->get()->get_pz_us_gap(),
         static_cast<int>(mc_trk_ptr->get()->get_charge()));		
      trk_iter->get()->set_trk_par(local_trk);		
      trk_iter->get()->set_charge(static_cast<int>(mc_trk_ptr->get()->get_charge()));		
    } else {

      // Initial momentum is of unit magnitude along the line connecting
      // the track with the nominal vertex (0,0,0)
      //			
      PHVector p_vector(mc_trk_ptr->get()->get_x_us_gap(),
      mc_trk_ptr->get()->get_y_us_gap(),
      mc_trk_ptr->get()->get_z_us_gap());

      p_vector.normalize();
      
      TMutTrkPar local_trk(mc_trk_ptr->get()->get_x_us_gap(),
         mc_trk_ptr->get()->get_y_us_gap(),
         mc_trk_ptr->get()->get_z_us_gap(),
         p_vector.getX(),
         p_vector.getY(),
         p_vector.getZ(),
         1);
      
      trk_iter->get()->set_trk_par(local_trk);		
    }
    
    TMutFitPar local_fit(mc_trk_ptr->get()->get_x_us_gap(),
       mc_trk_ptr->get()->get_y_us_gap(),
       mc_trk_ptr->get()->get_z_us_gap());
    
    trk_iter->get()->set_fit_par(local_fit);		

    //	Associate TMutCoord with TMutTrk using TMutMCHit
    //
    mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
    while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){
      TMutCoordMap::key_iterator coord_iter = mc_hit_ptr->get()->get_associated<TMutCoord>();
      while(TMutCoordMap::pointer coord_ptr = coord_iter.next())
      PHKey::associate(coord_ptr,trk_iter.current());
    }
  }
}

//______________________________________________________________
void mMutFindTrackMC::find_vtx()
{
  // Here we create TMutVtx objects and initialize using various
  // degrees of MC truth info depending on module runtime parameter
  // vtx mode.
  //
  for(int arm=0; arm<MUTOO::NumberOfArms; ++arm)
  {
  
    // Associate all unique pairs of TMutTrk with a vertex object
    TMutTrkMap::iterator trk_iter1 = _trk_map->get(arm);
    while(TMutTrkMap::pointer trk_ptr1 = trk_iter1.next())
    {
    
      TMutTrkMap::iterator trk_iter2 = trk_iter1;
      if(trk_iter2.at_end()) break;
      
      while(TMutTrkMap::pointer trk_ptr2 = trk_iter2.next())
      {
        
        // Require the Track has a stub in all three stations
        if( !( 
          trk_ptr1->get()->has_stub(MUTOO::Station1) && 
          trk_ptr2->get()->has_stub(MUTOO::Station1) &&
          trk_ptr1->get()->has_stub(MUTOO::Station2) && 
          trk_ptr2->get()->has_stub(MUTOO::Station2) &&
          trk_ptr1->get()->has_stub(MUTOO::Station3) && 
          trk_ptr2->get()->has_stub(MUTOO::Station3) 
         )	) continue;
      
        // Create a new TMutVtx object
        TMutVtxMap::iterator vtx_iter = _vtx_map->insert_new(trk_ptr2->get()->get_arm()); 
        PHKey::associate(vtx_iter.current(), trk_ptr1);
        PHKey::associate(vtx_iter.current(), trk_ptr2);
      
        // Capture ptrs to associated MC track
        TMutMCTrkMap::const_key_iterator trk1_mc_iter = trk_ptr1->get()->get_associated<TMutMCTrk>();
        TMutMCTrkMap::const_key_iterator trk2_mc_iter = trk_ptr2->get()->get_associated<TMutMCTrk>();
        if( trk1_mc_iter.at_end() || trk2_mc_iter.at_end()) continue;
        
        // PERFECT mode right the truth parameters 
        if(_mod_par->get_vtx_mode() == mMutFindTrackMCPar::VTX_PERFECT) {
          
          vtx_iter->get()->set_x( trk1_mc_iter->get()->get_x_orig() );
          vtx_iter->get()->set_y( trk1_mc_iter->get()->get_y_orig() );
          vtx_iter->get()->set_z( trk1_mc_iter->get()->get_z_orig() );
          
          vtx_iter->get()->set_px1( trk1_mc_iter->get()->get_px_orig() );
          vtx_iter->get()->set_py1( trk1_mc_iter->get()->get_py_orig() );
          vtx_iter->get()->set_pz1( trk1_mc_iter->get()->get_pz_orig() );
          vtx_iter->get()->set_charge1( trk1_mc_iter->get()->get_charge() );
          
          vtx_iter->get()->set_px2( trk2_mc_iter->get()->get_px_orig() );
          vtx_iter->get()->set_py2( trk2_mc_iter->get()->get_py_orig() );
          vtx_iter->get()->set_pz2( trk2_mc_iter->get()->get_pz_orig() );
          vtx_iter->get()->set_charge2( trk2_mc_iter->get()->get_charge() );
          
        } else {
          
          PHTrackIntegratorKF integrator;
          
          // Extrapolate first track to z = 0;
          integrator.initialize( *trk_ptr1->get()->get_trk_par_vtx() );
          integrator.extrapolate(0);
          if( integrator.get_error() )
          {
            cout << "mMutFindTrackMC::find_vtx - unable to extrapolate first track to z=0" << endl;
            continue;
          }
          
          // store extrapolated track parameters
          TMutTrkPar trk_par_vtx_1;
          integrator.finish( trk_par_vtx_1 );
            
          // Extrapolate second track to z = 0;
          integrator.initialize( *trk_ptr2->get()->get_trk_par_vtx() );          
          integrator.extrapolate(0);
          
          if( integrator.get_error() )
          {
            cout << "mMutFindTrackMC::find_vtx - unable to extrapolate second track to z=0" << endl;
            continue;
          }
          
          TMutTrkPar trk_par_vtx_2;
          integrator.finish( trk_par_vtx_2 );
          
          // Write the parameters from TMutTrk vtx data members & PHENIX nominal vertex
          vtx_iter->get()->set_x( 0 );
          vtx_iter->get()->set_y( 0 );
          vtx_iter->get()->set_z( 0 );
                                        
          vtx_iter->get()->set_px1( trk_par_vtx_1.get_px() );
          vtx_iter->get()->set_py1( trk_par_vtx_1.get_py() );
          vtx_iter->get()->set_pz1( trk_par_vtx_1.get_pz() );
          vtx_iter->get()->set_charge1( trk_par_vtx_1.get_charge() );
                                        
          vtx_iter->get()->set_px2( trk_par_vtx_2.get_px() );
          vtx_iter->get()->set_py2( trk_par_vtx_2.get_py() );
          vtx_iter->get()->set_pz2( trk_par_vtx_2.get_pz() );
          vtx_iter->get()->set_charge2( trk_par_vtx_2.get_charge());
          
        }
      }
    }
  }
} 

//______________________________________________________________
void mMutFindTrackMC::find_stubs()
{
  // Loop over tracks [
  //	 Loop over associated coords [
  //		 If coord is in gap0 [
  //			 Get associated monte-carlo hit
  //			 Instantiate stub using mc hit data
  //		 ]
  //	 ]
  // ]

  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next()){
    TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();		
    while(TMutCoordMap::pointer coord_ptr = coord_iter.next())
    if(coord_ptr->get()->get_gap() == 0 && coord_ptr->get()->get_cathode() == 0) {
      TMutMCHitMap::key_iterator mc_hit_iter = coord_ptr->get()->get_associated<TMutMCHit>();
      if(!mc_hit_iter.at_end()) {
        
        // Insert a new stub 
        TMutStubMap::iterator stub_iter = _stub_map->insert_new(mc_hit_iter->get()->get_arm(),
                      mc_hit_iter->get()->get_station(),
                      mc_hit_iter->get()->get_octant(),
                      mc_hit_iter->get()->get_half_octant());
        // Use TMutMCHit to set stubs TMutFitPar
        //
        stub_iter->get()->set_fit_par(mc_hit_iter->get()->get_fit_par());
    
        // Associate stub with track
        //
        PHKey::associate(trk_ptr,stub_iter.current());		
      }
    }
  }

  // Loop over tracks 
  //	 Loop over coords 
  //		 Loop over stubs 
  //			 if coord in same station as stub 
  //					associate stub and coord
  //					store associated gap coordinate in sortable list
  //			 ] 
  //		 ]
  //	 ]
  //
  //	 Sort and remove duplicate gap coordinates from local list
  //
  //	 Loop over stubs 
  //		 Loop over gap_coord
  //			 if gap_coord in same station as stub 
  //					associate stub and gap_coord
  //			 ] 
  //		 ]
  //	 ]
  // ]

  // Local sortable storage for gap coordinates
  //
  typedef list<TMutGapCoordMap::value_type> gap_coord_list_type;
  gap_coord_list_type gap_coord_list;
  
  trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next()){
    TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();		
    while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
      TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();		
      while(TMutStubMap::pointer stub_ptr = stub_iter.next())
      if(coord_ptr->get()->get_station() == stub_ptr->get()->get_station()) 
      PHKey::associate(coord_ptr,stub_ptr);
    }

    TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();		
    while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
      
      // clear local storage
      gap_coord_list.clear();
      TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();		
      while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
        // push gap coord associated with coord onto local list
        TMutGapCoordMap::key_iterator gap_iter = coord_ptr->get()->get_associated<TMutGapCoord>();
        if(!gap_iter.at_end()) gap_coord_list.push_back(*gap_iter);
      }			

      // Remove duplicates from gap coord list
      gap_coord_list.sort();
      gap_coord_list.unique();

      // Associate unique gap coords with stub
      gap_coord_list_type::iterator gap_iter = gap_coord_list.begin();
      for(;gap_iter!=gap_coord_list.end();++gap_iter)
      if(gap_iter->get()->get_station() == stub_ptr->get()->get_station()){
        PHKey::associate(*gap_iter,*stub_ptr);
        PHKey::associate(*gap_iter,*trk_ptr);
      }
    }
  }
}

//______________________________________________________________
void mMutFindTrackMC::set_reco_vtx_pars()
{	
  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
  {
    
    TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
    if(mc_trk_iter.at_end()) continue;
    
    // Initialize the integrator with the momentum at the vertex
    if(_mod_par->get_vtx_mode() == mMutFindTrackMCPar::VTX_PERFECT) 
    {
      
      trk_ptr->get()->set_trk_par_vtx(
        TMutTrkPar(
          mc_trk_iter->get()->get_x_orig(),
          mc_trk_iter->get()->get_y_orig(),
          mc_trk_iter->get()->get_z_orig(),
          mc_trk_iter->get()->get_px_orig(),
          mc_trk_iter->get()->get_py_orig(),
          mc_trk_iter->get()->get_pz_orig(),
          (int) mc_trk_iter->get()->get_charge()
        ) );
    
    } else {
      PHTrackIntegratorKF integrator;
      TMutTrkPar trk_par(
        mc_trk_iter->get()->get_x_us_gap(),
        mc_trk_iter->get()->get_y_us_gap(),
        mc_trk_iter->get()->get_z_us_gap(),
        mc_trk_iter->get()->get_px_us_gap(),
        mc_trk_iter->get()->get_py_us_gap(),
        mc_trk_iter->get()->get_pz_us_gap(),
        (int)mc_trk_iter->get()->get_charge() );
      integrator.initialize( trk_par );
      integrator.extrapolate( mc_trk_iter->get()->get_pz_orig() );
      if( integrator.get_error() ) 
      {
        cout << "mMutFindTrackMC::set_reco_vtx_pars - extrapolation to z=" <<  mc_trk_iter->get()->get_pz_orig() << " failed." << endl;
        continue;
      }
      
      // update track parameters
      integrator.finish( trk_par );
      trk_ptr->get()->set_trk_par_vtx( trk_par );
     
    }
    
  }
}

//______________________________________________________________
double mMutFindTrackMC::energy_loss(TMutTrkMap::pointer trk_ptr){
  
  static boost::array<double,4> theta_17 = {{1.135760, 1.026590, -0.000003, -1.034864}};
  static boost::array<double,4> theta_23 = {{1.180041, 1.027412, -0.000018, -1.146959}};
  static boost::array<double,4> theta_28 = {{1.218594, 1.031854, -0.000023, -1.173510}};
  static boost::array<double,4> theta_33 = {{1.264924, 1.035123, -0.000038, -1.269907}};
  
  double p_vertex=0;		
  
  TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    
    if(stub_ptr->get()->get_station() != MUTOO::Station2) continue;
    
    double r = sqrt(MUTOO::SQUARE(stub_ptr->get()->get_fit_par()->get_x()) +
       MUTOO::SQUARE(stub_ptr->get()->get_fit_par()->get_y()));		
    
    // theta from station 2 stub
    double theta_deg = MUTOO::RAD_TO_DEG*atan2(r, fabs(stub_ptr->get()->get_fit_par()->get_z()));	
    
    // total momentum at the vertex using parameterization by JN and reconstructed momentum
    if(theta_deg < 17) p_vertex = calc_p_vertex(theta_17,trk_ptr->get()->get_trk_par()->get_ptot());
    else if( theta_deg > 17 && theta_deg <= 23) {
      double w = (theta_deg-17)/5.0;		
      double p_1 = calc_p_vertex(theta_17,trk_ptr->get()->get_trk_par()->get_ptot());
      double p_2 = calc_p_vertex(theta_23,trk_ptr->get()->get_trk_par()->get_ptot());
      p_vertex = (1-w)*p_1 + w*p_2;
    } else if( theta_deg > 23 && theta_deg <= 28) {
      double w = (theta_deg-23)/5.0;		
      double p_1 = calc_p_vertex(theta_23,trk_ptr->get()->get_trk_par()->get_ptot());
      double p_2 = calc_p_vertex(theta_28,trk_ptr->get()->get_trk_par()->get_ptot());
      p_vertex = (1-w)*p_1 + w*p_2;
    } else {
      p_vertex = calc_p_vertex(theta_33,trk_ptr->get()->get_trk_par()->get_ptot());
    }
  }	
    
  // Return delta p
  return p_vertex - trk_ptr->get()->get_trk_par()->get_ptot();
}

//______________________________________________________________
double mMutFindTrackMC::calc_p_vertex(const boost::array<double,4>& pars, double p_trk)
{ return (pars[0] + pars[1]*p_trk + pars[2]*exp(-pars[3]*p_trk)); }



