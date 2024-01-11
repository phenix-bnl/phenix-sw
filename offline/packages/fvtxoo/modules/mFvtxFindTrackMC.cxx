// $Id: mFvtxFindTrackMC.cxx,v 1.10 2009/09/18 04:40:01 hpereira Exp $

/*!
  \file mFvtxFindTrackMC.cxx
  \brief Associate TFvtxCoord with TFvtxTrk using monte-carlo information (perfect pattern recognition)
  \author Melynda Brooks
  \version $Revision: 1.10 $
  \date $Date: 2009/09/18 04:40:01 $
*/

#include <TMutNode.h>

#include <TFvtxClusMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxTrkMap.h>
#include <TMutMCTrkMap.h>

#include <PHException.h>
#include <FVTXOO.h>

// STL/BOOST/GSL
#include <gsl/gsl_fit.h>
#include <gsl/gsl_randist.h>
#include <cmath>
#include <iostream>
#include <string>
#include <boost/array.hpp>

#include "mFvtxFindTrackMC.h"
#include "mFvtxFindTrackMCPar.h"

using namespace std;

/*! \ingroup modules */
//______________________________________________________________
mFvtxFindTrackMC::mFvtxFindTrackMC():
  _timer( PHTimeServer::get()->insert_new("mFvtxFindTrackMC") )
{ FVTXOO::TRACE("initializing module mFvtxFindTrackMC"); }

//______________________________________________________________
//! Event method
PHBoolean mFvtxFindTrackMC::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {
    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // clear maps
    _trk_map->clear();

    // Promote TFvtxMCHit association from TFvtxHit to TFvtxCoord
    promote_associations();

    // make TFvtxTrk objects from TMutMCCTrk objects and associate
    // TFvtxCoord, TFvtxMCHits using TMutMCTrk/TFvtxMCHit information
    find_tracks();


  } catch(std::exception& e) {
    FVTXOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the track map

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _trk_map->print();
  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();

  return True;
}

//______________________________________________________________
//! Reset IOC and external interface pointers
void mFvtxFindTrackMC::set_interface_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mFvtxFindTrackMCPar>::find_node(top_node,"mFvtxFindTrackMCPar");
  _pisa_hit_map = TMutNode<TFvtxPisaHitMap>::find_node(top_node,"TFvtxPisaHitMap");
  _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  _mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(top_node,"TFvtxMCHitMap");
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  _clus_map = TMutNode<TFvtxClusMap>::find_node(top_node,"TFvtxClusMap");
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");

}

//______________________________________________________________
void mFvtxFindTrackMC::promote_associations()
{
  // TFvtxMCHits are associated with TFvtxHit objects after running the mFvtxResponse
  // module.  It is convienient here to associate TFvtxMCHit's with TFvtxCoord since
  // these are associated with TFvtxTrk objects and are used in the reconstruction.
  // So here we loop over TFvtxCoord objects -- trace through their association tree
  // until we get to the underlying TFvtxMCHit and make the association between
  // TFvtxMCHit and TFvtxCoord explicit.
  TFvtxCoordMap::iterator coord_iter = _coord_map->range();
  while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){

    // TFvtxCoord -> TFvtxClus
    TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
    while(TFvtxClusMap::pointer clus_ptr = clus_iter.next()){

      // TFvtxClus -> TFvtxHit
      TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
      if(!hit_iter.at_end()){
        TFvtxHitMap::pointer hit_ptr = hit_iter.current();

        // TFvtxHit->TFvtxMCHit
        TFvtxMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TFvtxMCHit>();
        while(TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
        PHKey::associate(mc_hit_ptr,coord_ptr);

      }
    }
  }
}

//______________________________________________________________
void mFvtxFindTrackMC::find_tracks()
{

  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next())
  {

    // If no hits associated with this MC track -- continue
    TFvtxMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TFvtxMCHit>();
    if(mc_hit_iter.at_end()) continue;

    // Insert a new track into the track map
    TFvtxTrkMap::iterator trk_iter = _trk_map->insert_new(mc_trk_ptr->get()->get_arm());

    // Associate the new TMutMCTrk with the TFvtxTrk (IMPORTANT -- this invalidates the above key_iterator)
    PHKey::associate(trk_iter.current(),mc_trk_ptr);

    // Initialize the TFvtxTrkPar in the TFvtxTrk using the origin paramters from the
    // TMutMCTrk object
    if(_mod_par->get_init_mode() == mFvtxFindTrackMCPar::PERFECT)
    {

      TMutTrkPar local_trk(mc_trk_ptr->get()->get_x_orig(),
         mc_trk_ptr->get()->get_y_orig(),
         mc_trk_ptr->get()->get_z_orig(),
         mc_trk_ptr->get()->get_px_orig(),
         mc_trk_ptr->get()->get_py_orig(),
         mc_trk_ptr->get()->get_pz_orig(),
         static_cast<int>(mc_trk_ptr->get()->get_charge()));

      trk_iter->get()->set_trk_par(local_trk);
      trk_iter->get()->set_charge(static_cast<int>(mc_trk_ptr->get()->get_charge()));

    } else {

      // Initial momentum should be set to something non-perfect here,
      // I just set constant value, with appropriate pz sign for arm.
      PHVector p_vector(50.0, 50.0, 100.0);

      // Put 20% error on true MC vector and store this.
      p_vector.setX(mc_trk_ptr->get()->get_px_orig() + gsl_ran_gaussian(_rng.get(), 0.2)*mc_trk_ptr->get()->get_px_orig());
      p_vector.setY(mc_trk_ptr->get()->get_py_orig() + gsl_ran_gaussian(_rng.get(), 0.2)*mc_trk_ptr->get()->get_py_orig());
      p_vector.setZ(mc_trk_ptr->get()->get_pz_orig() + gsl_ran_gaussian(_rng.get(), 0.2)*mc_trk_ptr->get()->get_pz_orig());

      TMutTrkPar local_trk(0.0,
         0.0,
         mc_trk_ptr->get()->get_z_orig(),
         p_vector.getX(),
         p_vector.getY(),
         p_vector.getZ(),
         int(mc_trk_ptr->get()->get_charge()));  //use mutr reco sign in future

      if(_mod_par->get_verbosity() >= FVTXOO::ALOT){
        std::cout << "mFvtxFindTrackMC::find_tracks() initial track setting = " << std::endl;
        local_trk.print();
      }

      trk_iter->get()->set_trk_par(local_trk);
    }

    //  Associate TFvtxCoord and TFvtxMCHit with TFvtxTrk using TFvtxMCHit
    mc_hit_iter = mc_trk_ptr->get()->get_associated<TFvtxMCHit>();
    while(TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
    {

      // Optionally put a cut on the position of the MC hit before it is associated:
      if (sqrt( FVTXOO::SQUARE(mc_hit_ptr->get()->get_x()) +
                FVTXOO::SQUARE(mc_hit_ptr->get()->get_y())) < _mod_par->get_hit_min_r()) continue;

      PHKey::associate(mc_hit_ptr,trk_iter.current());

      TFvtxCoordMap::key_iterator coord_iter = mc_hit_ptr->get()->get_associated<TFvtxCoord>();
      while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
      { PHKey::associate(coord_ptr,trk_iter.current()); }
    }

    if( _mod_par->get_use_vtx_hits() )
    {

      TFvtxPisaHitMap::iterator iter( _pisa_hit_map->range() );
      while( TFvtxPisaHitMap::pointer ptr = iter.next() )
      {
        if ( ptr->get()->get_pisa_hit()->GetLayer() < 4 && (ptr->get()->get_pisa_hit()->GetMctrack() == mc_trk_ptr->get()->get_track_id() ) )
        {
          cout << "Found barrel silicon hit in mFvtxFindTrackMC at ZGlobal" << ptr->get()->get_pisa_hit()->GetZGlobal() << endl;
          PHKey::associate( trk_iter.current(), ptr );
        }
      }


      TFvtxPisaHitMap::key_iterator key_iter(trk_iter->get()->get_associated<TFvtxPisaHit>());
      while( TFvtxPisaHitMap::const_pointer ptr = key_iter.next() )
      { cout << "mFvtxFindTrackMC ZGlobal extracted= " << ptr->get()->get_pisa_hit()->GetZGlobal() << endl; }
    }
  }

}
