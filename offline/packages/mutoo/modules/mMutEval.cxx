// $Id: mMutEval.cxx,v 1.34 2011/12/24 04:48:29 slash Exp $

/*!
   \file mMutEval.cxx
   \brief associate MC_Hit and MC_Trk information with tracks
   \author S. Kelly
   \version $Revision: 1.34 $
   \date $Date: 2011/12/24 04:48:29 $
*/
///////////////////////////////////////////////////////////////////

// MUTOO headers
#include<mMutEval.h>
#include<mMutEvalPar.h>
#include<TMutNode.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TMutHitMap.h>
#include<TMutClusMap.h>
#include<TMutGapCoordMap.h>
#include<TMutCoordMap.h>
#include<PHException.h>
#include<MUTOO.h>
#include<PHTrackIntegratorKF.h>
#include<TMutTrackModel.h>

// STL/BOOST
#include<string>
#include<iostream>

using namespace std;

//_____________________________________________________________
mMutEval::mMutEval():
  _timer(PHTimeServer::get()->insert_new("mMutEval") )
{
  MUTOO::TRACE("initializing module mMutEval",MUTOO::ALOT);
}

//_____________________________________________________________
PHBoolean mMutEval::event(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{

  _timer.get()->restart();
  try {

    // Reset IOC pointers
    set_interface_ptrs(signal_node, top_node);

    // clear evaluation map
    _eval_map->clear();

    // Unless we ran perfect pattern recogniton promote
    // the TMutMCHit associations to TMutCoord and
    // determine the principal contributor to the
    // TMutMCTrk
    if(_mod_par->get_pr_mode() == mMutEvalPar::NORMAL) {

      // associate TMutMCHits with TMutCoord
      associate_mchit();

      // associate TMutMCTrk with TMutTrk
      associate_mctrk();
    }

    // Do the evaluation
    //
    evaluate();

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the eval map
  //
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _eval_map->print();

  return True;
}

//_____________________________________________________________
void mMutEval::set_interface_ptrs(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMutEvalPar>::find_node(top_node,"mMutEvalPar");

  _mctrk_map = TMutNode<TMutMCTrkMap>::find_node(signal_node,"TMutMCTrkMap");
  _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  _eval_map = TMutNode<TMutEvalMap>::find_node(top_node,"TMutEvalMap");
}

//_____________________________________________________________
void mMutEval::evaluate()
{

  // Loop over all the MC tracks [
  //	 Insert a new TMutEval object to TMutEval map for each TMutTrk.
  //	 Find associated found tracks.
  //	 If no associated track found, fill one eval object with created hits, etc.
  //	 If associated tracks found:
  //		 Fill TMutTrkEval field in TMutEval object for each associated track.
  //		 Fill TMutEvalRes list in TMutEval object.
  //	 Associate TMutEval with TMutTrk.
  // ]

  Short_t octant=0;

  TMutMCTrkMap::iterator mc_trk_iter = _mctrk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){

    TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
    if (!mc_hit_iter.at_end()){
      TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next();
      octant = mc_hit_ptr->get()->get_octant();
    }
    else octant = -999;

    TMutTrkMap::key_iterator trk_iter = mc_trk_ptr->get()->get_associated<TMutTrk>();


    // If no found track associated with this MC track, fill one eval entry anyway
    if (!trk_iter.count()){
      TMutEvalMap::iterator eval_iter = _eval_map->insert_new(mc_trk_ptr->get()->get_arm(), octant);
      fill_trk_eval(0, mc_trk_ptr, eval_iter.current());
      PHKey::associate(mc_trk_ptr,eval_iter.current());
    }

    // Fill the TMutTrkEval object contained in the	TMutEval object
    while(TMutTrkMap::pointer trk_ptr = trk_iter.next()){
      TMutEvalMap::iterator eval_iter = _eval_map->insert_new(mc_trk_ptr->get()->get_arm(), octant);
      fill_trk_eval(trk_ptr, mc_trk_ptr, eval_iter.current());

      // Fill the TMutEvalRes list contained in the TMutEval object
      fill_eval_res(trk_ptr, mc_trk_ptr, eval_iter.current());

      // Associate the TMutEval object with the TMutTrk
      PHKey::associate(trk_ptr,eval_iter.current());
      PHKey::associate(mc_trk_ptr,eval_iter.current());
    }
  }

}

//_____________________________________________________________
void	mMutEval::associate_mchit()
{
  // TMutMCHits are associated with TMutHit objects after running the mMutResponse
  // module.	It is convienient here to associate TMutMCHit's with TMutCoord since
  // these are associated with TMutTrk objects and are used in the reconstruction.
  // So here we loop over TMutCoord objects -- trace through their association tree
  // until we get to the underlying TMutMCHit and make the association between
  // TMutMCHit and TMutCoord explicit.
  //
  TMutCoordMap::iterator coord_iter = _coord_map->range();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){

    // TMutCoord -> TMutClus
    TMutClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
    while(TMutClusMap::pointer clus_ptr = clus_iter.next()){

      // TMutClus -> TMutHit
      TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
      while(TMutHitMap::pointer hit_ptr = hit_iter.next()){

        // TMutHit->TMutMCHit
        TMutMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMutMCHit>();
        while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()) PHKey::associate_unique(mc_hit_ptr,coord_ptr);

      }
    }
  }
}


//_____________________________________________________________
void mMutEval::associate_mctrk()
{
  typedef pair<int, int> trkid_pair;
  typedef vector<trkid_pair> trkid_list;

  // Get iterator to all TMutTrk's in map
  TMutTrkMap::iterator trk_iter = _trk_map->range();

  // Loop over TMutTrks [
  //	Find associated TMutCoords
  //	Loop over all the associated TMutCoords [
  //	 Find associated TMutMCHits
  //	 Loop over all the associated TMutMCHits [
  //		Find associated TMutMCTrks
  //		Select the most contributed MC track to the track.
  //	 ]
  //	]
  //
  //	Associate this TMutMCTrk with the TMutTrk.
  // ]

  while(TMutTrkMap::pointer trk_ptr = trk_iter.next()){
    TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();

    // Container for the pointers of	all the MC tracks which has contributed at least a MC hit
    // to the reconstructed track, this container is a vector of pairs, in each pair, the first
    // element is an int recording how many MC hits this MC track has contributed to the track
    // and the the second is the pointer to the MC track
    trkid_list trk_counter;
    while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
      TMutMCHitMap::const_key_iterator mc_hit_iter = coord_ptr->get()->get_associated<TMutMCHit>();
      while(TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next()){
        TMutMCTrkMap::key_iterator mc_trk_iter = mc_hit_ptr->get()->get_associated<TMutMCTrk>();
        while(TMutMCTrkMap::pointer mc_trk_ptr=mc_trk_iter.next()){
          bool already_in = false;
          trkid_list::iterator list_iter = trk_counter.begin();

          // Loop over all the pairs, matching the mc track pointer with this mc tracker
          // IF find the matching, pair.first + 1
          // else make a new pair and push it back into the vector
          for(; list_iter!=trk_counter.end(); ++list_iter)
          if(list_iter->second==(mc_trk_ptr->get()->get_track_id())){
            list_iter->first=list_iter->first+1;
            already_in=true;
          }
          if( !already_in )
          trk_counter.push_back(make_pair(1,mc_trk_ptr->get()->get_track_id()));

        }
      }
    }
    // Search for the primary contributor to the track, and
    // associate them together.
    //
    int max_hit =0;
    int max_track_id =0;
    trkid_list::iterator list_iter = trk_counter.begin();
    for(; list_iter!=trk_counter.end(); ++list_iter)
    if(list_iter->first>max_hit) {
      max_track_id = list_iter->second;
      max_hit = list_iter->first;
    }
    TMutMCTrkMap::iterator mc_trk_iter = _mctrk_map->range();
    while(TMutMCTrkMap::pointer mc_trk_ptr =	mc_trk_iter.next())
    if((mc_trk_ptr->get()->get_track_id())==max_track_id) PHKey::associate(mc_trk_ptr, trk_ptr);

  }
}

//_____________________________________________________________
void mMutEval::fill_trk_eval(TMutTrkMap::pointer trk_ptr,
  TMutMCTrkMap::pointer mctrk_ptr,
  TMutEvalMap::pointer eval_ptr)
{
  unsigned short plane;

  // Buffer for TMutTrkEval field in TMutEval class.
  TMutTrkEval trk_eval;

  // Total hits of MC track and what planes are hit.
  unsigned short nhits_mc = 2*(mctrk_ptr->get()->get_associated<TMutMCHit>()).count();

  // Retrieve hits from each plane and bit pack the hit planes into nhits_mc
  nhits_mc = 0;
  TMutMCHitMap::key_iterator mc_hit_iter = mctrk_ptr->get()->get_associated<TMutMCHit>();
  while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
  {
    plane = 6*mc_hit_ptr->get()->get_station() + 2*mc_hit_ptr->get()->get_gap();
    if (mc_hit_ptr->get()->get_associated<TMutCoord>().count())
    {
      TMutCoordMap::key_iterator coord_iter = mc_hit_ptr->get()->get_associated<TMutCoord>();
      while(TMutCoordMap::pointer coord_ptr = coord_iter.next())
      {
        if (!(nhits_mc & (1<<(plane + coord_ptr->get()->get_cathode()) )) )
        { nhits_mc += 1<<(plane + coord_ptr->get()->get_cathode()); }
      }
    }
  }

  trk_eval.set_n_total_true_hits(nhits_mc);

  // Total masked hit for MC track.
  unsigned short nhits_mask = get_n_maskhit(mctrk_ptr);
  trk_eval.set_n_masked_true_hits(nhits_mask);

  // Number of hits in a TMutTrk which are from primary contributor.
  if (trk_ptr) {

    unsigned short n_true_hits = get_true_hits(trk_ptr, mctrk_ptr);
    trk_eval.set_n_reco_true_hits(n_true_hits);

    // Number of hits in a TMutTrk which are not from primary contributor.
    unsigned short n_ghost = (trk_ptr->get()->get_n_coord()) - trk_eval.get_n_reco_true_hits();
    trk_eval.set_n_reco_ghost_hits(n_ghost);
    trk_eval.set_charge_reco(trk_ptr->get()->get_charge());

    // Track momentum at primary vertex.
    const TMutTrkPar* trk_par = trk_ptr->get()->get_trk_par_vtx();

    // Make sure track was fit at some point:
    if( trk_ptr->get()->get_reco_success() )
    {

      trk_eval.set_px_reco_vx(trk_par->get_px());
      trk_eval.set_py_reco_vx(trk_par->get_py());
      trk_eval.set_pz_reco_vx(trk_par->get_pz());
      trk_eval.set_ptot_reco_vx(trk_par->get_ptot());

      // Track momentum at reference surface in MUTR
      trk_par = &(trk_ptr->get()->get_trk_par_list()->front());

      // store upstream fitted point in evaluator
      trk_eval.set_px_reco_us(trk_par->get_px());
      trk_eval.set_py_reco_us(trk_par->get_py());
      trk_eval.set_pz_reco_us(trk_par->get_pz());
      trk_eval.set_ptot_reco_us(trk_par->get_ptot());

      PHTrackIntegratorKF integrator;

      // extrapolate to downstream point.
      trk_par = &(trk_ptr->get()->get_trk_par_list()->back());
      trk_eval.set_px_reco_ds(trk_par->get_px());
      trk_eval.set_py_reco_ds(trk_par->get_py());
      trk_eval.set_pz_reco_ds(trk_par->get_pz());
      trk_eval.set_ptot_reco_ds(trk_par->get_ptot());

/*      integrator.initialize( *trk_par );
      integrator.extrapolate( mctrk_ptr->get()->get_z_ds_gap() );

      if( !integrator.get_error() )
      {

        TMutTrkPar trk_par_out;
        integrator.finish( trk_par_out );

        trk_eval.set_px_reco_ds(trk_par_out.get_px());
        trk_eval.set_py_reco_ds(trk_par_out.get_py());
        trk_eval.set_pz_reco_ds(trk_par_out.get_pz());
        trk_eval.set_ptot_reco_ds(trk_par_out.get_ptot());

      } else cout << "mMutEval::fill_trk_eval - extrapolation to z=" << mctrk_ptr->get()->get_z_ds_gap() << " failed" << endl; */
    }
  }

  // Charge reconstructed and true
  trk_eval.set_charge_true(mctrk_ptr->get()->get_charge());

  // MC Track momentum at primary vertex
  trk_eval.set_px_true_vx(mctrk_ptr->get()->get_px_orig());
  trk_eval.set_py_true_vx(mctrk_ptr->get()->get_py_orig());
  trk_eval.set_pz_true_vx(mctrk_ptr->get()->get_pz_orig());
  trk_eval.set_ptot_true_vx(mctrk_ptr->get()->get_ptot_orig());

  // MC Track momentum at upstream gap.
  trk_eval.set_px_true_us(mctrk_ptr->get()->get_px_us_gap());
  trk_eval.set_py_true_us(mctrk_ptr->get()->get_py_us_gap());
  trk_eval.set_pz_true_us(mctrk_ptr->get()->get_pz_us_gap());
  trk_eval.set_ptot_true_us(mctrk_ptr->get()->get_ptot_us_gap());

  // MC Track momentum at downstream gap.
  trk_eval.set_px_true_ds(mctrk_ptr->get()->get_px_ds_gap());
  trk_eval.set_py_true_ds(mctrk_ptr->get()->get_py_ds_gap());
  trk_eval.set_pz_true_ds(mctrk_ptr->get()->get_pz_ds_gap());
  trk_eval.set_ptot_true_ds(mctrk_ptr->get()->get_ptot_ds_gap());

  // Fill TMutTrkEval field of TMutEval object.
  eval_ptr->get()->set_trk_eval(trk_eval);

}

//_____________________________________________________________
unsigned short mMutEval::get_n_maskhit(TMutMCTrkMap::pointer mctrk_ptr)
{
  unsigned short n_mask = 0;
  // Loop for all the associated TMutMCHit [
  //	 If this mc hit is inside the mask area.
  //	 n_mask = nmask + 1;
  // ]
  // TBI.
  return n_mask;
}

//_____________________________________________________________
unsigned short mMutEval::get_true_hits(TMutTrkMap::pointer trk_ptr, TMutMCTrkMap::pointer mctrk_ptr)
{
  unsigned short n_true_hits = 0;
  unsigned short plane;

  // Loop for all the associated TMutCoord [
  //	 Loop for all the associated TMutMCHit [
  //		 If(this associated MC track has the same trkid as primary contributor) [
  //			 n_true_hits+1;
  //		 ]
  //	 ]
  // ]
  // Mod:	Pack planes hit into n_true_hits rather than just recording total number of hits

  TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr=coord_iter.next()){
    TMutMCHitMap::const_key_iterator mchit_iter = coord_ptr->get()->get_associated<TMutMCHit>();
    while(TMutMCHitMap::const_pointer mchit_ptr=mchit_iter.next())
    if((mchit_ptr->get()->get_track_id())==(mctrk_ptr->get()->get_track_id())){
      plane = 6*coord_ptr->get()->get_station() + 2*coord_ptr->get()->get_gap() + coord_ptr->get()->get_cathode();
      if (! (n_true_hits &	1<<plane) ) n_true_hits += 1<<plane;
    }
  }
  return n_true_hits;
}

//_____________________________________________________________
void mMutEval::fill_eval_res(TMutTrkMap::pointer trk_ptr,
           TMutMCTrkMap::pointer mctrk_ptr,
           TMutEvalMap::pointer eval_ptr)
{
  // TBI-> In Working Progress.
  // Loop over all the TMutCoord which are associated with TMutTrk [
  //	 Pickup the TMutMChit which is associated with TMutCoord.
  //	 Define a buffer for TMutEvalRes object.
  //	 Fill the fields of TMutEvalRes object.
  // ]
  //

  TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr=coord_iter.next()){
    TMutMCHitMap::const_key_iterator mchit_iter = coord_ptr->get()->get_associated<TMutMCHit>();
    if(mchit_iter.count()<1) continue;

    while(TMutMCHitMap::const_pointer mchit_ptr = mchit_iter.next()){
      TMutEvalRes eval_res;

      /*! Set the locator of TMutEvalRes object. */
      eval_res.set_arm(coord_ptr->get()->get_arm());
      eval_res.set_station(coord_ptr->get()->get_station());
      eval_res.set_octant(coord_ptr->get()->get_octant());
      eval_res.set_half_octant(coord_ptr->get()->get_half_octant());
      eval_res.set_gap(coord_ptr->get()->get_gap());
      eval_res.set_cathode(coord_ptr->get()->get_cathode());

      /*! Fill phase space coordinates for MCHit */
      eval_res.set_x_true(mchit_ptr->get()->get_x());
      eval_res.set_y_true(mchit_ptr->get()->get_y());
      eval_res.set_z_true(mchit_ptr->get()->get_z());
      eval_res.set_px_true(mchit_ptr->get()->get_px());
      eval_res.set_py_true(mchit_ptr->get()->get_py());
      eval_res.set_pz_true(mchit_ptr->get()->get_pz());
      Float_t ptot = sqrt((eval_res.get_px_true())*(eval_res.get_px_true())+
        (eval_res.get_py_true())*(eval_res.get_py_true())+
        (eval_res.get_pz_true())*(eval_res.get_pz_true()));
      Float_t pt	 = sqrt((eval_res.get_px_true())*(eval_res.get_px_true())+
        (eval_res.get_py_true())*(eval_res.get_py_true()));
      if(pt==0){
        eval_res.set_theta_true(0.0);
        eval_res.set_phi_true(-999.0);
      } else{
        eval_res.set_theta_true(acos((eval_res.get_pz_true())/ptot));
        // We set phi angle range from -PI to PI. If py < 0, we give phi a
        // value less than 0.
        if(eval_res.get_py_true()>0) eval_res.set_phi_true(acos((eval_res.get_px_true())/pt));
        else eval_res.set_phi_true(-0.1*acos((eval_res.get_px_true())/pt));
      }

      /*! Fill phase space coordinate for reco hit ( central strip). */
      const TMutTrk::residual_list* trk_res = trk_ptr->get()->get_w_residual_list();
      TMutTrk::const_residual_iterator trk_res_iter = trk_res->begin();
      for(;trk_res_iter!=trk_res->end();++trk_res_iter){
        // We have to identify location of the residual to make sure that
        // we are doing the evaluation at the right place.
        if(trk_res_iter->get_station()!=eval_res.get_station()) continue;
        if(trk_res_iter->get_gap()!=eval_res.get_gap()) continue;
        if(trk_res_iter->get_cathode()!=eval_res.get_cathode()) continue;
        const TMutFitPar* trk_fitpar = trk_res_iter->get_fit_par();
        eval_res.set_x_reco(trk_fitpar->get_x());
        eval_res.set_y_reco(trk_fitpar->get_y());
        eval_res.set_z_reco(trk_fitpar->get_z());
        eval_res.set_theta_reco(trk_fitpar->get_dxdz());
        eval_res.set_phi_reco(trk_fitpar->get_dydz());

        /*! Fill residual information in TMutEvalRes. */
        eval_res.set_w_true(mchit_ptr->get()->get_w_true(eval_res.get_cathode()));
        eval_res.set_w_digit(mchit_ptr->get()->get_w_digit(eval_res.get_cathode()));
        eval_res.set_w_track(trk_res_iter->get_w_trk());
        eval_res.set_w_meas(trk_res_iter->get_w_meas());

        /*! Fill cathode geometry. */
        eval_res.set_theta_ac(trk_res_iter->get_cos_theta_ac());
        eval_res.set_theta_wz(trk_res_iter->get_cos_theta_wz());
      }
      eval_ptr->get()->push_eval_res_list(eval_res);
    }
  }
}
