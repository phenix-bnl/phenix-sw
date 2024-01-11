// $Id: mMuiEvalO.cxx,v 1.3 2009/09/24 10:20:50 hpereira Exp $

/*!
\file mMuiEvalO.cxx
\brief associate MC_Hit and MC_Trk information with roads
\author s. Kelly, chun zhang
\version $Revision: 1.3 $
\date $Date: 2009/09/24 10:20:50 $
*/

// MUIOO headers
//
#include<mMuiEvalO.h>
#include<mMuiEvalOPar.h>
#include<TMuiRoadEval.hh>
#include<TMutNode.h>
#include<PHException.h>
#include<MUIOO.h>
// STL/BOOST
//
#include<string>
#include<iostream>

//_________________________________________________
mMuiEvalO::mMuiEvalO():
_timer( PHTimeServer::get()->insert_new( "mMuiEvalO" ) )
{
  MUIOO::TRACE("initializing module mMuiEvalO");
}

//_________________________________________________
PHBoolean
  mMuiEvalO::event(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{
  _timer.get()->restart();

  try {
    // Reset IOC pointers
    set_interface_ptrs(signal_node, top_node);

    // clear maps
    _eval_map->clear();

    // Unless we ran perfect pattern recogniton promote
    // the TMuiMCHit associations to TMuiCluster and
    // determine the principal contributor to the
    // TMutMCTrk
    if(_mod_par->get_pr_mode() == mMuiEvalOPar::NORMAL) {
      // associate TMuiMCHits with TMuiCluster
      //
      associate_mchit();
      if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
        MUIOO::PRINT(std::cout, " After associate_mc_hit");
        _mctrk_map->print();
      }

      // associate TMutMCTrk with TMuiRoad
      //
      associate_mctrk();
      if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
        MUIOO::PRINT(std::cout, " After associate_mc_trk");
        _mctrk_map->print();
      }
    }

    // Do the evaluation
    //
    evaluate();
    if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
      MUIOO::PRINT(std::cout, " After evaluate");
      _mctrk_map->print();
    }

  }
  catch(std::exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();
  // If verbose dump the contents of the eval map
  //
  if(_mod_par->get_verbosity() >= MUIOO::ALOT) _eval_map->print();

  return True;
}

//_________________________________________________
void
  mMuiEvalO::set_interface_ptrs(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMuiEvalOPar>::find_node(top_node,"mMuiEvalOPar");

  // TMutMCTrk IOC
  _mctrk_map = TMutNode<TMutMCTrkMap>::find_node(signal_node,"TMutMCTrkMap");

  // TMuiHit IOC
  _hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");

  // TMuiCluster IOC
  _cluster_map = TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");

  // TMuiRoad IOC
  _road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");

  // TMuiEval IOC
  _eval_map = TMutNode<TMuiEvalMap>::find_node(top_node,"TMuiEvalMap");
}

//_________________________________________________
void
  mMuiEvalO::evaluate()
{
  // Loop over all the MC tracks [
  //	 Insert a new eval object to eval map for each road.
  //	 Find associated found roads.
  //	 If no associated road found, fill one eval object with created hits,etc.
  //	 If associated roads found:
  //		 Fill TMuiRoadEval field in TMuiEval object for each associated track.
  //		 Fill TMuiEvalRes list in TMuiEval object.
  //	 Associate TMuiEval with TMuiRoad
  // ]

  TMutMCTrkMap::iterator mc_trk_iter = _mctrk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){

    TMuiMCHitMapO::key_iterator mc_hit_iter =
      mc_trk_ptr->get()->get_associated<TMuiMCHitO>();

    TMuiRoadMapO::key_iterator road_iter =
      mc_trk_ptr->get()->get_associated<TMuiRoadO>();

    // If no found track associated with this MC track,
    // fill one eval entry (anyway) with not so much eval info..
    //
    if (!road_iter.count()){
      TMuiEvalMap::iterator eval_iter =
        _eval_map->insert_new(mc_trk_ptr->get()->get_arm());
      fill_road_eval(0, mc_trk_ptr, eval_iter.current());
      PHKey::associate(mc_trk_ptr, eval_iter.current());
    }

    // Fill the TMuiRoadEval object contained in the TMuiEval object
    //
    while(TMuiRoadMapO::pointer road_ptr = road_iter.next()){
      TMuiEvalMap::iterator eval_iter =
        _eval_map->insert_new(mc_trk_ptr->get()->get_arm());
      fill_road_eval(road_ptr, mc_trk_ptr, eval_iter.current());

      // Fill the TMuiEvalRes list contained in the TMuiEval object
      //
      fill_eval_res(road_ptr, mc_trk_ptr, eval_iter.current());

      // Associate the TMuiEval object with the TMuiRoad
      //
      PHKey::associate(road_ptr,eval_iter.current());
      PHKey::associate(mc_trk_ptr,eval_iter.current());
    }
  }
  return;
}

//_________________________________________________
void
  mMuiEvalO::associate_mchit()
{
  // TMuiMCHits are associated with TMuiHit objects after running the
  // mMuiResponse module.	It is convienient here to associate TMuiMCHit's
  // with TMuiClusters since these are associated with TMuiRoad objects and
  // are used in the reconstruction.
  // So here we loop over TMuiCluster objects -- trace through their
  // association tree until we get to the underlying TMuiMCHit and make
  // the association between TMuiMCHit and TMuiCluster explicit.
  //

  TMuiClusterMapO::iterator cluster_iter = _cluster_map->range();
  while(TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next()) {
    //
    TMuiHitMapO::key_iterator hit_iter = cluster_ptr->get()->get_associated<TMuiHitO>();
    if(!hit_iter.at_end()){
      TMuiHitMapO::pointer hit_ptr = hit_iter.current();
      //
      TMuiMCHitMapO::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMuiMCHitO>();
      while(TMuiMCHitMapO::pointer mc_hit_ptr = mc_hit_iter.next()){
        // Promote the association (TMuiMCHit -- TMuiCluster)
        //
        PHKey::associate(mc_hit_ptr,cluster_ptr);
      }
    }
  }
  return;
}

//_________________________________________________
void
  mMuiEvalO::associate_mctrk()
{
  typedef std::pair<int, int> roadid_pair;
  typedef std::vector<roadid_pair> roadid_list;

  // Get iterator to all TMuiRoad's in map
  //
  TMuiRoadMapO::iterator road_iter = _road_map->range();

  // Loop over TMuiRoads [
  //	Find associated TMuiClusters
  //	Loop over all the associated TMuiClusters [
  //	 Find associated TMuiMCHits
  //	 Loop over all the associated TMuiMCHits [
  //		Find associated TMutMCTrks
  //		Select the most contributed MC track to the track.
  //	 ]
  //	]
  //
  //	Associate this TMutMCTrk with the TMuiRoad.
  // ]

  while(TMuiRoadMapO::pointer road_ptr = road_iter.next()){
    TMuiClusterMapO::const_key_iterator cluster_iter =
      road_ptr->get()->get_associated<TMuiClusterO>();

    // Container for the pointers of	all the MC tracks which has
    // contributed at least a MC hit to the reconstructed track,
    // this container is a vector of pairs.
    // In each pair, the first
    // element is an int recording how many MC hits this MC track has contributed to the track
    // and the the second is the pointer to the MC track
    //
    roadid_list road_counter;
    while(TMuiClusterMapO::const_pointer cluster_ptr = cluster_iter.next()) {

      TMuiMCHitMapO::const_key_iterator mc_hit_iter =
        cluster_ptr->get()->get_associated<TMuiMCHitO>();

      while(TMuiMCHitMapO::const_pointer mc_hit_ptr = mc_hit_iter.next()){

        TMutMCTrkMap::key_iterator mc_trk_iter =
          mc_hit_ptr->get()->get_associated<TMutMCTrk>();

        while(TMutMCTrkMap::pointer mc_trk_ptr=mc_trk_iter.next()){
          bool already_in = false;
          roadid_list::iterator list_iter = road_counter.begin();

          // Loop over all the pairs, matching the mc track pointer with
          // this mc tracker
          // IF find the matching, pair.first + 1
          // else make a new pair and push it back into the vector
          //
          for(; list_iter!=road_counter.end(); ++list_iter) {

            if(list_iter->second==(mc_trk_ptr->get()->get_track_id())){
              list_iter->first=list_iter->first+1;
              already_in=true;
            }
          }
          if(already_in==false) {
            road_counter.push_back(std::make_pair(1,
              mc_trk_ptr->get()->get_track_id()));
          }
        }
      }
    }
    // Search for the primary contributor to the track, and
    // associate them together.
    //
    int max_hit =0;
    int max_track_id =0;
    roadid_list::iterator list_iter = road_counter.begin();
    for(; list_iter!=road_counter.end(); ++list_iter) {

      if(list_iter->first>max_hit) {
        max_track_id = list_iter->second;
        max_hit = list_iter->first;
      }
    }
    TMutMCTrkMap::iterator mc_trk_iter = _mctrk_map->range();
    while(TMutMCTrkMap::pointer mc_trk_ptr =	mc_trk_iter.next()){

      if((mc_trk_ptr->get()->get_track_id())==max_track_id) {
        PHKey::associate(mc_trk_ptr, road_ptr);
      }
    }
  }
  return;
}

//_________________________________________________
void
  mMuiEvalO::fill_road_eval(TMuiRoadMapO::pointer road_ptr,
  TMutMCTrkMap::pointer mctrk_ptr,
  TMuiEvalMap::pointer eval_ptr)
{
  // Buffer for TMuiRoadEval field in TMuiEval class.
  //
  TMuiRoadEval road_eval;

  // Total hits of MC track and what planes are hit.
  //
  UShort_t nhits_mc=0;

  // Loop over TMuiMCHitO associated with TMutMCTrk
  //
  TMuiMCHitMapO::key_iterator mc_hit_iter = mctrk_ptr->get()->get_associated<TMuiMCHitO>();
  while(TMuiMCHitMapO::pointer mc_hit_ptr = mc_hit_iter.next()){

    UShort_t plane = mc_hit_ptr->get()->get_plane();
    // Loop over assoicated TMuiClusterO
    //
    TMuiClusterMapO::key_iterator cluster_iter = mc_hit_ptr->get()->get_associated<TMuiClusterO>();
    while(TMuiClusterMapO::pointer cluster_ptr = cluster_iter.next()){
      // Bit pack cluster pattern associated with this road
      //
      nhits_mc |= 1 << (plane*MUIOO::MAX_ORIENTATION + cluster_ptr->get()->get_orientation());
    }
  }

  // Set hit true hit mask
  //
  road_eval.set_n_true_hits(nhits_mc);

  // Total masked hit for MC track.
  //
  UShort_t nhits_mask = get_n_maskhit(mctrk_ptr);
  road_eval.set_n_masked_hits(nhits_mask);

  // Number of hits in a TMuiRoadO which are from primary contributor.
  //
  if (road_ptr) {
    // get_true_hits returns the true hit pattern and the ghost hit pattern
    // as a bit mask
    //
    std::pair<UShort_t, UShort_t> masks = get_true_hits(road_ptr,mctrk_ptr);
    road_eval.set_n_reco_true_hits(masks.first);
    road_eval.set_n_reco_ghost_hits(masks.second);
  }

  // MC Track momentum at primary vertex
  //
  road_eval.set_px_true_vx(mctrk_ptr->get()->get_px_orig());
  road_eval.set_py_true_vx(mctrk_ptr->get()->get_py_orig());
  road_eval.set_pz_true_vx(mctrk_ptr->get()->get_pz_orig());
  road_eval.set_ptot_true_vx(mctrk_ptr->get()->get_ptot_orig());

  // Fill TMuiRoadEval field of TMuiEval object.
  //
  eval_ptr->get()->set_road_eval(road_eval);

  return;
}

//_________________________________________________
std::pair<UShort_t,UShort_t>
  mMuiEvalO::get_true_hits(TMuiRoadMapO::pointer road_ptr,
  TMutMCTrkMap::pointer mctrk_ptr)
{
  // Loop for all the associated TMuiClusterO [
  //	 Loop for all the associated TMuiMCHitO [
  //		 If(this associated MC track has the same roadid as primary contributor) [
  //			 n_true_hits+1;
  //		 ]
  //	 ]
  // ]
  // Mod:	Pack planes hit into n_true_hits rather than just recording total number of hits

  // Note in the below we bit pack the actual bit pattern into the word as opposed to
  // just keeping track of the totals.
  //
  UShort_t n_true_hits=0, n_ghost_hits=0;
  TMuiClusterMapO::const_key_iterator clus_iter = road_ptr->get()->get_associated<TMuiClusterO>();
  while(TMuiClusterMapO::const_pointer clus_ptr=clus_iter.next()){

    UShort_t mask = clus_ptr->get()->get_plane()*MUIOO::MAX_ORIENTATION+ clus_ptr->get()->get_orientation();

    // Iterator to all associated clusters
    //
    TMuiMCHitMapO::const_key_iterator mchit_iter = clus_ptr->get()->get_associated<TMuiMCHitO>();

    // If no associated TMuiMCHit then increment the ghost counter
    //
    if(mchit_iter.count() == 0) n_ghost_hits |= 1 << mask;
    while(TMuiMCHitMapO::const_pointer mchit_ptr=mchit_iter.next()){
      if( mchit_ptr->get()->get_track_id() == mctrk_ptr->get()->get_track_id() ) {
        n_true_hits |= 1<<mask;
      } else {
        n_ghost_hits |= 1<<mask;
      }
    }
  }
  return std::make_pair(n_true_hits,n_ghost_hits);
}

//_________________________________________________
UShort_t
  mMuiEvalO::get_n_maskhit(TMutMCTrkMap::pointer mctrk_ptr)
{
  // Loop over associated TMuiMCHitO
  //	Loop over twopack list
  //	 if two pack status is MASKED
  //		 set the bit for current plane in mask
  //	 ]
  //	]
  // ]
  UShort_t mask=0;
  typedef TMuiMCHitO::twopack_list twopack_list;
  typedef TMuiMCHitO::twopack_list::iterator twopack_iterator;

  TMuiMCHitMapO::const_key_iterator mchit_iter =
    mctrk_ptr->get()->get_associated<TMuiMCHitO>();
  while(TMuiMCHitMapO::const_pointer mchit_ptr = mchit_iter.next()){

    // Get an iterator for list of TMuiMCTwoPack from TMuiMCHit
    //
    twopack_list* twopacks = mchit_ptr->get()->get_mutable_twopack_list();
    twopack_iterator twopack_iter = twopacks->begin();
    for(;twopack_iter!=twopacks->end();++twopack_iter){
      if(twopack_iter->get_status() == TMuiMCTwoPack::MASKED) {
        UShort_t index = mchit_ptr->get()->get_plane()*MUIOO::MAX_ORIENTATION +
          twopack_iter->get_orient();
        mask |= 1 << index;
      }
    }
  }
  return mask;
}

//_________________________________________________
void
  mMuiEvalO::fill_eval_res(TMuiRoadMapO::pointer road_ptr,
  TMutMCTrkMap::pointer mctrk_ptr,
  TMuiEvalMap::pointer eval_ptr)
{
  // TBI-> In Working Progress.
  // Loop over all the TMuiClusterO which are associated with TMutTrk [
  //	 Pickup the TMutMChit which is associated with TMuiClusterO.
  //	 Define a buffer for TMuiEvalRes object.
  //	 Fill the fields of TMuiEvalRes object.
  // ]
  //

  TMuiClusterMapO::const_key_iterator cluster_iter =
    road_ptr->get()->get_associated<TMuiClusterO>();

  while(TMuiClusterMapO::const_pointer cluster_ptr=cluster_iter.next()){

    TMuiMCHitMapO::const_key_iterator mchit_iter = cluster_ptr->get()->get_associated<TMuiMCHitO>();

    if(mchit_iter.count()<1) {
      continue;
    }

    while(TMuiMCHitMapO::const_pointer mchit_ptr = mchit_iter.next()){
      TMuiEvalRes eval_res;

      /*! Set the locator of TMuiEvalRes object. */
      eval_res.set_arm(cluster_ptr->get()->get_arm());
      eval_res.set_plane(cluster_ptr->get()->get_plane());
      eval_res.set_panel(cluster_ptr->get()->get_panel());
      eval_res.set_orientation(cluster_ptr->get()->get_orientation());

      /*! Fill phase space clusterinates for MCHit */
      eval_res.set_x_true(mchit_ptr->get()->get_x());
      eval_res.set_y_true(mchit_ptr->get()->get_y());
      eval_res.set_z_true(mchit_ptr->get()->get_z());
      eval_res.set_px_true(mchit_ptr->get()->get_px());
      eval_res.set_py_true(mchit_ptr->get()->get_py());
      eval_res.set_pz_true(mchit_ptr->get()->get_pz());
      Float_t pt	 = sqrt((eval_res.get_px_true())*(eval_res.get_px_true())+
        (eval_res.get_py_true())*(eval_res.get_py_true()));
      Float_t ptot = sqrt((eval_res.get_px_true())*(eval_res.get_px_true())+
        (eval_res.get_py_true())*(eval_res.get_py_true())+
        (eval_res.get_pz_true())*(eval_res.get_pz_true()));

      if(pt==0){
        eval_res.set_theta_true(0.0);
        eval_res.set_phi_true(-999.0);
      }
      else{
        eval_res.set_theta_true(acos((eval_res.get_pz_true())/ptot));
        // We set phi angle range from -PI to PI. If py < 0, we give phi a
        // value less than 0.
        if(eval_res.get_py_true()>0){
          eval_res.set_phi_true(acos((eval_res.get_px_true())/pt));
        }
        else{
          eval_res.set_phi_true(-1.0*acos((eval_res.get_px_true())/pt));
        }
      }
      eval_ptr->get()->push_eval_res_list(eval_res);
    }
  }
}
