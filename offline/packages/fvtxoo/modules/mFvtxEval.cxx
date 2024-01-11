// $Id: mFvtxEval.cxx,v 1.6 2011/12/01 04:16:21 slash Exp $

/*!
	 \file mFvtxEval.cxx
	 \brief associate MC_Hit and MC_Trk information with tracks
	 \author M. Brooks
	 \version $Revision: 1.6 $
	 \date $Date: 2011/12/01 04:16:21 $
*/
///////////////////////////////////////////////////////////////////

// MUTOO headers
#include<mFvtxEval.h>
#include<mFvtxEvalPar.h>
#include<TMutNode.h>
#include<TFvtxMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TFvtxHitMap.h>
#include<TFvtxClusMap.h>
#include<TFvtxCoordMap.h>
#include<TFvtxTrkEval.h>
#include<TFvtxTrkMap.h>
#include<FVTXOO.h>

// STL/BOOST
#include<string>
#include<iostream>

using namespace std;

//_____________________________________________________________
mFvtxEval::mFvtxEval():
	_timer(PHTimeServer::get()->insert_new("mFvtxEval") )
{
	MUTOO::TRACE("initializing module mFvtxEval");
}

//_____________________________________________________________
PHBoolean mFvtxEval::event(PHCompositeNode* signal_node, PHCompositeNode* top_node)
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
		if(_mod_par->get_pr_mode() == mFvtxEvalPar::NORMAL) {

			// associate TMutMCHits with TMutCoord
			associate_mchit();

			// associate TMutMCTrk with TMutTrk
			associate_mctrk();
		}

		// Do the evaluation
		//
		evaluate();

	} catch(std::exception& e) {
		FVTXOO::TRACE(e.what());
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
void mFvtxEval::set_interface_ptrs(PHCompositeNode* signal_node, PHCompositeNode* top_node)
{
	// module runtime parameters
	_mod_par = TMutNode<mFvtxEvalPar>::find_node(top_node,"mFvtxEvalPar");
	_mctrk_map = TMutNode<TMutMCTrkMap>::find_node(signal_node,"TMutMCTrkMap");

	_coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
	_trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
	_eval_map = TMutNode<TFvtxEvalMap>::find_node(top_node,"TFvtxEvalMap");
}

//_____________________________________________________________
void mFvtxEval::evaluate()
{

}

//_____________________________________________________________
void	mFvtxEval::associate_mchit()
{
	// TMutMCHits are associated with TMutHit objects after running the mMutResponse
	// module.	It is convienient here to associate TMutMCHit's with TMutCoord since
	// these are associated with TMutTrk objects and are used in the reconstruction.
	// So here we loop over TMutCoord objects -- trace through their association tree
	// until we get to the underlying TMutMCHit and make the association between
	// TMutMCHit and TMutCoord explicit.
	//
	TFvtxCoordMap::iterator coord_iter = _coord_map->range();
	while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){

		// TFvtxCoord -> TFvtxClus
		TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
		while(TFvtxClusMap::pointer clus_ptr = clus_iter.next()){

			// TFvtxClus -> TFvtxHit
			TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
			while(TFvtxHitMap::pointer hit_ptr = hit_iter.next()){

				// TFvtxHit->TFvtxMCHit
				TFvtxMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TFvtxMCHit>();
				while(TFvtxMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()) PHKey::associate_unique(mc_hit_ptr,coord_ptr);

			}
		}
	}
}


struct MatchesValue : public std::unary_function<bool,int>
{
  int _v;
  MatchesValue(int v) : _v(v) {}
  bool operator()(const std::pair<int, int>& v) {
    if ( _v == v.second ) return true;
    return false;
  }
};

// Standard model of StrictWeakOrdering: for unstable sorting by
// ASCENDING order of reference count.
struct CompareCounts : public std::binary_function<bool,std::pair<int, int>,std::pair<int, int> >
{
  bool operator()(const std::pair<int, int>& a, const std::pair<int, int>& b)
  {
    if ( a.first < b.first ) return true;
    return false;
  }
};

// This version has the opposite sense to CompareCounts, intended to
// sort the list by DESCENDING order, not the default model of
// ascending order.
struct CompareCountsN : public std::binary_function<bool,std::pair<int, int>,std::pair<int, int> >
{
  bool operator()(const std::pair<int, int>& a, const std::pair<int, int>& b)
  {
    if ( a.first > b.first ) return true;
    return false;
  }
};

//_____________________________________________________________
void mFvtxEval::associate_mctrk()
{
	typedef std::pair<int, int> trkid_pair;
	typedef std::vector<trkid_pair> trkid_list;

	// Get iterator to all TFvtxTrk's in map
	TFvtxTrkMap::iterator trk_iter = _trk_map->range();

	// Loop over TFvtxTrks [
	//	Find associated TFvtxCoords
	//	Loop over all the associated TFvtxCoords [
	//	 Find associated TFvtxMCHits
	//	 Loop over all the associated TFvtxMCHits [
	//		Find associated TMutMCTrks
	//		Select the most contributed MC track to the track.
	//	 ]
	//	]
	//
	//	Associate this TMutMCTrk with the TFvtxTrk.
	// ]

	while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next()){
		TFvtxCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();

		// Container for the pointers of	all the MC tracks which has contributed at least a MC hit
		// to the reconstructed track, this container is a vector of pairs, in each pair, the first
		// element is an int recording how many MC hits this MC track has contributed to the track
		// and the the second is the pointer to the MC track
		trkid_list trk_counter;
		while(TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next()){
			TFvtxMCHitMap::const_key_iterator mc_hit_iter = coord_ptr->get()->get_associated<TFvtxMCHit>();
			while(TFvtxMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next()){
				TMutMCTrkMap::key_iterator mc_trk_iter = mc_hit_ptr->get()->get_associated<TMutMCTrk>();
				while(TMutMCTrkMap::pointer mc_trk_ptr=mc_trk_iter.next()){
// 				        bool already_in = false;
// 				        trkid_list::iterator list_iter = trk_counter.begin();

// 					// Loop over all the pairs, matching the mc track pointer with this mc tracker
// 					// IF find the matching, pair.first + 1
// 					// else make a new pair and push it back into the vector
// 					for(; list_iter!=trk_counter.end(); ++list_iter)
// 					if(list_iter->second==(mc_trk_ptr->get()->get_track_id())){
// 						list_iter->first=list_iter->first+1;
// 						already_in=true;
// 					}
// 					if( !already_in )
// 					trk_counter.push_back(std::make_pair(1,mc_trk_ptr->get()->get_track_id()));

				        // Alternate to above block.
				        // Search for the entry in the
				        // list for this mctrk id
				        int id = mc_trk_ptr->get()->get_track_id();
					trkid_list::iterator list_iter = std::find_if(trk_counter.begin(),trk_counter.end(),
										      MatchesValue(id));

					// If found, increment the contribution counter
					// if not found, add it to the list, with an initial value of 1 contribution
					if ( list_iter != trk_counter.end() )
					  (*list_iter).first++;
					else
					  trk_counter.push_back(std::make_pair(1,id));
				}
			}
		}
		// Search for the primary contributor to the track, and
		// associate them together.
		//
#if 0
// 		int max_hit =0;
// 		int max_track_id =0;
// 		trkid_list::iterator list_iter = trk_counter.begin();
// 		for(; list_iter!=trk_counter.end(); ++list_iter)
// 		  {
// 		    std::cout << "TFvtxTrk " << trk_ptr->get()->get_index() << ": "
// 			      << "MC id = " << list_iter->second << ", #hits = " << list_iter->first
// 			      << std::endl;
// 		    if(list_iter->first>max_hit) {
// 		      max_track_id = list_iter->second;
// 		      max_hit = list_iter->first;
// 		    }
// 		  }
// 		TMutMCTrkMap::iterator mc_trk_iter = _mctrk_map->range();
// 		while(TMutMCTrkMap::pointer mc_trk_ptr =	mc_trk_iter.next())
// 		  if((mc_trk_ptr->get()->get_track_id())==max_track_id) {
// 		    std::cout << "Associating mctrk " << mc_trk_ptr->get()->get_track_id()
// 			      << " with trk " << trk_ptr->get()->get_index() << std::endl;
// 		    PHKey::associate(mc_trk_ptr, trk_ptr);
// 		  }
#endif
		// An alternative to the above code.  First, sort the trackid hit-contribution
		// counter vector in descending order.  Take the mc track with the most hits
		// contributed to this reco track and associate it with the track.  BUT, if
		// there is more than one mc track that contributes the same number of max hits,
		// make an association for each of those tracks.
		if ( trk_counter.size() )
		  {
		    std::sort(trk_counter.begin(),trk_counter.end(),CompareCountsN());
		    int max_counts = trk_counter.front().first;
		    for ( trkid_list::iterator it=trk_counter.begin(); it!= trk_counter.end(); ++it )
		      {
			if ( (*it).first >= max_counts )
			  {
			    int id = (*it).second;
			    TMutMCTrkMap::iterator mc_trk_iter = _mctrk_map->range();
			    while( TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next() )
			      {
				if ( (mc_trk_ptr->get()->get_track_id()) == id )
				  {
				    PHKey::associate(mc_trk_ptr, trk_ptr);
				    break; // There's only one track with this id, so we can stop here, right???????
				  }
			      }
			  }
			else break; // since vector is sorted in descending order, no need to loop further
		      }
		  }
	}
}

//_____________________________________________________________
void mFvtxEval::fill_trk_eval(TFvtxTrkMap::pointer trk_ptr,
					 TMutMCTrkMap::pointer mctrk_ptr,
					 TFvtxEvalMap::pointer eval_ptr)
{

}

//_____________________________________________________________
unsigned short mFvtxEval::get_n_maskhit(TMutMCTrkMap::pointer mctrk_ptr)
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
unsigned short mFvtxEval::get_true_hits(TFvtxTrkMap::pointer trk_ptr, TMutMCTrkMap::pointer mctrk_ptr)
{
	unsigned short n_true_hits = 0;
	unsigned short station;

	// Loop for all the associated TFvtxCoord [
	//	 Loop for all the associated TFvtxMCHit [
	//		 If(this associated MC track has the same trkid as primary contributor) [
	//			 n_true_hits+1;
	//		 ]
	//	 ]
	// ]
	// Mod:	Pack planes hit into n_true_hits rather than just recording total number of hits

	TFvtxCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
	while(TFvtxCoordMap::const_pointer coord_ptr=coord_iter.next()){
		TFvtxMCHitMap::const_key_iterator mchit_iter = coord_ptr->get()->get_associated<TFvtxMCHit>();
		while(TFvtxMCHitMap::const_pointer mchit_ptr=mchit_iter.next())
		if((mchit_ptr->get()->get_track_id())==(mctrk_ptr->get()->get_track_id())){
			station = coord_ptr->get()->get_station();
			if (! (n_true_hits &	1<<station) ) n_true_hits += 1<<station;
		}
	}
	return n_true_hits;
}

//_____________________________________________________________
void mFvtxEval::fill_eval_res(TFvtxTrkMap::pointer trk_ptr,
					 TMutMCTrkMap::pointer mctrk_ptr,
					 TFvtxEvalMap::pointer eval_ptr)
{
	// TBI-> In Working Progress.
	// Loop over all the TMutCoord which are associated with TMutTrk [
	//	 Pickup the TMutMChit which is associated with TMutCoord.
	//	 Define a buffer for TMutEvalRes object.
	//	 Fill the fields of TMutEvalRes object.
	// ]
	//

}
