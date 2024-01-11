#include "MutrgTrimHit.hh"

#include <set>

#include "getClass.h"
#include "TMutTrkMap.h"
#include "TMutCoordMap.h"
#include "TMutClusMap.h"
#include "TMutHitMap.h"
#include "MutrgPar.hh"
#include "MutrgKey.hh"
#include "MutrgHitArray.hh"

using namespace std;

//////////////////////////////////////////////////////////////////////

MutrgTrimHit::MutrgTrimHit(const char *name) : SubsysReco(name){
  win_nstp_mutrg=3;
}

//////////////////////////////////////////////////////////////////////

int MutrgTrimHit::process_event(PHCompositeNode *top_node){
  MutrgHitArray *mutrg_hits=
    findNode::getClass<MutrgHitArray>(top_node,"MutrgHitArray");
  if(!mutrg_hits){return 0;}

  TMutTrkMap *mut_trkmap=findNode::getClass<TMutTrkMap>(top_node,"TMutTrkMap");
  if(!mut_trkmap){return 0;}

  mutrg_hits->Readback();

  // fill delete list
  set<unsigned int> del_hit;
  for(MutrgHitArray::const_private_itr itr_mutrg=mutrg_hits->Begin();
      itr_mutrg!=mutrg_hits->End(); itr_mutrg++){
    del_hit.insert(itr_mutrg->first);}

  TMutTrkMap::iterator itr_trk=mut_trkmap->range();
  while(TMutTrkMap::pointer ptr_trk=itr_trk.next()){
    TMutTrk *mut_trk=ptr_trk->get();
    if(mut_trk->get_ghost()){continue;}

    TMutCoordMap::key_iterator kitr_coord=mut_trk->get_associated<TMutCoord>();
    while(TMutCoordMap::pointer ptr_coord=kitr_coord.next()){
      TMutCoord *mut_coord=ptr_coord->get();
      int arm=mut_coord->get_arm();
      int st=mut_coord->get_station();
      int oct=mut_coord->get_octant();
      int hoct=mut_coord->get_half_octant();
      int gap=mut_coord->get_gap();
      int cath=mut_coord->get_cathode();
      int strip_peak=mut_coord->get_peak_strip();

      // skip stereo cathodes
      if(cath!=MutrgPar::INSTALL_CATHODE[st]){continue;}

      TMutClusMap::key_iterator kitr_clus=
	mut_coord->get_associated<TMutClus>();
      while(TMutClusMap::pointer ptr_clus=kitr_clus.next()){
	TMutHitMap::key_iterator kitr_hit=
	  ptr_clus->get()->get_associated<TMutHit>();
	while(TMutHitMap::pointer ptr_hit=kitr_hit.next()){
	  TMutHit *mut_hit=ptr_hit->get();
	  int strip=mut_hit->get_strip();

	  unsigned int key=MutrgKey::LocToKey(arm,st,oct,hoct,0,0,strip);
	  del_hit.erase(key);
	}
      }

      for(int istp=-win_nstp_mutrg; istp<win_nstp_mutrg+1; istp++){
	if(istp==0){continue;}

	int strip=strip_peak+istp;
	int nstp_hoct=MutrgPar::NSTRIP_IN_HALFOCTANT(arm,st,oct,hoct,gap,cath);
	if(strip<0 || strip>=nstp_hoct){continue;}

	unsigned int key=MutrgKey::LocToKey(arm,st,oct,hoct,0,0,strip);
	del_hit.erase(key);
      }
    }
  }

  // delete track un-associated objects
  for(set<unsigned int>::iterator itr_del=del_hit.begin();
      itr_del!=del_hit.end(); itr_del++){
    unsigned int key_del=*itr_del;
    mutrg_hits->Remove(key_del);
  }

  return 0;
}

//////////////////////////////////////////////////////////////////////
