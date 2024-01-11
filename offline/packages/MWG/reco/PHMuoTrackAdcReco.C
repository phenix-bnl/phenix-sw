#include "PHMuoTrackAdcReco.h"

#include "PHNodeIterator.h"
#include "Fun4AllReturnCodes.h"
#include "PHIODataNode.h"
#include "getClass.h"
#include "TMutTrkMap.h"
#include "TMutCoordMap.h"
#include "TMutClusMap.h"
#include "TMutHitMap.h"
#include "PHMuoTracksOut.h"
#include "PHMuoTrackAdc.h"
#include "PHMuoTracksAdc.h"

using namespace std;

///////////////////////////////////////////////////////////////////

PHMuoTrackAdcReco::PHMuoTrackAdcReco(const char *name) : SubsysReco(name){
}

///////////////////////////////////////////////////////////////////

int PHMuoTrackAdcReco::Init(PHCompositeNode *top_node){
  PHNodeIterator itr(top_node);
  PHCompositeNode *dst_node=
    dynamic_cast<PHCompositeNode*>(itr.findFirst("PHCompositeNode","DST"));

  if(!dst_node){
    printf("Error - %s::Init : No DST node\n",ClassName());
    return DONOTREGISTERSUBSYSTEM;
  }

  //PHMuoTracksAdcv1 *muotrks_adc=new PHMuoTracksAdcv1("PHMuoTrackAdcv1");
  PHMuoTracksAdc *muotrks_adc=PHMuoTracksAdc::NewPHMuoTracksAdc();

  PHIODataNode<PHMuoTracksAdc> *node=
    new PHIODataNode<PHMuoTracksAdc>(muotrks_adc,"PHMuoTracksAdc","PHObject");

  dst_node->addNode(node);

  return 0;
}

///////////////////////////////////////////////////////////////////

int PHMuoTrackAdcReco::process_event(PHCompositeNode *top_node){
  PHMuoTracksAdc *muotrks_adc=
    findNode::getClass<PHMuoTracksAdc>(top_node,"PHMuoTracksAdc");
  PHMuoTracksOut *muotrks=
    findNode::getClass<PHMuoTracksOut>(top_node,"PHMuoTracksOO");
  TMutTrkMap *mut_trkmap=findNode::getClass<TMutTrkMap>(top_node,"TMutTrkMap");

  if(!muotrks_adc || !muotrks || !mut_trkmap){
    printf("Error - %s::process_event : ",ClassName());
    printf("No PHMuoTracksAdc (%p) / ",muotrks_adc);
    printf("PHMuoTracksOut (%p) / ",muotrks);
    printf("TMutTrkMap (%p)\n",mut_trkmap);
    return ABORTRUN;
  }

  muotrks_adc->Reset();

  map<unsigned int,TMutTrk*> mut_trks;
  TMutTrkMap::iterator itr_trk=mut_trkmap->range();
  while(TMutTrkMap::pointer ptr_trk=itr_trk.next()){
    TMutTrk *trk=ptr_trk->get();
    unsigned int uid=trk->get_key().get_obj_key();
    mut_trks.insert(pair<unsigned int,TMutTrk*>(uid,trk));
  }

  for(unsigned int itrk=0; itrk<muotrks->get_npart(); itrk++){
    unsigned int uid=muotrks->get_uid(itrk);

    TMutTrk *trk=NULL;
    {
      map<unsigned int,TMutTrk*>::iterator itr=mut_trks.find(uid);
      if(itr==mut_trks.end()){
	printf("Error - %s : No TMutTrk with uid=%x\n",ClassName(),uid);
	continue;
      }

      trk=itr->second;
    }

    vector<unsigned char> strip_array(PHMuoTrackAdc::MAX_CATHODE_PLANE,0);
    vector<vector<unsigned short> >
      adc_array(PHMuoTrackAdc::MAX_CATHODE_PLANE,vector<unsigned short>(0));

    TMutCoordMap::key_iterator kitr_coord=trk->get_associated<TMutCoord>();
    while(TMutCoordMap::pointer ptr_coord=kitr_coord.next()){
      TMutCoord *coord=ptr_coord->get();
      int st=coord->get_station();
      int gap=coord->get_gap();
      int cath=coord->get_cathode();
      int cathode_num=cath+gap*2+st*6;

      map<int,vector<unsigned short> > adc_map;

      TMutClusMap::key_iterator kitr_clus=coord->get_associated<TMutClus>();
      while(TMutClusMap::pointer ptr_clus=kitr_clus.next()){
	TMutClus *clus=ptr_clus->get();
	TMutHitMap::key_iterator kitr_hit=clus->get_associated<TMutHit>();
	while(TMutHitMap::pointer ptr_hit=kitr_hit.next()){
	  TMutHit *hit=ptr_hit->get();
	  int strip=hit->get_strip();

	  vector<unsigned short> adc4;
	  for(int iadc=0; iadc<PHMuoTrackAdc::N_ADC_SAMPLE; iadc++){
	    adc4.push_back(hit->get_adc(iadc));
	  }

	  adc_map.insert(pair<int,vector<unsigned short> >(strip,adc4));
	}
      }

      int strip_beg=adc_map.begin()->first;
      int strip_end=adc_map.rbegin()->first;
      if(strip_end-strip_beg!=(int)adc_map.size()-1){
	printf("Warning - %s : Cluster size is not # of hits\n",ClassName());
	printf("Warning - %s : Fill absent hits with 0\n",ClassName());
	for(int strip=strip_beg+1; strip<strip_end; strip++){
	  vector<unsigned short> zero4(4,0);
	  adc_map.insert(pair<int,vector<unsigned short> >(strip,zero4));
	}
      }

      for(map<int,vector<unsigned short> >::iterator itr=adc_map.begin();
	  itr!=adc_map.end(); itr++){

	if(itr==adc_map.begin()){strip_array[cathode_num]=itr->first;}

	vector<unsigned short> &adc4=itr->second;
	for(unsigned int iadc=0; iadc<adc4.size(); iadc++){
	  adc_array[cathode_num].push_back(adc4[iadc]);
	}
      }
    }

    PHMuoTrackAdc *trk_adc=muotrks_adc->Insert();
    trk_adc->SetTrackUid(uid);
    trk_adc->SetAdc(strip_array,adc_array);
  }

  return EVENT_OK;
}

///////////////////////////////////////////////////////////////////
