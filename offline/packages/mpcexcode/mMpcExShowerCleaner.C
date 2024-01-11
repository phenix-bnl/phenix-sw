#include "mMpcExShowerCleaner.h"
#include "getClass.h"
#include "Fun4AllReturnCodes.h"
#include "MpcExConstants.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "phool.h"
#include "TMpcExShower.h"
#include "TMpcExShowerContainer.h"
#include <algorithm>
#include <vector>

bool OrderShowersDescending(TMpcExShower* a, TMpcExShower* b) {
  return a->get_roughTotE() > b->get_roughTotE();
}

mMpcExShowerCleaner::mMpcExShowerCleaner(){
  _pruned_showers = NULL;
}

mMpcExShowerCleaner::~mMpcExShowerCleaner(){
}

int mMpcExShowerCleaner::Init(PHCompositeNode *topNode){
  //add the pruned shower node 
  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode","DST"));

  _pruned_showers = new TMpcExShowerContainer();
  PHIODataNode<TMpcExShowerContainer>* showerNode = new PHIODataNode<TMpcExShowerContainer>(_pruned_showers,"TMpcExShowerContainer","PHObject");
  dstNode->addNode(showerNode);

  return EVENT_OK;
}

int mMpcExShowerCleaner::InitRun(PHCompositeNode* top_node) {
  // clear maps
  _pruned_showers->Reset(); 

  return EVENT_OK;
}

int mMpcExShowerCleaner::process_event(PHCompositeNode *topNode){

  TMpcExShowerContainer *all_showers = findNode::getClass<TMpcExShowerContainer>(topNode,"TMpcExRawShowerContainer");
  if(!all_showers){
    std::cout << PHWHERE <<":: I could not find TMpcExRawShowerContainer..."<<std::endl;
    return EVENT_OK;
  }

  _pruned_showers->Reset();

  //loop over all showers and build up stl container for easier manipulation
  //break out by arm
  unsigned int nshowers = all_showers->size();
  std::vector<TMpcExShower*> all_showers_vec[2];
  for(unsigned int is=0; is<nshowers; is++){
    TMpcExShower *shower = all_showers->getShower(is);
    int arm = shower->get_arm();
    //only keep showers that have good calibrations
    if((arm == 0 || arm == 1) && shower->get_CalibEInRange()==1)
      all_showers_vec[arm].push_back(shower);
  }

  for(int iarm=0; iarm<MpcExConstants::NARMS; iarm++) {
    //energy order the vectors
    std::sort(all_showers_vec[iarm].begin(),all_showers_vec[iarm].end(),OrderShowersDescending);
    //this is the vector with the same length that indicates that 
    //if the shower will be kept or not
    unsigned int nshowers = all_showers_vec[iarm].size();
    std::vector<bool> keepVector(nshowers,true);
    //set the keepVector to false for a shower whose central tower overlaps
    //with the 3x3 of any shower with greater energy
    for(unsigned int is1=0; is1<nshowers; is1++){
      //if this is already won't be kept, continue
      if(keepVector[is1] == false) continue;
      TMpcExShower *current = all_showers_vec[iarm][is1];
      for(unsigned int is2=is1+1; is2<nshowers; is2++){
	if(keepVector[is2] == false) continue;
	TMpcExShower *next = all_showers_vec[iarm][is2];
	if(std::abs(current->get_mpcPeakix()-next->get_mpcPeakix())<=1 && std::abs(current->get_mpcPeakiy()-next->get_mpcPeakiy())<=1){
	  keepVector[is2] = false;
	}
      }
    }
    //got all of the showers we are going to keep
    //so let's put them in the shower container
    for(unsigned int is1=0; is1<nshowers; is1++){
      if(keepVector[is1]){
	_pruned_showers->addShower(new TMpcExShower(all_showers_vec[iarm][is1]));
      }
    }
  }

  return EVENT_OK;
}

