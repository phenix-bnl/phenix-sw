#include "mMpcExSimEventHeader.h"
#include "PHCompositeNode.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "MpcExEventHeader.h"
#include "MpcExConstants.h"
#include <iostream>
#include <vector>

mMpcExSimEventHeader::mMpcExSimEventHeader() : SubsysReco("MMPCEXSIMEVENTHEADER") {
}

mMpcExSimEventHeader::~mMpcExSimEventHeader(){
}

int mMpcExSimEventHeader::process_event(PHCompositeNode *topNode){

  MpcExEventHeader *header = findNode::getClass<MpcExEventHeader>(topNode,"MpcExEventHeader");
  if(header == NULL){
    std::cout<<PHWHERE<<" I could not find MpcExEventHeader."<<std::endl;
    std::cout<<PHWHERE<<" Make sure you load mMpcExCreateNodeTree and set the SIMULATIONFLAG in recoConsts"<<std::endl;
    return ABORTRUN;
  }

  //assumes single event buffered
  header->setStack(1);

  //statephase of each pcaket = 0 when single buffered.
  //cellids of each chip = 1 for all chips will pass lock-up cut
  std::vector<unsigned short> statephases;
  std::vector<unsigned short> cellids;
  std::vector<unsigned short> parsttimes;
  for(unsigned short arm=0; arm<MpcExConstants::NARMS; arm++){
    for(unsigned short packet=0; packet<MpcExConstants::NPACKETS_PER_ARM; packet++){
      parsttimes.push_back(0); 
      statephases.push_back(0);
      for(unsigned short chain=0; chain<MpcExConstants::NCHAINS_PER_PACKET; chain++){
	for(unsigned short chip=0; chip<MpcExConstants::NCHIPS_PER_CHAIN; chip++){
	  int svxid = chip+chain*MpcExConstants::NCHIPS_PER_CHAIN;
	  cellids.push_back((arm << 15) + (packet << 12) + (svxid << 6) + 1);
	}
      }
    }
  }
  header->setStatephase(statephases);
  header->setCellIDs(cellids);
  header->setPARSTTime(parsttimes); 

  return EVENT_OK;
}
