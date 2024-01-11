
//
// This object will compare fcal to zdc to bbc
//
// RAS 13-Mar-03
//

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHDataNode.h"
#include "PHIODataNode.h"

#include "TFile.h"
#include "TNtuple.h"

#include "Event.h"
#include "FclRaw.h"
#include "ZdcOut.h"

#include "FclIndexer.h"
#include "FclCompare.h"

#include <iostream>

using namespace std;

typedef PHIODataNode<FclRaw> FclRawNode_t;
typedef PHIODataNode<ZdcOut> ZdcOutNode_t;
typedef PHDataNode<Event>    EventNode_t;

FclCompare::FclCompare()
{

  FclCompareFile = new TFile("fclcompare.root","RECREATE");
  FclCompareNt = new TNtuple("FclCompare","FclComp",
			     "evtNum:fclN:fclS:fclS2:zdcN:zdcS");

} /* end of routine FclCompare::FclCompare() */


// The routine called on each event.
int FclCompare::compare(PHCompositeNode *topNode)
{

  // First setup the node structure and get the fclRaw objects
  PHNodeIterator iter(topNode);

  PHTypedNodeIterator<FclRaw> fcliter(topNode);
  FclRawNode_t *fclRawNorthNode = fcliter.find("fclRawNorth");
  FclRawNode_t *fclRawSouthNode = fcliter.find("fclRawSouth");

  PHTypedNodeIterator<ZdcOut> zdciter(topNode);
  ZdcOutNode_t *ZdcOutNode = zdciter.find("ZdcOut");

  EventNode_t *EventNode = 
    dynamic_cast<EventNode_t*> (iter.findFirst("PHDataNode","PRDF"));

  Event *evt = NULL;
  FclRaw* fclRawNorth = NULL;
  FclRaw* fclRawSouth = NULL;
  ZdcOut* zdcOut = NULL;

  indexer = FclIndexer::Instance();

  if (fclRawNorthNode){
    fclRawNorth = fclRawNorthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclRawNorth node missing..." << endl;
    return 0;
  }

  if (fclRawSouthNode){
    fclRawSouth = fclRawSouthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclRawSouth node missing." << endl;
    return 0;
  }

  if (ZdcOutNode) {
    zdcOut = ZdcOutNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: ZdcOut node missing." << endl;
    return 0;
  }

  if (EventNode) {
    evt=EventNode->getData();
    if (!evt)
      {
	cout << PHWHERE 
	     << "[ERROR] NULL Event Pointer" << endl;
	fflush(stdout);
	return 0;
      }
    float evtNum = evt->getEvtSequence();

    float fclS  = 0;
    float fclS2 = 0;
    float fclN  = 0;

    // Sum over Fcal distributions

      for (int channel=0; channel<CHANTOT; channel++){

	float post = fclRawSouth->getLowAdcPost(channel);
	float pre = fclRawSouth->getLowAdcPre(channel);
	float column = indexer->getColumnSouth(channel);
	float row = indexer->getRowSouth(channel);

	fclS += post-pre;
	if (column > 1.5) fclS2 += post-pre;
	
	post = fclRawNorth->getLowAdcPost(channel);
	pre = fclRawNorth->getLowAdcPre(channel);
	column = indexer->getColumnNorth(channel);
	row = indexer->getRowNorth(channel);

	fclN += post-pre;
      }

    float zdcS = zdcOut->get_Energy(0);
    float zdcN = zdcOut->get_Energy(1);

    FclCompareNt->Fill(evtNum,fclN,fclS,fclS2,zdcN,zdcS);
    
  }
  
  else {
    cout << PHWHERE
	 << "[ERROR] PRDF Node missing" << endl;
    fflush(stdout); 
    return 0;
  }


  return 1;
} // end of routine FclCompare::compare() 


void FclCompare::saveCompare(){

  cout << "Saving fclcompare.root file." << endl;
  FclCompareFile->Write();

}
