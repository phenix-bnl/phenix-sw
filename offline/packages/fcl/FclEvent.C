//
// This object is called for each event.  It reads in the packet
// and loads the detector information into the FclRaw objects.
//
// SCJ (17 January 2002) First iteration.


#include "PHIODataNode.h"

#include "FclIndexer.h"
#include "FclRaw.h"
#include "FclOut.h"
#include "FclEvent.h"
#include "ZdcOut.h"

#include "Event.h"

#include "TFile.h"
#include "TNtuple.h"

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<FclRaw> FclRawNode_t;
typedef PHIODataNode<FclRaw> FclRawNode_t;
typedef PHIODataNode<FclOut> FclOutNode_t;
typedef PHDataNode<Event> EventNode_t;

int FCAL_DEBUG2 = 0;

FclEvent::FclEvent()
{
  ntupleFile = 0;
  fcalNtuple = 0;
  indexer = 0;

  if (getenv("FCAL_DEBUG2")) FCAL_DEBUG2=1;
  
  if (FCAL_DEBUG2){
    ntupleFile = new TFile("fcal.root","RECREATE");
    fcalNtuple = new TNtuple("fcalRaw","raw data",
			     "side:channel:lvchannel:row:column:tdc:lowGain:lowPre:lowPost:evt:zdce0:crossCtr:highPost:highPre:zdce1");
  }


} /* end of routine FclEvent::FclEvent() */

FclEvent::~FclEvent()
{
  if(ntupleFile)
  {
    ntupleFile->Write();
    ntupleFile->Close();
    delete ntupleFile;
    ntupleFile = 0;
  }
}

// The routine called on each event.
int FclEvent::event(PHCompositeNode *topNode)
{

  if (FCAL_DEBUG2){
    cout << endl << endl
	 <<"Well ... here we are in the FCAL software... ... " 
	 << endl << endl;
  }

  indexer = FclIndexer::Instance();


  // First setup the node structure and get your objects in order:
  PHNodeIterator iter(topNode);
  Event *evt = NULL;
  Packet *fclNorthPacket = NULL;
  Packet *fclSouthPacket = NULL;
  FclRaw* fclRawNorth = NULL;
  FclRaw* fclRawSouth = NULL;
  FclOut* fclOutNorth = NULL;
  FclOut* fclOutSouth = NULL;
  
  PHTypedNodeIterator<FclRaw> fcliter(topNode);
  FclRawNode_t *fclRawNorthNode = fcliter.find("fclRawNorth");
  FclRawNode_t *fclRawSouthNode = fcliter.find("fclRawSouth");

  PHTypedNodeIterator<FclOut> fclOutiter(topNode);
  FclOutNode_t *fclOutNorthNode = fclOutiter.find("fclOutNorth");
  FclOutNode_t *fclOutSouthNode = fclOutiter.find("fclOutSouth");

  EventNode_t *EventNode = 
    dynamic_cast<EventNode_t*> (iter.findFirst("PHDataNode","PRDF"));

  
  // Now get the raw data tables:
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

  if (fclOutNorthNode){
    fclOutNorth = fclOutNorthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclOutNorth node missing..." << endl;
    return 0;
  }

  if (fclOutSouthNode){
    fclOutSouth = fclOutSouthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclOutSouth node missing." << endl;
    return 0;
  }


  // Get the event data and fill the raw objects.
  if (EventNode)
    {
      evt=EventNode->getData();
      if (!evt)
	{
	  cout << PHWHERE 
	       << "[ERROR] NULL Event Pointer" << endl;
	  return 0;
	}
      int eventNumber= evt->getEvtSequence();
      // Get the packets
      fclNorthPacket=evt->getPacket(FEM_ID_NORTH);
      fclSouthPacket=evt->getPacket(FEM_ID_SOUTH);
      if (!fclNorthPacket || !fclSouthPacket)
	{
// 	      cout << "FclEvent::Fcal Packets not found" << endl;
	  return 0;
	}
      // and then use those to initialize the raw objects:


      fclRawNorth->readData(fclNorthPacket, 0); // north is side 0
      fclRawSouth->readData(fclSouthPacket, 1); // south is side 1

      //fill processed output objects
      fclOutNorth->readData(fclRawNorth);
      fclOutSouth->readData(fclRawSouth);
      //calibrate
      fclOutNorth->calibrateData(calibNorth);
      fclOutSouth->calibrateData(calibSouth);

      fclOutNorth->computeSums();
      fclOutSouth->computeSums();

      // get the GL1 trigger packet adrress
      Packet *p= evt->getPacket(14001);


      int crossingCounter;
      int beamCounter0;
      int beamCounter1;
      
      static int FclFirstBeam;
      if (p){
	crossingCounter = p->iValue(0, "CROSSCTR");
	beamCounter0 = p->iValue(0, "BEAMCTR0");
	beamCounter1 = p->iValue(0, "BEAMCTR1");
	//cout << "Event  = " << eventNumber << endl;
	//cout << "First  = " << FclFirstBeam << endl
	//   << "Beam0 = " << beamCounter0 << endl;
	if (FclFirstBeam==0) {
	  FclFirstBeam=beamCounter0-crossingCounter;
	}
	delete p;
      }else{
	crossingCounter = -1;
	beamCounter0 = -1;
	beamCounter1 = -1;
      }
      //      int gapCounter = (beamCounter0-crossingCounter)-FclFirstBeam;

      //cout << "Gap   = " << gapCounter << endl
      //   << "Cross = " << crossingCounter << endl;

      if (FCAL_DEBUG2){
	// Let's get the reconstructed zdc energy
	PHTypedNodeIterator<ZdcOut> zdciter(topNode);
	PHIODataNode<ZdcOut> *zdcNode = zdciter.find("ZdcOut");
	ZdcOut *zdcO = 0;
	float zdce0 = -1000.0;
	float zdce1 = -1000.0;
	if(zdcNode) zdcO= zdcNode->getData();
	// This swap is intentional and matches PHGlobal Convention
	if(zdcO){ zdce0 = zdcO->get_Energy(1);}
	if(zdcO){ zdce1 = zdcO->get_Energy(0);}

	for (int channel=0; channel<CHANTOT; channel++){
	  fcalNtuple->Fill(1, channel, 
			   indexer->getLVSouth(channel),
			   indexer->getRowSouth(channel),
			   indexer->getColumnSouth(channel),
			   fclRawSouth->getTdc(channel),
			   fclRawSouth->getLowGain(channel),
			   fclRawSouth->getLowAdcPre(channel),
			   fclRawSouth->getLowAdcPost(channel),
			   eventNumber, zdce0, crossingCounter,
			   fclRawSouth->getHighAdcPost(channel),
			   fclRawSouth->getHighAdcPre(channel),
			   zdce1
			   );
	  fcalNtuple->Fill(0, channel, 
			   indexer->getLVNorth(channel),
			   indexer->getRowNorth(channel),
			   indexer->getColumnNorth(channel),
			   fclRawNorth->getTdc(channel),
			   fclRawNorth->getLowGain(channel),
			   fclRawNorth->getLowAdcPre(channel),
			   fclRawNorth->getLowAdcPost(channel),
			   eventNumber, zdce0, crossingCounter,
			   fclRawNorth->getHighAdcPost(channel),
			   fclRawNorth->getHighAdcPre(channel),
			   zdce1
			   );
	}
      }

      delete fclNorthPacket; delete fclSouthPacket;
      fclNorthPacket = NULL; fclSouthPacket = NULL;
    }
  else{
    cout << PHWHERE
	 << "[ERROR] PRDF Node missing" << endl;
    fflush(stdout); 
    return 0;
  }

  return 1;
} // end of routine FclEvent::event() 


void FclEvent::saveDebug(){

  if (FCAL_DEBUG2){
    fcalNtuple->Write();
  }
  //ntupleFile->Write();


}

int FclEvent::setCalibration(PHTimeStamp &time){

  calibNorth.setSide(FCALNORTH);
  calibSouth.setSide(FCALSOUTH);

  calibNorth.getDatabaseInfo(time);
  calibSouth.getDatabaseInfo(time);
  return 1;
}

int FclEvent::setCalibration(int runNumber){

  calibNorth.setSide(FCALNORTH);
  calibSouth.setSide(FCALSOUTH);

  calibNorth.getDatabaseInfo(runNumber);
  calibSouth.getDatabaseInfo(runNumber);
  return 1;
}























