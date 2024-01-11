
//
// This object is called for each event.  It assumes the FclRaw objects 
// have been loaded (by FclEvent), and finds them.  The data from each
// event are used to fill TH2F objects, which the user can save to disk.
//
// RAS (2/20/03)
//

#include "TCanvas.h"
#include "TFile.h"
#include "TH2.h"
//INCLUDECHECKER: Removed this line: #include "TPad.h"

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHDataNode.h"
#include "PHIODataNode.h"

#include "Event.h"

#include "FclRaw.h"
#include "FclIndexer.h"
#include "FclDisplay.h"
#include <iostream>

using namespace std;

typedef PHIODataNode<FclRaw> FclRawNode_t;
typedef PHIODataNode<FclRaw> FclRawNode_t;
typedef PHDataNode<Event> EventNode_t;

FclDisplay::FclDisplay()
{

  FclDisplayFile = new TFile("fcldisplay.root","RECREATE");
  Fclc1 = new TCanvas("Fclc1","FclDisplay",200,10,700,500);
  Fclc1_1 = new TPad("Fclc1_1", "Fclc1_1",0.01,0.01,0.49,0.99);
  Fclc1_1->Draw();
  Fclc1_1->Range(-0.985844,-1.11792,0.985844,1.11792);
  Fclc1_2 = new TPad("Fclc1_2", "Fclc1_2",0.51,0.01,0.99,0.99);
  Fclc1_2->Draw();
  Fclc1_2->Range(-0.985844,-1.11792,0.985844,1.11792);

} /* end of routine FclDisplay::FclDisplay() */


// The routine called on each event.
int FclDisplay::display(PHCompositeNode *topNode)
{

  // First setup the node structure and get the fclRaw objects
  PHNodeIterator iter(topNode);

  PHTypedNodeIterator<FclRaw> fcliter(topNode);
  FclRawNode_t *fclRawNorthNode = fcliter.find("fclRawNorth");
  FclRawNode_t *fclRawSouthNode = fcliter.find("fclRawSouth");
  EventNode_t *EventNode = 
    dynamic_cast<EventNode_t*> (iter.findFirst("PHDataNode","PRDF"));

  Event *evt = NULL;
  FclRaw* fclRawNorth = NULL;
  FclRaw* fclRawSouth = NULL;

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

  // Create the 2d histograms for this event
  if (EventNode)
    {
      evt=EventNode->getData();
      if (!evt)
	{
	  cout << PHWHERE 
	       << "[ERROR] NULL Event Pointer" << endl;
	  fflush(stdout);
	  return 0;
	}
      int eventNumber= evt->getEvtSequence();

      FclDisplayFile->cd();
      char title[32], name[8];
      sprintf(name,"hs%06d",eventNumber);
      sprintf(title,"FCAL South Event%6d",eventNumber);
      Fclhs = new TH2F(name,title,10,0.,10.,10,0.,10.);

      sprintf(name,"hn%06d",eventNumber);
      sprintf(title,"FCAL North Event%6d",eventNumber);
      Fclhn = new TH2F(name,title,10,0.,10.,10,0.,10.);

      for (int channel=0; channel<CHANTOT; channel++){

	float post = fclRawSouth->getLowAdcPost(channel);
	float pre = fclRawSouth->getLowAdcPre(channel);
	float column = indexer->getColumnSouth(channel);
	float row = indexer->getRowSouth(channel);
	
	Fclhs->Fill(column,row,(post-pre));
	
	post = fclRawNorth->getLowAdcPost(channel);
	pre = fclRawNorth->getLowAdcPre(channel);
	column = indexer->getColumnNorth(channel);
	row = indexer->getRowNorth(channel);
	
	Fclhn->Fill(column,row,(post-pre));
	
      }

      // Display the pretty histograms, wait for prompt, and delete
      cout << "Draw histograms" << endl;
      Fclc1_1->cd();
      Fclhs->Draw("colz");
      Fclc1_2->cd();
      Fclhn->Draw("colz");
      Fclc1->Modified();
      Fclc1->Update();

      char input;
      cout << "Enter: (c)ontinue (q)uit (s)ave (p)rint" << endl;
      cin >> input;
      switch (input) {
      case 'c':
	delete Fclhs; Fclhs = NULL;
	delete Fclhn; Fclhn = NULL;
	break;
      case 'q':      
	return 0;
      case 's':
	cout << "Saving..." << endl;
	break;
      case 'p':
	cout << "Not yet implemented.  Use canvas menu for printing." << endl;
	break;
      }

    }

  else{
    cout << PHWHERE
	 << "[ERROR] PRDF Node missing" << endl;
    fflush(stdout); 
    return 0;
  }

  return 1;
} // end of routine FclDisplay::display() 


void FclDisplay::saveDisplay(){

  cout << "Saving fcldisplay.root file." << endl;
  FclDisplayFile->Write();

}
















