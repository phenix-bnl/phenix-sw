//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void tofChkPrdf(const Int_t minEvents=0, const Int_t maxEvents=1, const char *prdfIFile="phnx.prdf",  const Int_t TVCMIN = 10, const Int_t TVCMAX = 3800, const Int_t QVCMIN = 10, const Int_t QVCMAX = 4100){

  Int_t eventNumber = 0;

  // Executing initialization and parameter macros
  //gROOT->Macro("tofrecoini.C");
  //gROOT->Macro("tofrecopar.C");

  // Initialization
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libtof.so");

  Int_t verbose = 7;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  mTofUnpackModule* mTofUnpack = new mTofUnpackModule;

  // Initialize the tables
  size_t mr=1000;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
  dstNode->addNode(dTofRawNode);

  PhRootHistogramFactory::buildFactory();

  mainIter.cd();

  // Histogram Setting
  gROOT->cd();
  Int_t itof, ntof;
  TH1F *tofhist1 = new TH1F("tofhist1","TOF hits/event",
			    maxEvents+2,0.0,(Float_t)maxEvents+2);
  TH1F *tofhist2 = new TH1F("tofhist2","TOF hits/slatid",1000,0.0,1000.0);
  char title[80];
  sprintf(title,"TOF qvc  [%d - %d]",QVCMIN, QVCMAX);
  TH1F *tofhist3 = new TH1F("tofhist3",title,105,0.0,4200);
  sprintf(title,"TOF tvc  [%d - %d]",TVCMIN, TVCMAX);
  TH1F *tofhist4 = new TH1F("tofhist4",title,105,0.0,4200);
  TH1F *tofhist5 = new TH1F("tofhist5","TOF (tvc0-tvc1)/2",200,-1000,1000);
  TH1F *tofhist6 = new TH1F("tofhist6","TOF (tvc0+tvc1)/2",200,0.0,4000);

  if (verbose>5) printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    if (verbose>5) printf("Fetched event %d\n",eventNumber);


    if (verbose>10) printf("Calling first event only modules.\n");
    if (eventNumber == 1) {
      if (verbose>10) printf("Calling TofAddress\n");
      TofAddress->setTimeStamp(TimeStamp);
      TofAddress->fetchFromFile("toffemmap.txt");

      if (verbose>10) printf("Calling TofGeometry\n");
      TofGeometry->setTimeStamp(TimeStamp);
      TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");
    }
    if(eventNumber < minEvents) continue;

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling mTofUnpack\n");
     mTofUnpack->event(topNode, TofAddress);

    // Histogram Filling
    ntof = 0;
    itof = 0;
    for(itof = 0; itof < dTofRaw->RowCount(); itof++) {
      if((dTofRaw->get_qvc(0,itof)>QVCMIN&&dTofRaw->get_qvc(0,itof)<QVCMAX)&&
	 (dTofRaw->get_qvc(1,itof)>QVCMIN&&dTofRaw->get_qvc(1,itof)<QVCMAX)&&
	 (dTofRaw->get_tvc(0,itof)>TVCMIN&&dTofRaw->get_tvc(0,itof)<TVCMAX)&&
	 (dTofRaw->get_tvc(1,itof)>TVCMIN&&dTofRaw->get_tvc(1,itof)<TVCMAX)&&
	 eventNumber>=0){
	Int_t slatid = dTofRaw->get_slatid(itof);

	tofhist1->Fill((Float_t) eventNumber);
	tofhist2->Fill((Float_t) slatid);
	tofhist3->Fill((Float_t) dTofRaw->get_qvc(0,itof));
	tofhist3->Fill((Float_t) dTofRaw->get_qvc(1,itof));
	tofhist4->Fill((Float_t) dTofRaw->get_tvc(0,itof));
	tofhist4->Fill((Float_t) dTofRaw->get_tvc(1,itof));

	tofhist5->Fill(((Float_t) dTofRaw->get_tvc(0,itof) -
			(Float_t) dTofRaw->get_tvc(1,itof))/2);
	tofhist6->Fill(((Float_t) dTofRaw->get_tvc(0,itof) +
			(Float_t) dTofRaw->get_tvc(1,itof))/2);
	//tofhistz->Fill(TofGeometry->getZpos(slatid));

	ntof++;
      }
    }

    // Reset all data for this event
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TOF")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }

  // Let's look at the health histograms
  char *w = new char[1];
  TCanvas *c = new TCanvas("c","TOF Plot",600,800); 
  c->Divide(2,3);
  c->cd(1);
  tofhist1->Draw();
  c->cd(2);
  tofhist2->Draw();
  c->cd(3);
  tofhist3->Draw();
  c->cd(4);
  tofhist4->Draw();
  c->cd(5);
  tofhist5->Draw();
  c->cd(6);
  tofhist6->Draw();
  c->cd(2);
}

