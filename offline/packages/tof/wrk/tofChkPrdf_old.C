//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void tofChkPrdf_old(const Int_t maxEvents=1, const char *prdfIFile="phnx.prdf"){

  Int_t eventNumber = 0;
  const Int_t QVCMIN = 10;
  const Int_t QVCMAX = 4100;
  const Int_t TVCMIN = 10;
  const Int_t TVCMAX = 3800;

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
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libtof_tables.so");
  gSystem->Load("libtof.so");

  Int_t verbose = 12;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

  // Set up the modules
  mTofSetGeoModule* mTofSetGeo = new mTofSetGeoModule;
  mTofSetFEMmapModule* mTofSetFEMmap = new mTofSetFEMmapModule;
  mTofUnpackModule* mTofUnpack = new mTofUnpackModule;

  // Initialize the tables
  size_t mr=1;
  dTofGeoParWrapper* dTofGeoPar = new dTofGeoParWrapper("dTofGeoPar",mr);
  PHIODataNode<PHTable>* dTofGeoParNode = new PHIODataNode<PHTable>(dTofGeoPar,"dTofGeoPar");
  parNode->addNode(dTofGeoParNode);

  size_t mr=1500;
  dTofGeoWrapper* dTofGeo = new dTofGeoWrapper("dTofGeo",mr);
  PHIODataNode<PHTable>* dTofGeoNode = new PHIODataNode<PHTable>(dTofGeo,"dTofGeo");
  parNode->addNode(dTofGeoNode);

  size_t mr=150;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
  tofNode->addNode(dTofRawNode);

  size_t mr=960;
  dTofFEMmapWrapper* dTofFEMmap = new dTofFEMmapWrapper("dTofFEMmap",mr);
  PHIODataNode<PHTable>* dTofFEMmapNode = new PHIODataNode<PHTable>(dTofFEMmap,"dTofFEMmap");
  parNode->addNode(dTofFEMmapNode);

  size_t mr=8;
  dTofDCMWrapper* dTofDCM = new dTofDCMWrapper("dTofDCM",mr);
  PHIODataNode<PHTable>* dTofDCMNode = new PHIODataNode<PHTable>(dTofDCM,"dTofDCM");
  dcmNode->addNode(dTofDCMNode);

  PhRootHistogramFactory::buildFactory();

  // Parameter set-up
  // Setting dTofRawRecPar Parameters
  size_t nrc=1;
  dTofGeoPar->SetRowCount(nrc);
  dTofGeoPar->set_rslat(0,3.2905);
  dTofGeoPar->set_slat_width(0,1.52654);
  dTofGeoPar->set_scintz(0,0,69.631);
  dTofGeoPar->set_scintz(1,0,11.606);
  dTofGeoPar->set_scintz(2,0,56.609);
  dTofGeoPar->set_rpos(0,0,503.0);
  dTofGeoPar->set_rpos(1,0,503.0);
  dTofGeoPar->set_rpos(2,0,503.0);
  dTofGeoPar->set_rpos(3,0,503.0);
  dTofGeoPar->set_rpos(4,0,503.0);
  dTofGeoPar->set_rpos(5,0,503.0);
  dTofGeoPar->set_rpos(6,0,503.0);
  dTofGeoPar->set_rpos(7,0,503.0);
  dTofGeoPar->set_rpos(8,0,503.0);
  dTofGeoPar->set_rpos(9,0,503.0);
  dTofGeoPar->set_rpos(10,0,0.0);
  dTofGeoPar->set_phi(0,0,180.0);
  dTofGeoPar->set_phi(1,0,180.0);
  dTofGeoPar->set_phi(2,0,180.0);
  dTofGeoPar->set_phi(3,0,180.0);
  dTofGeoPar->set_phi(4,0,180.0);
  dTofGeoPar->set_phi(5,0,180.0);
  dTofGeoPar->set_phi(6,0,180.0);
  dTofGeoPar->set_phi(7,0,180.0);
  dTofGeoPar->set_phi(8,0,202.5);
  dTofGeoPar->set_phi(9,0,202.5);
  dTofGeoPar->set_phi(10,0,0.0);
  dTofGeoPar->set_zpos(0,0,-170.972);
  dTofGeoPar->set_zpos(1,0,-122.123);
  dTofGeoPar->set_zpos(2,0,-73.2739);
  dTofGeoPar->set_zpos(3,0,-24.4246);
  dTofGeoPar->set_zpos(4,0,24.4246);
  dTofGeoPar->set_zpos(5,0,73.2739);
  dTofGeoPar->set_zpos(6,0,122.123);
  dTofGeoPar->set_zpos(7,0,170.972);
  dTofGeoPar->set_zpos(8,0,-24.4246);
  dTofGeoPar->set_zpos(9,0,24.4246);
  dTofGeoPar->set_zpos(10,0,0.0);

  mainIter.cd();

  // Histogram Setting
  gROOT->cd();
  Int_t itof, ntof;
  TH1F *tofhist1 = new TH1F("tofhist1","TOF hits/event",
			    maxEvents+2,0.0,(Float_t)maxEvents+2);
  TH1F *tofhist2 = new TH1F("tofhist2","TOF hits/slatid",1100,0.0,1100.0);
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


    printf("Calling first event only modules.\n");
    if (eventNumber == 1) {

      if (verbose>10) printf("Calling mTofSetGeo\n");
      mTofSetGeo->event(topNode);

      if (verbose>10) printf("Calling mTofSetFEMmap\n");
      mTofSetFEMmap->event(topNode);
 
    }

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling TofGetDCM\n");
    TofGetDCM(topNode);

    if (verbose>10) printf("Calling mTofUnpack\n");
     mTofUnpack->event(topNode);

    // Histogram Filling
    tofhist1->Fill((Float_t) dTofRaw->RowCount());
    ntof = 0;
    itof = 0;
    for(itof = 0; itof < dTofRaw->RowCount(); itof++) {
      if((dTofRaw->get_qvc(0,itof)>QVCMIN&&dTofRaw->get_qvc(0,itof)<QVCMAX)&&
	 (dTofRaw->get_qvc(1,itof)>QVCMIN&&dTofRaw->get_qvc(1,itof)<QVCMAX)&&
	 (dTofRaw->get_tvc(0,itof)>TVCMIN&&dTofRaw->get_tvc(0,itof)<QVCMAX)&&
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
	ntof++;
      }
    }
    // Reset all data for this event
    if (mainIter.cd("DCM")) {
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

