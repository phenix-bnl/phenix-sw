//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void tofChkReco(const Int_t minEvents=0,const Int_t maxEvents=1, const char *prdfIFile="phnx.prdf", const Float_t ChargeCut = -10, const Float_t TvcPedeCut = 10) {

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

  Int_t verbose = 12;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
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
  TofCalibObject* TofCalib = new TofCalibObject();
  mTofUnpackModule* mTofUnpack = new mTofUnpackModule;
  mTofRawRecModule* mTofRawRec = new mTofRawRecModule;

  // Initialize the tables
  size_t mr=1000;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
  dstNode->addNode(dTofRawNode);

  size_t mr=960;
  dTofReconstructedWrapper* dTofReconstructed = new dTofReconstructedWrapper("dTofReconstructed",mr);
  PHIODataNode<PHTable>* dTofReconstructedNode = new PHIODataNode<PHTable>(dTofReconstructed,"dTofReconstructed");
  dstNode->addNode(dTofReconstructedNode);

  size_t mr=960;
  dTofRawRecWrapper* dTofRawRec = new dTofRawRecWrapper("dTofRawRec",mr);
  PHIODataNode<PHTable>* dTofRawRecNode = new PHIODataNode<PHTable>(dTofRawRec,"dTofRawRec");
  tofNode->addNode(dTofRawRecNode);

  size_t mr=1;
  dTofRawRecParWrapper* dTofRawRecPar = new dTofRawRecParWrapper("dTofRawRecPar",mr);
  PHIODataNode<PHTable>* dTofRawRecParNode = new PHIODataNode<PHTable>(dTofRawRecPar,"dTofRawRecPar");
  parNode->addNode(dTofRawRecParNode);


  PhRootHistogramFactory::buildFactory();

  // Parameter set-up
  // Setting dTofRawRecPar Parameters
  size_t nrc=1;
  dTofRawRecPar->SetRowCount(nrc);
  dTofRawRecPar->set_verbose(0,0);

  mainIter.cd();

  // Histogram Setting
  gROOT->cd();
  Int_t itof,ntof;
  TH1F *tofhist1 = new TH1F("tofhist1","TOF hits/event",
			    maxEvents+2,0.0,(Float_t)maxEvents+2);
  TH1F *tofhist2 = new TH1F("tofhist2","TOF hits/slatid",1000,0.0,1000.0);
  TH1F *tofhist3 = new TH1F("tofhist3","TOF time-of-flight",200,0.0,100.0);
  TH1F *tofhist4 = new TH1F("tofhist4","TOF energy loss",100,0.0,0.05);
  TH2F *tofhist5 = 
    new TH2F("tofhist5","TOF Y-Z position",80, -200, 200, 80, -300, 100);
  TH2F *tofhist0 = 
    new TH2F("tofhist0","TOF x vs y",100,-600.0,600.0,100,-600.0,600.0);
  TH1F *tofhisty = new TH1F("tofhisty","TOF Y position",80, -300, 100);
  TH1F *tofhistz = new TH1F("tofhistz","TOF Z position",80, -200, 200);

  if (verbose>5) printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    if (eventNumber == 1) {
      printf("Calling first event only modules.\n");
      if (verbose>10) printf("Calling TofAddress\n");
      TofAddress->setTimeStamp(TimeStamp);
      TofAddress->fetchFromFile("toffemmap.txt");

      if (verbose>10) printf("Calling TofGeometry\n");
      TofGeometry->setTimeStamp(TimeStamp);
      TofGeometry->setEastCarriage(0.0, 0.0, 0.0);
      TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");

      if (verbose>10) printf("Calling TofCalib\n");
      TofCalib->setTimeStamp(TimeStamp);
      TofCalib->fetchPedestalFromFile("tofPedestal.txt",TofAddress);
      TofCalib->fetchTvcConvFromFile("tofTvcConv.txt",TofAddress);
      TofCalib->fetchQvcConvFromFile("tofQvcConv.txt",TofAddress);
      TofCalib->fetchSlewParFromFile("tofSlewPar.txt");
      TofCalib->fetchToffsetFromFile("tofToffset.txt");
      TofCalib->fetchYoffsetFromFile("tofYoffset.txt");
      TofCalib->fetchVelocityFromFile("tofVelocity.txt");
      TofCalib->fetchElossConvFromFile("tofElossConv.txt");
      TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");

      if(verbose>10) printf(" TOF CUT: charge > %f  |tvc-pede| > %f \n",
			    ChargeCut,TvcPedeCut);
      mTofRawRec->setCutParameter(ChargeCut,TvcPedeCut);
    }

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling mTofUnpack\n");
     mTofUnpack->event(topNode, TofAddress);

    if (verbose>10) printf("Calling mTofRawRec\n");
     mTofRawRec->event(topNode, TofAddress, TofGeometry, TofCalib);

    // Histogram Filling
    ntof = 0;
    for(itof = 0; itof < dTofReconstructed->RowCount(); itof++){
      tofhist1->Fill((Float_t) eventNumber);
      tofhist2->Fill((Float_t) dTofReconstructed->get_slatid(itof));
      tofhist3->Fill((Float_t) dTofReconstructed->get_tof(itof));
      tofhist4->Fill((Float_t) dTofReconstructed->get_eloss(itof));
      tofhist5->Fill((Float_t) dTofReconstructed->get_xtof(2,itof), 
		     (Float_t) dTofReconstructed->get_xtof(1,itof));
      tofhist0->Fill((Float_t) dTofReconstructed->get_xtof(0,itof), 
		     (Float_t) dTofReconstructed->get_xtof(1,itof));
      tofhisty->Fill((Float_t) dTofReconstructed->get_xtof(1,itof));
      tofhistz->Fill((Float_t) dTofReconstructed->get_xtof(2,itof));
      ntof++;

      //cout<<" slatid = "<<dTofReconstructed->get_slatid(itof)<<"\t";
      //cout<<"tof = "<<dTofReconstructed->get_tof(itof)<<"\t";
      //cout<<"eloss = "<<dTofReconstructed->get_eloss(itof)<<"\t";
      //cout<<"ypos = "<<dTofReconstructed->get_xtof(1,itof)<<endl;
    }

    // Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
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
  TCanvas *c = new TCanvas("c","TOF plot", 600, 800); 
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
  tofhist0->Draw();
}

