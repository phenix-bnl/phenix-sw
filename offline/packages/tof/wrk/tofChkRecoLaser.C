//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void tofChkRecoLaser(const Int_t minEvents=0, const Int_t maxEvents=1, const char *prdfIFile="phnx.prdf") {

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
  gSystem->Load("libbbc.so");

  Int_t verbose = 7;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
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
  BbcEvent* mBbcEvent = new BbcEvent();
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;

  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();
  mTofUnpackModule* mTofUnpack = new mTofUnpackModule;
  mTofRawRecModule* mTofRawRec = new mTofRawRecModule;

  // Initialize the tables
  // TOF
  size_t mr=1000;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
  dstNode->addNode(dTofRawNode);

  size_t mr=960;
  dTofReconstructedWrapper* dTofReconstructed = new dTofReconstructedWrapper("dTofReconstructed",mr);
  PHIODataNode<PHTable>* dTofReconstructedNode = new PHIODataNode<PHTable>(dTofReconstructed,"dTofReconstructed");
  dstNode->addNode(dTofReconstructedNode);

  size_t mr=1000;
  dTofRawRecWrapper* dTofRawRec = new dTofRawRecWrapper("dTofRawRec",mr);
  PHIODataNode<PHTable>* dTofRawRecNode = new PHIODataNode<PHTable>(dTofRawRec,"dTofRawRec");
  tofNode->addNode(dTofRawRecNode);

  size_t mr=1;
  dTofRawRecParWrapper* dTofRawRecPar = new dTofRawRecParWrapper("dTofRawRecPar",mr);
  PHIODataNode<PHTable>* dTofRawRecParNode = new PHIODataNode<PHTable>(dTofRawRecPar,"dTofRawRecPar");
  parNode->addNode(dTofRawRecParNode);

  PHIODataNode<PHTable>* dTofDCMParNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofDCMPar");
  if (!dTofDCMParNode) {
    cout << "Par Read: Could not find data node dTofDCMPar" << endl;
  } else {
    dTofDCMParWrapper* dTofDCMPar = (dTofDCMParWrapper*)dTofDCMParNode->getData();
  }
  // BBC
  size_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  dstNode->addNode(dBbcRawNode);

  size_t mr=1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar",mr);
  PHIODataNode<PHTable>* dBbcRawHitParNode = new PHIODataNode<PHTable>(dBbcRawHitPar,"dBbcRawHitPar");
  parNode->addNode(dBbcRawHitParNode);

  size_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  PHIODataNode<TObject>* BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  BbcCalibPar->restore("BbcCalib");  // Additions for BBC

  size_t mr=1;
  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM",mr);
  PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  dcmNode->addNode(dBbcDCMNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  PhRootHistogramFactory::buildFactory();

  // Parameter set-up
  // Setting dTofRawRecPar Parameters
  size_t nrc=1;
  dTofRawRecPar->SetRowCount(nrc);
  dTofRawRecPar->set_verbose(0,0);
  // Setting dBbcGeo Parameters
  size_t nrc=1;
  dBbcGeo->SetRowCount(nrc);
  dBbcGeo->set_MaxPmtNo(0,128);
  // Setting dBbcRawHitPar Parameters
  size_t nrc=1;
  dBbcRawHitPar->SetRowCount(nrc);
  dBbcRawHitPar->set_MinAdc(0,0);
  dBbcRawHitPar->set_MaxAdc(0,4096);
  dBbcRawHitPar->set_MinTdc0(0,10);
  dBbcRawHitPar->set_MaxTdc0(0,4000);
  dBbcRawHitPar->set_MinTdc1(0,10);
  dBbcRawHitPar->set_MaxTdc1(0,4000);

  mainIter.cd();

  // Histogram Setting
  gROOT->cd();
  Int_t itof,ntof;
  TH1F *tofhist1 = new TH1F("tofhist1","TOF hits/event",
			    maxEvents+2,0.0,(Float_t)maxEvents+2);
  TH1F *tofhist2 = new TH1F("tofhist2","TOF hits/slatid",1000,0.0,1000.0);
  //TH1F *tofhist3 = new TH1F("tofhist3","TOF time-of-flight",800,0.0,80.0);
  TH1F *tofhist3 = new TH1F("tofhist3","TOF time-of-flight",480,22.0,34.0);
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
      if (verbose>10) printf("Calling mBbcSetGeo\n");
      mBbcSetGeo->event(topNode);

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

    }
    if(eventNumber < minEvents) continue;

    if (verbose>10) printf("Calling event modules\n");
    // Trigger Info.
    int trigger = 0;
    Packet *pGL1;
    if((pGL1 = thisEvent->getPacket(14001)) != 0){
      trigger = pGL1->iValue(0,"SCALEDTRIG");
    }
    delete pGL1;

    // BBC
    if (verbose>10) printf("Calling BbcGetDCM\n");
    BbcGetDCM(topNode);
    if (verbose>10) printf("Calling mBbcUnpack\n");
    mBbcUnpack->event(topNode);
    if (verbose>10) printf("Calling mBbcRawOut\n");
    mBbcRawOut->event(topNode);
    Float_t t0bbc = dBbcOut->get_TimeZero(0);
    Float_t z0bbc = dBbcOut->get_VertexPoint(0);

    // TOF
    if (verbose>10) printf("Calling mTofUnpack\n");
    mTofUnpack->event(topNode, TofAddress);

    if (verbose>10) printf("Calling mTofRawRec\n");
    //mTofRawRec->event(topNode, TofAddress, TofGeometry, TofCalib);
    if(trigger == 0x40000000){
      cout<<" This is Laser Event !!!"<<endl;
      mTofRawRec->event(topNode, TofAddress, TofGeometry, TofCalib);
    } else {
      //mTofRawRec->event(topNode, TofAddress, TofGeometry, TofCalib);
    }
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
    tofhist1->Fill((Float_t) ntof);

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
    if (mainIter.cd("BBC")) {
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

