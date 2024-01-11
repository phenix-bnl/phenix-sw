//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void tofCalibByPRDF(const Int_t minEvents=0, const Int_t maxEvents=1, const Int_t mode=0, const char *prdfIFile="phnx.prdf", const char *calibFile="calib.root",  const char *dstOFile="miniprdf.root", const Float_t ChargeCut = -200, const Float_t TvcPedeCut = 10){

  // Note:  mode parameter goes as follows
  //   0 = standard geometry, B=0
  //   1 = standard geometry, B on
  //   2 = retracted geometry, B=0
  //   3 = retracted geometry, B on

  Int_t eventNumber = 0;

  Int_t verbose = 7;

  // Executing initialization and parameter macros
  //gROOT->Macro("tofrecoini.C");
  //gROOT->Macro("tofrecopar.C");

  // Initialization
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libphgeo.so");

  // Loading subsystem libraries
  //gSystem->Load("libtof_calib.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");

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
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  PHNodeReset reset;

  // Set up input and output files
  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

  cout<<" Open PRDF file "<<prdfIFile<<endl;

  PHString dstOutFile = dstOFile;
  PHNodeIOManager *dstOut = new PHNodeIOManager(dstOutFile,PHWrite);

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  BbcEvent* bbcevent = new BbcEvent();

  ZdcEvent* zdcevent = new ZdcEvent();

  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();
  mTofUnpackModule* mTofUnpack = new mTofUnpackModule;
  TofEvtHeader* TofEvtHeader = new TofEvtHeader();
  TofCalibrator* TofCalibrator = new TofCalibrator();

  // Initialize the tables
  // TOF

  size_t mr=960;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
  dstNode->addNode(dTofRawNode);
  tofNode->addNode(dTofRawNode);  // create mini-PRDF

  size_t mr=1;
  dTofEvtHeaderWrapper* dTofEvtHeader = new dTofEvtHeaderWrapper("dTofEvtHeader",mr);
  PHIODataNode<PHTable>* dTofEvtHeaderNode = new PHIODataNode<PHTable>(dTofEvtHeader,"dTofEvtHeader");
  tofNode->addNode(dTofEvtHeaderNode);

  // BBC
  size_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  dstNode->addNode(dBbcRawNode);
  tofNode->addNode(dBbcRawNode);   // create mini-PRDF

  BbcCalib* BbcCalibPar = new BbcCalib();
  //BbcCalibPar->restore();
  BbcCalibPar->restore("BbcCalib");  // Bbc ascii file name
  PHIODataNode<TObject>* BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);
  tofNode->addNode(dBbcOutNode);  // create mini-PRDF

  // ZDC
  size_t mr=8;
  dZdcRawWrapper*  dZdcRaw = new dZdcRawWrapper("dZdcRaw",mr);
  PHIODataNode<PHTable>* dZdcRawNode = new PHIODataNode<PHTable>(dZdcRaw,"dZdcRaw");
  dstNode->addNode(dZdcRawNode);
  tofNode->addNode(dZdcRawNode);  // create mini-PRDF

  size_t mr=1;
  dZdcDCMWrapper*  dZdcDCM = new dZdcDCMWrapper("dZdcDCM",mr);
  PHIODataNode<PHTable>* dZdcDCMNode = new PHIODataNode<PHTable>(dZdcDCM,"dZdcDCM");
  dcmNode->addNode(dZdcDCMNode);

  size_t mr=1;
  dZdcOutWrapper*  dZdcOut = new dZdcOutWrapper("dZdcOut",mr);
  PHIODataNode<PHTable>* dZdcOutNode = new PHIODataNode<PHTable>(dZdcOut,"dZdcOut");
  dstNode->addNode(dZdcOutNode);
  tofNode->addNode(dZdcOutNode);  // create mini-PRDF

  size_t mr=1;
  ZdcCalib* ZdcCalibPar = new ZdcCalib();
  ZdcCalibPar->restore();
  PHIODataNode<TObject>*  ZdcCalibParNode = new PHIODataNode<TObject>(ZdcCalibPar,"ZdcCalibPar");
  parNode->addNode(ZdcCalibParNode);

  mainIter.cd();

  gROOT->cd();

  Int_t runNumber, runDate, runTime, evtSequence;

  if (verbose>5) printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

    evtSequence = thisEvent->getEvtSequence();
    
    // Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    if (eventNumber == 1) {
      runNumber = thisEvent->getRunNumber();
      runDate = thisEvent->getDate();
      runTime = thisEvent->getTime();
      cout<<" Run # "<<runNumber;
      cout<<"  Date: "<<runDate;
      cout<<"  Time: "<<runTime<<endl;
      
      if (verbose>10) printf("Calling first event only modules.\n");
      // Histogram Setting
      char title[80];
      sprintf(title,"TOF [run %d]",runNumber);
      TH1F *tofhist1 = new TH1F("tofhist1",title, maxEvents - minEvents + 4,
				(Float_t)minEvents-2,(Float_t)maxEvents+2);
      TH1F *tofhist3 = new TH1F("tofhist3","TOF hits",40,0.0,200.0);

      if (verbose>10) printf("Calling Bbc setCalibDataAll\n");
      bbcevent->setCalibDataAll(BbcCalibPar);

      if (verbose>10) printf("Calling Zdc setCalibDataAll\n");
      zdcevent->setCalibDataAll(ZdcCalibPar);  

      if (verbose>10) printf("Calling TofAddress\n");
      TofAddress->setTimeStamp(TimeStamp);
      TofAddress->fetchFromFile("toffemmap.txt","tofcablemap.txt");
      
      if (verbose>10) printf("Calling TofGeometry\n");
      TofGeometry->setTimeStamp(TimeStamp);
      if (mode==0 || mode==1) TofGeometry->setEastCarriage(0.0, 0.0, 0.0);
      if (mode==2 || mode==3) TofGeometry->setEastCarriage(-44.0, 0.0, 0.0);
      TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");
      
      if (verbose>10) printf("Calling TofCalib\n");
      TofCalib->setTimeStamp(TimeStamp);
      TofCalib->fetchPedestalFromFile("tofPedestal.txt", TofAddress);
      TofCalib->fetchTvcConvFromFile("tofTvcConv.txt", TofAddress);
      TofCalib->fetchQvcConvFromFile("tofQvcConv.txt", TofAddress);
      TofCalib->fetchVelocityFromFile("tofVelocity.txt");
  
      //TofCalib->fetchToffsetFromFile("tmpToffset.txt");
      //TofCalib->fetchYoffsetFromFile("tmpYoffset.txt");
      
      if (verbose>10) printf("Calling TofEvtHeader\n");
      TofEvtHeader->setDebugLevel(0);
  
      if (verbose>10) printf("Calling TofCalibrator\n");
      TofCalibrator->setDebugLevel(1);
      TofCalibrator->rootInit(calibFile);
      TofCalibrator->roughCalibInit(TofAddress);
      TofCalibrator->setCutParameter(ChargeCut, TvcPedeCut);
      TofCalibrator->setT0BbcCut(-200,1000);
      TofCalibrator->setT0BbcCut(-40,40);
    }
    if(eventNumber < minEvents) continue;
 
    if (verbose>5) printf("\n Fetched event %d\n",eventNumber);
    cout<<" Run # "<<runNumber;
    cout<<" Event # "<<evtSequence<<endl;

    if (verbose>10) printf("Calling event modules\n");

    // GL1 info.
    int trigger = 0;
    Packet *pGL1;
    if((pGL1 = thisEvent->getPacket(14001)) != 0){
      trigger = pGL1->iValue(0,"SCALEDTRIG");
      cout.setf(ios::showbase);
      cout<<" GL1 Trigger:";
      cout.setf(ios::hex, ios::basefield);
      cout<<"   scaled "<< pGL1->iValue(0,"SCALEDTRIG");
      cout<<"   raw "<<    pGL1->iValue(0,"RAWTRIG");
      cout<<"   live "<<   pGL1->iValue(0,"LIVETRIG");
      cout<<endl;
      cout.setf(ios::dec, ios::basefield);
    }
    delete pGL1;
    cout.setf(ios::dec, ios::basefield);

    // BBC
    if (verbose>10) printf("Calling BbcEvent\n");
    bbcevent->setRawData(topNode);
    bbcevent->calculate();
    bbcevent->DstStore(topNode);

    Float_t t0bbc = dBbcOut->get_TimeZero(0);
    Float_t z0bbc = dBbcOut->get_VertexPoint(0);

    //dBbcRaw->Show();
    //cout<<" dBbcOut: T0 = "<<dBbcOut->get_TimeZero(0);
    //cout<<"  Z0 = "<<dBbcOut->get_VertexPoint(0)<<endl;

    // ZDC
    if (verbose>10) printf("Calling ZdcGetDCM\n");
    ZdcGetDCM(topNode);
    if (verbose>10) printf("Calling ZdcEvent\n");
    zdcevent->Clear();
    zdcevent->DcmToRaw(topNode);
    zdcevent->PutEndProduct(topNode);

    //dZdcRaw->Show();
    //cout<<" dZdcOut: T0 = "<<dZdcOut->get_Timing(0,0);
    //cout<<"  "<<dZdcOut->get_Timing(1,0);
    //cout<<"  Z0 = "<<dZdcOut->get_Zvertex(0);
    //cout<<"  Energy = "<<dZdcOut->get_Energy(0,0);
    //cout<<"  "<<dZdcOut->get_Energy(1,0)<<endl;

    // TOF
    if (verbose>10) printf("Calling mTofUnpack\n");
    mTofUnpack->event(topNode, TofAddress);

    if (verbose>10) printf("Calling TofEvtHeader\n");
    TofEvtHeader->event(topNode);

    if (verbose>10) printf("Calling TofCalibrator\n");
    TofCalibrator->setRunNum(runNumber);
    TofCalibrator->setEvtSeq(evtSequence);

    if(trigger == 0x40000000){
      cout<<" This is Laser Event !!!"<<endl;
      TofCalibrator->laserCalib(topNode, TofAddress, TofGeometry ,TofCalib);
      //TofCalibrator->snapShots(30, topNode,TofAddress,TofGeometry,TofCalib);
    } elseif(trigger == 0x0) {
      cout<<" Missing Trigger bit !!!"<<endl;
      cout<<" dBbcOut: T0 = "<<dBbcOut->get_TimeZero(0);
      cout<<"  Z0 = "<<dBbcOut->get_VertexPoint(0)<<endl;
    } else {
      TofCalibrator->roughCalib(topNode, TofAddress, TofGeometry, TofCalib);
      //TofCalibrator->snapShots(30, topNode,TofAddress,TofGeometry,TofCalib);
    }

    // Write miniprdf file
    //dstOut->write(tofNode);

    tofhist1->Fill(eventNumber, (Float_t)TofCalibrator->getNhits());
    tofhist3->Fill((Float_t)TofCalibrator->getNhits());

    // Reset all data for this event
    mainIter.cd();
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
    if (mainIter.cd("ZDC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }
  TofCalibrator->rootSave();
  //TofCalibrator->fitting(TofCalib);

  TCanvas *c = new TCanvas("c","TOF Plot",600,400); 
  c->Divide(1,2);
  c->cd(1);
  tofhist1->SetXTitle("Event ID");
  tofhist1->Draw();
  c->cd(2);
  tofhist3->SetXTitle("Number of hits");
  tofhist3->Draw();

  tofNode->print();
}
