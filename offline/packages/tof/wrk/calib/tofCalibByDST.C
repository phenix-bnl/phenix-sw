//************************************************************
// Analysis macro written by Akio Kiyomichi
//************************************************************

void tofCalibByDST(Int_t maxEvents=2000, const char *dstIFile="dstData.root", const char *rootOFile="toftree.root") {

  Float_t getTofGlobalT( Int_t , const char *);
  Int_t minEvents=0;
  // Re Calculation Option  0:off, 1:on
  const Int_t bbcReCycle = 0;
  const Int_t tofReCycle = 1;
  const Int_t tofGlobalT = 1;

  Int_t verbose = 12;

  Int_t eventNumber = 0;

  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libEG.so"); 
  gSystem->Load("libemcCalib.so");
  gSystem->Load("libemc.so");
  //gSystem->Load("./libtof_tree.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libmom.so");
  gSystem->Load("libheader.so");
  gSystem->Load("liblv1.so"); // SCJ Add GL1 library for current build:

  // Specify the geometry flags and magnetic field flags for each dataset
  Int_t bFieldFlag = 1;  // 0 is off, 1 is on
  Int_t geomFlag = 1;    // 0 is retracted, 1 is standard

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString dstInFile = dstIFile;
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  dstIn->read(dstNode);

  // Set up the modules
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;
  BbcCalib* BbcCalibPar = new BbcCalib();
  TofEvent* TofEvent = new TofEvent();
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();
  TofCalibrator* TofCalibrator = new TofCalibrator();

  // Initialize the BBC Objects
  if(bbcReCycle){
    BbcCalibPar->restore("BbcCalib");
    PHIODataNode<TObject>* BbcCalibParNode 
      = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
    parNode->addNode(BbcCalibParNode);
  }
  // Initialize the TOF Objects
  if (verbose>10) printf("Calling TofAddress\n");
  //TofAddress->setTimeStamp(TimeStamp);
  TofAddress->fetchFromFile("toffemmap.txt");
  
  if (verbose>10) printf("Calling TofGeometry\n");
  //TofGeometry->setTimeStamp(TimeStamp);
  // Julia's geometry set-up (alignment)
  PHPoint wo(-41,0,0);
  PHPoint eo(44,0,0);
  PHVector Xaxis(1,0,0);
  PHVector Yaxis(0,1,0);
  PHFrame eastF(eo,Xaxis,Yaxis);
  PHFrame westF(wo,Xaxis,Yaxis);
  if (geomFlag == 0) TofGeometry->setEastCarriage(eastF);
  TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");

  if (verbose>10) printf("Calling TofCalib\n");
  //TofCalib->setTimeStamp(TimeStamp);
  TofCalib->fetchPedestalFromFile("tofPedestal.txt",TofAddress);
  TofCalib->fetchTvcConvFromFile("tofTvcConv.txt",TofAddress);
  TofCalib->fetchQvcConvFromFile("tofQvcConv.txt",TofAddress);
  TofCalib->fetchSlewParFromFile("tofSlewPar.txt");
  TofCalib->fetchToffsetFromFile("tofToffset.txt");
  TofCalib->fetchYoffsetFromFile("tofYoffset.txt");
  TofCalib->fetchVelocityFromFile("tofVelocity.txt");
  TofCalib->fetchElossConvFromFile("tofElossConv.txt");
  TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");

  // Book the DST checking histograms
  if (verbose>10) printf("Calling TofCalibrator Initialize\n");
  TofCalibrator->rootInit(rootOFile);
  TofCalibrator->setDebugLevel(2);
  TofCalibrator->cleanPass();
  TofCalibrator->setPass(1);
  TofCalibrator->booking(TofAddress);
  TofCalibrator->showPass();

  // Initialize the tables
  PHIODataNode<PHTable>* dBbcOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dBbcOut");
  PHIODataNode<PHTable>* dBbcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dBbcRaw");
  PHIODataNode<PHTable>* dZdcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dZdcRaw");
  PHIODataNode<PHTable>* dZdcOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dZdcOut");
  PHIODataNode<PHTable>* dMvddNdEtaOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvddNdEtaOut");
  PHIODataNode<PHTable>* dMvdMultOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvdMultOut");
  PHIODataNode<PHTable>* dMvdVertexOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvdVertexOut");
  PHIODataNode<PHTable>* dMvbRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvbRaw");
  PHIODataNode<PHTable>* dMvcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvcRaw");
  PHIODataNode<PHTable>* dDchHitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchHit");
  PHIODataNode<PHTable>* dDchTracksNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchTracks");
  PHIODataNode<PHTable>* dPc1RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc1Raw");
  PHIODataNode<PHTable>* dPc2RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc2Raw");
  PHIODataNode<PHTable>* dPc3RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc3Raw");
  PHIODataNode<PHTable>* dPc1ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc1Cluster");
  PHIODataNode<PHTable>* dPc2ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc2Cluster");
  PHIODataNode<PHTable>* dPc3ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc3Cluster");
  PHIODataNode<PHTable>* dCrkRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkRaw");
  PHIODataNode<PHTable>* dCrkHitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkHit");
  PHIODataNode<PHTable>* dCrkPidNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkPid");
  PHIODataNode<PHTable>* dTecTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecTrack");
  PHIODataNode<PHTable>* dTecCalibNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecCalib");
  PHIODataNode<PHTable>* dTecPIDNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecPID");
  PHIODataNode<PHTable>* dTofRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofRaw");
  PHIODataNode<PHTable>* dTofReconstructedNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofReconstructed");
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcCalibTower");
  PHIODataNode<PHTable>* dEmcClusterLocalNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcClusterLocal");
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcClusterLocalExt");
  PHIODataNode<PHTable>* dCglTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglTrack");
  PHIODataNode<PHTable>* dCglParticleNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglParticle");
  PHIODataNode<PHTable>* dCglPidNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglPid");
  PHIODataNode<PHTable>* dRunHeaderNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dRunHeader");
  PHIODataNode<PHTable>* dEventHeaderNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEventHeader");
  PHIODataNode<PHTable>* dPHTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPHTrack"); // new PHTrack

  PhRootHistogramFactory::buildFactory();

  // Time for the event loop
  eventNumber = 0;
  while (eventNumber < maxEvents) {
    eventNumber++;
    if (eventNumber > 1) {
      if(!(dstIn->read(dstNode)))break;
    }
    printf("Read event %d\n",eventNumber);

    if(eventNumber < minEvents) continue;

    // Grab the data nodes
    if (!dBbcOutNode) {
      cout << "Dst Read: Could not find data node dBbcOut" << endl;
    } else {
      dBbcOutWrapper* dBbcOut = (dBbcOutWrapper*)dBbcOutNode->getData();
    }
    if (!dBbcRawNode) {
      cout << "Dst Read: Could not find data node dBbcRaw" << endl;
    } else {
      dBbcRawWrapper* dBbcRaw = (dBbcRawWrapper*)dBbcRawNode->getData();
    }
    if (!dZdcRawNode) {
      cout << "Dst Read: Could not find data node dZdcRaw" << endl;
    } else {
      dZdcRawWrapper* dZdcRaw = (dZdcRawWrapper*)dZdcRawNode->getData();
    }
    if (!dZdcOutNode) {
      cout << "Dst Read: Could not find data node dZdcOut" << endl;
    } else {
      dZdcOutWrapper* dZdcOut = (dZdcOutWrapper*)dZdcOutNode->getData();
    }
    if (!dMvddNdEtaOutNode) {
      cout << "Dst Read: Could not find data node dMvddNdEtaOut" << endl;
    } else {
      dMvddNdEtaOutWrapper* dMvddNdEtaOut = (dMvddNdEtaOutWrapper*)dMvddNdEtaOutNode->getData();
    }
    if (!dMvdMultOutNode) {
      cout << "Dst Read: Could not find data node dMvdMultOut" << endl;
    } else {
      dMvdMultOutWrapper* dMvdMultOut = (dMvdMultOutWrapper*)dMvdMultOutNode->getData();
    }
    if (!dMvbRawNode) {
      cout << "Dst Read: Could not find data node dMvbRaw" << endl;
    } else {
      dMvbRawWrapper* dMvbRaw = (dMvbRawWrapper*)dMvbRawNode->getData();
    }
    if (!dMvcRawNode) {
      cout << "Dst Read: Could not find data node dMvcRaw" << endl;
    } else {
      dMvcRawWrapper* dMvcRaw = (dMvcRawWrapper*)dMvcRawNode->getData();
    }
    if (!dMvdVertexOutNode) {
      cout << "Dst Read: Could not find data node dMvdVertexOut" << endl;
    } else {
      dMvdVertexOutWrapper* dMvdVertexOut = (dMvdVertexOutWrapper*)dMvdVertexOut Node->getData();
    }
    if (!dDchHitNode) {
      cout << "Dst Read: Could not find data node dDchHit" << endl;
    } else {
      dDchHitWrapper* dDchHit = (dDchHitWrapper*)dDchHitNode->getData();
    }
    if (!dDchTracksNode) {
      cout << "Dst Read: Could not find data node dDchTracks" << endl;
    } else {
      dDchTracksWrapper* dDchTracks = (dDchTracksWrapper*)dDchTracksNode->getData();
    }
    if (!dPc1RawNode) {
      cout << "Dst Read: Could not find data node dPc1Raw" << endl;
    } else {
      dPadRawWrapper* dPc1Raw = (dPadRawWrapper*)dPc1RawNode->getData();
    }
    if (!dPc2RawNode) {
      cout << "Dst Read: Could not find data node dPc2Raw" << endl;
    } else {
      dPadRawWrapper* dPc2Raw = (dPadRawWrapper*)dPc2RawNode->getData();
    }
    if (!dPc3RawNode) {
      cout << "Dst Read: Could not find data node dPc3Raw" << endl;
    } else {
      dPadRawWrapper* dPc3Raw = (dPadRawWrapper*)dPc3RawNode->getData();
    }
    if (!dPc1ClusterNode) {
      cout << "Dst Read: Could not find data node dPc1Cluster" << endl;
    } else {
      dPadClusterWrapper* dPc1Cluster = (dPadClusterWrapper*)dPc1ClusterNode->getData();
    }
    if (!dPc2ClusterNode) {
      cout << "Dst Read: Could not find data node dPc2Cluster" << endl;
    } else {
      dPadClusterWrapper* dPc2Cluster = (dPadClusterWrapper*)dPc2ClusterNode->getData();
    }
    if (!dPc3ClusterNode) {
      cout << "Dst Read: Could not find data node dPc3Cluster" << endl;
    } else {
      dPadClusterWrapper* dPc3Cluster = (dPadClusterWrapper*)dPc3ClusterNode->getData();
    }
    if (!dCrkRawNode) {
      cout << "Dst Read: Could not find data node dCrkRaw" << endl;
    } else {
      dCrkRawWrapper* dCrkRaw = (dCrkRawWrapper*)dCrkRawNode->getData();
    }
    if (!dCrkHitNode) {
      cout << "Dst Read: Could not find data node dCrkHit" << endl;
    } else {
      dCrkHitWrapper* dCrkHit = (dCrkHitWrapper*)dCrkHitNode->getData();
    }
    if (!dCrkPidNode) {
      cout << "Dst Read: Could not find data node dCrkPid" << endl;
    } else {
      dCrkPidWrapper* dCrkPid = (dCrkPidWrapper*)dCrkPidNode->getData();
    }
    if (!dTecTrackNode) {
      cout << "Dst Read: Could not find data node dTecTrack" << endl;
    } else {
      dTecTrackWrapper* dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
    }
    if (!dTecCalibNode) {
      cout << "Dst Read: Could not find data node dTecCalib" << endl;
    } else {
      dTecCalibWrapper* dTecCalib = (dTecCalibWrapper*)dTecCalibNode->getData();
    }
    if (!dTecPIDNode) {
      cout << "Dst Read: Could not find data node dTecPID" << endl;
    } else {
      dTecPIDWrapper* dTecPID = (dTecPIDWrapper*)dTecPIDNode->getData();
    }
    if (!dTofRawNode) {
      cout << "Dst Read: Could not find data node dTofRaw" << endl;
    } else {
      dTofRawWrapper* dTofRaw = (dTofRawWrapper*)dTofRawNode->getData();
    }
    if (!dTofReconstructedNode) {
      cout << "Dst Read: Could not find data node dTofReconstructed" << endl;
    } else {
      dTofReconstructedWrapper* dTofReconstructed = (dTofReconstructedWrapper*)dTofReconstructedNode->getData();
    }
    if (!dEmcCalibTowerNode) {
      cout << "Dst Read: Could not find data node dEmcCalibTower" << endl;
    } else {
      dEmcCalibTowerWrapper* dEmcCalibTower = (dEmcCalibTowerWrapper*)dEmcCalibTowerNode->getData();
    }
    if (!dEmcClusterLocalNode) {
      cout << "Dst Read: Could not find data node dEmcClusterLocal" << endl;
    } else {
      dEmcClusterLocalWrapper* dEmcClusterLocal = (dEmcClusterLocalWrapper*)dEmcClusterLocalNode->getData();
    }
    if (!dEmcClusterLocalExtNode) {
      cout << "Dst Read: Could not find data node dEmcClusterLocalExt" << endl;
    } else {
      dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = (dEmcClusterLocalExtWrapper*)dEmcClusterLocalExtNode->getData();
    }
    if (!dCglTrackNode) {
      cout << "Dst Read: Could not find data node dCglTrack" << endl;
    } else {
      dCglTrackWrapper* dCglTrack = (dCglTrackWrapper*)dCglTrackNode->getData();
    }
    if (!dCglParticleNode) {
      cout << "Dst Read: Could not find data node dCglParticle" << endl;
    } else {
      dCglParticleWrapper* dCglParticle = (dCglParticleWrapper*)dCglParticleNode->getData();
    }
    if (!dCglPidNode) {
      cout << "Dst Read: Could not find data node dCglPid" << endl;
    } else {
      dCglPidWrapper* dCglPid = (dCglPidWrapper*)dCglPidNode->getData();
    }
    if (!dRunHeaderNode) {
      cout << "Dst Read: Could not find data node dRunHeader" << endl;
    } else {
      dRunHeaderWrapper* dRunHeader = (dRunHeaderWrapper*)dRunHeaderNode->getData();
    }
    if (!dEventHeaderNode) {
      cout << "Dst Read: Could not find data node dEventHeader" << endl;
    } else {
      dEventHeaderWrapper* dEventHeader = (dEventHeaderWrapper*)dEventHeaderNode->getData();
    }
    if (!dPHTrackNode) {
      cout << "Dst Read: Could not find data node dPHTrack" << endl;
    } else {
      dPHTrackWrapper* dPHTrack = (dPHTrackWrapper*)dPHTrackNode->getData();
    }

    // Set Global T
    if(eventNumber == 1&&tofGlobalT){
      int run = dRunHeader->get_run(0);
      float globalT = getTofGlobalT(run, "tofGlobalT_rundep.txt");
      cout<<" run = "<<run<<"  globalT = "<<globalT<<endl;
      TofCalib->setGlobalT(globalT);
    }
    // Check DST Contents
    if(verbose>15){
      cout<<"  BBC: t0 = "<<dBbcOut->get_TimeZero(0);
      cout<<"  z0 = "<<dBbcOut->get_VertexPoint(0)<<endl;
      cout<<"  TOF: "<<dTofReconstructed->RowCount();
      for(int itmp = 0; itmp < dTofReconstructed->RowCount();itmp++){
	cout<<" t:"<<dTofReconstructed->get_tof(itmp);
	//cout<<" x:"<<dTofReconstructed->get_xtof(0,itmp);
	cout<<" y:"<<dTofReconstructed->get_xtof(1,itmp);
	if(itmp > 3) break;
      }
      cout<<endl;
    }
    // Run BBC reconstruction module
    if(bbcReCycle){
      mBbcRawOut->event(topNode);
    }
    // Run TOF reconstruction module
    if(tofReCycle){
      TofEvent->DstReCycle(topNode,TofAddress,TofGeometry,TofCalib);
    }

    // Check DST Contents after Re-Calculation
    if(verbose>15&&(bbcReCycle||tofReCycle)){
      cout<<"  BBC: t0 = "<<dBbcOut->get_TimeZero(0);
      cout<<"  z0 = "<<dBbcOut->get_VertexPoint(0)<<endl;
      cout<<"  TOF: "<<dTofReconstructed->RowCount();
      for(int itmp = 0; itmp < dTofReconstructed->RowCount();itmp++){
	cout<<" t:"<<dTofReconstructed->get_tof(itmp);
	//cout<<" x:"<<dTofReconstructed->get_xtof(0,itmp);
	cout<<" y:"<<dTofReconstructed->get_xtof(1,itmp);
	if(itmp > 3) break;
      }
      cout<<endl;
    }

    // Fill the DST checking histograms
    if (verbose>15) printf("Calling TofCalibrator\n");
    TofCalibrator->filling(topNode, TofAddress, TofGeometry, TofCalib);
  }
  // end event loop
  if (verbose>10) printf("Calling TofCalibrator->rootSave()\n");
  TofCalibrator->rootSave();
}

Float_t getTofGlobalT(Int_t run=0,const char *txtFile="tofGlobalT_rundep.txt"){

  const int runset = 500;
  int runid = 0, runsetid, run0[runset];
  float globalT = 0.0, globalT0[runset];
  ifstream inputfile; 
  inputfile.open(txtFile);
  if(!inputfile){ 
    cerr << "  Can not open "<< txtFile <<" file." << endl;
    cerr << endl;
  }else{ 
    while(!inputfile.eof()){
      inputfile >> run0[runid] >> globalT0[runid];
      runid++;
    }
    // final run#
    run0[runid] = 12470;  // for Year-1 only
    globalT0[runid] = 0;
    runid++;
  }
  
  for(Int_t i = 0; i < runid-1;,i++){
    if(run >= run0[i] && run < run0[i+1]){
      globalT  = globalT0[i];
      runsetid = run0[i];
      break;
    }
  }
  cout<<" run = "<<run<<"  runset = "<<runsetid<<" GlobalT : "<<globalT<<endl;
  return globalT;
}
