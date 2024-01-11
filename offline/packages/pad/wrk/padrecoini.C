//************************************************************
// Initialization macro written by the Pyrite GUI for PHOOL   
//************************************************************

{

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
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libpad_tables.so");
  gSystem->Load("libpad.so");

  Int_t verbose = 12;
  Int_t runNumber = 0;
  Int_t runDate = 0;
  Int_t runTime = 0;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* doNode = new PHCompositeNode("DO");
  topNode->addNode(doNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* padNode = new PHCompositeNode("PAD");
  topNode->addNode(padNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

  PHString parInFile = parIFile;
  PHNodeIOManager *parIn = new PHNodeIOManager(parInFile,PHReadOnly);
  parIn->read(parNode);

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  PHBoolean padStatCh;
  PHBoolean padStatROC;
  PadCalibrationObject* PadCalibration = new PadCalibrationObject();
  PHpadDetectorGeo* mPadDetGeo = new PHpadDetectorGeo();
  mPadUnpackModule* mPc1Unpack = new mPadUnpackModule;
  mPadUnpackModule* mPc2Unpack = new mPadUnpackModule;
  mPadUnpackModule* mPc3Unpack = new mPadUnpackModule;
  PHBoolean padInclStat;
  padInclBad* padInclBad = new padInclBad();
  mPadRecModule* mPc1Rec = new mPadRecModule;
  mPadRecModule* mPc2Rec = new mPadRecModule;
  mPadRecModule* mPc3Rec = new mPadRecModule;

  // Initialize the tables
  PHIODataNode<PHTable>* dPad23ParNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPad23Par");
  if (!dPad23ParNode) {
    cout << "Par Read: Could not find data node dPad23Par" << endl;
  } else {
    dPad23ParWrapper* dPad23Par = (dPad23ParWrapper*)dPad23ParNode->getData();
  }

  PHIODataNode<PHTable>* dPadGeomNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPadGeom");
  if (!dPadGeomNode) {
    cout << "Par Read: Could not find data node dPadGeom" << endl;
  } else {
    dPadGeomWrapper* dPadGeom = (dPadGeomWrapper*)dPadGeomNode->getData();
  }

  size_t mr=1;
  dPadRecParWrapper* dPadRecPar = new dPadRecParWrapper("dPadRecPar",mr);
  PHIODataNode<PHTable>* dPadRecParNode = new PHIODataNode<PHTable>(dPadRecPar,"dPadRecPar");
  parNode->addNode(dPadRecParNode);

  PHIODataNode<PHTable>* dPadFEMParNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPadFEMPar");
  if (!dPadFEMParNode) {
    cout << "Par Read: Could not find data node dPadFEMPar" << endl;
  } else {
    dPadFEMParWrapper* dPadFEMPar = (dPadFEMParWrapper*)dPadFEMParNode->getData();
  }

  size_t mr=6000;
  dPadRawWrapper* dPc1Raw = new dPadRawWrapper("dPc1Raw",mr);
  PHIODataNode<PHTable>* dPc1RawNode = new PHIODataNode<PHTable>(dPc1Raw,"dPc1Raw");
  padNode->addNode(dPc1RawNode);

  size_t mr=6000;
  dPadRawWrapper* dPc2Raw = new dPadRawWrapper("dPc2Raw",mr);
  PHIODataNode<PHTable>* dPc2RawNode = new PHIODataNode<PHTable>(dPc2Raw,"dPc2Raw");
  padNode->addNode(dPc2RawNode);

  size_t mr=6000;
  dPadRawWrapper* dPc3Raw = new dPadRawWrapper("dPc3Raw",mr);
  PHIODataNode<PHTable>* dPc3RawNode = new PHIODataNode<PHTable>(dPc3Raw,"dPc3Raw");
  padNode->addNode(dPc3RawNode);

  size_t mr=1500;
  dPadClusterWrapper* dPc1Cluster = new dPadClusterWrapper("dPc1Cluster",mr);
  PHIODataNode<PHTable>* dPc1ClusterNode = new PHIODataNode<PHTable>(dPc1Cluster,"dPc1Cluster");
  dstNode->addNode(dPc1ClusterNode);

  size_t mr=1500;
  dPadClusterWrapper* dPc2Cluster = new dPadClusterWrapper("dPc2Cluster",mr);
  PHIODataNode<PHTable>* dPc2ClusterNode = new PHIODataNode<PHTable>(dPc2Cluster,"dPc2Cluster");
  dstNode->addNode(dPc2ClusterNode);

  size_t mr=1500;
  dPadClusterWrapper* dPc3Cluster = new dPadClusterWrapper("dPc3Cluster",mr);
  PHIODataNode<PHTable>* dPc3ClusterNode = new PHIODataNode<PHTable>(dPc3Cluster,"dPc3Cluster");
  dstNode->addNode(dPc3ClusterNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc1GhitRaw = new dPadGhitRawWrapper("dPc1GhitRaw",mr);
  PHIODataNode<PHTable>* dPc1GhitRawNode = new PHIODataNode<PHTable>(dPc1GhitRaw,"dPc1GhitRaw");
  evaNode->addNode(dPc1GhitRawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc2GhitRaw = new dPadGhitRawWrapper("dPc2GhitRaw",mr);
  PHIODataNode<PHTable>* dPc2GhitRawNode = new PHIODataNode<PHTable>(dPc2GhitRaw,"dPc2GhitRaw");
  evaNode->addNode(dPc2GhitRawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc3GhitRaw = new dPadGhitRawWrapper("dPc3GhitRaw",mr);
  PHIODataNode<PHTable>* dPc3GhitRawNode = new PHIODataNode<PHTable>(dPc3GhitRaw,"dPc3GhitRaw");
  evaNode->addNode(dPc3GhitRawNode);

  size_t mr=3000;
  dPadRawClusWrapper* dPc1RawClus = new dPadRawClusWrapper("dPc1RawClus",mr);
  PHIODataNode<PHTable>* dPc1RawClusNode = new PHIODataNode<PHTable>(dPc1RawClus,"dPc1RawClus");
  padNode->addNode(dPc1RawClusNode);

  size_t mr=3000;
  dPadRawClusWrapper* dPc2RawClus = new dPadRawClusWrapper("dPc2RawClus",mr);
  PHIODataNode<PHTable>* dPc2RawClusNode = new PHIODataNode<PHTable>(dPc2RawClus,"dPc2RawClus");
  padNode->addNode(dPc2RawClusNode);

  size_t mr=3000;
  dPadRawClusWrapper* dPc3RawClus = new dPadRawClusWrapper("dPc3RawClus",mr);
  PHIODataNode<PHTable>* dPc3RawClusNode = new PHIODataNode<PHTable>(dPc3RawClus,"dPc3RawClus");
  padNode->addNode(dPc3RawClusNode);

  size_t mr=64;
  dPadDCMWrapper* dPc1DCM = new dPadDCMWrapper("dPc1DCM",mr);
  PHIODataNode<PHTable>* dPc1DCMNode = new PHIODataNode<PHTable>(dPc1DCM,"dPc1DCM");
  dcmNode->addNode(dPc1DCMNode);

  size_t mr=64;
  dPadDCMWrapper* dPc2DCM = new dPadDCMWrapper("dPc2DCM",mr);
  PHIODataNode<PHTable>* dPc2DCMNode = new PHIODataNode<PHTable>(dPc2DCM,"dPc2DCM");
  dcmNode->addNode(dPc2DCMNode);

  size_t mr=64;
  dPadDCMWrapper* dPc3DCM = new dPadDCMWrapper("dPc3DCM",mr);
  PHIODataNode<PHTable>* dPc3DCMNode = new PHIODataNode<PHTable>(dPc3DCM,"dPc3DCM");
  dcmNode->addNode(dPc3DCMNode);

  size_t mr=18000;
  dPadNibbleGhitWrapper* dPc1NibbleGhit = new dPadNibbleGhitWrapper("dPc1NibbleGhit",mr);
  PHIODataNode<PHTable>* dPc1NibbleGhitNode = new PHIODataNode<PHTable>(dPc1NibbleGhit,"dPc1NibbleGhit");
  evaNode->addNode(dPc1NibbleGhitNode);

  size_t mr=18000;
  dPadNibbleGhitWrapper* dPc2NibbleGhit = new dPadNibbleGhitWrapper("dPc2NibbleGhit",mr);
  PHIODataNode<PHTable>* dPc2NibbleGhitNode = new PHIODataNode<PHTable>(dPc2NibbleGhit,"dPc2NibbleGhit");
  evaNode->addNode(dPc2NibbleGhitNode);

  size_t mr=18000;
  dPadNibbleGhitWrapper* dPc3NibbleGhit = new dPadNibbleGhitWrapper("dPc3NibbleGhit",mr);
  PHIODataNode<PHTable>* dPc3NibbleGhitNode = new PHIODataNode<PHTable>(dPc3NibbleGhit,"dPc3NibbleGhit");
  evaNode->addNode(dPc3NibbleGhitNode);

  PHIODataNode<TObject>* mPadDetGeoNode = new PHIODataNode<TObject>(mPadDetGeo,"mPadDetGeo");
  parNode->addNode(mPadDetGeoNode);


  PhRootHistogramFactory::buildFactory();

}

