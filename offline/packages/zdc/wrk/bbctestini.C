//************************************************************
// Test initialization macro for PRDF write (Bbc only version)  
//************************************************************

{  

  // Loading subsystem libraries
  gSystem->Load("libbbc_tables.so");
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc_tables.so");
  gSystem->Load("libzdc.so");

  // Set up the node tree to have BBC tables
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);

  // Set up the modules
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcSetUcalModule* mBbcSetUcal = new mBbcSetUcalModule;
  mBbcGhitRawModule* mBbcGhitRaw = new mBbcGhitRawModule;
  mBbcFEMModule* mBbcFEM = new mBbcFEMModule;
  mBbcDCMModule* mBbcDCM = new mBbcDCMModule;

  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;  // event reconstruction

  ZdcEvent* mZdcEvent = new ZdcEvent();

  //
  // Set up BBC tables (names to match STAF internal names)
  // NOTE: cannot have duplicated names in ROOT (e.g. geo used in both BBC and ZDC)
  //

  size_t mr=60000;
  bbcghitWrapper* bbcghit = new bbcghitWrapper("bbcghit",mr);
  PHIODataNode<PHTable>* bbcghitNode = new PHIODataNode<PHTable>(bbcghit,"bbcghit");
  geaNode->addNode(bbcghitNode);
  
  size_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);
  
  size_t mr=128;
  dBbcUcalWrapper* dBbcUcal = new dBbcUcalWrapper("dBbcUcal",mr);
  PHIODataNode<PHTable>* dBbcUcalNode = new PHIODataNode<PHTable>(dBbcUcal,"dBbcUcal");
  bbcNode->addNode(dBbcUcalNode);

  size_t mr=1;
  dBbcGhitRawParWrapper* dBbcGhitRawPar = new dBbcGhitRawParWrapper("dBbcGhitRawPar",mr);
  PHIODataNode<PHTable>* dBbcGhitRawParNode = new PHIODataNode<PHTable>(dBbcGhitRawPar,"dBbcGhitRawPar");
  parNode->addNode(dBbcGhitRawParNode);

  size_t mr=10000;
  dBbcGhitRawWrapper* dBbcGhitRaw = new dBbcGhitRawWrapper("dBbcGhitRaw",mr);
  PHIODataNode<PHTable>* dBbcGhitRawNode = new PHIODataNode<PHTable>(dBbcGhitRaw,"dBbcGhitRaw");
  evaNode->addNode(dBbcGhitRawNode);

  size_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  bbcNode->addNode(dBbcRawNode);

  size_t mr=1;
  dBbcFEMWrapper* dBbcFEM = new dBbcFEMWrapper("dBbcFEM",mr);
  PHIODataNode<PHTable>* dBbcFEMNode = new PHIODataNode<PHTable>(dBbcFEM,"dBbcFEM");
  bbcNode->addNode(dBbcFEMNode);

  size_t mr=1;
  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM",mr);
  PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  dcmNode->addNode(dBbcDCMNode);

  //
  // tables for reconstruction test
  //

  size_t mr=1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar",mr);
  PHIODataNode<PHTable>* dBbcRawHitParNode = new PHIODataNode<PHTable>(dBbcRawHitPar,"dBbcRawHitPar");
  parNode->addNode(dBbcRawHitParNode);

  size_t mr=128;
  dBbcCalWrapper* dBbcCal = new dBbcCalWrapper("dBbcCal",mr);
  PHIODataNode<PHTable>* dBbcCalNode = new PHIODataNode<PHTable>(dBbcCal,"dBbcCal");
  parNode->addNode(dBbcCalNode);

  size_t mr=1;
  BbcCalib* BbcCalibPar = new BbcCalib();
  BbcCalibPar->restore("BbcCalib");
  PHIODataNode<TObject>*  BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  size_t mr=8;
  dZdcRawWrapper*  dZdcRaw = new dZdcRawWrapper("dZdcRaw",mr);
  PHIODataNode<PHTable>* dZdcRawNode = new PHIODataNode<PHTable>(dZdcRaw,"dZdcRaw");
  zdcNode->addNode(dZdcRawNode);

  size_t mr=1;
  dZdcDCMWrapper*  dZdcDCM = new dZdcDCMWrapper("dZdcDCM",mr);
  PHIODataNode<PHTable>* dZdcDCMNode = new PHIODataNode<PHTable>(dZdcDCM,"dZdcDCM");
  dcmNode->addNode(dZdcDCMNode);

  size_t mr=1;
  dZdcOutWrapper*  dZdcOut = new dZdcOutWrapper("dZdcOut",mr);
  PHIODataNode<PHTable>* dZdcOutNode = new PHIODataNode<PHTable>(dZdcOut,"dZdcOut");
  dstNode->addNode(dZdcOutNode);

  size_t mr=1;
  ZdcCalib* ZdcCalibPar = new ZdcCalib();
  ZdcCalibPar->restore("ZdcCalib");
  PHIODataNode<TObject>*  ZdcCalibParNode = new PHIODataNode<TObject>(ZdcCalibPar,"ZdcCalibPar");
  parNode->addNode(ZdcCalibParNode);

  mZdcEvent->setCalibDataAll(ZdcCalibPar);  
} 
