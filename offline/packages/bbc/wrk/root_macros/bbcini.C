//************************************************************
// Initialization macro written by the Pyrite GUI for PHOOL   
//************************************************************

{

  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPdbCal.so");

  // Loading subsystem libraries
  gSystem->Load("libbbc_tables.so");
  gSystem->Load("libbbc.so");

  Int_t verbose = 100;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHNodeIterator mainIter(topNode);

  // Set up the modules
  mBbcSetGeoModule*   mBbcSetGeo = new mBbcSetGeoModule;
  mBbcUnpackModule*   mBbcUnpack = new mBbcUnpackModule;
  mBbcRawOutModule*   mBbcRawOut = new mBbcRawOutModule;

  size_t mr=1;
  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM",mr);
  PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  dcmNode->addNode(dBbcDCMNode);
  //
  // Set up Unpack Module and DCM table ONLY for PRDF which generated during MDC2.
  //
  //mBbcUnpackMDC2Module* mBbcUnpack = new mBbcUnpackMDC2Module;
  //size_t mr=1;
  //dBbcDCMMDC2Wrapper* dBbcDCM = new dBbcDCMMDC2Wrapper("dBbcDCM",mr);
  //PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  //dcmNode->addNode(dBbcDCMNode);

  // Initialize the tables
  size_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);

  size_t mr=1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar",mr);
  PHIODataNode<PHTable>* dBbcRawHitParNode = new PHIODataNode<PHTable>(dBbcRawHitPar,"dBbcRawHitPar");
  parNode->addNode(dBbcRawHitParNode);

  size_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  bbcNode->addNode(dBbcRawNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  size_t mr=128;
  dBbcCalWrapper* dBbcCal = new dBbcCalWrapper("dBbcCal",mr);
  PHIODataNode<PHTable>* dBbcCalNode = new PHIODataNode<PHTable>(dBbcCal,"dBbcCal");
  parNode->addNode(dBbcCalNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  BbcCalibPar->restore("BbcCalib");
  PHIODataNode<TObject>*  BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  PhRootHistogramFactory::buildFactory();

}

