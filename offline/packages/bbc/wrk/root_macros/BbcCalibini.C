//************************************************************
// Test initialization macro for PRDF evaluation (Bbc only version)  
//************************************************************

{  
  //
  // Load the PISA class libraries
  //
  gSystem->Load("libPISARoot.so");
  //
  // PISA interface GEA class libraries
  //
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libbbc_tables.so");
  gSystem->Load("libbbc.so");

  // Set up the node tree to have BBC tables
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);

  // Set up the modules
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcFEMModule* mBbcFEM = new mBbcFEMModule;
  mBbcDCMModule* mBbcDCM = new mBbcDCMModule;
  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcOutEvalModule* mBbcOutEval = new mBbcOutEvalModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;  // event reconstruction

  size_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);
  
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
  PHIODataNode<TObject>*  BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  size_t mr=1;
  dBbcEvalWrapper* dBbcEval = new dBbcEvalWrapper("dBbcEval",mr);
  PHIODataNode<PHTable>* dBbcEvalNode = new PHIODataNode<PHTable>(dBbcEval,"dBbcEval");
  evaNode->addNode(dBbcEvalNode);
}
