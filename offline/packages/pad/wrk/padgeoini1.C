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
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libpad_tables.so");
  gSystem->Load("libpad.so");

  Int_t verbose = 12;

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
  PHCompositeNode* padNode = new PHCompositeNode("PAD");
  topNode->addNode(padNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up the modules
  PHpadDetectorGeo* mPadDetGeo = new PHpadDetectorGeo();
  PHIODataNode<TObject>* padDetGeoNode = new PHIODataNode<TObject>(mPadDetGeo,"mPadDetGeo");
  parNode->addNode(padDetGeoNode);

  // Initialize the tables
  size_t mr=1;
  dPadGeomWrapper* dPadGeom = new dPadGeomWrapper("dPadGeom",mr);
  PHIODataNode<PHTable>* dPadGeomNode = new PHIODataNode<PHTable>(dPadGeom,"dPadGeom");
  parNode->addNode(dPadGeomNode);

  PhRootHistogramFactory::buildFactory();

}

