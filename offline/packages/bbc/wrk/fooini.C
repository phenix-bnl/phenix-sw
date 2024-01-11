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
  gSystem->Load("libbbc_tables.so");
  gSystem->Load("libbbc.so");

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
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

  PHString dstOutFile = dstOFile;
  PHNodeIOManager *dstOut = new PHNodeIOManager(dstOutFile,PHWrite);

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;

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
  dstNode->addNode(dBbcRawNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  BbcCalibPar->restore("BbcCalib");
  PHIODataNode<TObject>* BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  size_t mr=128;
  dBbcCalWrapper* dBbcCal = new dBbcCalWrapper("dBbcCal",mr);
  PHIODataNode<PHTable>* dBbcCalNode = new PHIODataNode<PHTable>(dBbcCal,"dBbcCal");
  parNode->addNode(dBbcCalNode);

  size_t mr=1;
  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM",mr);
  PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  dcmNode->addNode(dBbcDCMNode);


  PhRootHistogramFactory::buildFactory();

}

