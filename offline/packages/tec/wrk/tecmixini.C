
{

// Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");
  gSystem->Load("libPdbCal.so");

// Loading subsystem libraries
  gSystem->Load("libcgl_tables.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libtec_tables.so");
  gSystem->Load("libtec.so");
//  gSystem->Load("/phenix/workarea/lebedev/tectest/install/lib/libtec_tables.so");
//  gSystem->Load("/phenix/workarea/lebedev/tectest/install/lib/libtec.so");

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
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

// Set up input and output files
  PHString prdfInFile1 = prdfIFile1;
  PHString prdfInFile2 = prdfIFile2;
  Eventiterator *eventIter1 = new fileEventiterator(prdfInFile1.getString());
  Eventiterator *eventIter2 = new fileEventiterator(prdfInFile2.getString());
  Event *thisEvent1 = 0;
  Event *thisEvent2 = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent1, "PRDF"));

// Set up the modules
  mTecAlignModule* mTecAlign = new mTecAlignModule;
  mTecUnpackModule* mTecUnpack = new mTecUnpackModule;
  mTecRemapModule* mTecRemap = new mTecRemapModule;
  mTecCalibModule* mTecCalib = new mTecCalibModule;
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule;

// Initialize the tables

  size_t mr=7500;
  dTecRawWrapper* dTecRaw = new dTecRawWrapper("dTecRaw",mr);
  PHIODataNode<PHTable>* dTecRawNode = new PHIODataNode<PHTable>(dTecRaw,"dTecRaw");
  tecNode->addNode(dTecRawNode);

  size_t mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  size_t mr=7500;
  dTecGeomWrapper* dTecGeom = new dTecGeomWrapper("dTecGeom",mr);
  PHIODataNode<PHTable>* dTecGeomNode = new PHIODataNode<PHTable>(dTecGeom,"dTecGeom");
  parNode->addNode(dTecGeomNode);

// Create second dTecRaw table
  size_t mr=7500;
  dTecRawWrapper* dTecRaw2 = new dTecRawWrapper("dTecRaw2",mr);
  PHIODataNode<PHTable>* dTecRawNode2 = new PHIODataNode<PHTable>(dTecRaw2,"dTecRaw2");
  tecNode->addNode(dTecRawNode2);

  size_t mr=7500;
  dTecVectorWrapper* dTecVector = new dTecVectorWrapper("dTecVector",mr);
  PHIODataNode<PHTable>* dTecVectorNode = new PHIODataNode<PHTable>(dTecVector,"dTecVector");
  tecNode->addNode(dTecVectorNode);

  size_t mr=3000;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  dstNode->addNode(dTecTrackNode);

  size_t mr=3000;
  dTecVectTrackWrapper* dTecVectTrack = new dTecVectTrackWrapper("dTecVectTrack",mr);
  PHIODataNode<PHTable>* dTecVectTrackNode = new PHIODataNode<PHTable>(dTecVectTrack,"dTecVectTrack");
  tecNode->addNode(dTecVectTrackNode);

  size_t mr=7500;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  tecNode->addNode(dTecCalibNode);

  size_t mr=45120;
  dTecWireWrapper* dTecWire = new dTecWireWrapper("dTecWire",mr);
  PHIODataNode<PHTable>* dTecWireNode = new PHIODataNode<PHTable>(dTecWire,"dTecWire");
  parNode->addNode(dTecWireNode);

  size_t mr=8000;
  dTecDcmDataWrapper* dTecDcmData = new dTecDcmDataWrapper("dTecDcmData",mr);
  PHIODataNode<PHTable>* dTecDcmDataNode = new PHIODataNode<PHTable>(dTecDcmData,"dTecDcmData");
  dcmNode->addNode(dTecDcmDataNode);

  size_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  size_t mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

  PhRootHistogramFactory::buildFactory();

}

