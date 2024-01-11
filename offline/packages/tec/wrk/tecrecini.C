{

  Int_t verbose = 12;

//  Int_t isSimulation = 1;
  Int_t isSimulation = 0;
  Int_t UseObjy = 1;
//  Int_t UseObjy = 0;

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

  PHString prdfInFile = prdfIFile;
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

// Set up the modules

  mTecGainsModule* mTecGains = new mTecGainsModule();
  mTecAlignModule* mTecAlign = new mTecAlignModule();
  mTecDecodeModule* mTecDecode = new mTecDecodeModule();
  mTecRemapModule* mTecRemap = new mTecRemapModule();
  mTecCalibModule* mTecCalib = new mTecCalibModule();
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule();
  mTecPIDModule* mTecPID = new mTecPIDModule();
  mTecGainsModule* mTecGains = new mTecGainsModule();
  TecAddressObject* TecAddress = new TecAddressObject();
  TecGeometryObject* TecGeometry = new TecGeometryObject();
  TecCalibrationObject* TecCalibration = new TecCalibrationObject();

// Initialize the tables

  PHIODataNode<TObject>* TecDetGeoNode = 
                           new PHIODataNode<TObject>(TecGeometry,"TecGeometry");
  parNode->addNode(TecDetGeoNode);

  PHIODataNode<TObject>* TecCalibNode = 
                           new PHIODataNode<TObject>(TecCalibration,"TecCalibration");
  parNode->addNode(TecCalibNode);

  Int_t mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  Int_t mr=7500;
  dTecRawWrapper* dTecRaw = new dTecRawWrapper("dTecRaw",mr);
  PHIODataNode<PHTable>* dTecRawNode = new PHIODataNode<PHTable>(dTecRaw,"dTecRaw");
  tecNode->addNode(dTecRawNode);

//  Int_t mr=7500;
//  dTecVectorWrapper* dTecVector = new dTecVectorWrapper("dTecVector",mr);
//  PHIODataNode<PHTable>* dTecVectorNode = new PHIODataNode<PHTable>(dTecVector,"dTecVector");
//  tecNode->addNode(dTecVectorNode);

  Int_t mr=1000;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  dstNode->addNode(dTecTrackNode);

  Int_t mr=1000;
  dTecTrackTable* dTecTrackT = new dTecTrackTable("dTecTrackT",mr);
  PHIODataNode<PHTable>* dTecTrackTNode = new PHIODataNode<PHTable>(dTecTrackT,"dTecTrackT");
  tecNode->addNode(dTecTrackTNode);

  Int_t mr=1000;
  dTecPIDWrapper* dTecPID = new dTecPIDWrapper("dTecPID",mr);
  PHIODataNode<PHTable>* dTecPIDNode = new PHIODataNode<PHTable>(dTecPID,"dTecPID");
  dstNode->addNode(dTecPIDNode);

//  Int_t mr=3000;
//  dTecVectTrackWrapper* dTecVectTrack = new dTecVectTrackWrapper("dTecVectTrack",mr);
//  PHIODataNode<PHTable>* dTecVectTrackNode = new PHIODataNode<PHTable>(dTecVectTrack,"dTecVectTrack");
//  tecNode->addNode(dTecVectTrackNode);

  Int_t mr=7500;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  dstNode->addNode(dTecCalibNode);

  Int_t mr=45120;
  dTecWireWrapper* dTecWire = new dTecWireWrapper("dTecWire",mr);
  PHIODataNode<PHTable>* dTecWireNode = new PHIODataNode<PHTable>(dTecWire,"dTecWire");
  parNode->addNode(dTecWireNode);

  Int_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  Int_t mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

  PhRootHistogramFactory::buildFactory();

}

