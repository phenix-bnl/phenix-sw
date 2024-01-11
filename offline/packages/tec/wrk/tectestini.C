{

  Int_t verbose = 12;
  Int_t UseObjy = 1;

// Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* geaNode = new PHCompositeNode("GEA");
  topNode->addNode(geaNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* prdfNode = new PHCompositeNode("PRDF");
  topNode->addNode(prdfNode);
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

// PISA99 File reading set-up
 
  int mr=30000;
  fkinWrapper* fkin = new fkinWrapper("fkin",mr);
  PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkin,"fkin");
  evaNode->addNode(fkinNode);

  int mr=30000;
  primaryWrapper* primary = new primaryWrapper("primary",mr);
  PHIODataNode<PHTable>* primaryNode = new PHIODataNode<PHTable>(primary,"primary");
  evaNode->addNode(primaryNode);

  int mr=30000;
  pythiaWrapper* pythia = new pythiaWrapper("pythia",mr);
  PHIODataNode<PHTable>* pythiaNode = new PHIODataNode<PHTable>(pythia,"pythia");
  evaNode->addNode(pythiaNode);

  int mr=10;
  headerWrapper* header = new headerWrapper("header",mr);
  PHIODataNode<PHTable>* headerNode = new PHIODataNode<PHTable>(header,"header");
  evaNode->addNode(headerNode);

// Read in the PISA99 file
  TFile *pisaFile = new TFile(pisaIFile);
  TTree *T = (TTree*)pisaFile.Get("T");
  PISARun *pisarun = new PISARun();
  PISAEvent *pisaevent = new PISAEvent();
  TBranch *branch = T->GetBranch("pisaevent");
  branch->SetAddress(&pisaevent);
 

  Int_t kevent = 0;
// Set up input and output files
  PHString parOutFile = parOFile;
  PHNodeIOManager *parOut = new PHNodeIOManager(parOutFile,PHWrite);
  PHString relOutFile = relOFile;
  PHNodeIOManager *relOut = new PHNodeIOManager(relOutFile,PHWrite);


// Set up the modules
  mTecSlowSimModule* mTecSlowSim = new mTecSlowSimModule;
  mTecCalibModule* mTecCalib = new mTecCalibModule();
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule();

  TecAddressObject* TecAddress = new TecAddressObject();
  TecGeometryObject* TecGeometry = new TecGeometryObject();
  TecCalibrationObject* TecCalibration = new TecCalibrationObject();
  PHIODataNode<TObject>* TecDetGeoNode = 
                            new PHIODataNode<TObject>(TecGeometry,"TecGeometry");
  parNode->addNode(TecDetGeoNode);
  PHIODataNode<TObject>* TecCalibNode = 
                            new PHIODataNode<TObject>(TecCalibration,"TecCalibration");
  parNode->addNode(TecCalibNode);

// Initialize the tables

  TecOutV1* tecout = new TecOutV1();
  PHIODataNode<PHObject>* TecOutNode = new
    PHIODataNode<PHObject>((PHObject*)tecout,"TecOutV1","PHObject");
  tecNode->addNode(TecOutNode);

  int mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  int mr=7500;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  dstNode->addNode(dTecCalibNode);

  int mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  int mr=4500;
  tecghitWrapper* tecghit = new tecghitWrapper("tecghit",mr);
  PHIODataNode<PHTable>* tecghitNode = new PHIODataNode<PHTable>(tecghit,"tecghit");
  geaNode->addNode(tecghitNode);
  evaNode->addNode(tecghitNode);

  int mr=7500;
  dTecRawWrapper* dTecRaw = new dTecRawWrapper("dTecRaw",mr);
  PHIODataNode<PHTable>* dTecRawNode = new PHIODataNode<PHTable>(dTecRaw,"dTecRaw");
  tecNode->addNode(dTecRawNode);

  int mr=3000;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  dstNode->addNode(dTecTrackNode);

  int mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

  PhRootHistogramFactory::buildFactory();

}

