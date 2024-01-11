{

  Int_t verbose = 12;

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
 
  Int_t mr=30000;
  fkinWrapper* fkin = new fkinWrapper("fkin",mr);
  PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkin,"fkin");
  evaNode->addNode(fkinNode);

  Int_t mr=30000;
  primaryWrapper* primary = new primaryWrapper("primary",mr);
  PHIODataNode<PHTable>* primaryNode = new PHIODataNode<PHTable>(primary,"primary");
  evaNode->addNode(primaryNode);

  Int_t mr=30000;
  pythiaWrapper* pythia = new pythiaWrapper("pythia",mr);
  PHIODataNode<PHTable>* pythiaNode = new PHIODataNode<PHTable>(pythia,"pythia");
  evaNode->addNode(pythiaNode);

  Int_t mr=10;
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
  PHString prdfOutFile = prdfOFile;
  PHRawOManager *prdfOut = new PHRawOManager(prdfOutFile,0,60*1024*1024/4);
//  PHString parOutFile = parOFile;
//  PHNodeIOManager *parOut = new PHNodeIOManager(parOutFile,PHWrite);
//  PHString relOutFile = relOFile;
//  PHNodeIOManager *relOut = new PHNodeIOManager(relOutFile,PHWrite);

  TecAddressObject* TecAddress = new TecAddressObject();

// Set up the modules
  mTecSlowSimModule* mTecSlowSim = new mTecSlowSimModule();
  mTecPackModule* mTecPack = new mTecPackModule();

// Initialize the tables

  Int_t mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  Int_t mr=7500;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  dstNode->addNode(dTecCalibNode);

  Int_t mr=8000;
  dTecDcmDataWrapper* dTecDcmData = new dTecDcmDataWrapper("dTecDcmData",mr);
  PHIODataNode<PHTable>* dTecDcmDataNode = new PHIODataNode<PHTable>(dTecDcmData,"dTecDcmData");
  dcmNode->addNode(dTecDcmDataNode);

  Int_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  Int_t mr=150000;
  dTecGhitDcmWrapper* dTecGhitDcm = new dTecGhitDcmWrapper("dTecGhitDcm",mr);
  PHIODataNode<PHTable>* dTecGhitDcmNode = new PHIODataNode<PHTable>(dTecGhitDcm,"dTecGhitDcm");
  evaNode->addNode(dTecGhitDcmNode);

  Int_t mr=4500;
  tecghitWrapper* tecghit = new tecghitWrapper("tecghit",mr);
  PHIODataNode<PHTable>* tecghitNode = new PHIODataNode<PHTable>(tecghit,"tecghit");
  geaNode->addNode(tecghitNode);
  evaNode->addNode(tecghitNode);

  PhRootHistogramFactory::buildFactory();

}

