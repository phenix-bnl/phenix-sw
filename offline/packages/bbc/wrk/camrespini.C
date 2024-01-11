//************************************************************
// Initialization macro written by the Pyrite GUI for PHOOL   
//************************************************************

// This macro is valid for the new library dated Aug 8 at 06:53 am.

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
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");

  Int_t verbose = 12;

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
  PHCompositeNode* geaNode = new PHCompositeNode("GEA");
  topNode->addNode(geaNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* prdfNode = new PHCompositeNode("PRDF");
  topNode->addNode(prdfNode);
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // PISA99 File reading set-up
 
  size_t mr=30000;
  fkinWrapper* fkin = new fkinWrapper("fkin",mr);
  PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkin,"fkin");
  evaNode->addNode(fkinNode);

  size_t mr=30000;
  primaryWrapper* primary = new primaryWrapper("primary",mr);
  PHIODataNode<PHTable>* primaryNode = new PHIODataNode<PHTable>(primary,"primary");
  evaNode->addNode(primaryNode);

  size_t mr=30000;
  pythiaWrapper* pythia = new pythiaWrapper("pythia",mr);
  PHIODataNode<PHTable>* pythiaNode = new PHIODataNode<PHTable>(pythia,"pythia");
  evaNode->addNode(pythiaNode);

  size_t mr=10;
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
  PHRawOManager *prdfOut = new PHRawOManager(prdfOutFile,runNumber,60*1024*1024/4);
  PHString parOutFile = parOFile;
  PHNodeIOManager *parOut = new PHNodeIOManager(parOutFile,PHWrite);
  PHString relOutFile = relOFile;
  PHNodeIOManager *relOut = new PHNodeIOManager(relOutFile,PHWrite);

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcSetUcalModule* mBbcSetUcal = new mBbcSetUcalModule;
  mBbcGhitRawModule* mBbcGhitRaw = new mBbcGhitRawModule;
  mBbcFEMModule* mBbcFEM = new mBbcFEMModule;
  mBbcDCMModule* mBbcDCM = new mBbcDCMModule;

  // Initialize the tables
  size_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);

  size_t mr=1;
  dBbcGhitRawParWrapper* dBbcGhitRawPar = new dBbcGhitRawParWrapper("dBbcGhitRawPar",mr);
  PHIODataNode<PHTable>* dBbcGhitRawParNode = new PHIODataNode<PHTable>(dBbcGhitRawPar,"dBbcGhitRawPar");
  parNode->addNode(dBbcGhitRawParNode);

  size_t mr=3000;
  bbcghitWrapper* bbcghit = new bbcghitWrapper("bbcghit",mr);
  PHIODataNode<PHTable>* bbcghitNode = new PHIODataNode<PHTable>(bbcghit,"bbcghit");
  geaNode->addNode(bbcghitNode);

  size_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  dstNode->addNode(dBbcRawNode);

  size_t mr=128;
  dBbcUcalWrapper* dBbcUcal = new dBbcUcalWrapper("dBbcUcal",mr);
  PHIODataNode<PHTable>* dBbcUcalNode = new PHIODataNode<PHTable>(dBbcUcal,"dBbcUcal");
  bbcNode->addNode(dBbcUcalNode);

  size_t mr=1;
  dBbcFEMWrapper* dBbcFEM = new dBbcFEMWrapper("dBbcFEM",mr);
  PHIODataNode<PHTable>* dBbcFEMNode = new PHIODataNode<PHTable>(dBbcFEM,"dBbcFEM");
  bbcNode->addNode(dBbcFEMNode);

  size_t mr=1;
  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM",mr);
  PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  dcmNode->addNode(dBbcDCMNode);

  size_t mr=10000;
  dBbcGhitRawWrapper* dBbcGhitRaw = new dBbcGhitRawWrapper("dBbcGhitRaw",mr);
  PHIODataNode<PHTable>* dBbcGhitRawNode = new PHIODataNode<PHTable>(dBbcGhitRaw,"dBbcGhitRaw");
  evaNode->addNode(dBbcGhitRawNode);

  PhRootHistogramFactory::buildFactory();

}
