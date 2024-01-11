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
  gSystem->Load("libpad_tables.so");
  gSystem->Load("libpad.so");

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
  PHCompositeNode* geaNode = new PHCompositeNode("GEA");
  topNode->addNode(geaNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* prdfNode = new PHCompositeNode("PRDF");
  topNode->addNode(prdfNode);
  PHCompositeNode* padNode = new PHCompositeNode("PAD");
  topNode->addNode(padNode);
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
  mPadSlowSimModule* mPc1SlowSim = new mPadSlowSimModule;
  mPadSlowSimModule* mPc2SlowSim = new mPadSlowSimModule;
  mPadSlowSimModule* mPc3SlowSim = new mPadSlowSimModule;
  mPadFEMModule* mPc1FEM = new mPadFEMModule;
  mPadFEMModule* mPc2FEM = new mPadFEMModule;
  mPadFEMModule* mPc3FEM = new mPadFEMModule;
  mPadDCMModule* mPc1DCM = new mPadDCMModule;
  mPadDCMModule* mPc2DCM = new mPadDCMModule;
  mPadDCMModule* mPc3DCM = new mPadDCMModule;

  // Initialize the tables
  size_t mr=1;
  dPad23ParWrapper* dPad23Par = new dPad23ParWrapper("dPad23Par",mr);
  PHIODataNode<PHTable>* dPad23ParNode = new PHIODataNode<PHTable>(dPad23Par,"dPad23Par");
  parNode->addNode(dPad23ParNode);

  size_t mr=1;
  dPadGeomWrapper* dPadGeom = new dPadGeomWrapper("dPadGeom",mr);
  PHIODataNode<PHTable>* dPadGeomNode = new PHIODataNode<PHTable>(dPadGeom,"dPadGeom");
  parNode->addNode(dPadGeomNode);

  size_t mr=1;
  dPadSlowSimParWrapper* dPadSlowSimPar = new dPadSlowSimParWrapper("dPadSlowSimPar",mr);
  PHIODataNode<PHTable>* dPadSlowSimParNode = new PHIODataNode<PHTable>(dPadSlowSimPar,"dPadSlowSimPar");
  parNode->addNode(dPadSlowSimParNode);

  size_t mr=1;
  dPadFEMParWrapper* dPadFEMPar = new dPadFEMParWrapper("dPadFEMPar",mr);
  PHIODataNode<PHTable>* dPadFEMParNode = new PHIODataNode<PHTable>(dPadFEMPar,"dPadFEMPar");
  parNode->addNode(dPadFEMParNode);

  size_t mr=1;
  dPadDCMParWrapper* dPadDCMPar = new dPadDCMParWrapper("dPadDCMPar",mr);
  PHIODataNode<PHTable>* dPadDCMParNode = new PHIODataNode<PHTable>(dPadDCMPar,"dPadDCMPar");
  parNode->addNode(dPadDCMParNode);

  size_t mr=6000;
  dPadRawWrapper* dPc1Raw = new dPadRawWrapper("dPc1Raw",mr);
  PHIODataNode<PHTable>* dPc1RawNode = new PHIODataNode<PHTable>(dPc1Raw,"dPc1Raw");
  padNode->addNode(dPc1RawNode);

  size_t mr=6000;
  dPadRawWrapper* dPc2Raw = new dPadRawWrapper("dPc2Raw",mr);
  PHIODataNode<PHTable>* dPc2RawNode = new PHIODataNode<PHTable>(dPc2Raw,"dPc2Raw");
  padNode->addNode(dPc2RawNode);

  size_t mr=6000;
  dPadRawWrapper* dPc3Raw = new dPadRawWrapper("dPc3Raw",mr);
  PHIODataNode<PHTable>* dPc3RawNode = new PHIODataNode<PHTable>(dPc3Raw,"dPc3Raw");
  padNode->addNode(dPc3RawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc1GhitRaw = new dPadGhitRawWrapper("dPc1GhitRaw",mr);
  PHIODataNode<PHTable>* dPc1GhitRawNode = new PHIODataNode<PHTable>(dPc1GhitRaw,"dPc1GhitRaw");
  evaNode->addNode(dPc1GhitRawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc2GhitRaw = new dPadGhitRawWrapper("dPc2GhitRaw",mr);
  PHIODataNode<PHTable>* dPc2GhitRawNode = new PHIODataNode<PHTable>(dPc2GhitRaw,"dPc2GhitRaw");
  evaNode->addNode(dPc2GhitRawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc3GhitRaw = new dPadGhitRawWrapper("dPc3GhitRaw",mr);
  PHIODataNode<PHTable>* dPc3GhitRawNode = new PHIODataNode<PHTable>(dPc3GhitRaw,"dPc3GhitRaw");
  evaNode->addNode(dPc3GhitRawNode);

  size_t mr=32;
  dPadFEMWrapper* dPc1FEM = new dPadFEMWrapper("dPc1FEM",mr);
  PHIODataNode<PHTable>* dPc1FEMNode = new PHIODataNode<PHTable>(dPc1FEM,"dPc1FEM");
  padNode->addNode(dPc1FEMNode);

  size_t mr=32;
  dPadFEMWrapper* dPc2FEM = new dPadFEMWrapper("dPc2FEM",mr);
  PHIODataNode<PHTable>* dPc2FEMNode = new PHIODataNode<PHTable>(dPc2FEM,"dPc2FEM");
  padNode->addNode(dPc2FEMNode);

  size_t mr=32;
  dPadFEMWrapper* dPc3FEM = new dPadFEMWrapper("dPc3FEM",mr);
  PHIODataNode<PHTable>* dPc3FEMNode = new PHIODataNode<PHTable>(dPc3FEM,"dPc3FEM");
  padNode->addNode(dPc3FEMNode);

  size_t mr=64;
  dPadDCMWrapper* dPc1DCM = new dPadDCMWrapper("dPc1DCM",mr);
  PHIODataNode<PHTable>* dPc1DCMNode = new PHIODataNode<PHTable>(dPc1DCM,"dPc1DCM");
  dcmNode->addNode(dPc1DCMNode);

  size_t mr=64;
  dPadDCMWrapper* dPc2DCM = new dPadDCMWrapper("dPc2DCM",mr);
  PHIODataNode<PHTable>* dPc2DCMNode = new PHIODataNode<PHTable>(dPc2DCM,"dPc2DCM");
  dcmNode->addNode(dPc2DCMNode);

  size_t mr=64;
  dPadDCMWrapper* dPc3DCM = new dPadDCMWrapper("dPc3DCM",mr);
  PHIODataNode<PHTable>* dPc3DCMNode = new PHIODataNode<PHTable>(dPc3DCM,"dPc3DCM");
  dcmNode->addNode(dPc3DCMNode);

  size_t mr=18000;
  dPadNibbleGhitWrapper* dPc1NibbleGhit = new dPadNibbleGhitWrapper("dPc1NibbleGhit",mr);
  PHIODataNode<PHTable>* dPc1NibbleGhitNode = new PHIODataNode<PHTable>(dPc1NibbleGhit,"dPc1NibbleGhit");
  evaNode->addNode(dPc1NibbleGhitNode);

  size_t mr=18000;
  dPadNibbleGhitWrapper* dPc2NibbleGhit = new dPadNibbleGhitWrapper("dPc2NibbleGhit",mr);
  PHIODataNode<PHTable>* dPc2NibbleGhitNode = new PHIODataNode<PHTable>(dPc2NibbleGhit,"dPc2NibbleGhit");
  evaNode->addNode(dPc2NibbleGhitNode);

  size_t mr=18000;
  dPadNibbleGhitWrapper* dPc3NibbleGhit = new dPadNibbleGhitWrapper("dPc3NibbleGhit",mr);
  PHIODataNode<PHTable>* dPc3NibbleGhitNode = new PHIODataNode<PHTable>(dPc3NibbleGhit,"dPc3NibbleGhit");
  evaNode->addNode(dPc3NibbleGhitNode);

  size_t mr=1500;
  pcghitWrapper* pc1ghit = new pcghitWrapper("pc1ghit",mr);
  PHIODataNode<PHTable>* pc1ghitNode = new PHIODataNode<PHTable>(pc1ghit,"pc1ghit");
  geaNode->addNode(pc1ghitNode);

  size_t mr=1500;
  pcghitWrapper* pc2ghit = new pcghitWrapper("pc2ghit",mr);
  PHIODataNode<PHTable>* pc2ghitNode = new PHIODataNode<PHTable>(pc2ghit,"pc2ghit");
  geaNode->addNode(pc2ghitNode);

  size_t mr=1500;
  pcghitWrapper* pc3ghit = new pcghitWrapper("pc3ghit",mr);
  PHIODataNode<PHTable>* pc3ghitNode = new PHIODataNode<PHTable>(pc3ghit,"pc3ghit");
  geaNode->addNode(pc3ghitNode);


  PhRootHistogramFactory::buildFactory();

}

