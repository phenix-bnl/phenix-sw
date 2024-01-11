//************************************************************
// TestiInitialization macro for PRDF write (Crk only version)  
//************************************************************

{  
  
  // Loading subsystem libraries
  gSystem->Load("libcrk_tables.so");
  gSystem->Load("libcrk.so");

  // Set up the node tree to include Crk
  PHCompositeNode* crkNode = new PHCompositeNode("CRK");
  topNode->addNode(crkNode);

  // Set up the modules

  mCrkSetGeoModule* mCrkSetGeo = new mCrkSetGeoModule;
  mCrkSetUcalModule* mCrkSetUcal = new mCrkSetUcalModule;
  mCrkSetCalModule* mCrkSetCal = new mCrkSetCalModule;
  mCrkGhitRawModule* mCrkGhitRaw = new mCrkGhitRawModule;
  mCrkRawFEMModule* mCrkRawFEM = new mCrkRawFEMModule;
  mCrkDCMModule* mCrkDCM = new mCrkDCMModule;
  //  mCrkDCMoutputModule* mCrkDCMoutput = new mCrkDCMoutputModule;
  mCrkDCMRawModule *mCrkDCMRaw = new mCrkDCMRawModule;
  mCrkRawHitModule *mCrkRawHit = new mCrkRawHitModule;
  //
  //  mCrkDchProjModule *mCrkDchProj = new mCrkDchProjModule;
  mCrkProjPidModule *mCrkProjPid = new mCrkProjPidModule;

  // Initialize the tables
  size_t mr=1;
  dCrkGeoWrapper* dCrkGeo = new dCrkGeoWrapper("dCrkGeo",mr);
  PHIODataNode<PHTable>* dCrkGeoNode = new PHIODataNode<PHTable>(dCrkGeo,"dCrkGeo");
  parNode->addNode(dCrkGeoNode);
  
  size_t mr=1;
  dCrkGhitRawParWrapper* dCrkGhitRawPar = new dCrkGhitRawParWrapper("dCrkGhitRawPar",mr);
  PHIODataNode<PHTable>* dCrkGhitRawParNode = new PHIODataNode<PHTable>(dCrkGhitRawPar,"dCrkGhitRawPar");
  parNode->addNode(dCrkGhitRawParNode);
  
  size_t mr=1;
  dCrkRawFEMparWrapper* dCrkRawFEMpar = new dCrkRawFEMparWrapper("dCrkRawFEMpar",mr);
  PHIODataNode<PHTable>* dCrkRawFEMparNode = new PHIODataNode<PHTable>(dCrkRawFEMpar,"dCrkRawFEMpar");
  parNode->addNode(dCrkRawFEMparNode);
 
  size_t mr=1;
  dCrkDCMparWrapper* dCrkDCMpar = new dCrkDCMparWrapper("dCrkDCMpar",mr);
  PHIODataNode<PHTable>* dCrkDCMparNode = new PHIODataNode<PHTable>(dCrkDCMpar,"dCrkDCMpar");
  parNode->addNode(dCrkDCMparNode);

  size_t mr=1;
  dCrkRawHitParWrapper* dCrkRawHitPar = new dCrkRawHitParWrapper("dCrkRawHitPar",mr);
  PHIODataNode<PHTable>* dCrkRawHitParNode = new PHIODataNode<PHTable>(dCrkRawHitPar,"dCrkRawHitPar");
  parNode->addNode(dCrkRawHitParNode);

  size_t mr=60000;
  crkghitWrapper* crkghit = new crkghitWrapper("crkghit",mr);
  PHIODataNode<PHTable>* crkghitNode = new PHIODataNode<PHTable>(crkghit,"crkghit");
  geaNode->addNode(crkghitNode);

  size_t mr=5120;
  dCrkRawWrapper* dCrkRaw = new dCrkRawWrapper("dCrkRaw",mr);
  PHIODataNode<PHTable>* dCrkRawNode = new PHIODataNode<PHTable>(dCrkRaw,"dCrkRaw");
  crkNode->addNode(dCrkRawNode);

  size_t mr=5120;
  dCrkCalWrapper* dCrkCal = new dCrkCalWrapper("dCrkCal",mr);
  PHIODataNode<PHTable>* dCrkCalNode = new PHIODataNode<PHTable>(dCrkCal,"dCrkCal");
  crkNode->addNode(dCrkCalNode);

  size_t mr=5120;
  dCrkUcalWrapper* dCrkUcal = new dCrkUcalWrapper("dCrkUcal",mr);
  PHIODataNode<PHTable>* dCrkUcalNode = new PHIODataNode<PHTable>(dCrkUcal,"dCrkUcal");
  parNode->addNode(dCrkUcalNode);

  size_t mr=1500;
  dCrkFEMWrapper* dCrkFEM = new dCrkFEMWrapper("dCrkFEM",mr);
  PHIODataNode<PHTable>* dCrkFEMNode = new PHIODataNode<PHTable>(dCrkFEM,"dCrkFEM");
  crkNode->addNode(dCrkFEMNode);

  size_t mr=1500;
  dCrkDCMWrapper* dCrkDCM = new dCrkDCMWrapper("dCrkDCM",mr);
  PHIODataNode<PHTable>* dCrkDCMNode = new PHIODataNode<PHTable>(dCrkDCM,"dCrkDCM");
  dcmNode->addNode(dCrkDCMNode);

  size_t mr=5120;
  dCrkRel2sWrapper* dCrkRel2s = new dCrkRel2sWrapper("dCrkRel2s",mr);
  PHIODataNode<PHTable>* dCrkRel2sNode = new PHIODataNode<PHTable>(dCrkRel2s,"dCrkRel2s");
  evaNode->addNode(dCrkRel2sNode);

  size_t mr=5120;
  dCrkHitWrapper* dCrkHit = new dCrkHitWrapper("dCrkHit",mr);
  PHIODataNode<PHTable>* dCrkHitNode = new PHIODataNode<PHTable>(dCrkHit,"dCrkHit");
  dstNode->addNode(dCrkHitNode);

  size_t mr=1000;
  dCrkProjWrapper* dCrkProj = new dCrkProjWrapper("dCrkProj",mr);
  PHIODataNode<PHTable>* dCrkProjNode = new PHIODataNode<PHTable>(dCrkProj,"dCrkProj");
  dstNode->addNode(dCrkProjNode);

  size_t mr=1000;
  dCrkPidWrapper* dCrkPid = new dCrkPidWrapper("dCrkPid",mr);
  PHIODataNode<PHTable>* dCrkPidNode = new PHIODataNode<PHTable>(dCrkPid,"dCrkPid");
  dstNode->addNode(dCrkPidNode);

  size_t mr=1;
  dCrkProjPidParWrapper* dCrkProjPidPar = new dCrkProjPidParWrapper("dCrkProjPidPar",mr);
  PHIODataNode<PHTable>* dCrkProjPidParNode = new PHIODataNode<PHTable>(dCrkProjPidPar,"dCrkProjPidPar");
  parNode->addNode(dCrkProjPidParNode);

}
