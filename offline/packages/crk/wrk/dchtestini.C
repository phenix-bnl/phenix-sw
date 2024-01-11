//************************************************************
// TestiInitialization macro for PRDF write (Dch only version)  
//************************************************************

{  
  //
  // Should not be necessary to keep repeating the specification size_t mr each time?
  //

  // Loading subsystem libraries
  gSystem->Load("libdch_tables.so");
  gSystem->Load("libdch.so");

  // Set up the node tree to include Dch
  PHCompositeNode* dchNode = new PHCompositeNode("DCH");
  topNode->addNode(dchNode);

  // Set up the modules
  mDchFastSimModule* mDchFastSim = new mDchFastSimModule;
  mDchFEMModule* mDchFEM = new mDchFEMModule;
  mDchDCMModule* mDchDCM = new mDchDCMModule;
  mDchDCMoutputModule* mDchDCMoutput = new mDchDCMoutputModule;
  mDchUnpackModule* mDchUnpack = new mDchUnpackModule;
  mDchCalibModule* mDchCalib = new mDchCalibModule;
  mDchTrackerModule* mDchTracker = new mDchTrackerModule;
  // mDchCFMEvaluateModule* mDchCFMEvaluate = new mDchCFMEvaluateModule;

  // Initialize the tables
  size_t mr=1;
  dDchGeomWrapper* dDchGeom = new dDchGeomWrapper("dDchGeom",mr);
  PHIODataNode<PHTable>* dDchGeomNode = new PHIODataNode<PHTable>(dDchGeom,"dDchGeom");
  parNode->addNode(dDchGeomNode);

  size_t mr=1;
  dDchFastSimParWrapper* dDchFastSimPar = new dDchFastSimParWrapper("dDchFastSimPar",mr);
  PHIODataNode<PHTable>* dDchFastSimParNode = new PHIODataNode<PHTable>(dDchFastSimPar,"dDchFastSimPar");
  parNode->addNode(dDchFastSimParNode);

  size_t mr=1;
  dDchDCMParWrapper* dDchDCMPar = new dDchDCMParWrapper("dDchDCMPar",mr);
  PHIODataNode<PHTable>* dDchDCMParNode = new PHIODataNode<PHTable>(dDchDCMPar,"dDchDCMPar");
  parNode->addNode(dDchDCMParNode);

  size_t mr=1;
  dDchEvalParWrapper* dDchEvalPar = new dDchEvalParWrapper("dDchEvalPar",mr);
  PHIODataNode<PHTable>* dDchEvalParNode = new PHIODataNode<PHTable>(dDchEvalPar,"dDchEvalPar");
  parNode->addNode(dDchEvalParNode);

  size_t mr=60000;
  dDchRawWrapper* dDchRaw = new dDchRawWrapper("dDchRaw",mr);
  PHIODataNode<PHTable>* dDchRawNode = new PHIODataNode<PHTable>(dDchRaw,"dDchRaw");
  dchNode->addNode(dDchRawNode);

  size_t mr=60000;
  dDchHitWrapper* dDchHit = new dDchHitWrapper("dDchHit",mr);
  PHIODataNode<PHTable>* dDchHitNode = new PHIODataNode<PHTable>(dDchHit,"dDchHit");
  dstNode->addNode(dDchHitNode);

  size_t mr=160;
  dDchFEMWrapper* dDchFEM = new dDchFEMWrapper("dDchFEM",mr);
  PHIODataNode<PHTable>* dDchFEMNode = new PHIODataNode<PHTable>(dDchFEM,"dDchFEM");
  dchNode->addNode(dDchFEMNode);

  size_t mr=1500;
  dDchTracksWrapper* dDchTracks = new dDchTracksWrapper("dDchTracks",mr);
  PHIODataNode<PHTable>* dDchTracksNode = new PHIODataNode<PHTable>(dDchTracks,"dDchTracks");
  dstNode->addNode(dDchTracksNode);

  size_t mr=60000;
  dDchGhitRawWrapper* dDchGhitRaw = new dDchGhitRawWrapper("dDchGhitRaw",mr);
  PHIODataNode<PHTable>* dDchGhitRawNode = new PHIODataNode<PHTable>(dDchGhitRaw,"dDchGhitRaw");
  dchNode->addNode(dDchGhitRawNode);

  size_t mr=60000;
  dDchGhitHitsWrapper* dDchGhitHits = new dDchGhitHitsWrapper("dDchGhitHits",mr);
  PHIODataNode<PHTable>* dDchGhitHitsNode = new PHIODataNode<PHTable>(dDchGhitHits,"dDchGhitHits");
  dchNode->addNode(dDchGhitHitsNode);

  size_t mr=160;
  dDchDCMWrapper* dDchDCM = new dDchDCMWrapper("dDchDCM",mr);
  PHIODataNode<PHTable>* dDchDCMNode = new PHIODataNode<PHTable>(dDchDCM,"dDchDCM");
  dcmNode->addNode(dDchDCMNode);

  size_t mr=60000;
  dDchNibbleGhitWrapper* dDchNibbleGhit = new dDchNibbleGhitWrapper("dDchNibbleGhit",mr);
  PHIODataNode<PHTable>* dDchNibbleGhitNode = new PHIODataNode<PHTable>(dDchNibbleGhit,"dDchNibbleGhit");
  evaNode->addNode(dDchNibbleGhitNode);

  size_t mr=60000;
  dcghitWrapper* dcghit = new dcghitWrapper("dcghit",mr);
  PHIODataNode<PHTable>* dcghitNode = new PHIODataNode<PHTable>(dcghit,"dcghit");
  geaNode->addNode(dcghitNode);

}
