// padVertexIni initialization 
//************************************************************
{

  // Loading PHOOL libraries

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
  gSystem->Load("libgslblas.so");
  gSystem->Load("libgsl.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libEG.so");
  gSystem->Load("libemcCalib.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");

  gSystem->Load("libcgl.so");
  gSystem->Load("libdch.so");

  gSystem->Load("libcrk.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libmom.so");
  gSystem->Load("libcge.so");
  gSystem->Load("libheader.so");
  gSystem->Load("liblv1.so");
  gSystem->Load("libvtx.so");

  gSystem->Load("libmui.so");
  gSystem->Load("libmfm.so");

  gSystem->Load("libmutgeom.so");
  gSystem->Load("libmut.so");

  Int_t verbose = 12;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");

  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);

  PHCompositeNode* geaNode = new PHCompositeNode("GEA");
  topNode->addNode(geaNode);

  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);

  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // PISA2000 File reading set-up
  
  // Read in the PISA2000 file
  TFile *pisaFile = new TFile(pisaIFile);
  TTree *T = (TTree*)pisaFile.Get("T");
  PISARun *pisarun = new PISARun();
  PISAEvent *pisaevent = new PISAEvent();
  Event *thisEvent = 0;
  TBranch *branch = T->GetBranch("pisaevent");
  branch->SetAddress(&pisaevent);

  // Set up input and output files
  PHString relInFile = relIFile;
  //
  // Open the GEANT Evaluation File
  //
  PHNodeIOManager *ioEval = new PHNodeIOManager(relInFile, PHReadOnly);

  PHString dstInFile = dstIFile;
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  if (dstIn->read(dstNode)) eventGood = 1;

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

  // Initialize the tables

  PHIODataNode<PHTable>* dBbcOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dBbcOut");
  PHIODataNode<PHTable>* dBbcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dBbcRaw");
  PHIODataNode<PHTable>* dMvddNdEtaOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvddNdEtaOut");
  PHIODataNode<PHTable>* dMvdMultOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvdMultOut");
  PHIODataNode<PHTable>* dMvdVertexOutNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvdVertexOut");
  PHIODataNode<PHTable>* dMvbRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvbRaw");
  PHIODataNode<PHTable>* dMvcRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMvcRaw");
  PHIODataNode<PHTable>* dDchHitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchHit");
  PHIODataNode<PHTable>* dDchTracksNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchTracks");
  PHIODataNode<PHTable>* dPHDchTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPHDchTrack");
  PHIODataNode<PHTable>* dPc1RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc1Raw");
  PHIODataNode<PHTable>* dPc2RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc2Raw");
  PHIODataNode<PHTable>* dPc3RawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc3Raw");
  PHIODataNode<PHTable>* dPc1ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc1Cluster");
  PHIODataNode<PHTable>* dPc2ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc2Cluster");
  PHIODataNode<PHTable>* dPc3ClusterNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dPc3Cluster");
  PHIODataNode<PHTable>* dCrkRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkRaw");
  PHIODataNode<PHTable>* dCrkHitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkHit");
  PHIODataNode<PHTable>* dCrkPidNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCrkPid");
  PHIODataNode<PHTable>* dTecTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecTrack");
  PHIODataNode<PHTable>* dTecCalibNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecCalib");
  PHIODataNode<PHTable>* dTecPIDNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecPID");
  PHIODataNode<PHTable>* dTofRawNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofRaw");
  PHIODataNode<PHTable>* dTofReconstructedNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTofReconstructed");
  PHIODataNode<PHTable>* dEmcCalibTowerNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcCalibTower");
  PHIODataNode<PHTable>* dEmcClusterLocalNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcClusterLocal");
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEmcClusterLocalExt");
  PHIODataNode<PHTable>* dCglTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglTrack");
  PHIODataNode<PHTable>* dCglParticleNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglParticle");
  PHIODataNode<PHTable>* dCglPidNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dCglPid");

  PhRootHistogramFactory::buildFactory();

}
