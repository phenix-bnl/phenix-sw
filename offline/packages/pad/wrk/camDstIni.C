// camDstIni initialization 
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

  gSystem->Load("libgea.so");
  gSystem->Load("liblv1.so");

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
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
  gSystem->Load("libmut.so");
  gSystem->Load("libmui.so");
  gSystem->Load("libvtx.so");
  gSystem->Load("libheader.so");

  Int_t verbose = 12;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");

  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);

  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  PHString dstInFile = dstIFile;
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  if (!dstIn->read(dstNode)) {
    cerr << "\n Failed to read the first event of this DST; exiting " << endl;
    exit(1);
  }

  // Initialize the tables

  PHIODataNode<PHObject>* ZdcOutNode = (PHIODataNode<PHObject>*)mainIter->findFirst("PHIODataNode","ZdcOut");
  PHIODataNode<PHTable>* dEventHeaderNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dEventHeader");
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
  PHIODataNode<PHTable>* dMuoTracksNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMuoTracks");
  PHIODataNode<PHTable>* dMuiRoadsNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dMuiRoads");

  PhRootHistogramFactory::buildFactory();
}
