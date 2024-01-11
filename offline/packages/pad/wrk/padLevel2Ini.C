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
  gSystem->Load("libgslcblas.so");
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

  PHCompositeNode* padNode = new PHCompositeNode("PAD");
  topNode->addNode(padNode);

  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);

  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  mPadEvaluateModule* mPc1Evaluate = new mPadEvaluateModule;
  mPadEvaluateModule* mPc2Evaluate = new mPadEvaluateModule;
  mPadEvaluateModule* mPc3Evaluate = new mPadEvaluateModule;

  // PISA2000 File reading set-up
  PISARun *pisarun = new PISARun();
  cout << "\n Number of input files requested = " << mergeFiles + 1 << endl;
  //
  //  Use MERGE files method only
  //
  TFile *pisaFileArray[maxFiles];
  TTree *TTArray[maxFiles];
  TBranch *branchArray[maxFiles];
  PISAEvent *pisaEventArray[maxFiles];

  pisaFileArray[0] = new TFile(pisaIFile);
  cout << "\n pisaMergeFileOpen.C <I>: opened primary file " << pisaIFile << endl;

  if(mergeFiles > 0) {
    pisaFileArray[1] = new TFile(gname);
    cout << "\n pisaMergeFileOpen.C <I>: opened first merge file " << gname << endl;
  }

  if(mergeFiles > 1) {
    pisaFileArray[2] = new TFile(hname);
    cout << "\n pisaMergeFileOpen.C <I>: opened second merge file " << hname << endl;
  }

  if(mergeFiles > 2) {
    pisaFileArray[3] = new TFile(iname);
    cout << "\n pisaMergeFileOpen.C <I>: opened third merge file " << iname << endl;
  }

  if(mergeFiles > 3) {
    pisaFileArray[4] = new TFile(jname);
    cout << "\n pisaMergeFileOpen.C <I>: opened fourth merge file " << jname << endl;
  }

  for(Int_t iFile=0; iFile<mergeFiles+1; iFile++) {
    TTArray[iFile] = (TTree*)pisaFileArray[iFile]->Get("T");
    pisaEventArray[iFile] = new PISAEvent();
    branchArray[iFile] = TTArray[iFile]->GetBranch("pisaevent");
    branchArray[iFile]->SetAddress(&pisaEventArray[iFile]);
  } // loop over number of input files

  TTree *T = TTArray[0];
  PISAEvent *pisaevent = pisaEventArray[0];

  // Set up input and output files
  PHString relInFile = relIFile;
  //
  // Open the GEANT Evaluation File
  //
  PHNodeIOManager *ioEval = new PHNodeIOManager(relInFile, PHReadOnly);

  PHNodeIOManager *padFile = 0;
  if(padIFile != "") {
    PHString padInFile = padIFile;
    padFile =  new PHNodeIOManager(padInFile, PHReadOnly);
    cout << "\n padFile = " << padFile << endl;
    size_t mr=6000;
    dPadGhitClusWrapper* dPc1GhitClus = new dPadGhitClusWrapper("dPc1GhitClus",mr);
    PHIODataNode<PHTable>* dPc1GhitClusNode = new PHIODataNode<PHTable>(dPc1GhitClus,"dPc1GhitClus");
    evaNode->addNode(dPc1GhitClusNode);

    size_t mr=6000;
    dPadGhitClusWrapper* dPc2GhitClus = new dPadGhitClusWrapper("dPc2GhitClus",mr);
    PHIODataNode<PHTable>* dPc2GhitClusNode = new PHIODataNode<PHTable>(dPc2GhitClus,"dPc2GhitClus");
    evaNode->addNode(dPc2GhitClusNode);

    size_t mr=6000;
    dPadGhitClusWrapper* dPc3GhitClus = new dPadGhitClusWrapper("dPc3GhitClus",mr);
    PHIODataNode<PHTable>* dPc3GhitClusNode = new PHIODataNode<PHTable>(dPc3GhitClus,"dPc3GhitClus");
    evaNode->addNode(dPc3GhitClusNode);

    gROOT->Macro("padEval.C");
  }

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
