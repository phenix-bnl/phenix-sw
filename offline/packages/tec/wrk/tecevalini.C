//************************************************************
// Initialization macro written by the Pyrite GUI for PHOOL   
//************************************************************

{

// Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");
  gSystem->Load("libPdbCal.so");

// Loading subsystem libraries
  gSystem->Load("libcgl_tables.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libtec_tables.so");
  gSystem->Load("libtec.so");
//  gSystem->Load("/phenix/workarea/lebedev/tectest/install/lib/libtec_tables.so");
//  gSystem->Load("/phenix/workarea/lebedev/tectest/install/lib/libtec.so");

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
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
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

  const Int_t maxFiles = 3;

  if(kFiles<1 || kFiles>maxFiles) {
    cerr << "\n Illegal value for kFiles = " << kFiles << endl;
    exit(1);
  }

  TFile *pisaFileArray[maxFiles];
  TTree *TTArray[maxFiles];
  TBranch *branchArray[maxFiles];
  PISAEvent *pisaEventArray[maxFiles];
  pisaFileArray[0] = new TFile(fname);
  cout << "\n pisaMergeFileOpen.C <I>: opened primary file " << fname << endl;
  if(kFiles > 1) {
    pisaFileArray[1] = new TFile(gname);
    cout << "\n pisaMergeFileOpen.C <I>: opened first merge file " << gname << endl;
  }
  if(kFiles > 2) {
    pisaFileArray[2] = new TFile(hname);
    cout << "\n pisaMergeFileOpen.C <I>: opened second merge file " << hname << endl;
  }
  for(Int_t iFile=0; iFile<kFiles; iFile++) {
    TTArray[iFile] = (TTree*)pisaFileArray[iFile]->Get("T");
    pisaEventArray[iFile] = new PISAEvent();
    branchArray[iFile] = TTArray[iFile]->GetBranch("pisaevent");
    branchArray[iFile]->SetAddress(&pisaEventArray[iFile]);
  } // loop over number of input files
  PISARun *pisarun = new PISARun();  // create a run control instance
  Int_t nevent = TTArray[0]->GetEntries();

// Set up the modules
  mTecAlignModule* mTecAlign = new mTecAlignModule;
  mTecSlowSimModule* mTecSlowSim = new mTecSlowSimModule;
  mTecRemapModule* mTecRemap = new mTecRemapModule;
  mTecCalibModule* mTecCalib = new mTecCalibModule;
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule;
  mTecCalibrateModule* mTecCalibrate = new mTecCalibrateModule;
  mTecTrackEvalModule* mTecTrackEval = new mTecTrackEvalModule;

// Initialize the tables

  size_t mr=1;
  dTecGeomWrapper* dTecGeom = new dTecGeomWrapper("dTecGeom",mr);
  PHIODataNode<PHTable>* dTecGeomNode = new PHIODataNode<PHTable>(dTecGeom,"dTecGeom");
  parNode->addNode(dTecGeomNode);

  size_t mr=7500;
  dTecRawWrapper* dTecRaw = new dTecRawWrapper("dTecRaw",mr);
  PHIODataNode<PHTable>* dTecRawNode = new PHIODataNode<PHTable>(dTecRaw,"dTecRaw");
  tecNode->addNode(dTecRawNode);

  size_t mr=7500;
  dTecVectorWrapper* dTecVector = new dTecVectorWrapper("dTecVector",mr);
  PHIODataNode<PHTable>* dTecVectorNode = new PHIODataNode<PHTable>(dTecVector,"dTecVector");
  tecNode->addNode(dTecVectorNode);

  size_t mr=3000;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  dstNode->addNode(dTecTrackNode);

  size_t mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  size_t mr=3000;
  dTecVectTrackWrapper* dTecVectTrack = new dTecVectTrackWrapper("dTecVectTrack",mr);
  PHIODataNode<PHTable>* dTecVectTrackNode = new PHIODataNode<PHTable>(dTecVectTrack,"dTecVectTrack");
  tecNode->addNode(dTecVectTrackNode);

  size_t mr=7500;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  tecNode->addNode(dTecCalibNode);

  size_t mr=1;
  dTecCalibParWrapper* dTecCalibPar = new dTecCalibParWrapper("dTecCalibPar",mr);
  PHIODataNode<PHTable>* dTecCalibParNode = new PHIODataNode<PHTable>(dTecCalibPar,"dTecCalibPar");
  parNode->addNode(dTecCalibParNode);

  size_t mr=1;
  dTecRemapParWrapper* dTecRemapPar = new dTecRemapParWrapper("dTecRemapPar",mr);
  PHIODataNode<PHTable>* dTecRemapParNode = new PHIODataNode<PHTable>(dTecRemapPar,"dTecRemapPar");
  parNode->addNode(dTecRemapParNode);

  size_t mr=1;
  dTecAlignParWrapper* dTecAlignPar = new dTecAlignParWrapper("dTecAlignPar",mr);
  PHIODataNode<PHTable>* dTecAlignParNode = new PHIODataNode<PHTable>(dTecAlignPar,"dTecAlignPar");
  parNode->addNode(dTecAlignParNode);

  size_t mr=45120;
  dTecWireWrapper* dTecWire = new dTecWireWrapper("dTecWire",mr);
  PHIODataNode<PHTable>* dTecWireNode = new PHIODataNode<PHTable>(dTecWire,"dTecWire");
  parNode->addNode(dTecWireNode);

  size_t mr=1;
  dTecUnpackParWrapper* dTecUnpackPar = new dTecUnpackParWrapper("dTecUnpackPar",mr);
  PHIODataNode<PHTable>* dTecUnpackParNode = new PHIODataNode<PHTable>(dTecUnpackPar,"dTecUnpackPar");
  parNode->addNode(dTecUnpackParNode);

  size_t mr=8000;
  dTecDcmDataWrapper* dTecDcmData = new dTecDcmDataWrapper("dTecDcmData",mr);
  PHIODataNode<PHTable>* dTecDcmDataNode = new PHIODataNode<PHTable>(dTecDcmData,"dTecDcmData");
  dcmNode->addNode(dTecDcmDataNode);

  size_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  size_t mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

  size_t mr=1;
  dTecHoughTrackParWrapper* dTecHoughTrackPar = new dTecHoughTrackParWrapper("dTecHoughTrackPar",mr);
  PHIODataNode<PHTable>* dTecHoughTrackParNode = new PHIODataNode<PHTable>(dTecHoughTrackPar,"dTecHoughTrackPar");
  parNode->addNode(dTecHoughTrackParNode);

  size_t mr=4500;
  tecghitWrapper* tecghit = new tecghitWrapper("tecghit",mr);
  PHIODataNode<PHTable>* tecghitNode = new PHIODataNode<PHTable>(tecghit,"tecghit");
  geaNode->addNode(tecghitNode);

  PhRootHistogramFactory::buildFactory();

}

