//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void tofChkPisa(const Int_t maxEvents=1, const char *pisaIFile="pisa99.root") {

  Int_t eventNumber = 0;

  // Executing initialization and parameter macros
  //gROOT->Macro("tofgdigiini.C");
  //gROOT->Macro("tofgdigipar.C");

  // Initialization macro written by the Pyrite GUI for PHOOL   
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libtof.so");

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
  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);
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

  // Set up the modules
  mTofSetGeoModule* mTofSetGeo = new mTofSetGeoModule;
  mTofGhitGdigiModule* mTofGhitGdigi = new mTofGhitGdigiModule;
  mTofGdigiRecModule* mTofGdigiRec = new mTofGdigiRecModule;

  // Initialize the tables
  size_t mr=1;
  dTofGeoParWrapper* dTofGeoPar = new dTofGeoParWrapper("dTofGeoPar",mr);
  PHIODataNode<PHTable>* dTofGeoParNode = new PHIODataNode<PHTable>(dTofGeoPar,"dTofGeoPar");
  parNode->addNode(dTofGeoParNode);

  size_t mr=1500;
  dTofGeoWrapper* dTofGeo = new dTofGeoWrapper("dTofGeo",mr);
  PHIODataNode<PHTable>* dTofGeoNode = new PHIODataNode<PHTable>(dTofGeo,"dTofGeo");
  parNode->addNode(dTofGeoNode);

  size_t mr=1500;
  dTofGdigiWrapper* dTofGdigi = new dTofGdigiWrapper("dTofGdigi",mr);
  PHIODataNode<PHTable>* dTofGdigiNode = new PHIODataNode<PHTable>(dTofGdigi,"dTofGdigi");
  tofNode->addNode(dTofGdigiNode);

  size_t mr=1500;
  dTofReconstructedWrapper* dTofReconstructed = new dTofReconstructedWrapper("dTofReconstructed",mr);
  PHIODataNode<PHTable>* dTofReconstructedNode = new PHIODataNode<PHTable>(dTofReconstructed,"dTofReconstructed");
  dstNode->addNode(dTofReconstructedNode);

  size_t mr=1500;
  dTofGhitGdigiWrapper* dTofGhitGdigi = new dTofGhitGdigiWrapper("dTofGhitGdigi",mr);
  PHIODataNode<PHTable>* dTofGhitGdigiNode = new PHIODataNode<PHTable>(dTofGhitGdigi,"dTofGhitGdigi");
  tofNode->addNode(dTofGhitGdigiNode);

  size_t mr=1500;
  dTofGdigiRecWrapper* dTofGdigiRec = new dTofGdigiRecWrapper("dTofGdigiRec",mr);
  PHIODataNode<PHTable>* dTofGdigiRecNode = new PHIODataNode<PHTable>(dTofGdigiRec,"dTofGdigiRec");
  tofNode->addNode(dTofGdigiRecNode);

  size_t mr=1;
  dTofPerfParWrapper* dTofPerfPar = new dTofPerfParWrapper("dTofPerfPar",mr);
  PHIODataNode<PHTable>* dTofPerfParNode = new PHIODataNode<PHTable>(dTofPerfPar,"dTofPerfPar");
  parNode->addNode(dTofPerfParNode);

  size_t mr=4500;
  tofghitWrapper* tofghit = new tofghitWrapper("tofghit",mr);
  PHIODataNode<PHTable>* tofghitNode = new PHIODataNode<PHTable>(tofghit,"tofghit");
  geaNode->addNode(tofghitNode);


  PhRootHistogramFactory::buildFactory();

  // Parameter set-up
  // Setting dTofGeoPar Parameters
  size_t nrc=1;
  dTofGeoPar->SetRowCount(nrc);
  dTofGeoPar->set_rslat(0,3.2905);
  dTofGeoPar->set_slat_width(0,1.52654);
  dTofGeoPar->set_scintz(0,0,69.631);
  dTofGeoPar->set_scintz(1,0,11.606);
  dTofGeoPar->set_scintz(2,0,56.609);
  dTofGeoPar->set_rpos(0,0,503.0);
  dTofGeoPar->set_rpos(1,0,503.0);
  dTofGeoPar->set_rpos(2,0,503.0);
  dTofGeoPar->set_rpos(3,0,503.0);
  dTofGeoPar->set_rpos(4,0,503.0);
  dTofGeoPar->set_rpos(5,0,503.0);
  dTofGeoPar->set_rpos(6,0,503.0);
  dTofGeoPar->set_rpos(7,0,503.0);
  dTofGeoPar->set_rpos(8,0,503.0);
  dTofGeoPar->set_rpos(9,0,503.0);
  dTofGeoPar->set_rpos(10,0,0.0);
  dTofGeoPar->set_phi(0,0,180.0);
  dTofGeoPar->set_phi(1,0,180.0);
  dTofGeoPar->set_phi(2,0,180.0);
  dTofGeoPar->set_phi(3,0,180.0);
  dTofGeoPar->set_phi(4,0,180.0);
  dTofGeoPar->set_phi(5,0,180.0);
  dTofGeoPar->set_phi(6,0,180.0);
  dTofGeoPar->set_phi(7,0,180.0);
  dTofGeoPar->set_phi(8,0,202.5);
  dTofGeoPar->set_phi(9,0,202.5);
  dTofGeoPar->set_phi(10,0,0.0);
  dTofGeoPar->set_zpos(0,0,-170.972);
  dTofGeoPar->set_zpos(1,0,-122.123);
  dTofGeoPar->set_zpos(2,0,-73.2739);
  dTofGeoPar->set_zpos(3,0,-24.4246);
  dTofGeoPar->set_zpos(4,0,24.4246);
  dTofGeoPar->set_zpos(5,0,73.2739);
  dTofGeoPar->set_zpos(6,0,122.123);
  dTofGeoPar->set_zpos(7,0,170.972);
  dTofGeoPar->set_zpos(8,0,-24.4246);
  dTofGeoPar->set_zpos(9,0,24.4246);
  dTofGeoPar->set_zpos(10,0,0.0);

  // Setting dTofPerfPar Parameters
  size_t nrc=1;
  dTofPerfPar->SetRowCount(nrc);
  dTofPerfPar->set_verbose(0,0);

  mainIter.cd();

  // Histogram Setting
  gROOT->cd();
  Int_t itof,ntofrec;
  TH1F *tofhist1 = new TH1F("tofhist1","TOF hits/event",100,0.0,150.0);
  TH1F *tofhist2 = new TH1F("tofhist2","TOF hits/slatid",100,0.0,1100.0);
  TH1F *tofhist3 = new TH1F("tofhist3","TOF time-of-flight",100,0.0,80.0);
  TH1F *tofhist4 = new TH1F("tofhist4","TOF energy loss",100,0.0,0.05);
  TH2F *tofhist5 = 
    new TH2F("tofhist5","TOF Y-Z position",80, -200, 200, 80, -300, 100);
  TH1F *tofhisty = new TH1F("tofhisty","TOF Y position",80, -300, 100);
  TH1F *tofhistz = new TH1F("tofhistz","TOF Z position",80, -200, 200);

  TH1F *tofhist6 = new TH1F("tofhist6","TOF momentum",100, 0.0, 3.0);
  TH1F *tofhist7 = new TH1F("tofhist7","TOF pathlength",100, 400, 900);
  TH1F *tofhist8 = new TH1F("tofhist8","Particle on TOF",40, 0, 20);

  if (verbose>5) printf("Entering event loop.\n");
  while (kevent < maxEvents) {

    // Fetch a PISA99 event
    eventNumber = kevent + 1;
    pisarun->GetOneEvent(pisaevent,&kevent,T);

    mainIter.cd();

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    KinGetGEA(topNode);
    TofGetGEA(topNode);

    printf("Calling first event only modules.\n");
    if (eventNumber == 1) {

      if (verbose>10) printf("Calling mTofSetGeo\n");
      mTofSetGeo->event(topNode);
 
    }

    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling mTofGhitGdigi\n");
     mTofGhitGdigi->event(topNode);

    if (verbose>10) printf("Calling mTofGdigiRec\n");
     mTofGdigiRec->event(topNode);
     dTofReconstructed->Show();

    // Histogram Filling
    tofhist1->Fill((Float_t) dTofGdigi->RowCount());
    ntofrec = (Int_t) dTofGdigi->RowCount();
    itof = 0;
    while (itof < ntofrec) {
      tofhist2->Fill((Float_t) dTofGdigi->get_slatid(itof));
      tofhist3->Fill((Float_t) dTofGdigi->get_tof(itof));
      tofhist4->Fill((Float_t) dTofGdigi->get_eloss(itof));
      tofhist5->Fill((Float_t) dTofGdigi->get_pos_m(2,itof), 
                     (Float_t) dTofGdigi->get_pos_m(1,itof));
      tofhisty->Fill((Float_t) dTofGdigi->get_pos_m(1,itof));
      tofhistz->Fill((Float_t) dTofGdigi->get_pos_m(2,itof));
      Float_t ptot = sqrt((Double_t)dTofGdigi->get_p_m(0,itof)**2 +
			  (Double_t)dTofGdigi->get_p_m(1,itof)**2 +
			  (Double_t)dTofGdigi->get_p_m(2,itof)**2);
      tofhist6->Fill((Float_t) ptot);
      tofhist7->Fill((Float_t) (Float_t) dTofGdigi->get_path(itof));
      tofhist8->Fill((Float_t) dTofGdigi->get_partl(itof));
      itof++;
    }

    // Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("GEA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TOF")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

    pisarun->HitsClear();

  }

  // Take out the garbage

  pisaFile->Close();


}

