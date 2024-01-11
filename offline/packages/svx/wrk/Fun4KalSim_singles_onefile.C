//
// This is a macro to run Kalman Fit on simulated events
// with SVX subsystem ON. 
//
void Fun4KalSim_singles_onefile(
 int nEvents=10,
 char *inputfile =  "/phenix/subsys/vtx/lebedev/newsim/pisa/PISAEvent_php_10kevts_000.root",
 char *mergefile =  "/phenix/subsys/vtx/julyaf/pisafiles/dummy.root",
 char *outputfile = "/phenix/subsys/vtx/julyaf/simdst/simDST_test.root",
 Int_t magField=0,
 Int_t simVertexFlag=2, Float_t simZ0Vertex=0.0, Float_t simT0Vertex=0.0,
 Float_t simZ0VertexWidth=2.0, Float_t simT0VertexWidth=0.05
 )
{
  //gSystem->Load("/phenix/u/workarea/lebedev/andrewb/KalFit2/install/lib/libsvx.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");  // New (December 13, 2005) for split of XxxReco from Fun4All infrastructure in PRECO
  gSystem->Load("libsimreco.so");
  gSystem->Load("libKalFit.so");
  gSystem->Load("libsvxcgl.so");


  switch(magField)
    {
    case 0:  // Assume that there is no AFS available
       cout << "\n No AFS connection available; field map file fieldIntegral.dat assumed in local working area" << endl;
       break;
    case 1:  // Run1/Run2 (3D01)
      gSystem->Exec("ln -fs /afs/rhic.bnl.gov/phenix/software/calibration/run2001/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run1/Run2 (3D01)" << endl;
      break;
    case 2:  // Run3 (3D03)
      gSystem->Exec("ln -fs /afs/rhic.bnl.gov/phenix/software/calibration/run2003/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run3 (3D03), same as for Run4 (3D+0)" << endl;
      break;
    case 3:  // Run4 (3D++), also Run5, Run6, ...
      gSystem->Exec("ln -fs /afs/rhic.bnl.gov/phenix/software/calibration/run2004/fieldIntegral++.dat.run04 fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run4 (3D++), same polarity in both coils" << endl;
      break;
    case 4: // Run7 (3D+-), as of Run7
      gSystem->Exec("ln -fs /afs/rhic.bnl.gov/phenix/software/calibration/run2007/fieldIntegral+-.dat.run07 fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for 3D+-, fully reversed polarity in two coils" << endl;
      break;
    default:
      cout << "\n magField value " << magField << " is not recognized; job is aborting" << endl;
      return;
    } // Finish magnetic field map switching

  // recoConsts setup
  recoConsts *rc = recoConsts::instance();
  
  //rc->set_FloatFlag("ALPHATESTFACTOR", -1);  // Use -1 to apply corrections which ARE correct now...
  //rc->set_FloatFlag("DVWESTSCALE", 1.0010);
  //rc->set_FloatFlag("DVEASTSCALE", 1.0030);

  // Kalman Flags
  rc->set_FloatFlag("KALPMIN",0.400);
  rc->set_IntFlag("KALFILTERDCUV",1);
  rc->set_IntFlag("KALFIT",1);
  rc->set_IntFlag("KALTESTNTUPLE",0);
  rc->set_IntFlag("KALREGENDERIV",1);
  rc->set_IntFlag("KALUSEDCHX1X2",1);
  rc->set_IntFlag("KALUSEDCHUV",1);
  rc->set_IntFlag("KALUSEPC1",1);
  rc->set_IntFlag("KALUSEPC2",0);
  rc->set_IntFlag("KALUSEPC3",0);
  rc->set_IntFlag("KALUSETEC",0);
  rc->set_IntFlag("KALUSETOF",0);
  rc->set_IntFlag("KALUSEEMC",0);
  rc->set_IntFlag("KALUSESVX",1);
  rc->set_IntFlag("KALSVXASSOC",0); // set to 0 if you want to use cgl results for svx

//  rc->set_FloatFlag("PHFIELDMAPSCALE",1.0117);
  rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST
  rc->set_IntFlag("EMBEDFLAG",0);
  // rc->set_IntFlag("RUNNUMBER",+80312);  // This is the reference run number used in d+Au production
  // rc->set_IntFlag("RUNNUMBER",+122929);  // This is the reference run number used in Au+Au 63 GeV production (Run4)
  // rc->set_IntFlag("RUNNUMBER",+120496);  // This is the reference run number used in Au+Au 200 GeV production (Run4) 
  // rc->set_IntFlag("RUNNUMBER",+160260);  // This is the reference run number used in 63Cu+63Cu 200 GeV production (Run5) 
  // rc->set_IntFlag("RUNNUMBER",+161767);  // This is the reference run number used in 63Cu+63Cu 62.4 GeV production (Run5) 
  // rc->set_IntFlag("RUNNUMBER",+163621);  // This is the reference run number used in 63Cu+63Cu 22 GeV production (Run5) 
  rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5) 

  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);          // flag for Run2 p+p events
  rc->set_IntFlag("RUN3PP",0);          // flag for Run3 p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU200GEV",0);  // flag for Run4 Au+Au, 200 GeV data (or pp?)
  rc->set_IntFlag("RUN4AUAU63GEV",0);   // flag for Run4 Au+Au, 63 GeV data (or pp?)
  rc->set_IntFlag("RUN5PP200GEV",1);    // flag for Run5 pp 200 GeV data

  // simVertexFlag = 0 (default) means that the BBC Z0 value will be used
  // simVertexFlag = 1 means that the same simZ0Vertex centroid value is used for all events
  // simVertexFlag = 2 means that the Z0 centroid is taken from the PISA event header for each event
  // The centroid values are modified by the Width values which are Gaussian sigma values
  //
  rc->set_IntFlag("SIMVERTEXFLAG",simVertexFlag);
  rc->set_FloatFlag("SIMZ0VERTEX",simZ0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",simZ0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",simT0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",simT0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2

  rc->set_FloatFlag("TOFTIMINGRESOLUTION", 0.100);

  rc->set_IntFlag("SVXACTIVE",1);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(1);

 // Register SubSystems 

  SubsysReco *head = new HeadSimreco();
  se->registerSubsystem(head);

  SubsysReco *trig = new TrigSimreco();
  se->registerSubsystem(trig);

  SubsysReco *bbc     = new BbcSimreco();
  se->registerSubsystem(bbc);

  // pisa is used as an input vertex.
  // it overwrites the contents of the BBC out node.
  VtxSimreco* vtx_sim = new VtxSimreco();
  T0Simreco*  t0_sim  = new T0Simreco();
  vtx_sim->UseVtx( VtxSimreco::PISA );
  //vtx_sim->ZVertexSigma(0.5);
  //vtx_sim->T0Sigma(0.04);
  vtx_sim->ZVertexSigma(0.0);
  t0_sim->T0Sigma(0.0);
  se->registerSubsystem( vtx_sim );
  se->registerSubsystem( t0_sim );

  SubsysReco *pad     = new PadSimreco();
  se->registerSubsystem(pad);

  SubsysReco *vtx     = new VtxReco();
  se->registerSubsystem(vtx);

  se->registerSubsystem( new T0Reco() );

  SubsysReco *crk     = new CrkSimreco();
  se->registerSubsystem(crk);

  SubsysReco *tof     = new TofSimreco();
  se->registerSubsystem(tof);

//  SubsysReco *tec     = new TecSimreco();
//  se->registerSubsystem(tec);

  SubsysReco *emc     = new EmcReco3();
  se->registerSubsystem(emc);


  // register first 
  SubsysReco *svxpar = new SvxParManager();
  svxpar->Verbosity(1);
  (dynamic_cast<SvxParManager*>svxpar)->set_ReadGeoParFromFile(1);  // read parameters from ascii file
  se->registerSubsystem(svxpar);


  SubsysReco *svxsim     = new SvxSimulator();
  //(dynamic_cast<SvxSimulator*>svxsim)->set_ReadParFromFile(1);  // read parameters from ascii file
//  (dynamic_cast<SvxSimulator*>svxsim)->set_StripixelZeroSup(0);  // noise hit threshold
//  (dynamic_cast<SvxSimulator*>svxsim)->set_StripixelNoise(5.0);
  svxsim->Verbosity(1);
  se->registerSubsystem(svxsim);

  SubsysReco *svxreco     = new SvxReco();
  //(dynamic_cast<SvxReco*>svxreco)->set_ReadParFromFile(1);  // read parameters from ascii file
  svxreco->Verbosity(1);
  se->registerSubsystem(svxreco);
    
  SubsysReco *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  se->registerSubsystem(svxvtxseedfinder);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);
  se->registerSubsystem( svxstandalone );  

  SubsysReco *svxprimvtxfinder = new SvxPrimVertexFinder();
  se->registerSubsystem(svxprimvtxfinder);

  SubsysReco *dch     = new DchSimreco();
  se->registerSubsystem(dch);

  SubsysReco *cgl     = new CglReco();
  se->registerSubsystem(cgl);

  SubsysReco *ring    = new RingReco();
  se->registerSubsystem(ring);
  
  SubsysReco *svxcgl     = new SvxCglReco();
  se->registerSubsystem(svxcgl);
  
  
  SubsysReco *kal     = new KalFitReco();
// (dynamic_cast<KalFitReco*>kal)->SetErrorScaleFactor(0, 1.0);  
// (dynamic_cast<KalFitReco*>kal)->SetErrorScaleFactor(1, 1.0);  
// (dynamic_cast<KalFitReco*>kal)->SetErrorScaleFactor(2, 1.0);  
// (dynamic_cast<KalFitReco*>kal)->SetErrorScaleFactor(3, 1.0);  
//  kal->Verbosity(1);
  se->registerSubsystem(kal);
  
  
  SubsysReco *central = new CentraltrackReco(22);
  se->registerSubsystem(central);
  
  SubsysReco *global = new GlobalReco();
  se->registerSubsystem(global);

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  // Input Manager
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(inMan);
                                                                                                                             
  // Output Manager
  Fun4AllDstOutputManager *nsimDST  = new Fun4AllDstOutputManager("SIMCNT", outputfile);

//  nsimDST->AddNode("RunHeader");
//  nsimDST->AddNode("EventHeader");

  nsimDST->AddNode("fkin");
//  nsimDST->AddNode("pythia");
//  nsimDST->AddNode("primary");
//  nsimDST->AddNode("header");
//  nsimDST->AddNode("dcghit");
//  nsimDST->AddNode("crkghit");
//  nsimDST->AddNode("BbcOut");

  nsimDST->AddNode("PHCentralTrack");
  nsimDST->AddNode("PHTrackOut");
//  nsimDST->AddNode("McSingle");
  nsimDST->AddNode("PHGlobal");
  nsimDST->AddNode("CglTrack");
  nsimDST->AddNode("DchTrack");
//  nsimDST->AddNode("dDchGhitHits");
//  nsimDST->AddNode("dDchHit");
//  nsimDST->AddNode("dDchTracks");
  nsimDST->AddNode("Pc1Cluster");
  nsimDST->AddNode("Pc2Cluster");
  nsimDST->AddNode("Pc3Cluster");
  nsimDST->AddNode("emcClusterContainer");
  nsimDST->AddNode("CrkRing");
  nsimDST->AddNode("VtxOut");
//  nsimDST->AddNode("CrkHit");

  nsimDST->AddNode("KalFitOut");
  nsimDST->AddNode("SvxPisaHit");
  nsimDST->AddNode("SvxGhitList");
  nsimDST->AddNode("SvxRawhitList");
  nsimDST->AddNode("SvxGhitRawhitList");
  nsimDST->AddNode("SvxClusterList");
  nsimDST->AddNode("SvxRawhitClusterList");
  nsimDST->AddNode("SvxGhitClusterList");
  nsimDST->AddNode("SvxSegmentList");

  se->registerOutputManager(nsimDST);
                                                                                                                             
  inMan->Verbosity(0);
  int kEvents = nEvents;
  TFile f(inputfile);
  int jEvents = T->GetEntries();
  if(kEvents==0) {
    kEvents = jEvents;
    cout << "\n\n  Event processing will be for " << jEvents << " events total in this input file." << endl;
  }
  else {
    if(kEvents>jEvents) {
      cout << "\n\n  User requests " << kEvents << " input events, but";
      cout << " the input file contains only " << jEvents <<" events." << endl;
      cout << "  Event processing will be for only " << jEvents << " events." << endl;
      cout << "\n  Since in Simulation you are always supposed to know your input conditions perfectly," << endl;
      cout << "  you might want to reconsider what you are doing." << endl << endl;
      kEvents = jEvents;
    }
  }
  se->fileopen(inMan->Name(),inputfile);
                                                                                                                             
// do this for merging
//  (dynamic_cast<Fun4AllPisaInputManager*>inMan)->pisaMergeFileOpen( "/phenix/subsys/vtx/lebedev/blind/pisafiles/PISAEvent_bbbar2e_00_10kevts_r.root" );
//  (dynamic_cast<Fun4AllPisaInputManager*>inMan)->pisaMergeFileOpen( mergefile );

  gBenchmark->Start("eventLoop");   // start the timing clock
  se->run(kEvents);                 // process input events
  gBenchmark->Show("eventLoop");    // complete the timing clock
                                                                                                                             
  se->End();

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Successfully Completed Analysis." << endl;
}



