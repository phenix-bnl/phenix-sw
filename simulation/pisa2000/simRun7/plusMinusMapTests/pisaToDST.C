//
// Version as of September 30, 2006
//
void pisaToDST(Int_t nEvents=10, 
               char *dstout="simDST.root", 
	       char *cntout="simCNT.root",
	       char *hwgout="simHWG.root",
	       char *filein="PISAEvent.root",
               Int_t magField=0,
               Int_t simVertexFlag=2, Float_t simZ0Vertex=0.0, Float_t simT0Vertex=0.0,
               Float_t simZ0VertexWidth=2.0, Float_t simT0VertexWidth=0.05,
	       Int_t dchReqHits=3, Int_t pc1ReqHits=0, Int_t pc2ReqHits=0, Int_t pc3ReqHits=0,
	       Int_t tofReqHits=0, Int_t emcReqHits=0,
               const bool useEvaSimreco=true, const Int_t debugFlag=0)
{

  //
  // Fun4All macro to process PISA hits files into simulated DSTs, uDSTs, and nDSTs
  // Calls subsidiary macro pisaToDST_IOManager.C
  // Version for use at Vanderbilt based HEAD branch builds as of September 13, 2006
  //
  // This version has the Muon Arm code commented out 
  //
  // Original author (Run3 version): Charles F. Maguire (charles.f.maguire@vanderbilt.edu)
  // Revised after Run6 by Chris Pinkenburg for HEAD branch
  //

  cout << "\n\n Recording library path for this job:" << endl;
  gSystem->Exec("echo $LD_LIBRARY_PATH");
  cout << endl << endl;

  //
  // Using 3D++ integrated field map 
  //

  // Input parameters
  // Int_t nEvents   number of input events to process
  // char *dstout    name of output file for simulated DST
  // char *udstout   name of output file for simulated microDST
  // char *ndstout   name of output file for simulated nanoDST
  // char *filein    name of input PISA hits file
  // Int_t magField  magnetic field choice: 1 = Run1/Run2 (3D01) obtained from AFS
  //                                        2 = Run3 (3D03), also works for single coil Run4 (3D+0) obtained from AFS 
  //                                        3 = Run4 (3D++ or 3D--) both coils in same direction obtained from AFS
  //                                            This choice also works for Run5, Run6, ...
  //                                        4 = Run4 (3D+-) outer and inner coil reversed obtained from AFS
  //                                        0 = Field integral file is in local directory (default is Run4 3D++/3D--)
  //

  //
  // Do the softlink for the Central Arm fieldIntegral.dat file according to magField choice
  //
  switch(magField)
    {
    case 0:  // Assume that there is no AFS available
       cout << "\n No AFS connection available; field map file fieldIntegral.dat assumed in local working area" << endl;
       break;
    case 1:  // Run1/Run2 (3D01)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2001/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run1/Run2 (3D01)" << endl;
      break;
    case 2:  // Run3 (3D03)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2003/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run3 (3D03), same as for Run4 (3D+0)" << endl;
      break;
    case 3:  // Run4 (3D++), also Run5, Run6, ...
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2004/fieldIntegral++.dat.run04 fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run4 (3D++), same polarity in both coils" << endl;
      break;
    case 4: // Run4 (3D+-)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2004/fieldIntegral+-.dat.run04 fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for 3D+-, fully reversed polarity in two coils" << endl;
      break;
    default:
      cout << "\n magField value " << magField << " is not recognized; job is aborting" << endl;
      return;
    } // Finish magnetic field map switching

  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");  // New (December 13, 2005) for split of XxxReco from Fun4All infrastructure in PRECO
  gSystem->Load("libsimreco.so");

  gSystem->Load("libHWG.so");

  gROOT->ProcessLine(".L pisaToDST_IOManager.C");

  int iTest = 0;
  if(debugFlag) {
    //
    // Debug pause for doing attach gdb
    //
    cout << "\n debug pause " << endl;
    cin >> iTest;
    cout << "\n iTest = " << iTest << endl;
  } // used for attached debug mode


  ///////////////////////////////////////////
  // recoConsts setup (some of these no longer working in the HEAD branch
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST
  rc->set_IntFlag("EMBEDFLAG",0); // embedding is not yet operational in Fun4All (to be done after QM'04)
  //
  // EMCal needs a Run number, either positive or negative (not sure if the sign is important); -1 will NOT work
  // However, The UtiHadPID class requires a positive run number (could be obsolete after Run3??)
  //
  // rc->set_IntFlag("RUNNUMBER",+80312);  // This is the reference run number used in d+Au production
  // rc->set_IntFlag("RUNNUMBER",+122929);  // This is the reference run number used in Au+Au 63 GeV production (Run4)
  // rc->set_IntFlag("RUNNUMBER",+120496);  // This is the reference run number used in Au+Au 200 GeV production (Run4) 
  // rc->set_IntFlag("RUNNUMBER",+160260);  // This is the reference run number used in 63Cu+63Cu 200 GeV production (Runr54) 
  // rc->set_IntFlag("RUNNUMBER",+161767);  // This is the reference run number used in 63Cu+63Cu 62.4 GeV production (Run5) 
  // rc->set_IntFlag("RUNNUMBER",+163621);  // This is the reference run number used in 63Cu+63Cu 22 GeV production (Run5) 
  rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5) 
  rc->set_IntFlag("EVALUATIONFLAG", 1); // Requested by EMCal

  //
  // The Drift Chamber initialization code requires that one, and only one, of the following five flags be set to 1
  //
  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);          // flag for Run2 p+p events
  rc->set_IntFlag("RUN3PP",0);          // flag for Run3 p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU200GEV",0);  // flag for Run4 Au+Au, 200 GeV data (or pp?)
  rc->set_IntFlag("RUN4AUAU63GEV",0);   // flag for Run4 Au+Au, 63 GeV data (or pp?)
  rc->set_IntFlag("RUN5PP200GEV",1);    // flag for Run5 pp 200 GeV data
  rc->set_IntFlag("AFSABSENT", 1);  // assume AFS is absent as at ACCRE

  cout << "\n\n   pisaToDST <I>: Drift Chamber dead channel map corresponds to Run5 pp at 200 GeV\n" << endl;

  rc->set_FloatFlag("TOFTIMINGRESOLUTION", 0.100);  // this should be moved to the Init method of TofSimrecoo


  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  // se->Verbosity(100);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  SubsysReco *head = new HeadSimreco();
  se->registerSubsystem(head);
  SubsysReco *trig = new TrigSimreco();
  se->registerSubsystem(trig);

  SubsysReco *bbcSim = new BbcSimreco("BBC");
  se->registerSubsystem(bbcSim);

  VtxT0Simreco *vtxt0 = new VtxT0Simreco();
  vtxt0->UseVtx(VtxT0Simreco::PISA); // Use Pisa Vertex
  //vtxt0->UseVtx(VtxT0Simreco::FIXED); // Use Own Vertex
  vtxt0->ZVertexSigma(simZ0VertexWidth);
  // vtxt0->T0(double t0); // defaults to 0 so probably no one needs this
  vtxt0->T0Sigma(simT0Vertex);
  se->registerSubsystem(vtxt0);

  SubsysReco *padSim = new PadSimreco("PAD");
  se->registerSubsystem(padSim);

  //
  // The VtxReco works unchanged for both real and simulation events
  //
  SubsysReco *vtxRec = new VtxReco("VTX");
  se->registerSubsystem(vtxRec);

  //
  // The T0Reco works unchanged for both real and simulation events
  //
  SubsysReco *t0Rec = new T0Reco();
  se->registerSubsystem(t0Rec);

  //
  // As of January 2, 2004 the Dch has uninitialized variable warnings from Valgrind
  // There are also log file output warning messages
  //
  SubsysReco *dchSim = new DchSimreco("DCH");
  se->registerSubsystem(dchSim);

  SubsysReco *tecSim = new TecSimreco("TEC");
  se->registerSubsystem(tecSim);
  
  SubsysReco *tofSim = new TofSimreco("TOF");
  se->registerSubsystem(tofSim);

  //
  // As of January 2, 2004 the RICH has uninitialized variable warnings from Valgrind
  // There are also log file output warning messages
  //
  SubsysReco *crkSim = new CrkSimreco("CRK");
  se->registerSubsystem(crkSim);

  //
  // Aerogel subsystem as per e-mail from Narumi Kurihara on May 13, 2005
  //
  //   SubsysReco *accSim = new AccSimreco("ACC");
  //   se->registerSubsystem(accSim);

  //   SubsysReco *accReco = new AccReco();
  //   se->registerSubsystem(accReco);

  //
  //
  // EMCal uses the real data class
  //
  SubsysReco *emcRec = new EmcReco3();
  se->registerSubsystem(emcRec);

  //
  // The CglReco works unchanged for both real and simulation events
  //
  SubsysReco *cglRec = new CglReco("CGL");
  se->registerSubsystem(cglRec);

  //Aerogel cluster  (Needs to be after cglRec)
  SubsysReco *aerocl = new AccclusterReco();
  se->registerSubsystem(aerocl);
  
  //SubsysReco *tecpidSim = new TecpidSimreco("TECPID");
  //se->registerSubsystem(tecpidSim);

  //
  //  This is the class which makes the RICH Ring data structure ("CrkRingMicro")
  //
  SubsysReco *ring    = new RingReco();
  se->registerSubsystem(ring);

  //
  //  This is the class which makes the Central Tracks nanoDST output
  //
  // SubsysReco *central = new CentraltrackReco(11); // Run3 value
  SubsysReco *central = new CentraltrackReco(18); // Run4 value
  se->registerSubsystem(central);

  //
  //  This is the class which makes the GlobalEvent data on the nanoDST output
  //
  SubsysReco *global = new GlobalReco(); 
  se->registerSubsystem(global);

  //
  // This is the class which checks for charged particles going into EMCal
  //
  SubsysReco *veto = new ChargedvetoReco();
  se->registerSubsystem(veto);

  PHInclusiveNanoCuts *HWGcuts = new HWGInclusiveNanoCutsv2();
  SubsysReco *HWG     = new WGReco(HWGcuts,"HWGCentralTrack");
  se->registerSubsystem(HWG);

  //added the DC based global evaluation module
  //SubsysReco *mceval = new McEvalSimreco();
  //se->registerSubsystem(mceval);

  //
  // For Central Arm only simulations it is faster to comment out the Muon Arm code below
  //
  
  /*
  // Additions for Muon Arm
  // Mutoo pisa reader, response, reconstruction and NDST.
  //
  MuonUnpackPisa* Muon_pisa = new MuonUnpackPisa();
  se->registerSubsystem(Muon_pisa);
  Muon_pisa->Verbosity(1);
  
  Muon_pisa->set_do_response( true );

  //   
  //   	possibly adds a filter here to only select "good" MC events
  //   	possible filters are MC_SHALLOW_SHALLOW|MC_DEEP_SHALLOW|MC_DEEP_DEE
  //  
  //   MuonTrigFilter *muon_mc_filter = new MuonTrigFilter( "muon_mc_filter", MuonTrigFilter::MC_SHALLOW_SHALLOW );  
  //   muon_mc_filter->set_action(MuonTrigFilter::ABORT_EVENT);
  //   se->registerSubsystem( muon_mc_filter );
  
  // instantiate the muon reconstruction
  se->registerSubsystem( new MuiooReco() ); 
  se->registerSubsystem( new MuonDev() );
  
  // instantiate muon nanoDST module
  PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
  se->registerSubsystem(new MWGReco(MWGcuts));    

  //
  // End of Muon Arm code
  //
  */
  
  //
  // Special simulation evaluation class (not normally activiated)
  //
  SubsysReco *evaSim = 0;
  if(useEvaSimreco) {
    evaSim = new EvaSimreco("EVA", 2);
    se->registerSubsystem(evaSim);
  }

  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(inMan);

  ///////////////////////////////////////////
  // OutputManagers Set up functions  (these are still being developed)
  ///////////////////////////////////////////
  DST_IOManager(dstout, se);     // simulated DST
  CNT_IOManager(cntout, se);     // simulated CNT 
  HWG_IOManager(hwgout, se);     // simulated HWG 

  //inMan->Verbosity(100);
  int kEvents = nEvents;
  TFile f(filein);
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
  se->fileopen(inMan->Name(),filein);

  gBenchmark->Start("eventLoop");   // start the timing clock
  se->run(kEvents);                 // process input events
  gBenchmark->Show("eventLoop");    // complete the timing clock

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  se->End();

  if(evaSim) {
    //
    // Special simulation evaluation object, not normally activated
    //
    delete evaSim;
  }

  //
  // If you do not see this message, the job failed
  //
  cout << "Completed reconstruction." << endl;

}
