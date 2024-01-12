void Fun4MuPC(Int_t nEvents=0, char *dstout="simDST.root", 
	       char *udstout="simMicroDST.root",
	       char *ndstout="simNanoDST.root",
	       char *filein="PISAEvent.root",
               Int_t magField=1,
               Int_t simVertexFlag=2, Float_t simZ0Vertex=0.0, Float_t simT0Vertex=0.0,
               Float_t simZ0VertexWidth=0.0, Float_t simT0VertexWidth=0.0,
	       Int_t dchReqHits=0, Int_t pc1ReqHits=0, Int_t pc2ReqHits=0, Int_t pc3ReqHits=0,
	       Int_t tofReqHits=0, Int_t emcReqHits=0,
               const Int_t debugFlag=0)
{

  gSystem->Load("libfun4all.so");
  gSystem->Load("libsimreco.so");

  gROOT->ProcessLine(".L pisaToDST_IOManager.C");

  mMfmMT::setMapFileFlag(magField);

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
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST
  rc->set_IntFlag("EMBEDFLAG",0); // embedding is not yet operational in Fun4All (to be done after QM'04)
  //
  // EMCal needs a Run3 number, either positive or negative (not sure if the sign is important); -1 will NOT work
  // However, The UtiHadPID class requires a positive run number
  //
  rc->set_IntFlag("RUNNUMBER",+80312);  // This is the reference run number used in d+Au production
  rc->set_IntFlag("EVALUATIONFLAG", 0); // Requested by EMCal

  //
  // The Drift Chamber initialization code requires that one, and only one, of the following four flags be set to 1
  //
  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",1);          // change if simulating p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU63GEV",0);   // flag for Run4 Au+Au, 63 GeV data certified

  cout << "\n\n   pisaToDST <I>: Dead channel maps correspond to Run4\n" << endl;

  rc->set_FloatFlag("TOFTIMINGRESOLUTION", 0.100);  // this should be moved to the Init method of TofSimrecoo

  //
  //  Flags to abort event if required number of GEANT hits is not present in the subsystem
  //
  rc->set_IntFlag("DCHREQFLAG", dchReqHits);
  rc->set_IntFlag("PC1REQFLAG", pc1ReqHits);
  rc->set_IntFlag("PC2REQFLAG", pc2ReqHits);
  rc->set_IntFlag("PC3REQFLAG", pc3ReqHits);
  rc->set_IntFlag("TOFREQFLAG", tofReqHits);
  rc->set_IntFlag("EMCREQFLAG", emcReqHits); // not yet operational

  //
  // simVertexFlag = 0 (default) means that the BBC Z0 value will be used
  // simVertexFlag = 1 means that the same simZ0Vertex centroid value is used for all events
  // simVertexFlag = 2 means that the Z0 centroid is taken from the PISA event header for each event
  // The centroid values are modified by the Width values which are Gaussian sigma values
  //

  rc->set_IntFlag("SIMVERTEXFLAG",simVertexFlag);  
  //rc->set_FloatFlag("SIMZ0VERTEX",simZ0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",simZ0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",simT0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",simT0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  // se->Verbosity(100);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  //
  // The BbcSimreco must be used even for single particle simulations
  // The Z0 value will be artificially put into the BbcOut data when simVertexFlag is not zero
  // The Z0 then will be retrieved by the VtxReco for both Central and Muon arm simulations
  //
  // As of January 2, 2004 the BBC has uninitialized variable warnings from Valgrind
  //
  SubsysReco *bbcSim = new BbcSimreco("BBC");
  se->registerSubsystem(bbcSim);

  SubsysReco *vtxRec = new VtxReco("VTX");
  se->registerSubsystem(vtxRec);

  MuonUnpackPisa* Muon_pisa = new MuonUnpackPisa();
  se->registerSubsystem(Muon_pisa);
  Muon_pisa->Verbosity(1);
  
  Muon_pisa->set_do_response( true );

  // instanciate the muon reconstruction
  se->registerSubsystem( new MuiooReco() );
  se->registerSubsystem( new MuonDev() );

  // instanciate muon nanoDST module
  PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
  se->registerSubsystem(new MWGReco(MWGcuts));

  //... MuPC ...
  SubsysReco *MuPCSimreco = new MuPCSimreco(1);
  se->registerSubsystem(MuPCSimreco);

  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(inMan);

  ///////////////////////////////////////////
  // OutputManagers Set up functions  (these are still being developed)
  ///////////////////////////////////////////
  DST_IOManager(dstout, se);       // simulated DST
  UDST_IOManager(udstout, se);     // simulated microDST
  NDST_IOManager(ndstout, se);     // simulated nanoDST (may have more than one of these)

  inMan->Verbosity(100);
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

  se->End();

  cout << "Completed reconstruction." << endl;

}
