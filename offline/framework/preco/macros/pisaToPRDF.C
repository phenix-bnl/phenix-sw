void pisaToPRDF(
  Int_t nEvents=10, 
  char *prdfOut="sim.prdf", 
  char *filein="/phenix/data11/rhphemds/run3/macroTest/hji135evt_521auauminb200sq01_20Mar2004.root",
  Int_t dchReqHits=0, Int_t pc1ReqHits=0, Int_t pc2ReqHits=0, Int_t pc3ReqHits=0,
  Int_t tofReqHits=0, Int_t emcReqHits=0,
  const Int_t debugFlag=0)
{

  //
  // Fun4All macro to process PISA hits files into simulated PRDF
  // Modeled after Run2 PRECO macro PISAtoPRDF.C
  //

  //
  // Original author: Charles F. Maguire (charles.f.maguire@vanderbilt.edu)
  // Creation date: March 21, 2004
  // Last revision: March 21, 2004  (Initial Version, NOT YET OPERATIONAL)
  //

  // Input parameters
  // Int_t nEvents   number of input events to process
  // char *prdfout   name of output file for simulated PRDF

  gSystem->Load("libfun4all.so");
  gSystem->Load("libsimreco.so");

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
  rc->set_IntFlag("SIMULATIONFLAG",3); // 2 means PISA-To-DST; 3 means PISA-to-PRDF; PRDF-to-DST is not operational 
  rc->set_IntFlag("EMBEDFLAG",0); // embedding is not yet operational in Fun4All (to be done after QM'04)
  //
  // EMCal needs a Run3 number, either positive or negative (not sure if the sign is important); -1 will NOT work
  // However, The UtiHadPID class requires a positive run number
  //
  rc->set_IntFlag("RUNNUMBER",+80312);  // This is the reference run number used in d+Au production
  rc->set_IntFlag("EVALUATIONFLAG", 1); // Requested by EMCal

  //
  // The Drift Chamber initialization code requires that one of the following three flags be set to 1
  //
  rc->set_IntFlag("RUN2AUAU",0);   // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);     // change if simulating p+p events
  rc->set_IntFlag("RUN3DAU",1);    // flag for Run3 d+Au

  rc->set_FloatFlag("TOFTIMINGRESOLUTION", 0.100);  // this should be moved to the Init method of TofSimrecoo

  //
  //  Flags to abort event if required number of GEANT hits is not present in the subsystem
  //
  /*
    rc->set_IntFlag("DCHREQFLAG", dchReqHits);
    rc->set_IntFlag("PC1REQFLAG", pc1ReqHits);
    rc->set_IntFlag("PC2REQFLAG", pc2ReqHits);
    rc->set_IntFlag("PC3REQFLAG", pc3ReqHits);
    rc->set_IntFlag("TOFREQFLAG", tofReqHits);
    rc->set_IntFlag("EMCREQFLAG", emcReqHits); // not yet operational
  */
  
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  // se->Verbosity(100);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  //
  // As of January 2, 2004 the BBC has uninitialized variable warnings from Valgrind
  //
  SubsysReco *bbcSim = new BbcSimreco("BBC");
  se->registerSubsystem(bbcSim);

  /*
  //
  // As of January 2, 2004 the MVD is crashing when it tries to reconstruct HIJING events
  //
  if(simVertexFlag == 0 ) {
    SubsysReco *mvdSim = new MvdSimreco("MVD");
    se->registerSubsystem(mvdSim);
  }

  SubsysReco *padSim = new PadSimreco("PAD");
  se->registerSubsystem(padSim);

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
  //
  // EMCal uses the real data class
  //
  // SubsysReco *emcRec = new EmcReco3();
  // se->registerSubsystem(emcRec);
  */

  // muon Pisa unpacker.
  MuonUnpackPisa *Muon_pisa = new MuonUnpackPisa();
  se->registerSubsystem(Muon_pisa);
  Muon_pisa->Verbosity( 1 );
  
  /* 
    force pisa unpacker to run the response since 
    it is needed by the PRDF packer module.
  */
  Muon_pisa->set_do_response( true );

  // muon PRDF packer
  se->registerSubsystem( new MuonPackPRDF() );
  

  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(inMan);
  
  ///////////////////////////////////////////
  // Output manager (PRDF)
  ///////////////////////////////////////////
  Fun4AllPrdfOutputManager *prdf_io  = new Fun4AllPrdfOutputManager("PRDFOUT",prdfOut);
  prdf_io->InitPrdfNode( se->topNode() );
  se->registerIOManager(prdf_io);

  inMan->Verbosity(100);
  int kEvents = nEvents;
  TFile f(filein);
  int jEvents = T->GetEntries();
  if(kEvents==0) {
    kEvents = jEvents;
    cout << "\n\n  Event processing will be for " << jEvents << " events total in this input file." << endl;
  } else if(kEvents>jEvents) {
    cout << "\n\n  User requests " << kEvents << " input events, but";
    cout << " the input file contains only " << jEvents <<" events." << endl;
    cout << "  Event processing will be for only " << jEvents << " events." << endl;
    cout << "\n  Since in Simulation you are always supposed to know your input conditions perfectly," << endl;
    cout << "  you might want to reconsider what you are doing." << endl << endl;
    kEvents = jEvents;
  }
  se->fileopen(inMan->Name(),filein);

  gBenchmark->Start("eventLoop");   // start the timing clock
  se->run(kEvents);                 // process input events
  gBenchmark->Show("eventLoop");    //

  // this is only neccessary when input file is not exhausted
  se->fileclose(inMan->Name());

  se->EndRun();

  // If you do not see this message, the job failed
  cout << "Completed simulated PRDF production" << endl;

}
