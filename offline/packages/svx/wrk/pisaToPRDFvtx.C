void pisaToPRDFvtx(Int_t nEvents=10, char *prdfOut="sim.prdf", 
               char *filein="/phenix/gh/data07/ygu/svx/wrk/PISAEvent_001.root",
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
  // Last revision: Nov 10, 2010  TakashiHachiya (hachiya@rcf.rhic.bnl.gov)
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
  //rc->set_IntFlag("RUNNUMBER",+80312);  // This is the reference run number used in d+Au production
  rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5)
  //rc->set_IntFlag("EVALUATIONFLAG", 1); // Requested by EMCal





  //
  // The Drift Chamber initialization code requires that one of the following three flags be set to 1
  //
  rc->set_IntFlag("RUN2AUAU",0);       // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);         // flag for Run2 p+p
  rc->set_IntFlag("RUN3PP",0);         // flag for Run3 p+p
  rc->set_IntFlag("RUN3DAU",0);        // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU200GEV",0); // flag for Run4 Au+Au 200GeV
  rc->set_IntFlag("RUN4AUAU63GEV",0);  // flag for Run4 Au+Au  63GeV
  rc->set_IntFlag("RUN5PP200GEV",1);   // flag for Run5 p+p 200GeV


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

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(100);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  // run header and trigger setting
  se->registerSubsystem( new HeadSimreco() );
  se->registerSubsystem( new TrigSimreco() );

  //
  // As of January 2, 2004 the BBC has uninitialized variable warnings from Valgrind
  //
  SubsysReco *bbcSim = new BbcSimreco("BBC");
  se->registerSubsystem(bbcSim);

  /*
  */
  SubsysReco *svxpar = new SvxParManager();
  ((SvxParManager*)svxpar)->set_ReadGeoParFromFile(1);
  se->registerSubsystem(svxpar);
  

  //svxAddress::getInstance().set_Verbose(2);

  SubsysReco *svxsim     = new SvxSimulator();
//  ((SvxSimulator*)svxsim)->set_ReadParFromFile(1);
//  ((SvxSimulator*)svxsim)->set_StripixelZeroSup(0);
//  svxsim->Verbosity(100);
  se->registerSubsystem(svxsim);

/////////
  SubsysReco *svxprdf    = new SvxEncode();
//  svxprdf->Verbosity(100);
  se->registerSubsystem(svxprdf);
/////////




  SubsysReco *prdfSim = new PrdfSimreco("PRDF", prdfOut, 80312);
  se->registerSubsystem(prdfSim);

  cout<<"add PrdfSimReco"<<endl;

  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(inMan);

  cout<<" se->registerInputManager(inMan) "<<endl;

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

  cout<<"FileOpen"<<endl;

  gBenchmark->Start("eventLoop");   // start the timing clock
  cout<<"before run"<<endl;
  se->run(kEvents);                 // process input events
  cout<<"before run"<<endl;
  gBenchmark->Show("eventLoop");    //

  // this is only neccessary when input file is not exhausted
  se->fileclose(inMan->Name());

  cout<<"FileClose"<<endl;

  delete prdfSim;

  //
  // If you do not see this message, the job failed
  //
  cout << "Completed simulated PRDF production" << endl;

}
