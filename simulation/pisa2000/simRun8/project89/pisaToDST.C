void pisaToDST(Int_t nEvents=100, char *dstout="simDST.root", 
	       char *udstout="simMicroDST.root",
	       char *ndstout="simNanoDST.root",
	       char *filein="/phenix/data11/rhphemds/run4/macroTest/hji135evt_20auauminb200sq01_3D--_31Mar2004.root",
               Int_t magField=3,
               Int_t simVertexFlag=0, Float_t simZ0Vertex=0.0, Float_t simT0Vertex=0.0,
               Float_t simZ0VertexWidth=0.0, Float_t simT0VertexWidth=0.0,
	       Int_t dchReqHits=0, Int_t pc1ReqHits=0, Int_t pc2ReqHits=0, Int_t pc3ReqHits=0,
	       Int_t tofReqHits=0, Int_t emcReqHits=0,
               const Int_t debugFlag=0)
{

  //
  // Fun4All macro to process PISA hits files into simulated DSTs, uDSTs, and nDSTs
  // Modeled after Fun4All.C macro used in Run3 PRDF processing
  // Calls subsidiary macro pisaToDST_IOManager.C
  //

  //
  // Original author: Charles F. Maguire (charles.f.maguire@vanderbilt.edu)
  // Creation date: December 7, 2003
  // Last revision: April 7, 2004  (Set Flag for Run4 Au+Au at 62.4 GeV. Use Run4 PISA hits file as test input with 3D-- field)
  //

  // Input parameters
  // Int_t nEvents   number of input events to process
  // char *dstout    name of output file for simulated DST
  // char *udstout   name of output file for simulated microDST
  // char *ndstout   name of output file for simulated nanoDST
  // char *filein    name of input PISA hits file
  // Int_t magField  magnetic field choice: 1 = Run1/Run2 (3D01)
  //                                        2 = Run3 (3D03), also works for single coil Run4 (3D+0) 
  //                                        3 = Run4 (3D++ or 3D--) both coils in same direction
  //                                        4 = Run4 (3D+-) outer and inner coil reversed
  //
  //
  // Do the softlink for the Central Arm fieldIntegral.dat file
  //
  switch(magField)
    {
    case 1:  // Run1/Run2 (3D01)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2001/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run1/Run2 (3D01)" << endl;
      break;
    case 2:  // Run3 (3D03)
      gSystem->Exec("ln -fs /afs/rhic/phenix/software/calibration/run2003/fieldIntegral.dat fieldIntegral.dat");
      cout << "\n Magnetic field map fieldIntegral.dat file set for Run3 (3D03), same as for Run4 (3D+0)" << endl;
      break;
    case 3:  // Run4 (3D++)
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
	gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libsimreco.so");
  gSystem->Load("libMWGOO.so");

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
  // recoConsts setup
  //////////////////////////////////////////

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST
  rc->set_IntFlag("EMBEDFLAG",0); // embedding is not yet operational in Fun4All (to be done after QM'04)
  //
  // EMCal needs a Run3 number, either positive or negative (not sure if the sign is important); -1 will NOT work
  // However, The UtiHadPID class requires a positive run number
  //
  rc->set_IntFlag("RUNNUMBER",179846);  // This is the reference run number used in d+Au production
  rc->set_IntFlag("EVALUATIONFLAG", 1); // Requested by EMCal

  //
  // The Drift Chamber initialization code requires that one, and only one, of the following four flags be set to 1
  //
  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);          // change if simulating p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU63GEV",1);   // flag for Run4 Au+Au, 63 GeV data certified

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
  rc->set_FloatFlag("SIMZ0VERTEX",simZ0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",simZ0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",simT0Vertex);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",simT0VertexWidth);   // checked in BbcSimreco only when simVertexFlag = 1 or 2

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(1);

  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////

  //
  // The BbcSimreco must be used even for single particle simulations
  // The Z0 value will be artificially put into the BbcOut data when simVertexFlag is not zero
  // The Z0 then will be retrieved by the VtxReco for both Central and Muon arm simulations
  //
  // As of January 2, 2004 the BBC has uninitialized variable warnings from Valgrind
  SubsysReco *head = new HeadSimreco();
  se->registerSubsystem(head);

  SubsysReco *trig = new TrigSimreco();
  se->registerSubsystem(trig);
  
  SubsysReco *bbcSim = new BbcSimreco("BBC");
  se->registerSubsystem(bbcSim);

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
//  SubsysReco *t0Rec = new T0Reco();
 // se->registerSubsystem(t0Rec);
  //
  // As of January 2, 2004 the Dch has uninitialized variable warnings from Valgrind
  // There are also log file output warning messages
  
  SubsysReco *dchSim = new DchSimreco("DCH");
  se->registerSubsystem(dchSim);

  SubsysReco *tecSim = new TecSimreco("TEC");
  se->registerSubsystem(tecSim);
  
  SubsysReco *tofSim = new TofSimreco("TOF");
  se->registerSubsystem(tofSim);
  SubsysReco *hbd = new HbdSimreco();
  hbd->Verbosity(1);
  se->registerSubsystem(hbd);
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
  SubsysReco *emcRec = new EmcReco3();
  se->registerSubsystem(emcRec);

  //
  // The CglReco works unchanged for both real and simulation events
  //
  SubsysReco *cglRec = new CglReco("CGL");
  se->registerSubsystem(cglRec);

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
  SubsysReco *central = new CentraltrackReco(20); // Values other than 11 may not work
  se->registerSubsystem(central);

  //
  //  This is the class which makes the GlobalEvent data on the nanoDST output
  //
  SubsysReco *global = new GlobalReco(  ); 
  se->registerSubsystem(global);

  //
  // Include Monte Carlo information
  //
  SubsysReco *mceval = new McEvalSimreco();
  se->registerSubsystem(mceval);

  //
  // This is the class which checks for charged particles going into EMCal
  //
  SubsysReco *veto = new ChargedvetoReco();
  se->registerSubsystem(veto);

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

  //
  // Special simulation evaluation object, not normally activated
  //
  // delete evaSim;

  //
  // If you do not see this message, the job failed
  //
  cout << "Completed reconstruction." << endl;

}
