void pisaMergeToDST(Int_t nEvents=10, char *dstout="simDST.root", 
	       char *udstout="simMicroDST.root",
	       char *ndstout="simNanoDST.root",
	       char *filein="/phenix/data11/rhphemds/run3/macroTest/hji135evt_521auauminb200sq01_20Mar2004.root",
               Int_t magField=2,
               Int_t simVertexFlag=0, Float_t simZ0Vertex=0.0, Float_t simT0Vertex=0.0,
               Float_t simZ0VertexWidth=0.0, Float_t simT0VertexWidth=0.0,
	       Int_t dchReqHits=0, Int_t pc1ReqHits=0, Int_t pc2ReqHits=0, Int_t pc3ReqHits=0,
	       Int_t tofReqHits=0, Int_t emcReqHits=0,
               const Int_t debugFlag=0)
{

  //
  // Fun4All macro to process PISA hits files into simulated DSTs, uDSTs, and nDSTs
  // This version demonstrates the use of the PISA Merge Files facility
  // Modeled after Fun4All.C macro used in Run3 PRDF processing
  // Calls subsidiary macro pisaToDST_IOManager.C
  //

  //
  // Original author: Charles F. Maguire (charles.f.maguire@vanderbilt.edu)
  // Creation date: December 7, 2003
  // Last revision: March 28, 2004  (Include Muon Arm software, add +- magnetic field)
  //
  //

  // Input parameters
  // Int_t nEvents   number of input events to process
  // char *dstout    name of output file for simulated DST
  // char *udstout   name of output file for simulated microDST
  // char *ndstout   name of output file for simulated nanoDST
  // char *filein    name of input PISA hits file
  // Int_t magField  magnetic field choice: 1 = Run1/Run2 (3D01)
  //                                        2 = Run3 (3D03), also works for single coil Run4 (3D+0) 
  //                                        3 = Run4 (3D++) both coils in same direction
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
      return;
      break;
    default:
      cout << "\n magField value " << magField << " is not recognized; job is aborting" << endl;
      return;
    } // Finish magnetic field map switching

  gSystem->Load("libfun4all.so");
  gSystem->Load("libsimreco.so");

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
  rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST; PISA-to-PRDF and PRDF-to-DST are not yet operational 
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

  //
  // For single particle simulations the MvdSimreco should be omitted
  // For these cases, simVertexFlag should be set to 1 or 2
  //
  if(simVertexFlag == 0 ) {
    SubsysReco *mvdSim = new MvdSimreco("MVD");
    se->registerSubsystem(mvdSim);
  }

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
  SubsysReco *central = new CentraltrackReco(11); // Values other than 11 may not work
  se->registerSubsystem(central);

  //
  //  This is the class which makes the GlobalEvent data on the nanoDST output
  //
  SubsysReco *global = new GlobalReco( GlobalReco::MC ); 
  se->registerSubsystem(global);

  //
  // This is the class which checks for charged particles going into EMCal
  //
  SubsysReco *veto = new ChargedvetoReco();
  se->registerSubsystem(veto);

  //
  // Additions for Muon Arm
  // Mutoo pisa reader, response, recustruction and NDST.
  //
  MuonUnpackPisa* Muon_pisa = new MuonUnpackPisa();
  se->registerSubsystem(Muon_pisa);
  Muon_pisa->set_mode(0);
  Muon_pisa->set_do_response();
  
  se->registerSubsystem( new MuiooReco() ); 
  se->registerSubsystem( new MuonDev() );
  
  PHInclusiveNanoCuts *MWGcuts = new MWGInclusiveNanoCutsv2();
  se->registerSubsystem(new MWGReco(MWGcuts)); 

  //
  // Special simulation evaluation class (not normally activiated)
  //
  // SubsysReco *evaSim = new EvaSimreco("EVA", 2);
  // se->registerSubsystem(evaSim);

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

  //
  // Open primary input file, where the "filein" name is part of the pisaMergeToDST.C macro argument list
  //
  se->fileopen(inMan->Name(), filein);

  //
  // The following commands invoke the PISA Merge Files facility in Fun4All
  //
  (static_cast<Fun4AllPisaInputManager*>(inMan))->setMergeFiles(3); 
  // Above command Indicates that there will be 3 more PISA input files which will be merged with "filein"
  
  (static_cast<Fun4AllPisaInputManager*>(inMan))->pisaMergeFileOpen(
  "/phenix/data11/rhphemds/run3/macroTest/rvphiee_10sq01_031304.root", 1);

  (static_cast<Fun4AllPisaInputManager*>(inMan))->pisaMergeFileOpen(
  "/phenix/data11/rhphemds/run3/macroTest/rvphikk_10sq01_031304.root", 2);

  (static_cast<Fun4AllPisaInputManager*>(inMan))->pisaMergeFileOpen(
  "/phenix/data11/rhphemds/run3/macroTest/rvjpsiee_10sq01_031304.root", 3);

  //
  // The above 3 commands open the three named files for merging
  //

  gBenchmark->Start("eventLoop");   // start the timing clock
  se->run(kEvents);                 // process input events
  gBenchmark->Show("eventLoop");    // complete timing clock

  // this is only neccessary when input file is not exhausted
  se->fileclose(inMan->Name());

  se->EndRun();

  //
  // Special simulation evaluation object, not normally activated
  //
  // delete evaSim;

  //
  // If you do not see this message, the job failed
  //
  cout << "Completed reconstruction." << endl;

}
