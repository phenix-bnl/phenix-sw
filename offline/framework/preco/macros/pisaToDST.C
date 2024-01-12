// $Id: pisaToDST.C,v 1.55 2009/02/18 23:15:42 mazsi Exp $
/*!
   \file pisaToDST.C
   \brief pisa to DST reconstruction chain
   \author <a href="mailto:pereira@hep.saclay.cea.fr">Hugo Pereira</a>
   \version $Revision: 1.55 $
   \date $Date: 2009/02/18 23:15:42 $
*/

void pisaToDST(

  Int_t nEvents = 100, 
  char *filein="PISAEvent.root",
  char *dstout = "simDST.root", 
  int run_number = 231429
  
 )
{
 
  // print output
  cout << "pisaToDST - nEvents: " << nEvents << endl;
  cout << "pisaToDST - filein: " << filein << endl;
  if( dstout ) cout << "pisaToDST - dstout: " << dstout << endl;
  cout << "pisaToDST - run_number: " << run_number << endl;
  cout << endl;
   
  // load libraries
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so"); 
  gSystem->Load("libsimreco.so");
  gSystem->Load("libmuon_util.so" );
    
  gROOT->ProcessLine(".L pisaToDST_IOManager.C");
    
  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
    
  recoConsts *rc = recoConsts::instance();

  // 2 means PISA-To-DST
  rc->set_IntFlag("SIMULATIONFLAG",2); 
  
  // disable embedding
  rc->set_IntFlag("EMBEDFLAG",0); 
  
  // Reference run number used in 2007 Au+Au 200 GeV 
  rc->set_IntFlag("RUNNUMBER",run_number); 
  
  // Requested by EMCal
  rc->set_IntFlag("EVALUATIONFLAG", 1); 
  // rc->set_IntFlag("EMCSIMULATIONV2", 1); // enable if you want the new code
  // rc->set_IntFlag("EMCSIMULATIONV2NOQA", 1); // do not apply QA, disabled
  
  // Run flag
  rc->set_IntFlag("RUN7AUAU200GEV",1);        

  // this should be moved to the Init method of TofSimreco
  rc->set_FloatFlag("TOFTIMINGRESOLUTION", 0.100);  
  
  /*  
  Flags to abort event if required number of GEANT hits is not present in the subsystem
  Defaults are all 0 except for the Drift Chamber
  default setting is 3 Drift Chamber wire plane hits  
  */
  rc->set_IntFlag("DCHREQFLAG", 3);   
  rc->set_IntFlag("PC1REQFLAG", 0);
  rc->set_IntFlag("PC2REQFLAG", 0);
  rc->set_IntFlag("PC3REQFLAG", 0);
  rc->set_IntFlag("TOFREQFLAG", 0);
  
  // not yet operational
  rc->set_IntFlag("EMCREQFLAG", 0);
  
  // assume AFS is present as at RCF
  rc->set_IntFlag("AFSABSENT", 0);                                
    
  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance();
    
  ///////////////////////////////////////////
  // Activate the subsystems
  //////////////////////////////////////////
  
  // run header and trigger setting
  se->registerSubsystem( new HeadSimreco() );
  se->registerSubsystem( new TrigSimreco() );
  
  // counter
  se->registerSubsystem( new MuonCounter() );
  
  
  // BBC simReco
  se->registerSubsystem(new BbcSimreco("BBC"));
  
  // pisa is used as an input vertex.
  // it overwrites the contents of the BBC out node.
  VtxSimreco* vtx_sim = new VtxSimreco();
  vtx_sim->UseVtx( VtxSimreco::PISA );
  vtx_sim->ZVertexSigma(0.5);
  se->registerSubsystem( vtx_sim );

  T0Simreco* t0_sim = new T0Simreco();
  t0_sim->T0Sigma(0.04);
  se->registerSubsystem( t0_sim );

  
  // pad chambers
  se->registerSubsystem(new PadSimreco("PAD"));
                                      
  // The VtxReco works unchanged for both real and simulation events
  se->registerSubsystem(new VtxReco("VTX"));
                                      
  // The T0Reco works unchanged for both real and simulation events
  se->registerSubsystem(new T0Reco());
                                      
  // As of January 2, 2004 the Dch has uninitialized variable warnings from Valgrind
  // There are also log file output warning messages
  se->registerSubsystem( new DchSimreco("DCH") );
  
  // Time expansion chamber
  se->registerSubsystem( new TecSimreco("TEC"));
  
  // Time of flight detector
  se->registerSubsystem(new TofSimreco("TOF"));
  
  // Tof west
  se->registerSubsystem(new TfwSimreco("TFW"));
  
  // HBD
  se->registerSubsystem(new HbdSimreco("HBD"));
  
  // RICH
  se->registerSubsystem(new CrkSimreco("CRK"));
  
  // Aerogel subsystem as per e-mail from Narumi Kurihara on May 13, 2005
  se->registerSubsystem(new AccSimreco("ACC"));
  se->registerSubsystem(new AccReco());
  
  // EMCal uses the real data class
  se->registerSubsystem( new EmcReco3() );
  
  // The CglReco works unchanged for both real and simulation events
  se->registerSubsystem(new CglReco("CGL"));
  
  //Aerogel cluster  (Needs to be after cglRec)
  se->registerSubsystem(new AccclusterReco());
  
  //  This is the class which makes the RICH Ring data structure
  se->registerSubsystem( new RingReco() );
  
  // This is the class which makes the Central Tracks nanoDST output
  // 22 corresponds to the version used in pro.78 for Run7 Au+Au
  se->registerSubsystem(new CentraltrackReco( 22 ));
  
  //  This is the class which makes the GlobalEvent data on the nanoDST output
  se->registerSubsystem(new GlobalReco());
  
  // This is the class which checks for charged particles going into EMCal
  se->registerSubsystem(new ChargedvetoReco());

  // muon arms are not included in this macro. Are handled separately.
  // H. Pereira Da Costa
  
  //added the DC based global evaluation module
  se->registerSubsystem( new McEvalSimreco() );
    
  ///////////////////////////////////////////
  // InputManager 
  ///////////////////////////////////////////
  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP");
  se->registerInputManager(inMan);
  
  ///////////////////////////////////////////
  // OutputManagers Set up functions  
  ///////////////////////////////////////////
  if( dstout ) DST_IOManager(dstout, se);     
  
  ///////////////////////////////////////////
  // open input file
  se->fileopen(inMan->Name(),filein);
  
  // process input events
  gBenchmark->Start("eventLoop");     
  se->run(nEvents);             
  se->End();
  gBenchmark->Show("eventLoop");      
            
  // If you do not see this message, the job failed
  cout << "Completed reconstruction." << endl;
}
