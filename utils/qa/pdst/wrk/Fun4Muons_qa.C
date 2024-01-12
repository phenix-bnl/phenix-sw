void Fun4Muons_qa(
  const char* dstname="data.dst" , 
  const char* qaoutname="qaout.root", 
  const int nevts = 1000)
{

  // Load/intialize DST QA library
  gSystem->Load("libfun4all.so");
  gSystem->Load("libdstqa.so");
  gSystem->Load("libqasummary.so");

  Fun4AllServer *se = Fun4AllServer::instance();
	
  //----------------------------------------------------------------------
  /** 
      QAReco(char *QAoutputFile, char *option)
      option = 
      L -- Forward Calorimeter
      Z -- Zero Degree Calorimeter
      B -- Beam-Beam Counter
      N -- NTC Counter ( Run2 p-p )
      O -- NTCP Counter ( Run 3) 
      S -- T-zero Counter
      V -- Multiplicity Vertex Detector
      D -- Drift Chamber
      P -- Pad Chamber
      T -- Time Expansion Chamber
      F -- Time of Flight Detector
      E -- Electromagnetic Calorimeter
      C -- Ring Imaging Cherenkov Counter
      M -- Muon Tracker
      U -- Muon Identifier
      R -- EMC-RICH (ERT) trigger 
      electron -- EWG
      min_bias -- loop only into Minimum Bias Events
      
  the default option is "ZBNSVDPTFECMUR"
  */

  
  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  MuonReadbackDST *readback_dst = new MuonReadbackDST();
  readback_dst->Verbosity(1);
  se->registerSubsystem( readback_dst );
  readback_dst->set_do_dbinit( true );


  //SubsysReco *qa = new QAReco(qaoutname,"LZBOSVDPTFECMURelectron");
  SubsysReco *qa = new QAReco(qaoutname,"BMU");
  se->registerSubsystem(qa);

  
  ///////////////////////////////////////////
  // Input manager
  ///////////////////////////////////////////
  Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager("IMDST","DST");
  se->registerInputManager(signal);
  se->fileopen(signal->Name(),dstname);  
  
  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  se->run(nevts);
  se->End();

  cout << "Completed reconstruction." << endl;

}
