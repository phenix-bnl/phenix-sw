void Fun4MuonsProduction_qaDST(
  const char* dstname_mu2="data_mu2.dst" , 
  const char* dstname_mu2hit="data_mu2hit.dst" , 
  const char* dstname_mu2clus="data_mu2clus.dst" , 
  const char* dstname_evt="data_evt.dst" , 
  const char* qaoutname="qaout.root", 
  const int nevts = 100)
{

  // Load/intialize DST QA library
  gSystem->Load("libfun4all.so");
  gSystem->Load("libdstqa.so");

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
  //SubsysReco *qa = new QAReco(qaoutname,"LZBOSVDPTFECMURelectron");

  MuonReadbackDST *readback_dst = new MuonReadbackDST();
  readback_dst->Verbosity(1);
  se->registerSubsystem( readback_dst ); 
  readback_dst->set_do_dbinit( true );


  SubsysReco *qa = new QAReco(qaoutname,"BMU");
  se->registerSubsystem(qa);

  
  ///////////////////////////////////////////
  // Input manager
  ///////////////////////////////////////////


  Fun4AllInputManager *signal_mu2 = new Fun4AllDstInputManager("IMDST","DST");
  se->registerInputManager(signal_mu2);
  se->fileopen(signal_mu2->Name(),dstname_mu2);  

  Fun4AllInputManager *signal_mu2hit = new Fun4AllDstInputManager("IMDST1","DST");
  se->registerInputManager(signal_mu2hit);
  se->fileopen(signal_mu2hit->Name(),dstname_mu2hit);  

  Fun4AllInputManager *signal_mu2clus = new Fun4AllDstInputManager("IMDST2","DST");
  se->registerInputManager(signal_mu2clus);
  se->fileopen(signal_mu2clus->Name(),dstname_mu2clus);  

  Fun4AllInputManager *signal_evt = new Fun4AllDstInputManager("IMDST3","DST");
  se->registerInputManager(signal_evt);
  se->fileopen(signal_evt->Name(),dstname_evt);  
  
  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////
  se->run(nevts);
  se->End();

  cout << "Completed reconstruction." << endl;

}
