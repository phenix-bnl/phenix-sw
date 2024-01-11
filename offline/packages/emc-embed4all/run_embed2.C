
// simply a wrapper around the gSystem->Load() system call
void myload(const char * name){
    cout << "loading " << name << "... " << flush;
    gSystem->Load(name);
    cout << "done." << endl;
}

void run_embed2(const char * realfile = "realdst.root", const char* simfile = "dstin.root", const char * outfile = "outdst.root", int nevents = 0, int nskipevents = 0){

  const double myPbScScale= 0.95;
  const double myPbGlScale= 1.01;
  const double myPbScSmear= 0.08;
  const double myPbGlSmear= 0.135;

  myload("libfun4all.so");
  myload("libfun4allfuncs.so");
  myload("libFROG.so"); FROG fr;
  myload("libsimreco.so");  
  gSystem->Load("libCNT.so");
  gSystem->Load("librecal.so");
  gSystem->Load("libcompactCNT.so");
  myload("libtrigger.so");
  //gSystem->Load("libsimDSTCheck.so");
  
  myload("libemcEmbed4all.so");
  myload("libemc-evaluation.so");

  gSystem->ListLibraries();

  Fun4AllServer* se = Fun4AllServer::instance();
  se->Verbosity(0);
  


  // input file with real data
  const char* realname = "REAL";
  Fun4AllDstInputManager * input1 = new Fun4AllNoSyncDstInputManager("real", "DST", realname);
  se->registerInputManager( input1 );
  realfile = fr.location( realfile );
  if( se->fileopen("real", realfile) != 0 ){ cerr << "failed to open '" << realfile << "'" << endl; exit(-1); }

  // input file with simulated data
  const char* simname = "SIM";
  Fun4AllNoSyncDstInputManager * input2 = new Fun4AllNoSyncDstInputManager("sim", "DST", simname);
  input2->NoRunTTree();
  se->registerInputManager( input2 );
  simfile = fr.location( simfile );
  if( se->fileopen("sim", simfile) != 0 ){ cerr << "failed to open '" << simfile << "'" << endl; exit(-1); }




  // for importing emcTowerContainerDST
  SubsysRecoStack * chroot = new SubsysRecoStack("emcTowerContainerDSTImp", Fun4AllServer::instance()->topNode(realname));
  chroot->x_push_back( new EmcTowerContainerResurrector() );
  se->registerSubsystem( chroot );


#if 1
  //
  // if you want to fine-tune the emcal embedding process, use this 
  // expanded version.  if you are happy with the defaults (or you can
  // achive what you want using the EmcEmbedDriver2::set*() methods)
  // then you can safely delete this part and use the simple form
  // in the #else branch.
  //

  PHCompositeNode * real = Fun4AllServer::instance()->topNode( realname );
  PHCompositeNode * sim = Fun4AllServer::instance()->topNode( simname ); 

  // copy nonemc data: our modules will need some of them as input.
  // also so that they will appear in the output dst.
  se->registerSubsystem( new CopyNonEMCNodes(sim, "CopyNonEMCNodesSimPre") ); // copy from simulation..
  se->registerSubsystem( new CopyNonEMCNodes(real, "CopyNonEMCNodesRealPre") ); // ..and overwrite with real

  // import real data
  SubsysRecoStack * realimp = new EmcRealContainerImporter( real );
  realimp->x_push_back( new EmcUnclusterizer() );
  realimp->x_push_back( new EmcApplyQA( EmcApplyQA::TOWER ) ); // importer does a poor job
  se->registerSubsystem( realimp );

  // import simulated data
  SubsysRecoStack * simimp = new EmcGeaContainerImporter( sim );
  simimp->x_push_back( new EmcUnclusterizer() );
  simimp->x_push_back( new EmcApplyQA( EmcApplyQA::TOWER ) );
  EmcTowerScalerSmearer * emcsm = new EmcTowerScalerSmearer(1., 0.02); // smear simulated data with 2%
	#define RUN14_AUAU_200GEV
	#ifdef RUN14_AUAU_200GEV
  emcsm->SetScale(0.988, 0.990, 0.985, 0.981, 0.980, 0.985, 1.025, 1.023);
  emcsm->SetSmear(0.055, 0.066, 0.055, 0.059, 0.057, 0.056, 0.072, 0.072);
  emcsm->SetSmear2(0.011, 0.017, 0.011, 0.013, 0.012, 0.012, 0.063, 0.062);
  #else
  emcsm->SetScale(myPbScScale,myPbGlScale);
  emcsm->SetSmear(myPbScSmear,myPbGlSmear);
  #endif

  simimp->x_push_back( emcsm );
  se->registerSubsystem( simimp );

  // merge data
  EmcDataMerger * merger = new EmcDataMerger();
  merger->AddSourceNode( realname );
  merger->AddSourceNode( simname );
  se->registerSubsystem( merger );
  se->registerSubsystem( new EmcApplyQA( EmcApplyQA::TOWER ) );
    
  // clusterize data
  se->registerSubsystem( new EmcEmbedReclusterizer("TOP", "TOP", "TOP", "", 1) ); // 0 --kReal, 1 -- kPISA for mEmcGeometryModule

#else
  se->registerSubsystem( new EmcEmbedDriver2(realname, simname) );

#endif

  //se->registerSubsystem( new EmcEvaluatorModule(false, false, false) );

  // output file
  Fun4AllDstOutputManager * output  = new Fun4AllDstOutputManager("output", outfile);

  output->RemoveNode("*");
  output->AddNode("EventHeader");
  output->AddNode("Sync");
  output->AddNode("PHGlobal");
  output->AddNode("VtxOut");
  output->AddNode("emcGeaTrackContainer");
  output->AddNode("emcTowerContainer");
  output->AddNode("emcClusterContainer");

  se->registerOutputManager(output);




  // run analysis
  gBenchmark->Start("embed"); 
  se->run(nevents);
  gBenchmark->Show("embed");
  se->End();





  cout << "done." << endl;
}
