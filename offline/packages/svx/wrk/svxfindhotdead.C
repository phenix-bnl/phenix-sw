//
// This is a macro to run svx reconstruction from pisa hits. 
//
void svxfindhotdead(
   int nEvents = 100, 
   char *filein="/phenix/subsys/vtx/lebedev/newsim/pisa/pions_100evts_00.root", 
            )
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libsvx.so");
  gSystem->Load("libsimreco.so");
  cout << "Libraries loaded...\n";
  
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",+179846);
  rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST
  rc->set_IntFlag("EMBEDFLAG",0);
  rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5)

  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);          // flag for Run2 p+p events
  rc->set_IntFlag("RUN3PP",0);          // flag for Run3 p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU200GEV",0);  // flag for Run4 Au+Au, 200 GeV data (or pp?)
  rc->set_IntFlag("RUN4AUAU63GEV",0);   // flag for Run4 Au+Au, 63 GeV data (or pp?)
  rc->set_IntFlag("RUN5PP200GEV",1);    // flag for Run5 pp 200 GeV data

  // simVertexFlag = 0 (default) means that the BBC Z0 value will be used
  // simVertexFlag = 1 means that the same simZ0Vertex centroid value is used for all events
  // simVertexFlag = 2 means that the Z0 centroid is taken from the PISA event header for each event
  // The centroid values are modified by the Width values which are Gaussian sigma values
  //
  rc->set_IntFlag("SIMVERTEXFLAG",2);
  rc->set_FloatFlag("SIMZ0VERTEX",0.0);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMZ0VERTEXWIDTH",0.0);   // checked in BbcSimreco only when simVertexFlag = 1 or 2
  rc->set_FloatFlag("SIMT0VERTEX",0.0);             // checked in BbcSimreco only when simVertexFlag = 1
  rc->set_FloatFlag("SIMT0VERTEXWIDTH",0.0);   // checked in BbcSimreco only when simVertexFlag = 1 or 2


  //  Reconstruction Modules...

  SubsysReco *head = new HeadSimreco();
  se->registerSubsystem(head);

  SubsysReco *trig = new TrigSimreco();
  se->registerSubsystem(trig);

  SubsysReco *bbc     = new BbcSimreco();
  se->registerSubsystem(bbc);

  // pisa is used as an input vertex.
  // it overwrites the contents of the BBC out node.
  VtxSimreco* vtx_sim = new VtxSimreco();
  T0Simreco*  t0_sim  = new T0Simreco();
  vtx_sim->UseVtx( VtxSimreco::PISA );
  //vtx_sim->ZVertexSigma(0.5);
  //vtx_sim->T0Sigma(0.04);
  vtx_sim->ZVertexSigma(0.0);
  t0_sim->T0Sigma(0.0);
  se->registerSubsystem( vtx_sim );
  se->registerSubsystem( t0_sim );



  SubsysReco *svxpar = new SvxParManager();
  (dynamic_cast<SvxParManager*>(svxpar))->set_ReadGeoParFromFile(0);
  se->registerSubsystem(svxpar);


  SubsysReco *svxsim = new SvxSimulator();
//  (dynamic_cast<SvxSimulator*>(svxsim))->set_ChargeAsymXUWidth(0.1);
  (dynamic_cast<SvxSimulator*>(svxsim))->set_StripixelNoise(0.0);
//  (dynamic_cast<SvxSimulator*>(svxsim))->set_StripixelZeroSup(31);
//  (dynamic_cast<SvxSimulator*>(svxsim))->set_StripixelAdcThreshold(31);
//  (dynamic_cast<SvxSimulator*>(svxsim))->set_StripixelAdcSumThreshold(50);
  svxsim->Verbosity(0);
  se->registerSubsystem(svxsim);

  SubsysReco *svxhotdead = new SvxFindHotDead();
  svxhotdead->Verbosity(1);
  se->registerSubsystem(svxhotdead);

  Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP",0);
  //inMan->Verbosity(100);
  inMan->Verbosity(0);
  se->registerInputManager(inMan);

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

}
