//
// This is a macro to run svx reconstruction from pisa hits. 
//
void svxPrdfRead
(
 int nEvents = 10, char *filein="sim.prdf", char *fileout="recoDST.root"
 )
{
  cout << "Loading libraries...\n";
  //  gSystem->Load("../bld/lib/libsvx.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  //gSystem->Load("libsvx.so");
  //gSystem->Load("libsimreco.so");
  cout << "Libraries loaded...\n";
  
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  //rc->set_IntFlag("RUNNUMBER",+179846);
  //rc->set_IntFlag("SIMULATIONFLAG",2); // 2 means PISA-To-DST
  //  rc->set_IntFlag("SIMULATIONFLAG",0); // 2 means PISA-To-DST
  rc->set_IntFlag("EMBEDFLAG",0);
  // rc->set_IntFlag("RUNNUMBER",+80312);  // This is the reference run number used in d+Au production
  // rc->set_IntFlag("RUNNUMBER",+122929);  // This is the reference run number used in Au+Au 63 GeV production (Run4)
  // rc->set_IntFlag("RUNNUMBER",+120496);  // This is the reference run number used in Au+Au 200 GeV production (Run4)
  // rc->set_IntFlag("RUNNUMBER",+160260);  // This is the reference run number used in 63Cu+63Cu 200 GeV production (Run5)
  // rc->set_IntFlag("RUNNUMBER",+161767);  // This is the reference run number used in 63Cu+63Cu 62.4 GeV production (Run5)
  // rc->set_IntFlag("RUNNUMBER",+163621);  // This is the reference run number used in 63Cu+63Cu 22 GeV production (Run5)
  //rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5)

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
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *vtx     = new VtxReco();

  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(vtx);



/*
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
*/


  SubsysReco *svxpar = new SvxParManager();
  (dynamic_cast<SvxParManager*>(svxpar))->set_ReadGeoParFromFile(1);
  svxpar->Verbosity(1);
  se->registerSubsystem(svxpar);


  SubsysReco *svxdecode = new SvxDecode();
  (dynamic_cast<SvxDecode*>(svxdecode))->setKeepHotDeadHits(false);
  //svxdecode->Verbosity(1);
  se->registerSubsystem(svxdecode);


  SubsysReco *svxapplyhotdead = new SvxApplyHotDead();
  svxapplyhotdead->Verbosity(1);
  se->registerSubsystem(svxapplyhotdead);
  

  SubsysReco *svxrec = new SvxReco();
  svxrec->Verbosity(1);
  se->registerSubsystem(svxrec);


  SubsysReco *svxprimseed = new SvxPriVertexSeedFinder();
  se->registerSubsystem(svxprimseed);


  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(1);
  se->registerSubsystem(svxstandalone);


  SubsysReco *svxprivtx = new SvxPrimVertexFinder();
  se->registerSubsystem(svxprivtx);


  //Fun4AllInputManager *inMan = new Fun4AllPisaInputManager("PisaIn","TOP",0);
  Fun4AllInputManager *inMan = new Fun4AllNoSyncDstInputManager("DSTin1","DST");
  //inMan->Verbosity(100);
  inMan->Verbosity(0);
  se->registerInputManager(inMan);

  Fun4AllDstOutputManager *svxDST  = new Fun4AllDstOutputManager("SIMDST", fileout );
//  Fun4AllDstOutputManager *svxDST  = new Fun4AllDstOutputManager("DSTOUT", fileout );
//  svxDST->AddNode("fkin");
//--  svxDST->AddNode("SvxPisaHit");
//--  svxDST->AddNode("SvxGhitList");
  svxDST->AddNode("SvxRawhitList");
//--  svxDST->AddNode("SvxRawhitClusterList");
  svxDST->AddNode("SvxClusterList");
  svxDST->AddNode("SvxSegmentList");
//  svxDST->AddNode("PRDF");
  se->registerOutputManager(svxDST);
  svxDST->Print();


  pfileopen(filein);

  prun(nEvents);

  se->End();

  cout<<"End of Macro"<<endl;

}
