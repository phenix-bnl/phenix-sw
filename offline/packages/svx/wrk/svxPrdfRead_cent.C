//
// This is a macro to run svx reconstruction from pisa hits. 
//
void svxPrdfRead_cent
(
 int nEvents = 10, char *filein="sim.prdf", char *fileout="recoDST.root"
 )
{
  cout << "Loading libraries...\n";
  gSystem->Load("~purschke/install/lib/libEvent.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  cout << "Libraries loaded...\n";

 gSystem->ListLibraries();
  
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);

  recoConsts *rc = recoConsts::instance();
  //rc->set_IntFlag("RUNNUMBER",+179846);
  //rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5)

  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);          // flag for Run2 p+p events
  rc->set_IntFlag("RUN3PP",0);          // flag for Run3 p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU200GEV",0);  // flag for Run4 Au+Au, 200 GeV data (or pp?)
  rc->set_IntFlag("RUN4AUAU63GEV",0);   // flag for Run4 Au+Au, 63 GeV data (or pp?)
  //rc->set_IntFlag("RUN5PP200GEV",1);    // flag for Run5 pp 200 GeV data



  //  Reconstruction Modules...
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  SubsysReco *bbc     = new BbcReco();
  SubsysReco *vtx     = new VtxReco();

  SubsysReco *ert     = new ErtReco();
  SubsysReco *pad     = new PadReco();
  SubsysReco *emc     = new EmcReco3();
  SubsysReco *tof     = new TofReco();
  SubsysReco *aero    = new AccReco();
  SubsysReco *dch     = new DchReco();
  SubsysReco *crk     = new CrkReco();
  SubsysReco *cgl     = new CglReco();
  SubsysReco *aerocl  = new AccclusterReco();
  SubsysReco *ring    = new RingReco();
//  SubsysReco *tofw    = new TofwReco();
  SubsysReco *global  = new GlobalReco(); 
  SubsysReco *global_central  = new GlobalReco_central(); 

  se->registerSubsystem(head);
  se->registerSubsystem(trig);
  se->registerSubsystem(bbc);
  se->registerSubsystem(vtx);

  se->registerSubsystem(ert);
  se->registerSubsystem(pad);
  se->registerSubsystem(emc);
  se->registerSubsystem(tof);
  se->registerSubsystem(aero);
  se->registerSubsystem(dch);
  se->registerSubsystem(crk);
  se->registerSubsystem(cgl);
  se->registerSubsystem(aerocl);
  se->registerSubsystem(ring);
//  se->registerSubsystem(tofw);
  se->registerSubsystem(global);
  se->registerSubsystem(global_central);

  ////////////////////////
  SubsysReco *svxpar = new SvxParManager();
  (dynamic_cast<SvxParManager*>(svxpar))->set_ReadGeoParFromFile(1);
  se->registerSubsystem(svxpar);

  SubsysReco *svxdecode = new SvxDecode();
//  svxdecode->Verbosity(1);
  (dynamic_cast<SvxDecode*>svxdecode)->includePixel(true);
//  (dynamic_cast<SvxDecode*>svxdecode)->includeStripixel(false);
  (dynamic_cast<SvxDecode*>svxdecode)->includeStripixel(true);
  (dynamic_cast<SvxDecode*>svxdecode)->setAdcOffset(24);
  (dynamic_cast<SvxDecode*>svxdecode)->setAdcCutoff(0);
  se->registerSubsystem(svxdecode);

  
  SubsysReco *svxrec = new SvxReco();
  (dynamic_cast<SvxReco*>(svxrec))->set_StripixelAdcThreshold(30);
  (dynamic_cast<SvxReco*>(svxrec))->set_StripixelAdcSumThreshold(30);
  svxrec->Verbosity(1);
  se->registerSubsystem(svxrec);
  
  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);
  se->registerSubsystem(svxstandalone);
  

  Fun4AllInputManager *inMan = new Fun4AllNoSyncDstInputManager("DSTin1","DST");
  inMan->Verbosity(0);
  se->registerInputManager(inMan);

  Fun4AllDstOutputManager *svxDST  = new Fun4AllDstOutputManager("SIMDST", fileout );
  svxDST->AddNode("EventHeader");
  svxDST->AddNode("TrigLvl1");
  svxDST->AddNode("BbcOut");
  svxDST->AddNode("BbcRaw");
  svxDST->AddNode("VtxOut");

  svxDST->AddNode("ErtOut");
  svxDST->AddNode("Pc1Cluster");
  svxDST->AddNode("Pc2Cluster");
  svxDST->AddNode("Pc3Cluster");
  svxDST->AddNode("TofOut");
  svxDST->AddNode("DchHitLineTable");
  svxDST->AddNode("DchTrack");
  svxDST->AddNode("CrkHit");
  svxDST->AddNode("AccCluster");
  svxDST->AddNode("emcClusterContainer");
  svxDST->AddNode("emcClusterAuxInfo");
  svxDST->AddNode("emcHitContainer");
  svxDST->AddNode("CglTrack");
  svxDST->AddNode("CglTrackBack");
  svxDST->AddNode("PHTrackOut");
  svxDST->AddNode("PHTrackOutBack");
  svxDST->AddNode("PHDchTrackOut");
  svxDST->AddNode("CrkRing");
  svxDST->AddNode("CrkRingBack");
  svxDST->AddNode("CrkProj");
  svxDST->AddNode("CrkProjBG");
  svxDST->AddNode("PHGlobal");
  svxDST->AddNode("PHGlobal_CENTRAL");


//  svxDST->AddNode("fkin");
//  svxDST->AddNode("SvxPisaHit");
//  svxDST->AddNode("SvxGhitList");
  svxDST->AddNode("SvxRawhitList");
  svxDST->AddNode("SvxRawhitClusterList");
  svxDST->AddNode("SvxClusterList");
  svxDST->AddNode("SvxSegmentList");

  se->registerOutputManager(svxDST);
  svxDST->Print();


  pfileopen(filein);

  prun(nEvents);

  se->End();

}
