//
// This is a macro to run svx reconstruction from pisa hits. 
//
void svxPrdfRead_data
(
 int nEvents = 10, char *filein="/phenix/scratch/lebedev/EVENTDATA_P00-0000347129-0001.PRDFF", char *fileout="recoDST.root"
 )
{
  cout << "Loading libraries...\n";
//  gSystem->Load("~purschke/install/lib/libEvent.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  cout << "Libraries loaded...\n";

 gSystem->ListLibraries();
  
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(1);

/*
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RUNNUMBER",+179846);
  rc->set_IntFlag("RUNNUMBER",+179846);  // This is the reference run number used in pp 200 GeV production (Run5)

  rc->set_IntFlag("RUN2AUAU",0);        // flag for Run2 Au+Au
  rc->set_IntFlag("PPFLAG",0);          // flag for Run2 p+p events
  rc->set_IntFlag("RUN3PP",0);          // flag for Run3 p+p events
  rc->set_IntFlag("RUN3DAU",0);         // flag for Run3 d+Au
  rc->set_IntFlag("RUN4AUAU200GEV",0);  // flag for Run4 Au+Au, 200 GeV data (or pp?)
  rc->set_IntFlag("RUN4AUAU63GEV",0);   // flag for Run4 Au+Au, 63 GeV data (or pp?)
  rc->set_IntFlag("RUN5PP200GEV",1);    // flag for Run5 pp 200 GeV data
*/


  //  Reconstruction Modules...
  //--SubsysReco *head    = new HeadReco();
  //--se->registerSubsystem(head);

  //--SubsysReco *trig    = new TrigReco();
  //--se->registerSubsystem(trig);

  //--SubsysReco *bbc     = new BbcReco();
  //--se->registerSubsystem(bbc);

  //--SubsysReco *vtx     = new VtxReco();
  //--se->registerSubsystem(vtx);

  ////////////////////////
  SubsysReco *svxpar = new SvxParManager();
  svxpar->Verbosity(1);
//  (dynamic_cast<SvxParManager*>(svxpar))->set_ReadGeoParFromFile(1);
//  (dynamic_cast<SvxParManager*>(svxpar))->set_ReadStripHotDeadFromFile(true);
//  (dynamic_cast<SvxParManager*>(svxpar))->set_StripHotDeadFileName("/phenix/u/workarea/lebedev/svx/offline/packages/svx/wrk/DeadChannelMap_Stripixel_0000347129-0000.txt");
  se->registerSubsystem(svxpar);

  SubsysReco *svxdecode = new SvxDecode();
//  svxdecode->Verbosity(1);
//  (dynamic_cast<SvxDecode*>svxdecode)->setAdcOffset(24);
//  (dynamic_cast<SvxDecode*>svxdecode)->setAdcCutoff(0);
  se->registerSubsystem(svxdecode);

  SubsysReco *svxhd = new SvxApplyHotDead();
  svxhd->Verbosity(1);
  se->registerSubsystem(svxhd);

  //--
  SubsysReco *svxrec = new SvxReco();
//  (dynamic_cast<SvxReco*>(svxrec))->set_StripixelAdcThreshold(30);
//  (dynamic_cast<SvxReco*>(svxrec))->set_StripixelAdcSumThreshold(30);
  svxrec->Verbosity(1);
  se->registerSubsystem(svxrec);
  //--
  //--SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  //--svxstandalone->Verbosity(0);
  //--svxstandalone->setVertexRecoFlag(1);
  //--se->registerSubsystem(svxstandalone);
  //--

  //Fun4AllInputManager *inMan = new Fun4AllNoSyncDstInputManager("DSTin1","DST");
  Fun4AllInputManager *inMan = new Fun4AllPrdfInputManager("DSTin1");
  inMan->Verbosity(0);
  inMan->fileopen(filein);
  se->registerInputManager(inMan);

/*
  Fun4AllDstOutputManager *svxDST  = new Fun4AllDstOutputManager("SIMDST", fileout );
  svxDST->AddNode("EventHeader");
  svxDST->AddNode("TrigLvl1");
  svxDST->AddNode("BbcOut");
  svxDST->AddNode("BbcRaw");
  svxDST->AddNode("VtxOut");

//  svxDST->AddNode("fkin");
//--  svxDST->AddNode("SvxPisaHit");
//--  svxDST->AddNode("SvxGhitList");
  svxDST->AddNode("SvxRawhitList");
  //--svxDST->AddNode("SvxRawhitClusterList");
  svxDST->AddNode("SvxClusterList");
  //--svxDST->AddNode("SvxSegmentList");

  se->registerOutputManager(svxDST);
  svxDST->Print();
*/

//  se->fileopen(inMan->Name(),inputfile);

  se->run(nEvents);                 // process input events

  se->End();

}
