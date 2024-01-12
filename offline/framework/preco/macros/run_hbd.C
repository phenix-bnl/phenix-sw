void run_hbd(const char *prdffile = "data.prdf")
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libhbd.so");
  
  Fun4AllServer *se = Fun4AllServer::instance();
  recoConsts *rc = recoConsts::instance();

  //  Reconstruction Modules...
  SubsysReco *sync = new SyncReco();
  SubsysReco *head = new HeadReco();
  SubsysReco *bbc  = new BbcReco();
  SubsysReco *zdc  = new ZdcReco();
  SubsysReco *hbd  = new HbdReco();
  SubsysReco *vtx  = new VtxReco("VTX1");

  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(bbc);
  se->registerSubsystem(zdc);
  se->registerSubsystem(hbd);
  se->registerSubsystem(vtx);

  //  Output Managers...
  Fun4AllDstOutputManager *dstManager  = new Fun4AllDstOutputManager("DSTOUT",  "HBD-DST.root");
  se->registerIOManager(dstManager);
  Fun4AllDstOutputManager *ndstManager  = new Fun4AllDstOutputManager("NDSTOUT",  "HBD-nDST.root");
  se->registerIOManager(ndstManager);

  dstManager->AddNode("Sync");
  dstManager->AddNode("BbcOut");
  dstManager->AddNode("ZdcOut");
  dstManager->AddNode("HbdRaw");
  dstManager->AddNode("HbdHit");
  dstManager->AddNode("HbdCluster");
  dstManager->AddNode("VtxOut");

  ndstManager->AddNode("Sync");
  ndstManager->AddNode("HbdCluster");

  // Do IT!!!
  pfileopen(prdffile);
  prun(200);
  se->EndRun();
}
