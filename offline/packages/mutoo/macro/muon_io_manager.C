void muon_io_manager(const char *dstout = "/phenix/data22/zhangc/mutoo_dst.root")
{
  // Server singleton
  //
  Fun4AllServer *se = Fun4AllServer::instance();

  // DST output manager node.
  //
  Fun4AllIOManager *io = new Fun4AllIOManager("DSTOUT", dstout);

  // NDST output manager node.
  //
  char *mwgfile = "MWG.root";
  Fun4AllIOManager *io_mwg = new Fun4AllIOManager("MWGOUT",mwgfile);

  io->AddNode("TMuiHitO");
  io->AddNode("TMuiClusterO");
  io->AddNode("TMuiRoadO");
  io->AddNode("TMuiPseudoBLTO");
  //io->AddNode("TMutMuiRoad");  
  io->AddNode("TMutHit");  
  io->AddNode("TMutClus");  
  io->AddNode("TMutCoord");  
  io->AddNode("TMutGapCoord");  
  io->AddNode("TMutStub");
  io->AddNode("TMutTrk");  
  io->AddNode("TMutVtx");

  io->AddNode("RunHeader");  
  io->AddNode("EventHeader");  
  io->AddNode("VtxOut");  
  io->AddNode("BbcOut");  
  io->AddNode("BbcRaw");  
  io->AddNode("GlpOut");  
  io->AddNode("dGl1AcptEvtDCM");  
  io->AddNode("TrigLvl1");  
    // For ndst
  //
  io_mwg->AddNode("PHGlobal");
  io_mwg->AddNode("PHMuoTracksOO");

  se->registerIOManager(io);
  se->registerIOManager(io_mwg);

  io->Print();
  io_mwg->Print();
}

