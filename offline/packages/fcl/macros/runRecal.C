runRecal(int maxevents = 0, const char *dstinput = "dstin.root", const char *outfile = "dstout.root"){

  gSystem->Load("libfun4all.so");

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
  SubsysReco *sync = new SyncReco();
  se->registerSubsystem(sync);
  
  SubsysReco *fcal = new FclRecal;
  se->registerSubsystem(fcal);

  SubsysReco *fglobal = new GlobalReco(GlobalReco::RUN3_dAU);
  se->registerSubsystem(fglobal);

  TrigSelect *tsel = new TrigSelect("MINBIAS");
  tsel->AddTrigger("MINBIAS");
  //tsel->Verbosity(2);
  se->registerSubsystem(tsel);

  //  Fun4AllDstOutputManager *mbio = new Fun4AllDstOutputManager("MinBias", "dstout_mb.root");
  Fun4AllDstOutputManager *io = new Fun4AllDstOutputManager("AllTrig", outfile);
 
  //  io->AddNode("BbcOut");
  //  io->AddNode("ZdcOut");
  //  io->AddNode("VtxOut");
  io->AddNode("fclRawNorth");
  io->AddNode("fclRawSouth");
  //  io->AddNode("fclOutNorth");
  //  io->AddNode("fclOutSouth");

  io->AddNode("Sync");
  io->AddNode("TrigLvl1");
  io->AddNode("PHGlobal");
  io->Print();

  //  mbio->AddNode("fclRawNorth");
  //  mbio->AddNode("fclRawSouth");
//   mbio->AddNode("fclOutNorth");
//   mbio->AddNode("fclOutSouth");
//   mbio->AddNode("EventHeader");
//   mbio->AddNode("Sync");
//   mbio->AddNode("TrigLvl1");
//   mbio->AddNode("PHGlobal");
//   mbio->Print();



//  mbio->AddEventSelector("MINBIAS");
  se->registerOutputManager(io);
  //  se->registerOutputManager(mbio);

  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
  std::cout<<"runRecal.C Opening "<<dstinput<<std::endl;

  se->fileopen("DSTin1",dstinput);
  se->run(maxevents);
  se->End();

}
