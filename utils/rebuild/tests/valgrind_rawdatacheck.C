void valgrind_rawdatacheck(const int nevents=1000, const char* input="/phenix/data14/phnxreco/TestBuild/prdfs/EVENTDATAxxx_P01-0000120849-0001.PRDFF")
{
  gSystem->Load("libRawDataCheck.so"); 
  recoConsts *rc = recoConsts::instance();
  rc->set_CharFlag("CVSTAG", "testrun");
  Fun4AllServer *se = Fun4AllServer::instance();
  RawDataCheck *chk = RawDataCheck::instance();

  //  Create Check Modules
  GranuleCheck *Acc = new CheckAcc();
  GranuleCheck *Bbc = new CheckBbc();
  GranuleCheck *DchEast = new CheckDch("EAST");
  GranuleCheck *DchWest = new CheckDch("WEST");
  GranuleCheck *EmcEastBottom = new CheckEmc("EAST", "BOTTOM");
  GranuleCheck *EmcEastTop = new CheckEmc("EAST", "TOP");
  GranuleCheck *EmcWestBottom = new CheckEmc("WEST", "BOTTOM");
  GranuleCheck *EmcWestTop = new CheckEmc("WEST", "TOP");
  GranuleCheck *ErtEast = new CheckErt("EAST");
  GranuleCheck *ErtWest = new CheckErt("WEST");
  GranuleCheck *Hbd = new CheckHbd();
  GranuleCheck *Mpc = new CheckFcal("MPC");
  GranuleCheck *MuidNorth = new CheckMuid("NORTH");
  GranuleCheck *MuidSouth = new CheckMuid("SOUTH");
  GranuleCheck *MutrNorth = new CheckMutr("NORTH");
  GranuleCheck *MutrSouth = new CheckMutr("SOUTH");
  GranuleCheck *PadEast = new CheckPad("EAST");
  GranuleCheck *PadWest = new CheckPad("WEST");
  GranuleCheck *RichEast = new CheckRich("EAST");
  GranuleCheck *RichWest = new CheckRich("WEST");
  GranuleCheck *Rxnp = new CheckFcal("RXNP");
  GranuleCheck *Tec = new CheckTec();
  GranuleCheck *TofEast = new CheckTof("EAST");
  GranuleCheck *TofWest = new CheckTof("WEST");
  GranuleCheck *Zdc = new CheckZdc();

  //  OK, register them all...
  chk->registerGranule(Acc);
  chk->registerGranule(Bbc);
  chk->registerGranule(DchEast);
  chk->registerGranule(DchWest);
  chk->registerGranule(EmcEastBottom);
  chk->registerGranule(EmcEastTop);
  chk->registerGranule(EmcWestBottom);
  chk->registerGranule(EmcWestTop);
  chk->registerGranule(ErtEast);
  chk->registerGranule(ErtWest);
  chk->registerGranule(Hbd);
  chk->registerGranule(Mpc);
  chk->registerGranule(MuidNorth);
  chk->registerGranule(MuidSouth);
  chk->registerGranule(MutrNorth);
  chk->registerGranule(MutrSouth);
  chk->registerGranule(PadEast);
  chk->registerGranule(PadWest);
  chk->registerGranule(RichEast);
  chk->registerGranule(RichWest);
  chk->registerGranule(Rxnp);
  chk->registerGranule(Tec);
  chk->registerGranule(TofEast);
  chk->registerGranule(TofWest);
  chk->registerGranule(Zdc);

  // register everything with Fun4All
  se->registerSubsystem(chk);
  chk->UpdateDB(0); // prevent db updates in test mode, 

  pfileopen(input);
  prun(nevents);
  se->End();
  delete se;
}
