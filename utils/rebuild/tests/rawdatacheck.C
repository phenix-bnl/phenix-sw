// These routines need to stay together.
// The raw data check now puts bad events into a DB and
// we do not want to declare an event bad for muon reconstruction
// only
// This rarely happens anyway - the only way to get this is if
// a granule drops out of sync temporarily and I have not seen
// that in Run4
//
// chp

void rawdatacheckRun4()
{
// just in case this lib gets disentangled
  gSystem->Load("libRawDataCheck.so"); 
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
  GranuleCheck *Fcal = new CheckFcal();
  GranuleCheck *MuidNorth = new CheckMuid("NORTH");
  GranuleCheck *MuidSouth = new CheckMuid("SOUTH");
  GranuleCheck *MutrNorth = new CheckMutr("NORTH");
  GranuleCheck *MutrSouth = new CheckMutr("SOUTH");
  GranuleCheck *Mvd = new CheckMvd();
  GranuleCheck *PadEast = new CheckPad("EAST");
  GranuleCheck *PadWest = new CheckPad("WEST");
  GranuleCheck *RichEast = new CheckRich("EAST");
  GranuleCheck *RichWest = new CheckRich("WEST");
  GranuleCheck *Tec = new CheckTec();
  GranuleCheck *Tof = new CheckTof();
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
  chk->registerGranule(Fcal);
  chk->registerGranule(MuidNorth);
  chk->registerGranule(MuidSouth);
  chk->registerGranule(MutrNorth);
  chk->registerGranule(MutrSouth);
  chk->registerGranule(Mvd);
  chk->registerGranule(PadEast);
  chk->registerGranule(PadWest);
  chk->registerGranule(RichEast);
  chk->registerGranule(RichWest);
  chk->registerGranule(Tec);
  chk->registerGranule(Tof);
  chk->registerGranule(Zdc);

  // register everything with Fun4All
  se->registerSubsystem(chk);
  //  chk->UpdateDB(0); // prevent db updates in test mode, 
}

