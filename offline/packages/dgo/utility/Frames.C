void Frames(int run)
{
  gSystem->Load("libfun4all.so");
  
  //  You can use some of these constants to
  //  disable wire shift corrections...if desired...
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag  ("DCHWIRETILT"    ,0); 
  rc->set_IntFlag  ("DCHBEAMPOSITION",0);
  rc->set_FloatFlag("ALPHATESTFACTOR",0);

  // "RunToTimeFede"...
  PHTimeStamp Tstart;
  PHTimeStamp Tstop;
  Tstop.setToFarFuture();

  RunToTime *rtt = RunToTime::instance();
  Tstart = *(rtt->getBeginTime(run));

  //  Set up bank names and validity ranges...
  PdbBankID bankID;
  bankID.set("*.DCH.FIRST.AUTO");
  bankID.setInternalValue(1); 

  //  Set up BASE names of databases
  char * calibNameDB    = "calibdch_v3_cal";
  char * addressNameDB  = "calibdch_v3_add";
  char * geometryNameDB = "calibdch_v3_geo";
  char * noiseNameDB    = "calibdch_v3_noise";

  // Make an address object for dependency purposes...
  PHDchAddressObject *add = new PHDchAddressObject();
  add->setFlagMC(0);
  add->initialize();
  add->fetch(Tstart,addressNameDB,bankID); 
  add->commit();
  
  //  Make the Geometry Object with debug on...
  PHDchGeometryObject *geo = new PHDchGeometryObject(add,1);
  geo->setCommittingFlag(3);  // 1=Geo 2=Wires

  // You can use lines like below to read from file...
  geo->setFileNames("NONE","NONE","FramesData.dat","NONE","NONE","NONE","NONE");
  geo->fetchFramesFromFile(); // should fetch the frames...
  geo->screenDump();

  char *descrip = "Initial Run5 Alignment SCC TKH";
  geo->update(Tstart,Tstop,geometryNameDB,bankID,descrip);
  geo->commit();
  delete geo;

}
