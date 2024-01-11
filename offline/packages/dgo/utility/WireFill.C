void WireFill(int start, char * wirefile)
{

  gSystem->Load("libfun4all.so");

  //  Used to disable wire nudges...
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag  ("DCHWIRETILT"    ,0); 
  rc->set_IntFlag  ("DCHBEAMPOSITION",0);
  rc->set_FloatFlag("ALPHATESTFACTOR",0);

  // "RunToTimeFede"...
  PHTimeStamp Tfetch;

  RunToTime *rtt = RunToTime::instance();
  TFetch = *(rtt->getBeginTime(start));

  //  Set up bank names and validity ranges...
  PdbBankID bankID;
  bankID.set("*.DCH.FIRST.AUTO");
  bankID.setInternalValue(1); 

  //  Set up BASE names of databases
  char * addressNameDB  = "calibdch_v3_add";
  char * geometryNameDB = "calibdch_v3_geo";
  char * noiseNameDB    = "calibdch_v3_noise";

  // Make an address object for dependency purposes...
  PHDchAddressObject *add = new PHDchAddressObject();
  add->setFlagMC(0);
  add->initialize();
  add->fetch(Tfetch,addressNameDB,bankID); 
  add->commit();
  
  //  Make the Geometry Object with debug on...
  PHDchGeometryObject *geo = new PHDchGeometryObject(add,1);
  geo->setFileNames("dummy",wirefile,"dummy");
  geo->fetchWirePositionsFromFile();
  
  PHTimeStamp tstart;
  PHTimeStamp tstop;
  
  tstart = *(rtt->getBeginTime(start));
  
  tstop.setToFarFuture();
  
  char* descrip="Insertion by Hand.";
  geo->setCommittingFlag(2);
  geo->update(tstart,tstop,geometryNameDB,bankID,descrip);
  geo->commit();
  delete geo;
  
  
}
