void Fetch(int run)
{
  gSystem->Load("libfun4all.so");
  
  //  You can use some of these constants to
  //  disable wire shift corrections...if desired...
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag  ("DCHWIRETILT"    ,0); 
  rc->set_IntFlag  ("DCHBEAMPOSITION",0);
  rc->set_FloatFlag("ALPHATESTFACTOR",0);

  // "RunToTimeFede"...
  PHTimeStamp Tfetch;

  RunToTime *rtt = RunToTime::instance();
  Tfetch = *(rtt->getBeginTime(run));

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
  add->fetch(Tfetch,addressNameDB,bankID); 
  add->commit();
  
  //  Make the Geometry Object with debug on...
  //PHDchGeometryObject *geo = new PHDchGeometryObject(add,1);
  //geo->setCommittingFlag(5);  // 1=Geo 2=Wires
  //geo->fetch(Tfetch,geometryNameDB,bankID);
  //geo->commit();

  //  Make a Calibration Object to exercise the calibrations...
  PHDchCalibrationObject *cal = new PHDchCalibrationObject(add);
  cal->fetch(Tfetch,calibNameDB,bankID);

  PdbIndex *index = new PdbIndex(0,12799,0,"GLOBAL");
  index->setValue(4500);
  cal->printCalibration(index);
  
  index->setValue(1000);
  cal->printCalibration(index);
  
  // You can use lines like below to read from file...
  //geo->setFileNames("NONE","wireNew.txt","NONE","NONE","NONE","NONE","NONE");
  //geo->fetchWirePositionsFromFile(); // should update the wires...
  //geo->screenDump();


}
