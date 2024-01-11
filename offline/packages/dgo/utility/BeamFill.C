void BeamFill(int start, char * beamfile)
{
  //int start=1;
  //char * beamfile="beams_zero.dat";

  gSystem->Load("libfun4all.so");
  
  // Make an address object for dependency purposes...
  PHDchAddressObject *add = new PHDchAddressObject();
  add->setFlagMC(0);
  add->initialize();
  
  //  Make the Geometry Object with debug on...
  PHDchGeometryObject *geo = new PHDchGeometryObject(add,1);
  geo->setFileNames("dummy","dummy","dummy","dummy","dummy",beamfile);
  geo->fetchBeamsFromFile();
  
  //  Set up bank names and validity ranges...
  PdbBankID bankID;
  bankID.set("*.DCH.FIRST.AUTO");
  bankID.setInternalValue(1); 
  char * geometryNameDB = "calibdch_v3_geo";
  
  PHTimeStamp tstart;
  PHTimeStamp tstop;

  RunToTime *rtt = RunToTime::instance();
  tstart = *(rtt->getBeginTime(start));

  tstop.setToFarFuture();
  
  char* descrip="Insertion by Hand.";
  geo->setCommittingFlag(6);
  geo->screenDump();
  //geo->update(tstart,tstop,geometryNameDB,bankID,descrip);
  //geo->commit();
  delete geo;

}
