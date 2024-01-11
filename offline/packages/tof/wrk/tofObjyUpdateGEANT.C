//************************************************************

{
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libtof.so");

  // Define the time stamp for database access
  PHTimeStamp TimeStart = PHTimeStamp(2000,1,1,0,0,0);
  PHTimeStamp TimeStop = PHTimeStamp(2000,4,30,0,0,0);

  // TOF Detector Object
  TofAddressObject* TofAddress = new TofAddressObject();
  TofAddress->fetchFromFile("toffemmap.txt.geant");
  TofAddress->setDebugLevel(1);
  TofAddress->setFemMapName("map.tof.geantfemmap0");   // for GEANT
  //TofAddress->setFemMapName("map.tof.femmap0");   // for year1
  TofAddress->update(TimeStart, TimeStop);

  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofGeometry->fetchFromFile("tofpanelgeo.txt.geant","tofslatoffset.txt");
  TofGeometry->setGeomPanelName("geom.tof.geantpanel0");     // for GEANT
  TofGeometry->setGeomSlatOffName("geom.tof.geantslatoff0"); // for GEANT
  //TofGeometry->setGeomPanelName("geom.tof.panel0");     // for year1
  //TofGeometry->setGeomSlatOffName("geom.tof.slatoff0"); // for year1
  TofGeometry->setDebugLevel(1);
  
  TofGeometry->update(TimeStart, TimeStop);

}
