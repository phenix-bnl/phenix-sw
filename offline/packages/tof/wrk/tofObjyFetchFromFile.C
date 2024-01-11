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
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);
  // TOF Detector Object
  TofAddressObject* TofAddress = new TofAddressObject();
  TofAddress->setTimeStamp(TimeStamp);
  TofAddress->setDebugLevel(1);
  TofAddress->fetchFromFile("toffemmap.txt","tofcablemap.txt");

  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofGeometry->setTimeStamp(TimeStamp);
  TofGeometry->setDebugLevel(1);
  TofGeometry->setEastCarriage(-41.0, 0.0, 0.0);
  TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");

  //TofAddress->print(100);
  //TofGeometry->print(250);
}
