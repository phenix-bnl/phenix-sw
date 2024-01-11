//************************************************************
// TofAddress/TofGeometry for Run (Year1)
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
  PHTimeStamp TimeStamp = PHTimeStamp(2000,8,1,0,0,0);
  // TOF Detector Object
  TofAddressObject* TofAddress = new TofAddressObject();
  TofAddress->setTimeStamp(TimeStamp);
  TofAddress->setDebugLevel(1);
  TofAddress->fetch();
  TofAddress->writeToFile("toffemmap.out.year1");

  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofGeometry->setTimeStamp(TimeStamp);
  TofGeometry->setDebugLevel(1);
  TofGeometry->setEastCarriage(-41.0, 0.0, 0.0);
  TofGeometry->fetch();
  TofGeometry->writeToFile("tofpanelgeo.out.year1","tofslatoffset.out.year1");

  //TofAddress->print(100);
  //TofGeometry->print(250);
}
