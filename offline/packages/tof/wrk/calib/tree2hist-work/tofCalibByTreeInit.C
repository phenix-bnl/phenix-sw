// TofTree Analisys 
{
  Int_t verbose = 12;

  // Initialization
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libphgeo.so");

  // Loading subsystem libraries
  gSystem->Load("libtof.so");
  gSystem->Load("libtof_calib.so");

  // Specify the geometry flags and magnetic field flags for each dataset
  Int_t bFieldFlag = 1;  // 0 is off, 1 is on
  Int_t geomFlag = 1;    // 0 is retracted, 1 is standard

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();
  //TofCalibrator* TofCalibrator = new TofCalibrator();

  if (verbose>10) printf("Calling TofAddress\n");
  //TofAddress->setTimeStamp(TimeStamp);
  TofAddress->fetchFromFile("toffemmap.txt","tofcablemap.txt");

  if (verbose>10) printf("Calling TofGeometry\n");
  //TofGeometry->setTimeStamp(TimeStamp);
  // Julia's geometry set-up (alignment)
  PHPoint wo(-41,0,0);
  PHPoint eo(44,0,0);
  PHVector Xaxis(1,0,0);
  PHVector Yaxis(0,1,0);
  PHFrame eastF(eo,Xaxis,Yaxis);
  PHFrame westF(wo,Xaxis,Yaxis);
  if (geomFlag == 0) TofGeometry->setEastCarriage(eastF);
  TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");
  if (verbose>10) printf("Calling TofCalib\n");
  //TofCalib->setTimeStamp(TimeStamp);
  TofCalib->fetchPedestalFromFile("tofPedestal.txt", TofAddress);
  TofCalib->fetchTvcConvFromFile("tofTvcConv.txt", TofAddress);
  TofCalib->fetchQvcConvFromFile("tofQvcConv.txt", TofAddress);
  TofCalib->fetchToffsetFromFile("tofToffset.txt");
  TofCalib->fetchYoffsetFromFile("tofYoffset.txt");
  TofCalib->fetchVelocityFromFile("tofVelocity.txt");
  TofCalib->fetchSlewParFromFile("tofSlewPar.txt");
  TofCalib->fetchElossConvFromFile("tofElossConv.txt");
  //TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");
  TofCalib->fetchMipPeakFromFile("tofMipPeak.txt.pass0");

  if (verbose>10) printf(" === Start TofTree Analysis ===\n");
}
