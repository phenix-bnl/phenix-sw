void writeStripThresholds()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  PHTimeStamp* Tbeg = new PHTimeStamp(2010,7,1,0,0,0);
  PHTimeStamp* Tend = new PHTimeStamp();
  Tend->setToFarFuture();
  PHTimeStamp* Tsearch = new PHTimeStamp(2011,7,1,0,0,0);

  svxAddress* address = new svxAddress();
  address->set_usedatabase(1);  // use database
  address->setFetchTime(Tsearch);
  address->Initialize();

  StripThreshold = new SvxStripThreshold(address);
  
  bool success1 = StripThreshold->readFromFile("DeadChannelMap_Stripixel_0000347129-0000.txt");
  //bool success = StripThreshold->readFromThresholdFile("threshold.h");
  cout << "Reading thresholds from file: " << success1 << endl;
  
//  StripThreshold->print(0, 0, 0);

  bool success2 = StripThreshold->writeToDatabase(Tbeg, Tend);
  cout << "Writing thresholds to database: " << success2 << endl;

  cout << "writeStripThresholds.C completed." << endl;
}

