void readStripThresholds()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  PHTimeStamp* Tsearch = new PHTimeStamp(2011,7,1,0,0,0);

  svxAddress* address = new svxAddress();
  address->set_usedatabase(1);  // use database
  address->setFetchTime(Tsearch);
  address->Initialize();

  StripThreshold = new SvxStripThreshold(address);
  
  bool success1 = StripThreshold->readFromDatabase(Tsearch);
  cout << "Reading thresholds from Database: " << success1 << endl;
  
  StripThreshold->print(0, 0, 0);

  cout << "readStripThresholds.C completed." << endl;
}

