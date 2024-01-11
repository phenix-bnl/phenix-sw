void readDeadHotHybridsFromDB()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  SvxDeadMap deadMap(0);
  PHTimeStamp* now = new PHTimeStamp();
  
  now->setToSystemTime();
  bool success = deadMap.readHybridsFromDatabase (now);
  
  bool success2 = deadMap.writeHybridsToFile ("test.txt");
  
//  if (success) deadMap.print();

  cout << "readDeadHotHybridsFromDB completed." << endl;
}

