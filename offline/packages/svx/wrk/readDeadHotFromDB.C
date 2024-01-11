void readDeadHotFromDB()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  SvxDeadMap deadMap(0);
  PHTimeStamp* now = new PHTimeStamp();
  
  now->setToSystemTime();
  bool success = deadMap.readFromDatabase (now);
  
  cout << "Read deadMap from database: " << (success ? "success" : "failure")
       << endl;
       
//  if (success) deadMap.print();

  cout << "readDeadhotFromDB: completed." << endl;
}

