void loadDeadHotFromFile()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  PHTimeStamp* Tbeg = new PHTimeStamp(2010,7,1,0,0,0);
  PHTimeStamp* Tend = new PHTimeStamp();

  Tend->setToFarFuture();

  SvxDeadMap deadMap (0);
  
  std::string infile = "DCM_Stripixel_0000347129_0000.txt"; // newer file with fewer bad hybrids
  //std::string infile = "DeadChannelMap_Stripixel_0000347129-0000.txt";

  cout << "reading bad hybrids..." << endl;
  bool zerothsuccess = deadMap.readReadoutsFromFile (infile);
  //bool zerothsuccess = deadMap.readReadoutsFromReadoutsFile ("BadStripHybrids_347129.txt");
  if(!zerothsuccess) return;

  cout << "reading bad channels..." << endl;
  bool firstsuccess = deadMap.readFromFile (infile);
  if(!firstsuccess) return;
  cout << "writing bad hybrids to database..." << endl;
  bool success1 = deadMap.writeReadoutsToDatabase (Tbeg, Tend);
  cout << "writing bad channels to database..." << endl;
  bool success2 = deadMap.writeToDatabase (Tbeg, Tend);
  
  cout << "success = " << success1 << " " << success2 << endl;

  cout << "loadDeadhotFromFile: completed." << endl;
}

