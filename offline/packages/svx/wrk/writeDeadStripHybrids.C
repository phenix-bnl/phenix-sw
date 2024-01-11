void writeDeadStripHybrids()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  PHTimeStamp* Tbeg = new PHTimeStamp(2010,7,1,0,0,0);
  PHTimeStamp* Tend = new PHTimeStamp();

  Tend->setToFarFuture();

  SvxDeadMap deadMap (0);
  
  //std::string infile = "DCM_Stripixel_0000347129_0000.txt"; // newer file with fewer bad hybrids
  std::string infile = "DeadChannelMap_Stripixel_0000347129-0000.txt";
  //bool firstsuccess = deadMap.readHybridsFromHybridsFile ("BadStripHybrids_347129.txt");
  bool firstsuccess = deadMap.readHybridsFromFile (infile);
  //bool zerothsuccess = deadMap.readFromFile (infile);
  if(firstsuccess) {
    bool success = deadMap.writeHybridsToDatabase (Tbeg, Tend);
  }
  else {cerr << "Failed to load dead hybrid list from a file!"<< endl;}

  cout << "writeDeadStripHybrids completed." << endl;
}

