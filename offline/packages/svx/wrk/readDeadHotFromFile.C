void readDeadHotFromFile()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";

  SvxDeadMap deadMap (0); // verbosity level

  bool success = deadMap.readFromFile ("DeadChannelMap_Stripixel_0000347129-0000.txt");
  
//  cout << "Reading deadMap from an ascii file: " << (success ? "success" : "failure")
//       << endl;
       
//  if (success) deadMap2.print();

  cout << "readDeaHhotFromFile: completed. Success = " << success << endl;
}

