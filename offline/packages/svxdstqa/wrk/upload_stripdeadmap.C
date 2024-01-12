void upload_stripdeadmap()
{
  cout << "Loading libraries...\n";
  gSystem->Load("libfun4all.so");
  gSystem->Load("libsvx.so");
  cout << "Libraries loaded...\n";
  
  const char* runFile = "run14He3Au_runs.list";
  vector<int> runNumber;
  ifstream fin(runFile);
  if (!fin)
    {
      cout << "!!ERROR!! Unable to open " << runFile << endl;
      return;
    }

  int tmpNumber;
  fin >> tmpNumber;
  while (!fin.eof())
    {
      if (true) 
	{
	  runNumber.push_back(tmpNumber);
	  cout << tmpNumber << " ";
	}
      fin >> tmpNumber;

    }
  cout << endl;
  cout << " Found " << runNumber.size() << " runs:" << endl;

  for (unsigned int irun = 0; irun < runNumber.size(); irun++)
    {

      std::string infile1 = Form("/direct/phenix+hhj/theok/theok/stability/strip_deadmaps/run14/run%d_strip_hotdeadReadouts.txt",runNumber[irun]);
      std::string infile2 = Form("/direct/phenix+hhj/theok/theok/stability/strip_deadmaps/run14/run%d_strip_hotdeadChannels.txt",runNumber[irun]);

      SvxDeadMap deadMap (0);

      bool zerothsuccess = deadMap.readReadoutsFromReadoutsFile(infile1);

      if(!zerothsuccess) 
	{
	  cout<<"failure reading readouts from file"<<endl;
	  continue;
	}

      cout << "reading bad channels..." << endl;
      bool firstsuccess = deadMap.readFromFile(infile2);
      if(!firstsuccess)
	{
	  cout<<"failure reading channels from file"<<endl;
	  continue;
	}
      
      cout << "uploading bad readouts to DB"<<endl;
      bool secondsuccess = deadMap.writeReadoutsToDatabase(runNumber[irun],runNumber[irun]);
      if(!secondsuccess)
	{
	  cout <<" failure writing readouts to DB"<<endl;
	  continue;
	}
      
      cout << "uploading bad channels to DB"<<endl;

      bool thirdsuccess = deadMap.writeToDatabase(runNumber[irun],runNumber[irun]);
      if(!thirdsuccess)
	{
	  cout <<" failure writing bad channels to DB"<<endl;
	  continue;
	}
    }
  cout << " completed." << endl;


  return;
}
