{
// This macro writes to the database VTX geometry represented
// by PHPanels. These panels are used for global tracking

gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxDetectorGeo* geo = new svxDetectorGeo();
  geo->set_Verbose(1);
  geo->FetchFromFile(); // read PHPanels from svxGeometry.txt file
                        // this file is created from svaPISA.par file
                        // by create_svxGeometry.C macro

  //PHTimeStamp* Tbeg = new PHTimeStamp(2009,9,17,0,0,0);
  PHTimeStamp* Tbeg = new PHTimeStamp(2010,8,9,0,0,0);
  PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
  Tbeg->print(); cout << endl; Tend->print(); cout << endl;

  
    cout << "*** starting update..." << endl;
    PHBoolean success2 = geo->Update(Tbeg, Tend);
    cout << "success = " << success2 << endl << endl;

}

