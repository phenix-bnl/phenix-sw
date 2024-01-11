{
gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxDetectorGeo* geo = new svxDetectorGeo();
  geo->set_Verbose(1);

  PHTimeStamp* Tsearch = new PHTimeStamp(2009,10,1,0,0,0);
  Tsearch->print(); cout << endl; 

  
    cout << "*** start reading..." << endl;
    PHBoolean success = geo->Fetch(Tsearch);
    cout << "success = " << success << endl << endl;

    cout << "writing out to svxGeometry.txt file... " << endl;
    PHBoolean success2 = geo->PutIntoFile("svxGeometry_fromDB.txt");
    cout << "success2 = " << success2 << endl << endl;

}

