{

gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxDetectorGeo* geo = new svxDetectorGeo();
  geo->set_Verbose(1);

//  PHTimeStamp* Tsearch = new PHTimeStamp(2009,10,1,0,0,0);
//  Tsearch->print(); cout << endl; 

    cout << "*** start reading..." << endl;
    PHBoolean success1 = geo->Read_svxPISApar();
    cout << "success1 = " << success1 << endl << endl;
    cout << "*** start writing..." << endl;
    PHBoolean success3 = geo->Write_svxPISApar();
    cout << "success3 = " << success3 << endl << endl;

}

