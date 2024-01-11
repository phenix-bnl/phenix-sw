{
gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxDetectorGeo* geo = new svxDetectorGeo();
  geo->set_Verbose(1);

    cout << "Get current geometry first..." << endl;
    //PHTimeStamp* Tsearch = new PHTimeStamp(2011,10,1,0,0,0);
    //Tsearch->print(); cout << endl; 
    //PHBoolean success = geo->Fetch(Tsearch);
    PHBoolean success1 = geo->Read_svxPISApar("svxPISA.par.ExternalAlignment");
    cout << "   success1 = " << success1 << endl << endl;

    cout << "Replace strip part with survey results..." << endl;
    PHBoolean success2 = geo->Read_StripSurvey("Strippixel_Survey_fromKieran.txt");
    cout << "   success2 = " << success2 << endl << endl;

    cout << "Write out new geometry..." << endl;
    PHBoolean success3 = geo->Write_svxPISApar("svxPISA.par.new");
    cout << "success3 = " << success3 << endl << endl;

}

