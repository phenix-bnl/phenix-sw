{
// This macro created svxGeometry.txt file with VTX ladders
// represented by PHPanels using information from svxPISA.par file

gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxDetectorGeo* geo = new svxDetectorGeo();
  geo->set_Verbose(1);


    cout << "Start reading svxPISA.par..." << endl;
//    PHTimeStamp* Tsearch = new PHTimeStamp(2009,10,1,0,0,0);
//    Tsearch->print(); cout << endl; 
//    PHBoolean success = geo->Fetch(Tsearch);
    PHBoolean success1 = geo->Read_svxPISApar("svxPISA.par.new");
    cout << "   success1 = " << success1 << endl << endl;

    cout << "Create panels..." << endl;
    PHBoolean success2 = geo->CreatePanels();
    cout << "   success2 = " << success2 << endl << endl;

    //PHBoolean success3 = geo->PutIntoFile("svxGeometry_fromsvxPISApar.txt");
    //PHBoolean success3 = geo->PutIntoFile("svxGeometry_externalAlignment.txt");
    PHBoolean success3 = geo->PutIntoFile("svxGeometry_new.txt");
    cout << "   success3 = " << success3 << endl << endl;

}

