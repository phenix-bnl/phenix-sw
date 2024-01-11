{
gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  svxAddress* a = new svxAddress();
  a->set_Verbose(1);

//  a->Initialize(); // by default, this will fill hard-coded map
  PHTimeStamp* Tsearch = new PHTimeStamp(2010,5,27,0,0,0);
  Tsearch->print(); cout << endl;
  a->Fetch(Tsearch);

  a->DumpIntoFile();

}

