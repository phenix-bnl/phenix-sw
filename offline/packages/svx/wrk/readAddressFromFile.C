{
gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  PHTimeStamp* Tsearch = new PHTimeStamp(2010,5,27,0,0,0);
  Tsearch->print(); cout << endl; 

  svxAddress* a = new svxAddress();
  a->set_Verbose(1);

//  a->Fetch(Tsearch);
  a->FetchFromFile();

}

