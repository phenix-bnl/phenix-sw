{
gSystem->Load("libfun4all");
gSystem->Load("libsvx");

  PHTimeStamp* Tbeg = new PHTimeStamp(2010,1,1,0,0,0);
  PHTimeStamp* Tend = new PHTimeStamp(2029,12,31,0,0,0);
  Tbeg->print(); cout << endl; Tend->print(); cout << endl;

  svxAddress* a = new svxAddress();
  a->set_Verbose(1);
  a->Initialize();

  a->Update(Tbeg, Tend);

}

