class PHTimeStamp;

void fetchsvxPISApar()
{

  gSystem->Load("libsvx.so");
  svxDetectorGeo* svxgeo = new svxDetectorGeo();
  svxgeo->set_Verbose(999); 

  cout<<endl;
  cout<<"Database setting"<<endl;
  cout<<"ODBCINI : "<<gSystem->Getenv("ODBCINI")<<endl;
  cout<<"PGHOST  : "<<gSystem->Getenv("PGHOST")<<endl;
  
  int run = 347128;
  RunToTime   *rt      = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);     printTime(Tsearch);

//  PHTimeStamp* Tsearch = new PHTimeStamp(2005,1,1,0,0,0);
//  PHTimeStamp* Tsearch = new PHTimeStamp(2009,10,1,0,0,0);
//  Tsearch->print(); cout << endl; 

  cout << "start reading database..." << endl;
  PHBoolean success2 = svxgeo->Fetch_svxPISApar(Tsearch);
  cout << "   ...success = " << success2 << endl << endl;
  
  svxgeo->Write_svxPISApar("svxPISA.par.fromDB");
  
}


void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
