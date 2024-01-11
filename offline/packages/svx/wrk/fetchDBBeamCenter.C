class PHTimeStamp;

void printTime(PHTimeStamp *time);
void fetchDBBeamCenter(int runnumber){
  gSystem->Load("libsvx.so");

  cout<<endl;
  cout<<"Database setting"<<endl;
  cout<<"ODBCINI : "<<gSystem->Getenv("ODBCINI")<<endl;
  cout<<"PGHOST  : "<<gSystem->Getenv("PGHOST")<<endl;

  SvxBeamCenterPar *par = new SvxBeamCenterPar();
  par->Verbosity(99); 
  par->setParameters(10, 10, 0, 0); 
  par->print(); 

  //int run      = 342401;
//  int runnumber  = 359293;
  int beginRun = 342346;
  int endRun   = 343030;


  RunToTime   *rt      = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(runnumber);     printTime(Tsearch);

  //par->fetchFromDB(Tsearch); 
  par->fetchFromDB(runnumber); 
  par->print(); 
}

void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
