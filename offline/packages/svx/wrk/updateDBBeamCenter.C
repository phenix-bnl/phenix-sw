class PHTimeStamp;

void printTime(PHTimeStamp *time);

void updateDBBeamCenter(){
  gSystem->Load("libsvx.so");

  cout<<endl;
  cout<<"Database setting"<<endl;
  cout<<"ODBCINI : "<<gSystem->Getenv("ODBCINI")<<endl;
  cout<<"PGHOST  : "<<gSystem->Getenv("PGHOST")<<endl;


  SvxBeamCenterPar *par = new SvxBeamCenterPar();
  par->Verbosity(99); 
  par->setParameters(0, 0, 0, 0); 
  par->print(); 

  int run = 349679;
  int beginRun = 342346;
  int endRun   = 343030;


  RunToTime   *rt      = RunToTime::instance();

  PHTimeStamp *Tsearch = rt->getBeginTime(run);     printTime(Tsearch);
//  PHTimeStamp *Tbegin = rt->getBeginTime(beginRun); printTime(Tbegin);
//  PHTimeStamp *Tend   = rt->getEndTime(endRun);     printTime(Tend);
  PHTimeStamp *Tbegin = new PHTimeStamp(1999, 1,1,0,0,0); printTime(Tbegin);
  PHTimeStamp *Tend   = new PHTimeStamp(2030,12,31,0,0,0); printTime(Tend);

  char *c_desc = "Default value (0,0) is updated for the test purpose. T.Hachiya";
  par->updateToDB(Tbegin, Tend, c_desc); 
}

void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
