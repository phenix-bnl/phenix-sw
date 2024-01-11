
class PHTimeStamp;

void printTime(PHTimeStamp *time);

void writePISApar(string svxpisaname)
{
  gSystem->Load("libsvx.so");
  gSystem->Load("libfun4all");
  gSystem->Load("libsimreco");

  svxDetectorGeo* svxgeo = new svxDetectorGeo();

  svxgeo->set_Verbose(99); 

//  int beginRun = 343031;
//  int endRun = 343596;

//  int beginRun = 344891;
//  int endRun = 345875;
//  int beginRun = 346009;
//  int endRun =   346670;

  int beginRun = 346973;
  int endRun =   349680;

  RunToTime   *rt      = RunToTime::instance();

  //PHTimeStamp* Tbeg = new PHTimeStamp(2009,9,17,0,0,0);
 //  PHTimeStamp* Tbeg = new PHTimeStamp(2009,8,9,0,0,0);
 // PHTimeStamp* Tend = new PHTimeStamp(2009,12,31,0,0,0);
    
  PHTimeStamp *Tbeg = rt->getBeginTime(beginRun); printTime(Tbeg);
  PHTimeStamp *Tend   = rt->getEndTime(endRun);     printTime(Tend);
 
  char *c_description = "update svx Geometry ";

  Tbeg->print(); cout << endl; Tend->print(); cout << endl;

  int success1 = svxgeo->Read_svxPISApar(svxpisaname); // read geometry from ascii file
  if(success1) {
    cout << "starting update..." << endl;
    PHBoolean success2 = svxgeo->Update_svxPISApar(Tbeg, Tend, c_description);
    cout << "   ...success = " << success2 << endl << endl;
  }
  else {
    cout << "success1 = " << success1 << endl;
  }

}

void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
