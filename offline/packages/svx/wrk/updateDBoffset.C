class PHTimeStamp;

void printTime(PHTimeStamp *time);

void updateDBoffset(){
  gSystem->Load("libsvx.so");

  cout<<endl;
  cout<<"Database setting"<<endl;
  cout<<"ODBCINI : "<<gSystem->Getenv("ODBCINI")<<endl;
  cout<<"PGHOST  : "<<gSystem->Getenv("PGHOST")<<endl;


  svxDetectorGeo *geo = new svxDetectorGeo();
  geo->set_Verbose(99); 
  geo->setOffsetUsedRunNumber(-1);//run number of zero field run
  geo->setOffsetGeometryVersion(-1);// cvs version of svxPISA.par
  geo->setOffsetEastToWest(0, 0, 0);
  geo->setOffsetVtxToCnt(0, 0, 0);
  geo->setOffsetCntEastToWest(0, 0, 0);

  ////////////////////
  double dEW[3], dCntVtx[3], dCntEW[3];
  int usedRun = geo->getOffsetUsedRunNumber();
  int geoVer  = geo->getOffsetGeometryVersion();
  geo->getOffsetEastToWest(dEW[0], dEW[1], dEW[2]);
  geo->getOffsetVtxToCnt(dCntVtx[0], dCntVtx[1], dCntVtx[2]);
  geo->getOffsetCntEastToWest(dCntEW[0], dCntEW[1], dCntEW[2]);

  cout << std::endl;
  cout << "Used RunNumber   : " << usedRun << std::endl;

  cout << "Offset Vtx East to West : " << dEW[0]    <<", "<<dEW[1]    <<", "<<dEW[2]    << endl;
  cout << "Offset Cnt to Vtx       : " << dCntVtx[0]<<", "<<dCntVtx[1]<<", "<<dCntVtx[2]<<endl;
  cout << "Offset Cnt East to West : " << dCntEW[0] <<", "<<dCntEW[1] <<", "<<dCntEW[2] <<endl;

  cout << "Geometry Version : " << geoVer << std::endl;


  int run = 349679;
  int beginRun = 342346;
  int endRun   = 343030;


  RunToTime   *rt      = RunToTime::instance();

  PHTimeStamp *Tsearch = rt->getBeginTime(run);     printTime(Tsearch);
  PHTimeStamp *Tbegin = rt->getBeginTime(beginRun); printTime(Tbegin);
  PHTimeStamp *Tend   = rt->getEndTime(endRun);     printTime(Tend);
//  PHTimeStamp *Tbegin = new PHTimeStamp(1999, 1,1,0,0,0); printTime(Tbegin);
//  PHTimeStamp *Tend   = new PHTimeStamp(2030,12,31,0,0,0); printTime(Tend);


  char *c_desc = "Default value (0,0,0) is updated for the test purpose1. T.Hachiya";
  geo->Update_coordinateOffset(Tbegin, Tend, c_desc); 

}

void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
