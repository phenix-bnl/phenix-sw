class PHTimeStamp;

void printTime(PHTimeStamp *time);

void fetchDBoffset(){
  gSystem->Load("libsvx.so");

  gSystem->ListLibraries();

  cout<<endl;
  cout<<"Database setting"<<endl;
  cout<<"ODBCINI : "<<gSystem->Getenv("ODBCINI")<<endl;
  cout<<"PGHOST  : "<<gSystem->Getenv("PGHOST")<<endl;

  svxDetectorGeo *geo = new svxDetectorGeo();
  geo->set_Verbose(99); 

  int run      = 342401;
  int beginRun = 342346;
  int endRun   = 343030;


  RunToTime   *rt      = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);     printTime(Tsearch);

  geo->Fetch_coordinateOffset(Tsearch); 

  double dEW[3], dCntVtx[3], dCntEW[3];
  int usedRun = geo->getOffsetUsedRunNumber();
  int geoVer  = geo->getOffsetGeometryVersion();
  geo->getOffsetEastToWest(dEW[0], dEW[1], dEW[2]);
  geo->getOffsetVtxToCnt(dCntVtx[0], dCntVtx[1], dCntVtx[2]);
  geo->getOffsetCntEastToWest(dCntEW[0], dCntEW[1], dCntEW[2]);

  cout << std::endl;
  cout << "Used RunNumber   : " << usedRun << std::endl;

  cout << "Offset Vtx East to West : " << dEW[0]    <<", "<<dEW[1]    <<", "<<dEW[2]    << endl;
  cout << "Offset Vtx to Cnt       : " << dCntVtx[0]<<", "<<dCntVtx[1]<<", "<<dCntVtx[2]<<endl;
  cout << "Offset Cnt East to West : " << dCntEW[0] <<", "<<dCntEW[1] <<", "<<dCntEW[2] <<endl;

  cout << "Geometry Version : " << geoVer << std::endl;


}

void printTime(PHTimeStamp *time){
  if(time!=NULL){
    time->print();
    cout<<endl;
  } else {
    cout<<"TimeStamp is Null"<<endl;
  }
}
