void test_svxaddrDB(){
  gSystem->Load("libsvx.so");

  //int runnumber = 340000;

  svxAddress& addr = svxAddress::getInstance();

  //----- fetch -----
  //int runnumber = 340200; //new
  int runnumber = 340000; // old for example
  PHTimeStamp *beginTime = RunToTime::instance()->getBeginTime(runnumber);
  addr.set_usedatabase(1);
  addr.setFetchTime(beginTime);
  addr.Initialize();

 //------ commit  ------

/*
  addr.set_usedatabase(0); // initialized with default

  PHTimeStamp endTime;
  endTime.setToFarFuture();

  //-------------
  // oldmap
  addr.set_useoldmap(true);  // after run 340200 
  PHTimeStamp *beginTime = new PHTimeStamp(0);
  char *desc = "map of vtx pixel module <-> packetId";
  //-------------

  //-------------
  // newmap
  addr.set_useoldmap(false);  // after run 340200 
  int runnumber = 340200;
  PHTimeStamp *beginTime = RunToTime::instance()->getBeginTime(runnumber);
  cout<<beginTime->getTics()<<endl;
  char *desc = "connection was changed from run 340200 (run11)";
  //-------------


  addr.Initialize();


  addr.commitPixelMap(desc, beginTime, &endTime);
*/

  //----- view ------

  addr.showPixelPacketID();
}
