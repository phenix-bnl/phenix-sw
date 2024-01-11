{
  //use this macro to obtain the svxPISA.par from the DB for the run number selected  
  gSystem->Load("libfun4all");
  gSystem->Load("libsvx");

  svxDetectorGeo *geo = new svxDetectorGeo();

  int runno = 414988;

  RunToTime *rt = RunToTime::instance();
  PHTimeStamp* tStamp = rt->getBeginTime(runno);
  
  bool read_sucess = geo->Fetch_svxPISApar(tStamp);

  if(!read_sucess) cout<<"Error, failed to read geo from DB"<<endl;

  geo->Write_svxPISApar(Form("svxPISA_%d_DB_master.par",runno));


}
