void db_read(char *filename=""){
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libmutrg.so");

  MutrgDBIOTrkMap *dbio_trk=new MutrgDBIOTrkMap();
  dbio_trk->SetVersion(MutrgPar::TRKDV_V3_MUTRG_SG1);
  dbio_trk->SetTimeSearch(2011,4,1,0,0,0);
  dbio_trk->ReadDB();

  if(string(filename)==""){
    dbio_trk->print();
  }
  else{
    ostream fs(filename);
    dbio_trk->print(fs);
  }

  return;
}
