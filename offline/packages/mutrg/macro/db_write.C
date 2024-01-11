void db_write(char *filename){
  gSystem->Load("libpdbcalBase.so");
  gSystem->Load("libPgCal.so");
  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("libmutrg.so");

  MutrgDBIOTrkMap *dbio_trk=new MutrgDBIOTrkMap();
  dbio_trk->ReadFile(filename);
  dbio_trk->SetVersion(MutrgPar::TRKDV_V3_MUTRG_SG2);
  dbio_trk->SetDescription("MuTRG v3 map, SG2, w/o RPC");
  dbio_trk->SetUserName("fukao");
  dbio_trk->SetTimeStart(2000,1,1,0,0,0);
  dbio_trk->SetTimeEnd(2030,1,1,0,0,0);
  dbio_trk->WriteDB();

  return;
}
