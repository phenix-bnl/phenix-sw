void centcalib(int start=0, int stop=99999) {
                                                                                                                             
  gSystem->Load("libfun4all.so");
  gSystem->Load("/phenix/data23/lebedev/offline/packages/uti/install/lib/libuti.so");
                                                                                                                             
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
                                                                                                                             
  SubsysReco *calib = new CentralityCalibrator();
  se->registerSubsystem(calib);
                                                                                                                             
// Input manager(s)
  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  se->registerInputManager(in1);
                                                                                                                             
  ifstream list1;
  list1.open("test.txt");
                                                                                                                             
  cout << "Analysis started ";
  gSystem->Exec("date");
                                                                                                                             
  char filename1[250];
  for(int i=0; i<99999; i++) {
                                                                                                                             
    list1 >> filename1;
    if(list1.eof()) break;
    if(strlen(filename1)==0) continue;
                                                                                                                             
    if(i>=start && i<=stop) {
      cout << i << " "; cout << " file: " << filename1 << endl;
      se->fileopen(in1->Name(),filename1);
      se->run();
    }
                                                                                                                             
  }
                                                                                                                             
//  se->fileclose(in1->Name()); // this is necessary only if you don't process all events
                                                                                                                             
  se->End();
                                                                                                                             
  cout << "Analysis finished ";
  gSystem->Exec("date");
                                                                                                                             
}
                                                                                                                             

