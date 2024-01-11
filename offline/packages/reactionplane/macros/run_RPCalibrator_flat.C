
// This macro can be executed with RunMyMacro.C
void run_RPCalibrator_flat(int nevt=0, char* infile, char* output = "RPCalib.root"
)
{
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libsimreco.so");
  gSystem->Load("libcompactCNT.so");
  gSystem->Load("librecal.so");
  gSystem->Load("libreactionplane.so" );
  

  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("RPCALIB_READFROMDB", 0);
  rc->set_CharFlag("RPCALIB_CALIBFILENAME", "/phenix/hhj/hachiya/13.09/source/reactionplane/macros/plot/rpcalib_recent_347602.dat");

  rc->set_IntFlag("RP_SKIP_RECENTERING", 0);
  rc->set_IntFlag("RP_SKIP_FLATTENING",  1);

  
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);  

  SubsysReco *mr = new MasterRecalibratorManager("MASTERRECALIBRATORMANAGER");
  se->registerSubsystem(mr);

  //int recalflag  = 0; // 0:re-centering 1:flattening 2:use calibration parameter 
  int recalflag  = 1; // 0:re-centering 1:flattening 2:use calibration parameter 
  int masterflag = 1; // 1;
  int vnflag     = 0;

  RPCalibrator *ana = new RPCalibrator(output);
  ana->setRecalFlag(recalflag);
  ana->setMasterRecalFlag(masterflag);
  ana->setVnFlag(vnflag);
  se->registerSubsystem(ana);// need


  //Input manager(s);
  Fun4AllInputManager *in1 = new Fun4AllDstInputManager("DSTin1","DST");
  in1->Verbosity(0);
  se->registerInputManager(in1);
  
  se->fileopen("DSTin1", infile);
  
  cout << "Analysis started " << endl;
  gSystem->Exec("date");
  
  se->run(nevt);
  
  se->End();
  
  cout << "Analysis finished " << endl;
  gSystem->Exec("date");
  
}


void InputData(vector<string> &indata)
{
  indata.push_back("DST_EVE");
  indata.push_back("CNT");

  return;
}
