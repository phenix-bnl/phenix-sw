
// This macro can be executed with RunMyMacro.C
void Run_RPCalibrator(char* output = "RPCalib.root"){    
  gSystem->Load( "libreactionplane.so" );
  
  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);  
  recoConsts *rc = recoConsts::instance();
  
  int iseg = 0005 ;
  int recalflag = 0; // 0:re-centering 1:flattening 2:use calibration parameter 
  int masterflag= 1;
  int vnflag=0;

  RPCalibrator *ana = new RPCalibrator(output);

  ana->setRecalFlag(recalflag);
  //ana->setMasterRecalFlag(masterflag);
  //ana->setVnFlag(vnflag);
  se->registerSubsystem(ana);// need
  
}


void InputData(vector<string> &indata)
{
  indata.push_back("DST_EVE");
  indata.push_back("CNT");

  return;
}
