
void fetchGeometry(
const int runNumber=100000
)
{

  gSystem->Load("libphool.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libacc.so");

  AccGeometry* acc = new AccGeometry();
  acc->set_debug(1);
  acc->fetch(runNumber);
  acc->write("AccCalib.geo.new");
  
}

void fetchCalibration(
const int runNumber=100000
)
{

  gSystem->Load("libphool.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libacc.so");

  AccCalib* acc = new AccCalib();
  acc->set_debug(1);
  acc->fetchAdcPedestal(runNumber);
  acc->writeAdcPedestal("AccCalib.pedestal.new");
  
}
