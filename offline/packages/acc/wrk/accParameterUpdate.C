//
//  BEGIN OF RUN4 (run number : 100000)
//  start time : Tue Nov 18 13:51:22 2003
//
//


void updateGeometry(
const char* geofile="AccCalib.geo.Run4",
const int runNumber = 100000
)
{

  gSystem->Load("libphool.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("/libacc.so");

  // update parameter
  AccGeometry* geo = new AccGeometry();
  geo->set_debug(1);
  geo->fetch(geofile);
  geo->update(runNumber);
}

void updateCalibration(
const char* calibfile="AccCalib.pedestal",
const int runNumber = 100000
){

  gSystem->Load("libphool.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("/libacc.so");

  // update parameter
  AccCalib* calib = new AccCalib();
  calib->set_debug(1);

  // ex. adc pedestal
  // you should prepare adc pedestal file
  // the format is following
  // status | peak count | mean | sigma | chi2/ndf
  // status
  // 0: bad 1: good (you can add other flag for status)
  calib->fetchAdcPedestal(calibfile);
  calib->updateAdcPedestal(runNumber);
} 
