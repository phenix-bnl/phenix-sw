
#include <cmath>
#include <iomanip>
#include <iostream>

#include <phool.h>
#include "AccCalibPar.h"
#include "AccCalib.h"

using namespace std;

//____________________________________________________________________________
AccCalib::AccCalib():
  acc_adcpedestal("adcpedestal"),
  acc_adcgain("adcgain"),
  acc_tdcpedestal("tdcpedestal"),
  acc_tdcgain("tdcgain"),
  acc_slewing("slewing")
{}

//____________________________________________________________________________
AccCalib::~AccCalib()
{}

//____________________________________________________________________________
float AccCalib::get_Npe(const int ich, const int adc)
{
  // get number of photoelectron

  float pedestal = acc_adcpedestal.get_CalibPar(ich,0);
  float adcgain  = acc_adcgain.get_CalibPar(ich,0);

  return (adcgain!=0.) ? (adc - pedestal) / adcgain : 0.0 ;
}

//____________________________________________________________________________
float AccCalib::get_Timing(const int ich, const int adc, const int tdc)
{
  // get Timing

  float adcpedestal = acc_adcpedestal.get_CalibPar(ich,0);
  float adcgain     = acc_adcgain.get_CalibPar(ich,0);
  float tdcpedestal = acc_tdcpedestal.get_CalibPar(ich,0);
  float tdcsigma    = acc_tdcpedestal.get_CalibPar(ich,1);
  float tdcgain     = acc_tdcgain.get_CalibPar(ich,0);
  float slewing0    = acc_slewing.get_CalibPar(ich,0);
  float slewing1    = acc_slewing.get_CalibPar(ich,1);

  float Adc = adc - adcpedestal;

  if( adcgain < 1000.
     && (tdc > 0)
     && (tdc > (tdcpedestal + NSIGMA_PEDESTAL * tdcsigma) 
     && Adc > 0.) ){

    return ( tdc * tdcgain + slewing0 + slewing1/sqrt(Adc) );
  }
  else{
    return -9999.;
  }

}

//____________________________________________________________________________
float AccCalib::get_Tof(const int ibox, const int* adc, const int* tdc)
{
  // get Time of Flight
  // TOF = (Timing0 + Timing1)/2

  float timing[2];
  for(int ipmt=0;ipmt<2;ipmt++){
    int ich = ACC::getPmtID(ipmt, ibox);
    timing[ipmt] = AccCalib::get_Timing(ich, adc[ich], tdc[ich]);
  }

  return (timing[0]+timing[1])/2.;
}

//____________________________________________________________________________
float AccCalib::get_Tdiff(const int ibox, const int* adc, const int* tdc)
{
  // get timing difference
  // Tdiff = (Timing0 - Timing1)/2

  float timing[2];
  for(int ipmt=0;ipmt<2;ipmt++){
    int ich = ACC::getPmtID(ipmt, ibox);
    timing[ipmt] = AccCalib::get_Timing(ich, adc[ich], tdc[ich]);
  }

  return (timing[0]-timing[1])/2.;
}

//____________________________________________________________________________
float AccCalib::get_Zpos(const int ibox, const int* adc, const int* tdc)
{
  // get Z position
  // not implemented yet
  
  return -9999;
}

float AccCalib::get_adcpedvalue(const int  ich, const int  sec)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    return -9999;
  }
  float adcpedestal = acc_adcpedestal.get_CalibPar(ich, sec);
  return adcpedestal;
}

float AccCalib::get_adcgainvalue(const int  ich, const int  sec)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    return -9999;
  }
  float adcgain = acc_adcgain.get_CalibPar(ich, sec);
  return adcgain;
}

float AccCalib::get_tdcpedvalue(const int  ich, const int  sec)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    return -9999;
  }
  float tdcpedestal = acc_tdcpedestal.get_CalibPar(ich, sec);
  return tdcpedestal;
}

float AccCalib::get_tdcgainvalue(const int  ich, const int  sec)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    return -9999;
  }
  float tdcgain = acc_tdcgain.get_CalibPar(ich, sec);
  return tdcgain;
}

float AccCalib::get_slewingvalue(const int  ich, const int  sec)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    return -9999;
  }
  float slewing = acc_slewing.get_CalibPar(ich, sec);
  return slewing;
}

void AccCalib::set_adcpedstatus(const int ich, const int val)
{
  if(ich<0 || ich>ACC::ACC_NCH){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  acc_adcpedestal.set_Status(ich, val);
}

void AccCalib::set_adcgainstatus(const int ich, const int val)
{
  if(ich<0 || ich>ACC::ACC_NCH){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  acc_adcgain.set_Status(ich, val);
}

void AccCalib::set_tdcpedstatus(const int ich, const int val)
{
  if(ich<0 || ich>ACC::ACC_NCH){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  acc_tdcpedestal.set_Status(ich, val);
}

void AccCalib::set_tdcgainstatus(const int ich, const int val)
{
  if(ich<0 || ich>ACC::ACC_NCH){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  acc_tdcgain.set_Status(ich, val);
}

void AccCalib::set_slewingstatus(const int ich, const int val)
{
  if(ich<0 || ich>ACC::ACC_NCH){
    cout << PHWHERE << " Invalid channel number, ch= " << ich << endl;
    return;
  }

  acc_slewing.set_Status(ich, val);
}

void AccCalib::set_adcpedvalue(const int ich, const int sec, const float val)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    cout << PHWHERE << " Invalid channel or sec, (ich,sec)=(" << ich << "," << sec << ")" << endl; 
    return;
  }

  acc_adcpedestal.set_Par(ich, sec, val);
}

void AccCalib::set_adcgainvalue(const int ich, const int sec, const float val)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    cout << PHWHERE << " Invalid channel or sec, (ich,sec)=(" << ich << "," << sec << ")" << endl; 
    return;
  }

  acc_adcgain.set_Par(ich, sec, val);
}

void AccCalib::set_tdcpedvalue(const int ich, const int sec, const float val)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    cout << PHWHERE << " Invalid channel or sec, (ich,sec)=(" << ich << "," << sec << ")" << endl; 
    return;
  }

  acc_tdcpedestal.set_Par(ich, sec, val);
}

void AccCalib::set_tdcgainvalue(const int ich, const int sec, const float val)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    cout << PHWHERE << " Invalid channel or sec, (ich,sec)=(" << ich << "," << sec << ")" << endl; 
    return;
  }

  acc_tdcgain.set_Par(ich, sec, val);
}

void AccCalib::set_slewingvalue(const int ich, const int sec, const float val)
{
  if(ich<0 || ich>ACC::ACC_NCH || sec<0 || sec>3){
    cout << PHWHERE << " Invalid channel or sec, (ich,sec)=(" << ich << "," << sec << ")" << endl; 
    return;
  }

  acc_slewing.set_Par(ich, sec, val);
}

//____________________________________________________________________________
int AccCalib::fetch(const int run)
{
  // Fetch parameters from run number

  int status  = fetchAdcPedestal(run);
  status     += fetchAdcGain(run);
  status     += fetchTdcPedestal(run);
  status     += fetchTdcGain(run);
  status     += fetchSlewing(run);

  return status;
}

//____________________________________________________________________________
int AccCalib::fetch(const char* filename)
{
  // Fetch parameters from file 
  // -. filename.calibname

  string head = filename;

  return fetch(head);
}

//____________________________________________________________________________
int AccCalib::fetch(string filename)
{
  // Fetch parameters from file 
  // -. filename.calibname

  int status  = acc_adcpedestal.fetch(filename + "." + acc_adcpedestal.get_calibName());
  status     += acc_adcgain.fetch(filename  + "." + acc_adcgain.get_calibName());
  status     += acc_tdcpedestal.fetch(filename + "." + acc_tdcpedestal.get_calibName());
  status     += acc_tdcgain.fetch(filename  + "." + acc_tdcgain.get_calibName());
  status     += acc_slewing.fetch(filename  + "." + acc_slewing.get_calibName());

  return status;
}

//____________________________________________________________________________
// fetch parameter from run number
//____________________________________________________________________________
int AccCalib::fetchAdcPedestal(const int run)
{
  return acc_adcpedestal.fetch(run);
}

//____________________________________________________________________________
int AccCalib::fetchAdcGain(const int run)
{
  return acc_adcgain.fetch(run);
}

//____________________________________________________________________________
int AccCalib::fetchTdcPedestal(const int run)
{
  return acc_tdcpedestal.fetch(run);
}

//____________________________________________________________________________
int AccCalib::fetchTdcGain(const int run)
{
  return acc_tdcgain.fetch(run);
}

//____________________________________________________________________________
int AccCalib::fetchSlewing(const int run)
{
  return acc_slewing.fetch(run);
}

//____________________________________________________________________________
// fetch parameter from file
//____________________________________________________________________________
int AccCalib::fetchAdcPedestal(const char* filename)
{
  return acc_adcpedestal.fetch(filename);
}

//____________________________________________________________________________
int AccCalib::fetchAdcGain(const char* filename)
{
  return acc_adcgain.fetch(filename);
}

//____________________________________________________________________________
int AccCalib::fetchTdcPedestal(const char* filename)
{
  return acc_tdcpedestal.fetch(filename);
}

//____________________________________________________________________________
int AccCalib::fetchTdcGain(const char* filename)
{
  return acc_tdcgain.fetch(filename);
}

//___________________________________________________________________
int AccCalib::write()
{
  // Write parameters to default file
  // -. AccCalibPar.calibname

  return write(acc_adcpedestal.GetName());
}

//___________________________________________________________________
int AccCalib::write(const char* filename)
{
  // Write parameters to default file
  // -. filename.calibname

  string head = filename;

  return write(head);
}

//___________________________________________________________________
int AccCalib::write(string filename)
{
  // Write parameters to default file
  // -. filename.calibname

  int status  = (acc_adcpedestal.isCalibrationOK()) 
              ? writeAdcPedestal(filename + "." + acc_adcpedestal.get_calibName()) : 0 ;

  status     += (acc_adcgain.isCalibrationOK())  
              ? writeAdcGain(filename  + "." + acc_adcgain.get_calibName())  : 0 ;

  status     += (acc_tdcpedestal.isCalibrationOK()) 
              ? writeTdcPedestal(filename + "." + acc_tdcpedestal.get_calibName()) : 0 ;

  status     += (acc_tdcgain.isCalibrationOK())  
              ? writeTdcGain(filename  + "." + acc_tdcgain.get_calibName())  : 0 ;

  status     += (acc_slewing.isCalibrationOK()) 
              ? writeSlewing(filename  + "." + acc_slewing.get_calibName())  : 0 ;

  return status;
}

// write
//___________________________________________________________________
int AccCalib::writeAdcPedestal(const char* filename)
{
  return acc_adcpedestal.write(filename);
}

//___________________________________________________________________
int AccCalib::writeAdcGain(const char* filename)
{
  return acc_adcgain.write(filename);
}

//___________________________________________________________________
int AccCalib::writeTdcPedestal(const char* filename)
{
  return acc_tdcpedestal.write(filename);
}

//___________________________________________________________________
int AccCalib::writeTdcGain(const char* filename)
{
  return acc_tdcgain.write(filename);
}

//___________________________________________________________________
int AccCalib::writeSlewing(const char* filename)
{
  return acc_slewing.write(filename);
}

//___________________________________________________________________
int AccCalib::writeAdcPedestal(const string filename)
{
  return writeAdcPedestal(filename.c_str());
}

//___________________________________________________________________
int AccCalib::writeAdcGain(const string filename)
{
  return writeAdcGain(filename.c_str());
}

//___________________________________________________________________
int AccCalib::writeTdcPedestal(const string filename)
{
  return writeTdcPedestal(filename.c_str());
}

//___________________________________________________________________
int AccCalib::writeTdcGain(const string filename)
{
  return writeTdcGain(filename.c_str());
}

//___________________________________________________________________
int AccCalib::writeSlewing(const string filename)
{
  return writeTdcGain(filename.c_str());
}


// update
//___________________________________________________________________
int AccCalib::updateAdcPedestal(const int beginrun, const int endrun)
{
  return acc_adcpedestal.update(beginrun, endrun);
}

//___________________________________________________________________
int AccCalib::updateAdcGain(const int beginrun, const int endrun)
{
  return acc_adcgain.update(beginrun, endrun);
}

//___________________________________________________________________
int AccCalib::updateTdcPedestal(const int beginrun, const int endrun)
{
  return acc_tdcpedestal.update(beginrun, endrun);
}

//___________________________________________________________________
int AccCalib::updateTdcGain(const int beginrun, const int endrun)
{
  return acc_tdcgain.update(beginrun, endrun);
}

//___________________________________________________________________
int AccCalib::updateSlewing(const int beginrun, const int endrun)
{
  return acc_slewing.update(beginrun, endrun);
}

// Use Time Stamp
//___________________________________________________________________

int AccCalib::updateAdcPedestal(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  return acc_adcpedestal.update(tstart, tstop);
}

//___________________________________________________________________
int AccCalib::updateAdcGain(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  return acc_adcgain.update(tstart, tstop);
}

//___________________________________________________________________
int AccCalib::updateTdcPedestal(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  return acc_tdcpedestal.update(tstart, tstop);
}

//___________________________________________________________________
int AccCalib::updateTdcGain(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  return acc_tdcgain.update(tstart, tstop);
}

//___________________________________________________________________
int AccCalib::updateSlewing(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  return acc_slewing.update(tstart, tstop);
}

//___________________________________________________________________
void AccCalib::print()
{
  // print calibration parameters
  cout << endl << " -*-*-*-*- Acc calibration parameters -*-*-*-*-" << endl << endl;

  int w = 5;
  cout.precision(3);
  cout.setf(ios::left);
  cout.setf(ios::showpoint);
  for(int ich=0;ich<ACC::ACC_NCH;ich++){
    float adcpedestal = acc_adcpedestal.get_CalibPar(ich,0);
    float adcgain     = acc_adcgain.get_CalibPar(ich,0);
    float tdcpedestal = acc_tdcpedestal.get_CalibPar(ich,0);
    float tdcgain     = acc_tdcgain.get_CalibPar(ich,0);
    float slewing0    = acc_slewing.get_CalibPar(ich,0);
    float slewing1    = acc_slewing.get_CalibPar(ich,1);

    cout << " ch # " << setw(w) << ich
              << " AdcPede = "     << setw(w) << adcpedestal
              << " AdcGain = "  << setw(w) << adcgain
              << " TdcPede = " << setw(w) << tdcpedestal
              << " TdcGain = "  << setw(w) << tdcgain
	      << " Slewing0 = "  << setw(w) << slewing0
              << " Slewing1 = "  << setw(w) << slewing1
              << endl;
  }

  cout << endl << " -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*- " << endl << endl;

  return;
}

//___________________________________________________________________
void AccCalib::Verbosity(const int v)
{
  verbosity = v;

  acc_adcpedestal.set_verbosity(v);
  acc_adcgain.set_verbosity(v);
  acc_tdcpedestal.set_verbosity(v);
  acc_tdcgain.set_verbosity(v);
  acc_slewing.set_verbosity(v);
}
