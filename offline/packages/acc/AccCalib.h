
#ifndef __ACCCALIB_H_
#define __ACCCALIB_H_

#include <string>
#include "PHTimeStamp.h"

#include "AccCalibPar.h"

class AccCalib
{
 public:
  AccCalib();
  virtual ~AccCalib();

  // get 
  float get_Npe(const int ich, const int adc);              // Number of photoelectron
  float get_Timing(const int ich, const int adc, const int tdc);  // Timing [ns]
  float get_Tof(const int ibox, const int* adc, const int* tdc);  // Time of Flight [ns]
  float get_Tdiff(const int ibox, const int* adc, const int* tdc); // Timing difference
  float get_Zpos(const int ibox, const int* adc, const int* tdc); // Local z position [cm]
  float get_adcpedvalue(const int ich, const int sec);
  float get_adcgainvalue(const int ich, const int sec);
  float get_tdcpedvalue(const int ich, const int sec);
  float get_tdcgainvalue(const int ich, const int sec);
  float get_slewingvalue(const int ich, const int sec);

  void set_adcpedstatus(const int ich, const int val);
  void set_adcgainstatus(const int ich, const int val);
  void set_tdcpedstatus(const int ich, const int val);
  void set_tdcgainstatus(const int ich, const int val);
  void set_slewingstatus(const int ich, const int val);

  void set_adcpedvalue(const int ich, const int sec, const float val);
  void set_adcgainvalue(const int ich, const int sec, const float val);
  void set_tdcpedvalue(const int ich, const int sec, const float val);
  void set_tdcgainvalue(const int ich, const int sec, const float val);
  void set_slewingvalue(const int ich, const int sec, const float val);

  // fetch parameter 
  int fetch(const int run);           // fetch calibration parameters from run number
  int fetch(const char* filename);    // fetch calibration parameters from file
  int fetch(std::string filename);

  // fetch parameter from run number
  int fetchAdcPedestal(const int run);
  int fetchAdcGain(const int run);
  int fetchTdcPedestal(const int run);
  int fetchTdcGain(const int run);
  int fetchSlewing(const int run);

  // fetch parameter from file for each calibration
  int fetchAdcPedestal(const char* filename);
  int fetchAdcGain(const char* filename);
  int fetchTdcPedestal(const char* filename);
  int fetchTdcGain(const char* filename);

  // write parameter
  int write(); // write calibration parameters to default file
  int write(const char* filename); // write calibration parameters to default file
  int write(std::string filename);

  // write parameter to file for each calibration
  int writeAdcPedestal(const char* filename);
  int writeAdcGain(const char* filename);
  int writeTdcPedestal(const char* filename);
  int writeTdcGain(const char* filename);
  int writeSlewing(const char* filename);

  int writeAdcPedestal(const std::string filename);
  int writeAdcGain(const std::string filename);
  int writeTdcPedestal(const std::string filename);
  int writeTdcGain(const std::string filename);
  int writeSlewing(const std::string filename);

  // update parameter to DB from this run to far future
  int updateAdcPedestal(const int beginrun, const int endrun=-1);
  int updateAdcGain(const int beginrun, const int endrun=-1);
  int updateTdcPedestal(const int beginrun, const int endrun=-1);
  int updateTdcGain(const int beginrun, const int endrun=-1);
  int updateSlewing(const int beginrun, const int endrun=-1);

  // update parameter to DB by TimeStamp
  int updateAdcPedestal(const PHTimeStamp& tstart, const PHTimeStamp& tstop);
  int updateAdcGain(const PHTimeStamp& tstart, const PHTimeStamp& tstop);
  int updateTdcPedestal(const PHTimeStamp& tstart, const PHTimeStamp& tstop);
  int updateTdcGain(const PHTimeStamp& tstart, const PHTimeStamp& tstop);
  int updateSlewing(const PHTimeStamp& tstart, const PHTimeStamp& tstop);

  void print(); // print all calibration parameters

//   AccCalibPar* get_adcpedestal() {return acc_adcpedestal;}
//   AccCalibPar* get_adcgain()  {return acc_adcgain;}
//   AccCalibPar* get_tdcpedestal() {return acc_tdcpedestal;}
//   AccCalibPar* get_tdcgain()  {return acc_tdcgain;}
//   AccCalibPar* get_slewing()  {return acc_slewing;}

  void Verbosity(const int v);

 private:

  static const int NSIGMA_PEDESTAL = 5;

  //!@name calibrations
  /*! 
  references are used in place of pointers. The constructor
  is called in the parent object constructor, and the objects are automatically 
  deleted when the parent object is.
  */
  //@{
  AccCalibPar acc_adcpedestal; // adc pedestal mean & sigma
  AccCalibPar acc_adcgain;     // 1 p.e. mean & sigma
  AccCalibPar acc_tdcpedestal; // tdc pedestal mean & sigma
  AccCalibPar acc_tdcgain;     // tdc conversion parameter
  AccCalibPar acc_slewing;     // slewing parameter
  //@}
  
  int verbosity;
};

#endif 
