
#ifndef __ACCCALIBPAR_H_
#define __ACCCALIBPAR_H_

#include <string>
#include <vector>

#include "Acc.h"
#include "PHTimeStamp.h"

class AccCalibPar
{
 public:
  AccCalibPar();
  AccCalibPar(const char* name);
  virtual ~AccCalibPar();

  int get_Status(const int ich);
  float get_CalibPar(const int ich, const int seq);
  float get_CalibPar0(const int ich);
  float get_CalibPar1(const int ich);
  float get_CalibPar2(const int ich);
  float get_CalibPar3(const int ich);

  void set_Status(const int ich, const int val);
  void set_Par(const int ich, const int seq, const float val);
  void set_Par0(const int ich, const float val);
  void set_Par1(const int ich, const float val);
  void set_Par2(const int ich, const float val);
  void set_Par3(const int ich, const float val);

  int fetch(const int run);           // fetch calibration parameter from FD
  int fetch(const char* filename); // fetch calibration parameter from file
  int fetch(std::string filename); // fetch calibration parameter from file
  int fetch(const PHTimeStamp& time); // fetch calibration parameter from FD

  int write(const char* filename); // write calibration parameter to file
  int write(std::string filename); // write calibration parameter to file
  int update(const int beginrun, const int endrun=-1);       // update calibration parameter to FD
  int update(const PHTimeStamp& start, const PHTimeStamp& stop); // update calibration parameter to FD

  const char* GetName() const {return "AccCalib";} // get class name
  const std::string get_calibName() const {return calibName;} // get parameter name

  PHTimeStamp* get_startTime() {return &startTime;}
  PHTimeStamp* get_endTime() {return &endTime;}

  int isCalibrationOK() const {return calibrationOK;}
  void set_calibrationStatus(int flag) {calibrationOK = flag;}

  void Print(); // print calibration parameter
  void set_verbosity(const int v) {verbosity = v;}

 private:

  static const int NPAR = 4;
  std::vector<int> Status;  // array for calibration parameter
  std::vector<float> Par0;  // array for calibration parameter
  std::vector<float> Par1;  // array for calibration parameter
  std::vector<float> Par2;  // array for calibration parameter
  std::vector<float> Par3;  // array for calibration parameter

  std::string calibName;       // parameter name

  PHTimeStamp startTime; // start time for calibration parameter
  PHTimeStamp endTime;   // end time for calibration parameter

  // status flag
  int calibrationOK; // 0: failed 1: good
  int verbosity;

};

#endif 
