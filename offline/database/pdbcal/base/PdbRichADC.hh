//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbRichADC
//
//  Purpose: User defined storage class
//
//  Description:
//    This is a class to store ADC calibration data of RICH
//    In ADC calibration, the following parameters are determined
//       pedestal, pedestal witdth
//       1 photo-electron peak, 1 p.e. width (r.m.s)
//       # of counts in pedestal, 1 p.e. peak, 2 p.e. peak, and 3 p.e. peak
//    All of those values are stored in the database.
//
//  Author: akiba
//-----------------------------------------------------------------------------
#ifndef __PDBRICHADC_HH__
#define __PDBRICHADC_HH__

#include <iostream>
#include "PHTimeStamp.h"
#include "PdbCalChan.hh"

class PdbRichADC : public PdbCalChan {
public:
  PdbRichADC();
  PdbRichADC(PdbRichADC const& rhs);
  PdbRichADC & operator = (PdbRichADC const& rhs);

  PdbRichADC(int PMTid,
	     float ped, float pedw, float PE, float PEw,
	     float Nped, float N1, float N2, float N3,
	     float chisqr);
  virtual ~PdbRichADC();
  const char* classname() {
    const char *name = "PdbRichADC";
    return name;
  }
  virtual void print() const;
  void write(std::ostream &os) const;
  bool read(std::istream &is);

  void set_ok()  {d_status = STAT_OK;}
  void set_bad() {d_status = STAT_BAD;}
  void set_small() {d_status = STAT_SM;}
  void set_empty() {d_status = STAT_EM;}
  void set_nohist() {d_status = STAT_NH;}

  bool is_ok()   {return d_status == STAT_OK;}
  bool is_bad()  {return d_status == STAT_BAD;}
  bool is_empty() {return d_status == STAT_EM;}
  bool is_nohist() {return d_status == STAT_NH;}
  bool is_small() {return d_status == STAT_SM;}

  void set_prev(PHTimeStamp cal_time) {
    d_status = STAT_PREV;
    d_cal_time = cal_time;
  }
  bool is_prev() {return d_status == STAT_PREV;}
  bool get_cal_time(PHTimeStamp &cal_time) {
   if( d_status == STAT_PREV ) {
     cal_time = d_cal_time;
     return true;
   } else return false;
  }

  void setValues(int PMTid,
		 float ped, float pedw, float PE, float PEw,
		 float Nped, float N1, float N2, float N3,
		 float chisqr);
  void getValues(int &PMTid,
		 float &ped, float &pedw, float &PE, float &PEw,
		 float &Nped, float &N1, float &N2, float &N3,
		 float &chisqr);

private:
  int   d_status;     // status code.
  int   d_PMTid;      // PMT id (unique id of RICH PMTs. 0 to 5119)
  float d_pedestal;   // pedestal position
  float d_ped_width;  // r.m.s. width of the pedestal
  float d_PE_pos;     // peak position of 1 photo-electron
  float d_PE_width;   // r.m.s. width of 1 photo-electron peak
  float d_Nped;       // number of counts in pedestal
  float d_N1PE;       // number of counts in the 1 p.e. peak
  float d_N2PE;       // number of counts in the 2 p.e. peak
  float d_N3PE;       // number of counts in the 3 p.e. peak
  float d_chisqr;     // chi-squares per D.O.F. of the fits
  PHTimeStamp d_cal_time; // optinal calibration time.
                          // valid only when d_status == STAT_PREV 
  // symbolic name for status code.
  // STAT_OK   : status is OK. Fit was good. No problem.
  // STAT_BAD  : status is bad. The data for this PMT id is invalid.
  // STAT_PREV : When status of this PMT is bad, a previous value in the
  //             database is stored as "current" value. The d_cal_time
  //             is then set to the value of the previous calibration.
  //             This value is not stored in the database
  // Following code is stored for the data of online/offine calbiration
  // histogram status
  //
  // STAT_SM   : yeild in calibration histogram is small.
  // STAT_EM   : historgram is empty
  // STAT_NH   : no histogram (the PMT channel is turned off, etc)
  // 
  enum {STAT_OK = 1, STAT_BAD = -1, STAT_PREV = -2, STAT_SM = -3,
	STAT_EM = -4, STAT_NH = -5};

  ClassDef(PdbRichADC,1);

};

#endif /* __PDBRICHADC_HH__ */
