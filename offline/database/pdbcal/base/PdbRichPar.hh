//  Declaration of class PdbRichPar
//  Purpose: User defined storage class
//  Author: akiba

#ifndef __PDBRICHPAR_HH__
#define __PDBRICHPAR_HH__

#include <iosfwd>
#include "PdbCalChan.hh"
#include "PHTimeStamp.h"

class PdbRichPar : public PdbCalChan {
public:
  PdbRichPar();
  PdbRichPar(int PMTid, float param);
  virtual ~PdbRichPar();

  const char* classname() {
    const char *name = "PdbRichPar";
    return name;
  }
  virtual void print() const;
  void write(std::ostream&) const;
  bool read(std::istream&);

  void set_ok()  {d_status = STAT_OK;}
  void set_bad() {d_status = STAT_BAD;}
  bool is_ok()   {return d_status == STAT_OK;}
  bool is_bad()  {return d_status == STAT_BAD;}
  
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

  void setValues(int PMTid, float param);
  void getValues(int &PMTid, float &param);

private:
  int   d_status;         // status code
  int   d_PMTid;          // ID of RICH PMT (redundant)
  float d_param;          // parameter data (1 float)
  PHTimeStamp d_cal_time; // optional calibration time
                          // valid only when d_status == STAT_PREV 

  // symbolic name for status code.
  // STAT_OK   : status is OK. Fit was good. No problem.
  // STAT_BAD  : status is bad. The data for this PMT id is invalid.
  // STAT_PREV : When status of this PMT is bad, a previous value in the
  //             database is stored as "current" value. The d_cal_time
  //             is then set to the value of the previous calibration.
  //
  enum {STAT_OK = 1, STAT_BAD = -1, STAT_PREV = -2};

  ClassDef(PdbRichPar,1);
};

#endif /* __PDBRICHPAR_HH__ */
