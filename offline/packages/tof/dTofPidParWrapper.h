#ifndef __DTOFPIDPARWRAPPER_H__
#define __DTOFPIDPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofPidPar.h"
class dTofPidParWrapper: public PHTable
{
public:
  dTofPidParWrapper(const char* name = "dTofPidPar", const size_t& max_rows = 1);
  dTofPidParWrapper(const dTofPidParWrapper& source);
  dTofPidParWrapper& operator=(const dTofPidParWrapper& source);

  ~dTofPidParWrapper();

  void* RawTableData();
  DTOFPIDPAR_ST* TableData();

  DTOFPIDPAR_ST& operator[](const size_t& row);
  const DTOFPIDPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_m2mean(size_t n, float v) {
    fTableData[n].m2mean = v;
  }
  float get_m2mean(size_t n) const {
    return fTableData[n].m2mean;
  }
  void set_cm2(size_t d0, size_t n, float v) {
    fTableData[n].cm2[d0] = v;
  }
  float get_cm2(size_t d0, size_t n) const {
    return fTableData[n].cm2[d0];
  }
  void set_m2min(size_t n, float v) {
    fTableData[n].m2min = v;
  }
  float get_m2min(size_t n) const {
    return fTableData[n].m2min;
  }
  void set_m2max(size_t n, float v) {
    fTableData[n].m2max = v;
  }
  float get_m2max(size_t n) const {
    return fTableData[n].m2max;
  }
  void set_pmin(size_t n, float v) {
    fTableData[n].pmin = v;
  }
  float get_pmin(size_t n) const {
    return fTableData[n].pmin;
  }
  void set_pmax(size_t n, float v) {
    fTableData[n].pmax = v;
  }
  float get_pmax(size_t n) const {
    return fTableData[n].pmax;
  }
  void set_charge(size_t n, short v) {
    fTableData[n].charge = v;
  }
  short get_charge(size_t n) const {
    return fTableData[n].charge;
  }
  void set_factor(size_t n, float v) {
    fTableData[n].factor = v;
  }
  float get_factor(size_t n) const {
    return fTableData[n].factor;
  }

private:
  DTOFPIDPAR_ST* fTableData;

  ClassDef(dTofPidParWrapper,1)
};
#endif /*__DTOFPIDPARWRAPPER_H__*/
