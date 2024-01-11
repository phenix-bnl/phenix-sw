#ifndef __DCRKUCALWRAPPER_H__
#define __DCRKUCALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkUcal.h"
class dCrkUcalWrapper: public PHTable
{
public:
  dCrkUcalWrapper(const char* name = "dCrkUcal", const size_t& max_rows = 1);
//  dCrkUcalWrapper(const dCrkUcalWrapper& source);
//  dCrkUcalWrapper& operator=(const dCrkUcalWrapper& source);

  ~dCrkUcalWrapper();

  void* RawTableData();
  DCRKUCAL_ST* TableData();

  DCRKUCAL_ST& operator[](const size_t& row);
  const DCRKUCAL_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pmt(size_t n, short v) {
    fTableData[n].pmt = v;
  }
  short get_pmt(size_t n) const {
    return fTableData[n].pmt;
  }
  void set_gain(size_t n, float v) {
    fTableData[n].gain = v;
  }
  float get_gain(size_t n) const {
    return fTableData[n].gain;
  }
  void set_ped(size_t n, float v) {
    fTableData[n].ped = v;
  }
  float get_ped(size_t n) const {
    return fTableData[n].ped;
  }
  void set_clock(size_t n, float v) {
    fTableData[n].clock = v;
  }
  float get_clock(size_t n) const {
    return fTableData[n].clock;
  }
  void set_t0(size_t n, float v) {
    fTableData[n].t0 = v;
  }
  float get_t0(size_t n) const {
    return fTableData[n].t0;
  }
  void set_slew(size_t n, float v) {
    fTableData[n].slew = v;
  }
  float get_slew(size_t n) const {
    return fTableData[n].slew;
  }
  void set_N0p(size_t n, float v) {
    fTableData[n].N0p = v;
  }
  float get_N0p(size_t n) const {
    return fTableData[n].N0p;
  }
  void set_P_noise(size_t n, float v) {
    fTableData[n].P_noise = v;
  }
  float get_P_noise(size_t n) const {
    return fTableData[n].P_noise;
  }
  void set_mean_noise(size_t n, float v) {
    fTableData[n].mean_noise = v;
  }
  float get_mean_noise(size_t n) const {
    return fTableData[n].mean_noise;
  }
  void set_sigma_pe(size_t n, float v) {
    fTableData[n].sigma_pe = v;
  }
  float get_sigma_pe(size_t n) const {
    return fTableData[n].sigma_pe;
  }
  void set_sigma_t(size_t n, float v) {
    fTableData[n].sigma_t = v;
  }
  float get_sigma_t(size_t n) const {
    return fTableData[n].sigma_t;
  }

private:
  DCRKUCAL_ST* fTableData;

  ClassDef(dCrkUcalWrapper,1)
};
#endif /*__DCRKUCALWRAPPER_H__*/
