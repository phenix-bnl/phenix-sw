#ifndef __DCRKCALWRAPPER_H__
#define __DCRKCALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkCal.h"
class dCrkCalWrapper: public PHTable
{
public:
  dCrkCalWrapper(const char* name = "dCrkCal", const size_t& max_rows = 1);
//  dCrkCalWrapper(const dCrkCalWrapper& source);
//  dCrkCalWrapper& operator=(const dCrkCalWrapper& source);

  ~dCrkCalWrapper();

  void* RawTableData();
  DCRKCAL_ST* TableData();

  DCRKCAL_ST& operator[](const size_t& row);
  const DCRKCAL_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pmt(size_t n, short v) {
    fTableData[n].pmt = v;
  }
  short get_pmt(size_t n) const {
    return fTableData[n].pmt;
  }
  void set_adc_gain(size_t n, float v) {
    fTableData[n].adc_gain = v;
  }
  float get_adc_gain(size_t n) const {
    return fTableData[n].adc_gain;
  }
  void set_adc_ped(size_t n, float v) {
    fTableData[n].adc_ped = v;
  }
  float get_adc_ped(size_t n) const {
    return fTableData[n].adc_ped;
  }
  void set_tdc_clock(size_t n, float v) {
    fTableData[n].tdc_clock = v;
  }
  float get_tdc_clock(size_t n) const {
    return fTableData[n].tdc_clock;
  }
  void set_tdc_t0(size_t n, float v) {
    fTableData[n].tdc_t0 = v;
  }
  float get_tdc_t0(size_t n) const {
    return fTableData[n].tdc_t0;
  }
  void set_slew(size_t n, float v) {
    fTableData[n].slew = v;
  }
  float get_slew(size_t n) const {
    return fTableData[n].slew;
  }

private:
  DCRKCAL_ST* fTableData;

  ClassDef(dCrkCalWrapper,1)
};
#endif /*__DCRKCALWRAPPER_H__*/
