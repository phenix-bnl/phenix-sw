#ifndef __DBBCCALWRAPPER_H__
#define __DBBCCALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcCal.h"
class dBbcCalWrapper: public PHTable
{
public:
  dBbcCalWrapper(const char* name = "dBbcCal", const size_t& max_rows = 1);
  dBbcCalWrapper(const dBbcCalWrapper& source);
  dBbcCalWrapper& operator=(const dBbcCalWrapper& source);

  ~dBbcCalWrapper();

  void* RawTableData();
  DBBCCAL_ST* TableData();

  DBBCCAL_ST& operator[](const size_t& row);
  const DBBCCAL_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Pmt(size_t n, short v) {
    fTableData[n].Pmt = v;
  }
  short get_Pmt(size_t n) const {
    return fTableData[n].Pmt;
  }
  void set_AdcChGain(size_t n, float v) {
    fTableData[n].AdcChGain = v;
  }
  float get_AdcChGain(size_t n) const {
    return fTableData[n].AdcChGain;
  }
  void set_TdcChGain0(size_t n, float v) {
    fTableData[n].TdcChGain0 = v;
  }
  float get_TdcChGain0(size_t n) const {
    return fTableData[n].TdcChGain0;
  }
  void set_TdcChGain1(size_t n, float v) {
    fTableData[n].TdcChGain1 = v;
  }
  float get_TdcChGain1(size_t n) const {
    return fTableData[n].TdcChGain1;
  }
  void set_Pedestal(size_t n, float v) {
    fTableData[n].Pedestal = v;
  }
  float get_Pedestal(size_t n) const {
    return fTableData[n].Pedestal;
  }
  void set_AdcGainFac(size_t n, float v) {
    fTableData[n].AdcGainFac = v;
  }
  float get_AdcGainFac(size_t n) const {
    return fTableData[n].AdcGainFac;
  }
  void set_SlewParA(size_t n, float v) {
    fTableData[n].SlewParA = v;
  }
  float get_SlewParA(size_t n) const {
    return fTableData[n].SlewParA;
  }
  void set_TdcOffset0(size_t n, float v) {
    fTableData[n].TdcOffset0 = v;
  }
  float get_TdcOffset0(size_t n) const {
    return fTableData[n].TdcOffset0;
  }
  void set_TdcOffset1(size_t n, float v) {
    fTableData[n].TdcOffset1 = v;
  }
  float get_TdcOffset1(size_t n) const {
    return fTableData[n].TdcOffset1;
  }
  void set_MeanTransitTime(size_t n, float v) {
    fTableData[n].MeanTransitTime = v;
  }
  float get_MeanTransitTime(size_t n) const {
    return fTableData[n].MeanTransitTime;
  }

private:
  DBBCCAL_ST* fTableData;

  ClassDef(dBbcCalWrapper,1)
};
#endif /*__DBBCCALWRAPPER_H__*/
