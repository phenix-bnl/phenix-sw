#ifndef __DZDCOUTWRAPPER_H__
#define __DZDCOUTWRAPPER_H__

//#include <stddef.h>
#include <cstddef>
#include "PHTable.hh"
#include "dZdcOut.h"

class dZdcOutWrapper: public PHTable
{
public:
  dZdcOutWrapper(const char* name = "dZdcOut", const size_t& max_rows = 1);
  dZdcOutWrapper(const dZdcOutWrapper& source);
  dZdcOutWrapper& operator=(const dZdcOutWrapper& source);

  ~dZdcOutWrapper();

  void* RawTableData();
  DZDCOUT_ST* TableData();

  DZDCOUT_ST& operator[](const size_t& row);
  const DZDCOUT_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Charge(size_t d0, size_t n, float v) {
    fTableData[n].Charge[d0] = v;
  }
  float get_Charge(size_t d0, size_t n) const {
    return fTableData[n].Charge[d0];
  }
  void set_Timing0(size_t d0, size_t n, float v) {
    fTableData[n].Timing0[d0] = v;
  }
  float get_Timing0(size_t d0, size_t n) const {
    return fTableData[n].Timing0[d0];
  }
  void set_Timing1(size_t d0, size_t n, float v) {
    fTableData[n].Timing1[d0] = v;
  }
  float get_Timing1(size_t d0, size_t n) const {
    return fTableData[n].Timing1[d0];
  }
  void set_Energy(size_t d0, size_t n, float v) {
    fTableData[n].Energy[d0] = v;
  }
  float get_Energy(size_t d0, size_t n) const {
    return fTableData[n].Energy[d0];
  }
  void set_Timing(size_t d0, size_t n, float v) {
    fTableData[n].Timing[d0] = v;
  }
  float get_Timing(size_t d0, size_t n) const {
    return fTableData[n].Timing[d0];
  }
  void set_Zvertex(size_t n, float v) {
    fTableData[n].Zvertex = v;
  }
  float get_Zvertex(size_t n) const {
    return fTableData[n].Zvertex;
  }
  void set_ZvertexError(size_t n, float v) {
    fTableData[n].ZvertexError = v;
  }
  float get_ZvertexError(size_t n) const {
    return fTableData[n].ZvertexError;
  }
  void set_TimeZero(size_t n, float v) {
    fTableData[n].TimeZero = v;
  }
  float get_TimeZero(size_t n) const {
    return fTableData[n].TimeZero;
  }
  void set_timeZeroError(size_t n, float v) {
    fTableData[n].timeZeroError = v;
  }
  float get_timeZeroError(size_t n) const {
    return fTableData[n].timeZeroError;
  }

private:
  DZDCOUT_ST* fTableData;

  ClassDef(dZdcOutWrapper,1)
};
#endif /*__DZDCOUTWRAPPER_H__*/
