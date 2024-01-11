#ifndef __DCRKRAWWRAPPER_H__
#define __DCRKRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkRaw.h"
class dCrkRawWrapper: public PHTable
{
public:
  dCrkRawWrapper(const char* name = "dCrkRaw", const size_t& max_rows = 1);
//  dCrkRawWrapper(const dCrkRawWrapper& source);
//  dCrkRawWrapper& operator=(const dCrkRawWrapper& source);

  ~dCrkRawWrapper();

  void* RawTableData();
  DCRKRAW_ST* TableData();

  DCRKRAW_ST& operator[](const size_t& row);
  const DCRKRAW_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pmt(size_t n, short v) {
    fTableData[n].pmt = v;
  }
  short get_pmt(size_t n) const {
    return fTableData[n].pmt;
  }
  void set_adc(size_t n, short v) {
    fTableData[n].adc = v;
  }
  short get_adc(size_t n) const {
    return fTableData[n].adc;
  }
  void set_tdc(size_t n, short v) {
    fTableData[n].tdc = v;
  }
  short get_tdc(size_t n) const {
    return fTableData[n].tdc;
  }

private:
  DCRKRAW_ST* fTableData;

  ClassDef(dCrkRawWrapper,1)
};
#endif /*__DCRKRAWWRAPPER_H__*/
