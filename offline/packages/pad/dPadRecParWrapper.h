#ifndef __DPADRECPARWRAPPER_H__
#define __DPADRECPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadRecPar.h"
class dPadRecParWrapper: public PHTable
{
public:
  dPadRecParWrapper(const char* name = "dPadRecPar", const size_t& max_rows = 1);
//  dPadRecParWrapper(const dPadRecParWrapper& source);
//  dPadRecParWrapper& operator=(const dPadRecParWrapper& source);

  ~dPadRecParWrapper();

  void* RawTableData();
  DPADRECPAR_ST* TableData();

  DPADRECPAR_ST& operator[](const size_t& row);
  const DPADRECPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_pcnumber(size_t n, short v) {
    fTableData[n].pcnumber = v;
  }
  short get_pcnumber(size_t n) const {
    return fTableData[n].pcnumber;
  }
  void set_method(size_t d0, size_t n, short v) {
    fTableData[n].method[d0] = v;
  }
  short get_method(size_t d0, size_t n) const {
    return fTableData[n].method[d0];
  }

private:
  DPADRECPAR_ST* fTableData;

  ClassDef(dPadRecParWrapper,1)
};
#endif /*__DPADRECPARWRAPPER_H__*/
