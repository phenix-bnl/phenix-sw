#ifndef __DPADDCMPARWRAPPER_H__
#define __DPADDCMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadDCMPar.h"
class dPadDCMParWrapper: public PHTable
{
public:
  dPadDCMParWrapper(const char* name = "dPadDCMPar", const size_t& max_rows = 1);
//  dPadDCMParWrapper(const dPadDCMParWrapper& source);
//  dPadDCMParWrapper& operator=(const dPadDCMParWrapper& source);

  ~dPadDCMParWrapper();

  void* RawTableData();
  DPADDCMPAR_ST* TableData();

  DPADDCMPAR_ST& operator[](const size_t& row);
  const DPADDCMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_debug(size_t n, short v) {
    fTableData[n].debug = v;
  }
  short get_debug(size_t n) const {
    return fTableData[n].debug;
  }
  void set_fout(size_t n, short v) {
    fTableData[n].fout = v;
  }
  short get_fout(size_t n) const {
    return fTableData[n].fout;
  }
  void set_scheme(size_t n, short v) {
    fTableData[n].scheme = v;
  }
  short get_scheme(size_t n) const {
    return fTableData[n].scheme;
  }
  void set_idx(size_t n, short v) {
    fTableData[n].idx = v;
  }
  short get_idx(size_t n) const {
    return fTableData[n].idx;
  }

private:
  DPADDCMPAR_ST* fTableData;

  ClassDef(dPadDCMParWrapper,1)
};
#endif /*__DPADDCMPARWRAPPER_H__*/
