#ifndef __DPADFEMPARWRAPPER_H__
#define __DPADFEMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadFEMPar.h"
class dPadFEMParWrapper: public PHTable
{
public:
  dPadFEMParWrapper(const char* name = "dPadFEMPar", const size_t& max_rows = 1);
//  dPadFEMParWrapper(const dPadFEMParWrapper& source);
//  dPadFEMParWrapper& operator=(const dPadFEMParWrapper& source);

  ~dPadFEMParWrapper();

  void* RawTableData();
  DPADFEMPAR_ST* TableData();

  DPADFEMPAR_ST& operator[](const size_t& row);
  const DPADFEMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pcnumber(size_t n, short v) {
    fTableData[n].pcnumber = v;
  }
  short get_pcnumber(size_t n) const {
    return fTableData[n].pcnumber;
  }
  void set_mode(size_t n, short v) {
    fTableData[n].mode = v;
  }
  short get_mode(size_t n) const {
    return fTableData[n].mode;
  }
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
  void set_skipg(size_t n, short v) {
    fTableData[n].skipg = v;
  }
  short get_skipg(size_t n) const {
    return fTableData[n].skipg;
  }
  void set_last(size_t n, unsigned int v) {
    fTableData[n].last = v;
  }
  unsigned int get_last(size_t n) const {
    return fTableData[n].last;
  }

private:
  DPADFEMPAR_ST* fTableData;

  ClassDef(dPadFEMParWrapper,1)
};
#endif /*__DPADFEMPARWRAPPER_H__*/
