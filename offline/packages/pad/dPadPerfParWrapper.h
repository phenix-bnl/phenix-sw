#ifndef __DPADPERFPARWRAPPER_H__
#define __DPADPERFPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadPerfPar.h"
class dPadPerfParWrapper: public PHTable
{
public:
  dPadPerfParWrapper(const char* name = "dPadPerfPar", const size_t& max_rows = 1);
//  dPadPerfParWrapper(const dPadPerfParWrapper& source);
//  dPadPerfParWrapper& operator=(const dPadPerfParWrapper& source);

  ~dPadPerfParWrapper();

  void* RawTableData();
  DPADPERFPAR_ST* TableData();

  DPADPERFPAR_ST& operator[](const size_t& row);
  const DPADPERFPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_pc1(size_t n, short v) {
    fTableData[n].pc1 = v;
  }
  short get_pc1(size_t n) const {
    return fTableData[n].pc1;
  }
  void set_pc2(size_t n, short v) {
    fTableData[n].pc2 = v;
  }
  short get_pc2(size_t n) const {
    return fTableData[n].pc2;
  }
  void set_pc3(size_t n, short v) {
    fTableData[n].pc3 = v;
  }
  short get_pc3(size_t n) const {
    return fTableData[n].pc3;
  }

private:
  DPADPERFPAR_ST* fTableData;

  ClassDef(dPadPerfParWrapper,1)
};
#endif /*__DPADPERFPARWRAPPER_H__*/
