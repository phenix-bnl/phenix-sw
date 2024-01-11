#ifndef __DDCHPERFPARWRAPPER_H__
#define __DDCHPERFPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchPerfPar.h"
class dDchPerfParWrapper: public PHTable
{
public:
  dDchPerfParWrapper(const char* name = "dDchPerfPar", const size_t& max_rows = 1);
  ~dDchPerfParWrapper();

  void* RawTableData();
  DDCHPERFPAR_ST* TableData();

  DDCHPERFPAR_ST& operator[](const size_t& row);
  const DDCHPERFPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }
  void set_localStudy(size_t n, short v) {
    fTableData[n].localStudy = v;
  }
  short get_localStudy(size_t n) const {
    return fTableData[n].localStudy;
  }

private:
  DDCHPERFPAR_ST* fTableData;

  ClassDef(dDchPerfParWrapper,1)
};
#endif /*__DDCHPERFPARWRAPPER_H__*/
