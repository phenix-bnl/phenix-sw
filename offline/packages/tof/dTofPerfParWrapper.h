#ifndef __DTOFPERFPARWRAPPER_H__
#define __DTOFPERFPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofPerfPar.h"
class dTofPerfParWrapper: public PHTable
{
public:
  dTofPerfParWrapper(const char* name = "dTofPerfPar", const size_t& max_rows = 1);
  dTofPerfParWrapper(const dTofPerfParWrapper& source);
  dTofPerfParWrapper& operator=(const dTofPerfParWrapper& source);

  ~dTofPerfParWrapper();

  void* RawTableData();
  DTOFPERFPAR_ST* TableData();

  DTOFPERFPAR_ST& operator[](const size_t& row);
  const DTOFPERFPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_verbose(size_t n, short v) {
    fTableData[n].verbose = v;
  }
  short get_verbose(size_t n) const {
    return fTableData[n].verbose;
  }

private:
  DTOFPERFPAR_ST* fTableData;

  ClassDef(dTofPerfParWrapper,1)
};
#endif /*__DTOFPERFPARWRAPPER_H__*/
