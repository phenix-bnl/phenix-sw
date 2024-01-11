#ifndef __DCRKRAWFEMPARWRAPPER_H__
#define __DCRKRAWFEMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dCrkRawFEMpar.h"
class dCrkRawFEMparWrapper: public PHTable
{
public:
  dCrkRawFEMparWrapper(const char* name = "dCrkRawFEMpar", const size_t& max_rows = 1);
//  dCrkRawFEMparWrapper(const dCrkRawFEMparWrapper& source);
//  dCrkRawFEMparWrapper& operator=(const dCrkRawFEMparWrapper& source);

  ~dCrkRawFEMparWrapper();

  void* RawTableData();
  DCRKRAWFEMPAR_ST* TableData();

  DCRKRAWFEMPAR_ST& operator[](const size_t& row);
  const DCRKRAWFEMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_mplex(size_t n, unsigned short v) {
    fTableData[n].mplex = v;
  }
  unsigned short get_mplex(size_t n) const {
    return fTableData[n].mplex;
  }
  void set_fem_mask(size_t n, unsigned short v) {
    fTableData[n].fem_mask = v;
  }
  unsigned short get_fem_mask(size_t n) const {
    return fTableData[n].fem_mask;
  }

private:
  DCRKRAWFEMPAR_ST* fTableData;

  ClassDef(dCrkRawFEMparWrapper,1)
};
#endif /*__DCRKRAWFEMPARWRAPPER_H__*/
