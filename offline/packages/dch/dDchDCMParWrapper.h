#ifndef __DDCHDCMPARWRAPPER_H__
#define __DDCHDCMPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchDCMPar.h"
class dDchDCMParWrapper: public PHTable
{
public:
  dDchDCMParWrapper(const char* name = "dDchDCMPar", const size_t& max_rows = 1);
  ~dDchDCMParWrapper();

  void* RawTableData();
  DDCHDCMPAR_ST* TableData();

  DDCHDCMPAR_ST& operator[](const size_t& row);
  const DDCHDCMPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_scheme(size_t n, unsigned int v) {
    fTableData[n].scheme = v;
  }
  unsigned int get_scheme(size_t n) const {
    return fTableData[n].scheme;
  }

private:
  DDCHDCMPAR_ST* fTableData;

  ClassDef(dDchDCMParWrapper,1)
};
#endif /*__DDCHDCMPARWRAPPER_H__*/
