#ifndef __ITRPARWRAPPER_H__
#define __ITRPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "itrpar.h"
class itrparWrapper: public PHTable
{
public:
  itrparWrapper(const char* name = "itrpar", const size_t& max_rows = 1);
//  itrparWrapper(const itrparWrapper& source);
//  itrparWrapper& operator=(const itrparWrapper& source);

  ~itrparWrapper();

  void* RawTableData();
  ITRPAR_ST* TableData();

  ITRPAR_ST& operator[](const size_t& row);
  const ITRPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ncl1mx(size_t n, short v) {
    fTableData[n].ncl1mx = v;
  }
  short get_ncl1mx(size_t n) const {
    return fTableData[n].ncl1mx;
  }
  void set_idateDC(size_t n, int v) {
    fTableData[n].idateDC = v;
  }
  int get_idateDC(size_t n) const {
    return fTableData[n].idateDC;
  }
  void set_idatePC1(size_t n, int v) {
    fTableData[n].idatePC1 = v;
  }
  int get_idatePC1(size_t n) const {
    return fTableData[n].idatePC1;
  }

private:
  ITRPAR_ST* fTableData;

  ClassDef(itrparWrapper,1)
};
#endif /*__ITRPARWRAPPER_H__*/
