#ifndef __DPADGHITRAWWRAPPER_H__
#define __DPADGHITRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadGhitRaw.h"
class dPadGhitRawWrapper: public PHTable
{
public:
  dPadGhitRawWrapper(const char* name = "dPadGhitRaw", const size_t& max_rows = 1);
//  dPadGhitRawWrapper(const dPadGhitRawWrapper& source);
//  dPadGhitRawWrapper& operator=(const dPadGhitRawWrapper& source);

  ~dPadGhitRawWrapper();

  void* RawTableData();
  DPADGHITRAW_ST* TableData();

  DPADGHITRAW_ST& operator[](const size_t& row);
  const DPADGHITRAW_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }

private:
  DPADGHITRAW_ST* fTableData;

  ClassDef(dPadGhitRawWrapper,1)
};
#endif /*__DPADGHITRAWWRAPPER_H__*/
