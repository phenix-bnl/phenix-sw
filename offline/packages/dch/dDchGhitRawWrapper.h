#ifndef __DDCHGHITRAWWRAPPER_H__
#define __DDCHGHITRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchGhitRaw.h"
class dDchGhitRawWrapper: public PHTable
{
public:
  dDchGhitRawWrapper(const char* name = "dDchGhitRaw", const size_t& max_rows = 1);
  ~dDchGhitRawWrapper();

  void* RawTableData();
  DDCHGHITRAW_ST* TableData();

  DDCHGHITRAW_ST& operator[](const size_t& row);
  const DDCHGHITRAW_ST& operator[](const size_t& row) const;

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
  DDCHGHITRAW_ST* fTableData;

  ClassDef(dDchGhitRawWrapper,1)
};
#endif /*__DDCHGHITRAWWRAPPER_H__*/
