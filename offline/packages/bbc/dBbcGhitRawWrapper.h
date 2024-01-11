#ifndef __DBBCGHITRAWWRAPPER_H__
#define __DBBCGHITRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcGhitRaw.h"
class dBbcGhitRawWrapper: public PHTable
{
public:
  dBbcGhitRawWrapper(const char* name = "dBbcGhitRaw", const size_t& max_rows = 1);
  dBbcGhitRawWrapper(const dBbcGhitRawWrapper& source);
  dBbcGhitRawWrapper& operator=(const dBbcGhitRawWrapper& source);

  ~dBbcGhitRawWrapper();

  void* RawTableData();
  DBBCGHITRAW_ST* TableData();

  DBBCGHITRAW_ST& operator[](const size_t& row);
  const DBBCGHITRAW_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }
  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }

private:
  DBBCGHITRAW_ST* fTableData;

  ClassDef(dBbcGhitRawWrapper,1)
};
#endif /*__DBBCGHITRAWWRAPPER_H__*/
