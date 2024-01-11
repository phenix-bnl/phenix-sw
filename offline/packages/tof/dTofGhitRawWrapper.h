#ifndef __DTOFGHITRAWWRAPPER_H__
#define __DTOFGHITRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGhitRaw.h"
class dTofGhitRawWrapper: public PHTable
{
public:
  dTofGhitRawWrapper(const char* name = "dTofGhitRaw", const size_t& max_rows = 1);
  dTofGhitRawWrapper(const dTofGhitRawWrapper& source);
  dTofGhitRawWrapper& operator=(const dTofGhitRawWrapper& source);

  ~dTofGhitRawWrapper();

  void* RawTableData();
  DTOFGHITRAW_ST* TableData();

  DTOFGHITRAW_ST& operator[](const size_t& row);
  const DTOFGHITRAW_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_ghitid(size_t n, short v) {
    fTableData[n].ghitid = v;
  }
  short get_ghitid(size_t n) const {
    return fTableData[n].ghitid;
  }
  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_rawid(size_t n, short v) {
    fTableData[n].rawid = v;
  }
  short get_rawid(size_t n) const {
    return fTableData[n].rawid;
  }

private:
  DTOFGHITRAW_ST* fTableData;

  ClassDef(dTofGhitRawWrapper,1)
};
#endif /*__DTOFGHITRAWWRAPPER_H__*/
