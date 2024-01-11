#ifndef __DTOFGHITGDIGIWRAPPER_H__
#define __DTOFGHITGDIGIWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGhitGdigi.h"
class dTofGhitGdigiWrapper: public PHTable
{
public:
  dTofGhitGdigiWrapper(const char* name = "dTofGhitGdigi", const size_t& max_rows = 1);
  dTofGhitGdigiWrapper(const dTofGhitGdigiWrapper& source);
  dTofGhitGdigiWrapper& operator=(const dTofGhitGdigiWrapper& source);

  ~dTofGhitGdigiWrapper();

  void* RawTableData();
  DTOFGHITGDIGI_ST* TableData();

  DTOFGHITGDIGI_ST& operator[](const size_t& row);
  const DTOFGHITGDIGI_ST& operator[](const size_t& row) const;
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
  void set_gdigiid(size_t n, short v) {
    fTableData[n].gdigiid = v;
  }
  short get_gdigiid(size_t n) const {
    return fTableData[n].gdigiid;
  }

private:
  DTOFGHITGDIGI_ST* fTableData;

  ClassDef(dTofGhitGdigiWrapper,1)
};
#endif /*__DTOFGHITGDIGIWRAPPER_H__*/
