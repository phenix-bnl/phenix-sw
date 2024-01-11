#ifndef __DTOFGDIGIRECWRAPPER_H__
#define __DTOFGDIGIRECWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dTofGdigiRec.h"
class dTofGdigiRecWrapper: public PHTable
{
public:
  dTofGdigiRecWrapper(const char* name = "dTofGdigiRec", const size_t& max_rows = 1);
  dTofGdigiRecWrapper(const dTofGdigiRecWrapper& source);
  dTofGdigiRecWrapper& operator=(const dTofGdigiRecWrapper& source);

  ~dTofGdigiRecWrapper();

  void* RawTableData();
  DTOFGDIGIREC_ST* TableData();

  DTOFGDIGIREC_ST& operator[](const size_t& row);
  const DTOFGDIGIREC_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_gdigiid(size_t n, short v) {
    fTableData[n].gdigiid = v;
  }
  short get_gdigiid(size_t n) const {
    return fTableData[n].gdigiid;
  }
  void set_slatid(size_t n, short v) {
    fTableData[n].slatid = v;
  }
  short get_slatid(size_t n) const {
    return fTableData[n].slatid;
  }
  void set_recid(size_t n, short v) {
    fTableData[n].recid = v;
  }
  short get_recid(size_t n) const {
    return fTableData[n].recid;
  }

private:
  DTOFGDIGIREC_ST* fTableData;

  ClassDef(dTofGdigiRecWrapper,1)
};
#endif /*__DTOFGDIGIRECWRAPPER_H__*/
