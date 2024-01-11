#ifndef __DTECGHITRAWWRAPPER_H__
#define __DTECGHITRAWWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecGhitRaw.h"
class dTecGhitRawWrapper: public PHTable
{
public:
  dTecGhitRawWrapper(const char* name = "dTecGhitRaw", const size_t& max_rows = 1);
//  dTecGhitRawWrapper(const dTecGhitRawWrapper& source);
//  dTecGhitRawWrapper& operator=(const dTecGhitRawWrapper& source);

  ~dTecGhitRawWrapper();

  void* RawTableData();
  DTECGHITRAW_ST* TableData();

  DTECGHITRAW_ST& operator[](const size_t& row);
  const DTECGHITRAW_ST& operator[](const size_t& row) const;

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
  void set_binnum(size_t n, short v) {
    fTableData[n].binnum = v;
  }
  short get_binnum(size_t n) const {
    return fTableData[n].binnum;
  }
  void set_fraction(size_t n, float v) {
    fTableData[n].fraction = v;
  }
  float get_fraction(size_t n) const {
    return fTableData[n].fraction;
  }

private:
  DTECGHITRAW_ST* fTableData;

  ClassDef(dTecGhitRawWrapper,1)
};
#endif /*__DTECGHITRAWWRAPPER_H__*/
