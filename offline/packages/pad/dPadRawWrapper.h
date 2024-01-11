#ifndef __DPADRAWWRAPPER_H__
#define __DPADRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dPadRaw.h"
class dPadRawWrapper: public PHTable
{
public:
  dPadRawWrapper(const char* name = "dPadRaw", const size_t& max_rows = 1);
//  dPadRawWrapper(const dPadRawWrapper& source);
//  dPadRawWrapper& operator=(const dPadRawWrapper& source);

  ~dPadRawWrapper();

  void* RawTableData();
  DPADRAW_ST* TableData();

  DPADRAW_ST& operator[](const size_t& row);
  const DPADRAW_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_padx(size_t n, short v) {
    fTableData[n].padx = v;
  }
  short get_padx(size_t n) const {
    return fTableData[n].padx;
  }
  void set_padz(size_t n, short v) {
    fTableData[n].padz = v;
  }
  short get_padz(size_t n) const {
    return fTableData[n].padz;
  }
  void set_padtype(size_t n, short v) {
    fTableData[n].padtype = v;
  }
  short get_padtype(size_t n) const {
    return fTableData[n].padtype;
  }

private:
  DPADRAW_ST* fTableData;

  ClassDef(dPadRawWrapper,1)
};
#endif /*__DPADRAWWRAPPER_H__*/
