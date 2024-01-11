#ifndef __DTECRAWWRAPPER_H__
#define __DTECRAWWRAPPER_H__

#include <cstddef>
#include "PHTable.hh"
#include "dTecRaw.h"
class dTecRawWrapper: public PHTable
{
public:
  dTecRawWrapper(const char* name = "dTecRaw", const size_t& max_rows = 1);
//  dTecRawWrapper(const dTecRawWrapper& source);
//  dTecRawWrapper& operator=(const dTecRawWrapper& source);

  ~dTecRawWrapper();

  void* RawTableData();
  DTECRAW_ST* TableData();

  DTECRAW_ST& operator[](const size_t& row);
  const DTECRAW_ST& operator[](const size_t& row) const;

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
  void set_plane(size_t n, short v) {
    fTableData[n].plane = v;
  }
  short get_plane(size_t n) const {
    return fTableData[n].plane;
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
  void set_wire(size_t n, short v) {
    fTableData[n].wire = v;
  }
  short get_wire(size_t n) const {
    return fTableData[n].wire;
  }
  void set_amplitude(size_t d0, size_t n, short v) {
    fTableData[n].amplitude[d0] = v;
  }
  short get_amplitude(size_t d0, size_t n) const {
    return fTableData[n].amplitude[d0];
  }

private:
  DTECRAW_ST* fTableData;

  ClassDef(dTecRawWrapper,1)
};
#endif /*__DTECRAWWRAPPER_H__*/
