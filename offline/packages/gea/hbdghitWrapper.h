#ifndef __HBDGHITWRAPPER_H__
#define __HBDGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "hbdghit.h"
class hbdghitWrapper: public PHTable
{
public:
  hbdghitWrapper(const char* name = "hbdghit", const size_t& max_rows = 1);
//  hbdghitWrapper(const hbdghitWrapper& source);
//  hbdghitWrapper& operator=(const hbdghitWrapper& source);

  ~hbdghitWrapper();

  void* RawTableData();
  HBDGHIT_ST* TableData();

  HBDGHIT_ST& operator[](const size_t& row);
  const HBDGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_xyzinloc(size_t d0, size_t n, float v) {
    fTableData[n].xyzinloc[d0] = v;
  }
  float get_xyzinloc(size_t d0, size_t n) const {
    return fTableData[n].xyzinloc[d0];
  }
  void set_xyzoutloc(size_t d0, size_t n, float v) {
    fTableData[n].xyzoutloc[d0] = v;
  }
  float get_xyzoutloc(size_t d0, size_t n) const {
    return fTableData[n].xyzoutloc[d0];
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_sector(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_sector(size_t n) const {
    return fTableData[n].sector;
  }
  void set_detflag(size_t n, short v) {
    fTableData[n].sector = v;
  }
  short get_detflag(size_t n) const {
    return fTableData[n].sector;
  }
  void set_idpart(size_t n, int v) {
    fTableData[n].sector = v;
  }
  int get_idpart(size_t n) const {
    return fTableData[n].sector;
  }
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }

private:
  HBDGHIT_ST* fTableData;

  ClassDef(hbdghitWrapper,1)
};
#endif /*__HBDGHITWRAPPER_H__*/
