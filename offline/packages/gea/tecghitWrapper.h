#ifndef __TECGHITWRAPPER_H__
#define __TECGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "tecghit.h"
class tecghitWrapper: public PHTable
{
public:
  tecghitWrapper(const char* name = "tecghit", const size_t& max_rows = 1);
//  tecghitWrapper(const tecghitWrapper& source);
//  tecghitWrapper& operator=(const tecghitWrapper& source);

  ~tecghitWrapper();

  void* RawTableData();
  TECGHIT_ST* TableData();

  TECGHIT_ST& operator[](const size_t& row);
  const TECGHIT_ST& operator[](const size_t& row) const;

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
  void set_xyzinglo(size_t d0, size_t n, float v) {
    fTableData[n].xyzinglo[d0] = v;
  }
  float get_xyzinglo(size_t d0, size_t n) const {
    return fTableData[n].xyzinglo[d0];
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_dedx(size_t n, float v) {
    fTableData[n].dedx = v;
  }
  float get_dedx(size_t n) const {
    return fTableData[n].dedx;
  }
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
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }

private:
  TECGHIT_ST* fTableData;

  ClassDef(tecghitWrapper,1)
};
#endif /*__TECGHITWRAPPER_H__*/
