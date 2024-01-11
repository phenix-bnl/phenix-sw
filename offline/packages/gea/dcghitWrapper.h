#ifndef __DCGHITWRAPPER_H__
#define __DCGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dcghit.h"
class dcghitWrapper: public PHTable
{
public:
  dcghitWrapper(const char* name = "dcghit", const size_t& max_rows = 1);
//  dcghitWrapper(const dcghitWrapper& source);
//  dcghitWrapper& operator=(const dcghitWrapper& source);

  ~dcghitWrapper();

  void* RawTableData();
  DCGHIT_ST* TableData();

  DCGHIT_ST& operator[](const size_t& row);
  const DCGHIT_ST& operator[](const size_t& row) const;

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
  void set_pathLength(size_t n, float v) {
    fTableData[n].pathLength = v;
  }
  float get_pathLength(size_t n) const {
    return fTableData[n].pathLength;
  }
  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_plane(size_t n, short v) {
    fTableData[n].plane = v;
  }
  short get_plane(size_t n) const {
    return fTableData[n].plane;
  }
  void set_cell(size_t n, short v) {
    fTableData[n].cell = v;
  }
  short get_cell(size_t n) const {
    return fTableData[n].cell;
  }
  void set_arm(size_t n, short v) {
    fTableData[n].arm = v;
  }
  short get_arm(size_t n) const {
    return fTableData[n].arm;
  }
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }

private:
  DCGHIT_ST* fTableData;

  ClassDef(dcghitWrapper,1)
};
#endif /*__DCGHITWRAPPER_H__*/
