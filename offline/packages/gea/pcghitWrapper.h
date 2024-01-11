#ifndef __PCGHITWRAPPER_H__
#define __PCGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "pcghit.h"
class pcghitWrapper: public PHTable
{
public:
  pcghitWrapper(const char* name = "pcghit", const size_t& max_rows = 1);
//  pcghitWrapper(const pcghitWrapper& source);
//  pcghitWrapper& operator=(const pcghitWrapper& source);

  ~pcghitWrapper();

  void* RawTableData();
  PCGHIT_ST* TableData();

  PCGHIT_ST& operator[](const size_t& row);
  const PCGHIT_ST& operator[](const size_t& row) const;

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
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }

private:
  PCGHIT_ST* fTableData;

  ClassDef(pcghitWrapper,1)
};
#endif /*__PCGHITWRAPPER_H__*/
