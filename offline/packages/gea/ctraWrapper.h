#ifndef __CTRAWRAPPER_H__
#define __CTRAWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "ctra.h"
class ctraWrapper: public PHTable
{
public:
  ctraWrapper(const char* name = "ctra", const size_t& max_rows = 1);
//  ctraWrapper(const ctraWrapper& source);
//  ctraWrapper& operator=(const ctraWrapper& source);

  ~ctraWrapper();

  void* RawTableData();
  CTRA_ST* TableData();

  CTRA_ST& operator[](const size_t& row);
  const CTRA_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_x(size_t n, float v) {
    fTableData[n].x = v;
  }
  float get_x(size_t n) const {
    return fTableData[n].x;
  }
  void set_y(size_t n, float v) {
    fTableData[n].y = v;
  }
  float get_y(size_t n) const {
    return fTableData[n].y;
  }
  void set_z(size_t n, float v) {
    fTableData[n].z = v;
  }
  float get_z(size_t n) const {
    return fTableData[n].z;
  }
  void set_pvx(size_t n, float v) {
    fTableData[n].pvx = v;
  }
  float get_pvx(size_t n) const {
    return fTableData[n].pvx;
  }
  void set_pvy(size_t n, float v) {
    fTableData[n].pvy = v;
  }
  float get_pvy(size_t n) const {
    return fTableData[n].pvy;
  }
  void set_pvz(size_t n, float v) {
    fTableData[n].pvz = v;
  }
  float get_pvz(size_t n) const {
    return fTableData[n].pvz;
  }
  void set_vx(size_t n, float v) {
    fTableData[n].vx = v;
  }
  float get_vx(size_t n) const {
    return fTableData[n].vx;
  }
  void set_vy(size_t n, float v) {
    fTableData[n].vy = v;
  }
  float get_vy(size_t n) const {
    return fTableData[n].vy;
  }
  void set_vz(size_t n, float v) {
    fTableData[n].vz = v;
  }
  float get_vz(size_t n) const {
    return fTableData[n].vz;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_itra(size_t n, short v) {
    fTableData[n].itra = v;
  }
  short get_itra(size_t n) const {
    return fTableData[n].itra;
  }
  void set_detector(size_t n, short v) {
    fTableData[n].detector = v;
  }
  short get_detector(size_t n) const {
    return fTableData[n].detector;
  }

private:
  CTRA_ST* fTableData;

  ClassDef(ctraWrapper,1)
};
#endif /*__CTRAWRAPPER_H__*/
