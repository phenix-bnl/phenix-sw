#ifndef __DDCHHITWRAPPER_H__
#define __DDCHHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchHit.h"
class dDchHitWrapper: public PHTable
{
public:
  dDchHitWrapper(const char* name = "dDchHit", const size_t& max_rows = 1);
  ~dDchHitWrapper();

  void* RawTableData();
  DDCHHIT_ST* TableData();

  DDCHHIT_ST& operator[](const size_t& row);
  const DDCHHIT_ST& operator[](const size_t& row) const;

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
  void set_cell(size_t n, short v) {
    fTableData[n].cell = v;
  }
  short get_cell(size_t n) const {
    return fTableData[n].cell;
  }
  void set_side(size_t n, short v) {
    fTableData[n].side = v;
  }
  short get_side(size_t n) const {
    return fTableData[n].side;
  }
  void set_distance(size_t n, float v) {
    fTableData[n].distance = v;
  }
  float get_distance(size_t n) const {
    return fTableData[n].distance;
  }
  void set_width(size_t n, float v) {
    fTableData[n].width = v;
  }
  float get_width(size_t n) const {
    return fTableData[n].width;
  }
  void set_time1(size_t n, int v) {
    fTableData[n].time1 = v;
  }
  int get_time1(size_t n) const {
    return fTableData[n].time1;
  }
  void set_time2(size_t n, int v) {
    fTableData[n].time2 = v;
  }
  int get_time2(size_t n) const {
    return fTableData[n].time2;
  }
  void set_idraw1(size_t n, short v) {
    fTableData[n].idraw1 = v;
  }
  short get_idraw1(size_t n) const {
    return fTableData[n].idraw1;
  }
  void set_idraw2(size_t n, short v) {
    fTableData[n].idraw2 = v;
  }
  short get_idraw2(size_t n) const {
    return fTableData[n].idraw2;
  }
  void set_idmirror(size_t n, short v) {
    fTableData[n].idmirror = v;
  }
  short get_idmirror(size_t n) const {
    return fTableData[n].idmirror;
  }
  void set_used(size_t n, short v) {
    fTableData[n].used = v;
  }
  short get_used(size_t n) const {
    return fTableData[n].used;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }
  void set_err(size_t d0, size_t n, float v) {
    fTableData[n].err[d0] = v;
  }
  float get_err(size_t d0, size_t n) const {
    return fTableData[n].err[d0];
  }
  void set_vxyz(size_t d0, size_t n, float v) {
    fTableData[n].vxyz[d0] = v;
  }
  float get_vxyz(size_t d0, size_t n) const {
    return fTableData[n].vxyz[d0];
  }

private:
  DDCHHIT_ST* fTableData;

  ClassDef(dDchHitWrapper,1)
};
#endif /*__DDCHHITWRAPPER_H__*/
