#ifndef __CRKGHITWRAPPER_H__
#define __CRKGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "crkghit.h"
class crkghitWrapper: public PHTable
{
public:
  crkghitWrapper(const char* name = "crkghit", const size_t& max_rows = 1);
//  crkghitWrapper(const crkghitWrapper& source);
//  crkghitWrapper& operator=(const crkghitWrapper& source);

  ~crkghitWrapper();

  void* RawTableData();
  CRKGHIT_ST* TableData();

  CRKGHIT_ST& operator[](const size_t& row);
  const CRKGHIT_ST& operator[](const size_t& row) const;

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
  void set_px(size_t n, float v) {
    fTableData[n].px = v;
  }
  float get_px(size_t n) const {
    return fTableData[n].px;
  }
  void set_py(size_t n, float v) {
    fTableData[n].py = v;
  }
  float get_py(size_t n) const {
    return fTableData[n].py;
  }
  void set_pz(size_t n, float v) {
    fTableData[n].pz = v;
  }
  float get_pz(size_t n) const {
    return fTableData[n].pz;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_bp1(size_t n, float v) {
    fTableData[n].bp1 = v;
  }
  float get_bp1(size_t n) const {
    return fTableData[n].bp1;
  }
  void set_bp2(size_t n, float v) {
    fTableData[n].bp2 = v;
  }
  float get_bp2(size_t n) const {
    return fTableData[n].bp2;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_parent(size_t n, int v) {
    fTableData[n].parent = v;
  }
  int get_parent(size_t n) const {
    return fTableData[n].parent;
  }
  void set_pmt(size_t n, short v) {
    fTableData[n].pmt = v;
  }
  short get_pmt(size_t n) const {
    return fTableData[n].pmt;
  }
  void set_tra(size_t n, short v) {
    fTableData[n].tra = v;
  }
  short get_tra(size_t n) const {
    return fTableData[n].tra;
  }
  void set_nbf(size_t n, short v) {
    fTableData[n].nbf = v;
  }
  short get_nbf(size_t n) const {
    return fTableData[n].nbf;
  }
  void set_bi1(size_t n, short v) {
    fTableData[n].bi1 = v;
  }
  short get_bi1(size_t n) const {
    return fTableData[n].bi1;
  }
  void set_bi2(size_t n, short v) {
    fTableData[n].bi2 = v;
  }
  short get_bi2(size_t n) const {
    return fTableData[n].bi2;
  }

private:
  CRKGHIT_ST* fTableData;

  ClassDef(crkghitWrapper,1)
};
#endif /*__CRKGHITWRAPPER_H__*/
