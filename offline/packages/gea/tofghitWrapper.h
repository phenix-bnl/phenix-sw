#ifndef __TOFGHITWRAPPER_H__
#define __TOFGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "tofghit.h"
class tofghitWrapper: public PHTable
{
public:
  tofghitWrapper(const char* name = "tofghit", const size_t& max_rows = 1);
//  tofghitWrapper(const tofghitWrapper& source);
//  tofghitWrapper& operator=(const tofghitWrapper& source);

  ~tofghitWrapper();

  void* RawTableData();
  TOFGHIT_ST* TableData();

  TOFGHIT_ST& operator[](const size_t& row);
  const TOFGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pos_m(size_t d0, size_t n, float v) {
    fTableData[n].pos_m[d0] = v;
  }
  float get_pos_m(size_t d0, size_t n) const {
    return fTableData[n].pos_m[d0];
  }
  void set_pos_hit_slat(size_t n, float v) {
    fTableData[n].pos_hit_slat = v;
  }
  float get_pos_hit_slat(size_t n) const {
    return fTableData[n].pos_hit_slat;
  }
  void set_p_m(size_t d0, size_t n, float v) {
    fTableData[n].p_m[d0] = v;
  }
  float get_p_m(size_t d0, size_t n) const {
    return fTableData[n].p_m[d0];
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_dele(size_t n, float v) {
    fTableData[n].dele = v;
  }
  float get_dele(size_t n) const {
    return fTableData[n].dele;
  }
  void set_subvol(size_t n, short v) {
    fTableData[n].subvol = v;
  }
  short get_subvol(size_t n) const {
    return fTableData[n].subvol;
  }
  void set_panel(size_t n, short v) {
    fTableData[n].panel = v;
  }
  short get_panel(size_t n) const {
    return fTableData[n].panel;
  }
  void set_column(size_t n, short v) {
    fTableData[n].column = v;
  }
  short get_column(size_t n) const {
    return fTableData[n].column;
  }
  void set_pslat(size_t n, short v) {
    fTableData[n].pslat = v;
  }
  short get_pslat(size_t n) const {
    return fTableData[n].pslat;
  }
  void set_slat_seq(size_t n, short v) {
    fTableData[n].slat_seq = v;
  }
  short get_slat_seq(size_t n) const {
    return fTableData[n].slat_seq;
  }
  void set_partl(size_t n, short v) {
    fTableData[n].partl = v;
  }
  short get_partl(size_t n) const {
    return fTableData[n].partl;
  }
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }

private:
  TOFGHIT_ST* fTableData;

  ClassDef(tofghitWrapper,1)
};
#endif /*__TOFGHITWRAPPER_H__*/
