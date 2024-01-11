#ifndef __BBCGHITWRAPPER_H__
#define __BBCGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "bbcghit.h"
class bbcghitWrapper: public PHTable
{
public:
  bbcghitWrapper(const char* name = "bbcghit", const size_t& max_rows = 1);
//  bbcghitWrapper(const bbcghitWrapper& source);
//  bbcghitWrapper& operator=(const bbcghitWrapper& source);

  ~bbcghitWrapper();

  void* RawTableData();
  BBCGHIT_ST* TableData();

  BBCGHIT_ST& operator[](const size_t& row);
  const BBCGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_pos(size_t d0, size_t n, float v) {
    fTableData[n].pos[d0] = v;
  }
  float get_pos(size_t d0, size_t n) const {
    return fTableData[n].pos[d0];
  }
  void set_mom(size_t d0, size_t n, float v) {
    fTableData[n].mom[d0] = v;
  }
  float get_mom(size_t d0, size_t n) const {
    return fTableData[n].mom[d0];
  }
  void set_del(size_t n, float v) {
    fTableData[n].del = v;
  }
  float get_del(size_t n) const {
    return fTableData[n].del;
  }
  void set_tof(size_t n, float v) {
    fTableData[n].tof = v;
  }
  float get_tof(size_t n) const {
    return fTableData[n].tof;
  }
  void set_len(size_t n, float v) {
    fTableData[n].len = v;
  }
  float get_len(size_t n) const {
    return fTableData[n].len;
  }
  void set_pmt(size_t n, short v) {
    fTableData[n].pmt = v;
  }
  short get_pmt(size_t n) const {
    return fTableData[n].pmt;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }

private:
  BBCGHIT_ST* fTableData;

  ClassDef(bbcghitWrapper,1)
};
#endif /*__BBCGHITWRAPPER_H__*/
