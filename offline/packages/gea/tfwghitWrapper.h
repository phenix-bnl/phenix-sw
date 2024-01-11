#ifndef __TFWGHITWRAPPER_H__
#define __TFWGHITWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "tfwghit.h"
class tfwghitWrapper: public PHTable
{
public:
  tfwghitWrapper(const char* name = "tfwghit", const size_t& max_rows = 1);

  ~tfwghitWrapper();

  void* RawTableData();
  TFWGHIT_ST* TableData();

  TFWGHIT_ST& operator[](const size_t& row);
  const TFWGHIT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_xyzinglo(size_t d0, size_t n, float v) {
    fTableData[n].xyzinglo[d0] = v;
  }
  float get_xyzinglo(size_t d0, size_t n) const {
    return fTableData[n].xyzinglo[d0];
  }
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
  void set_pathLength(size_t n, float v) {
    fTableData[n].pathLength = v;
  }
  float get_pathLength(size_t n) const {
    return fTableData[n].pathLength;
  }
  void set_dele(size_t n, float v) {
    fTableData[n].dele = v;
  }
  float get_dele(size_t n) const {
    return fTableData[n].dele;
  }
  void set_track(size_t n, short v) {
    fTableData[n].track = v;
  }
  int get_track(size_t n) const {
    return fTableData[n].track;
  }
  void set_isubevent(size_t n, short v) {
    fTableData[n].isubevent = v;
  }
  int get_isubevent(size_t n) const {
    return fTableData[n].isubevent;
  }
  void set_panel(size_t n, short v) {
    fTableData[n].panel = v;
  }
  int get_panel(size_t n) const {
    return fTableData[n].panel;
  }
  void set_idpart(size_t n, int v) {
    fTableData[n].panel = v;
  }
  int get_idpart(size_t n) const {
    return fTableData[n].panel;
  }
  void set_mctrack(size_t n, int v) {
    fTableData[n].mctrack = v;
  }
  int get_mctrack(size_t n) const {
    return fTableData[n].mctrack;
  }
  void set_nFile(size_t n, int v) {
    fTableData[n].nFile = v;
  }
  int get_nFile(size_t n) const {
    return fTableData[n].nFile;
  }
private:
  TFWGHIT_ST* fTableData;

  ClassDef(tfwghitWrapper,1)
};
#endif /*__TFWGHITWRAPPER_H__*/
