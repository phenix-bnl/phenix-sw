#ifndef __MUMHITSWRAPPER_H__
#define __MUMHITSWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "mumhits.h"
class mumhitsWrapper: public PHTable
{
public:
  mumhitsWrapper(const char* name = "mumhits", const size_t& max_rows = 1);
//  mumhitsWrapper(const mumhitsWrapper& source);
//  mumhitsWrapper& operator=(const mumhitsWrapper& source);

  ~mumhitsWrapper();

  void* RawTableData();
  MUMHITS_ST* TableData();

  MUMHITS_ST& operator[](const size_t& row);
  const MUMHITS_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_t(size_t n, float v) {
    fTableData[n].t = v;
  }
  float get_t(size_t n) const {
    return fTableData[n].t;
  }
  void set_e(size_t n, float v) {
    fTableData[n].e = v;
  }
  float get_e(size_t n) const {
    return fTableData[n].e;
  }
  void set_x(size_t d0, size_t n, float v) {
    fTableData[n].x[d0] = v;
  }
  float get_x(size_t d0, size_t n) const {
    return fTableData[n].x[d0];
  }
  void set_p(size_t d0, size_t n, float v) {
    fTableData[n].p[d0] = v;
  }
  float get_p(size_t d0, size_t n) const {
    return fTableData[n].p[d0];
  }
  void set_track(size_t n, int v) {
    fTableData[n].track = v;
  }
  int get_track(size_t n) const {
    return fTableData[n].track;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_plane(size_t n, short v) {
    fTableData[n].plane = v;
  }
  short get_plane(size_t n) const {
    return fTableData[n].plane;
  }

private:
  MUMHITS_ST* fTableData;

  ClassDef(mumhitsWrapper,1)
};
#endif /*__MUMHITSWRAPPER_H__*/
