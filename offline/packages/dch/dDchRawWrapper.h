#ifndef __DDCHRAWWRAPPER_H__
#define __DDCHRAWWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchRaw.h"
class dDchRawWrapper: public PHTable
{
public:
  dDchRawWrapper(const char* name = "dDchRaw", const size_t& max_rows = 1);
  ~dDchRawWrapper();

  void* RawTableData();
  DDCHRAW_ST* TableData();

  DDCHRAW_ST& operator[](const size_t& row);
  const DDCHRAW_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_global(size_t n, int v) {
    fTableData[n].global = v;
  }
  int get_global(size_t n) const {
    return fTableData[n].global;
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
  void set_edge(size_t n, short v) {
    fTableData[n].edge = v;
  }
  short get_edge(size_t n) const {
    return fTableData[n].edge;
  }
  void set_time(size_t n, int v) {
    fTableData[n].time = v;
  }
  int get_time(size_t n) const {
    return fTableData[n].time;
  }

private:
  DDCHRAW_ST* fTableData;

  ClassDef(dDchRawWrapper,1)
};
#endif /*__DDCHRAWWRAPPER_H__*/
