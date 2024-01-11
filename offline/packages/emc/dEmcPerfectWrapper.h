#ifndef __DEMCPERFECTWRAPPER_H__
#define __DEMCPERFECTWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dEmcPerfect.h"
class dEmcPerfectWrapper: public PHTable
{
public:
  dEmcPerfectWrapper(const char* name = "dEmcPerfect", const size_t& max_rows = 1);
//  dEmcPerfectWrapper(const dEmcPerfectWrapper& source);
//  dEmcPerfectWrapper& operator=(const dEmcPerfectWrapper& source);

  ~dEmcPerfectWrapper();

  void* RawTableData();
  DEMCPERFECT_ST* TableData();

  DEMCPERFECT_ST& operator[](const size_t& row);
  const DEMCPERFECT_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_id(size_t n, short v) {
    fTableData[n].id = v;
  }
  short get_id(size_t n) const {
    return fTableData[n].id;
  }
  void set_true_track(size_t n, int v) {
    fTableData[n].true_track = v;
  }
  int get_true_track(size_t n) const {
    return fTableData[n].true_track;
  }
  void set_pid(size_t n, short v) {
    fTableData[n].pid = v;
  }
  short get_pid(size_t n) const {
    return fTableData[n].pid;
  }
  void set_xyz(size_t d0, size_t n, float v) {
    fTableData[n].xyz[d0] = v;
  }
  float get_xyz(size_t d0, size_t n) const {
    return fTableData[n].xyz[d0];
  }

private:
  DEMCPERFECT_ST* fTableData;

  ClassDef(dEmcPerfectWrapper,1)
};
#endif /*__DEMCPERFECTWRAPPER_H__*/
